{-
Copyright 2015–2018 comatose contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}
module Render (render) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.List (nub, sort, sortBy, isPrefixOf)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson (encode)
import Text.BibTeX.Entry as E
import Text.BibTeX.Format (entry)
import qualified Data.Map as M
import Text.Parsec.Error
import Text.ParserCombinators.Parsec.Prim
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BS
import Network.URI (isReserved, escapeURIString)
import Lucid

import Util
import Db

maybeToHtml = maybe mempty toHtml

scholarSearch q = "http://scholar.google.com/scholar?q=" ++ escapeURIString isReserved q

resolveDoi :: String -> String
resolveDoi q = "https://doi.org/" ++ q

-- |List of protocol features
protofeatures :: Database -> Protocol -> Html ()
protofeatures _ p | (M.size $ pfeatures p) == 0 = mempty
protofeatures db p = do
        dt_ "Features"
        dd_ $ ul_ [class_ "features"] $ forM_ (sort $ M.keys $ pfeatures p) item
    where
        item :: String -> Html ()
        item name = li_ [data_ "id" (T.pack name)] $ do
            toHtml $ maybeLookup $ getFeatureBase name
            ": "
            toHtml $ maybeLookup name
        maybeLookup name = maybe ("" :: String) fname $ M.lookup name (dfeatures db)

-- |List of protocol publications
protopapers :: [T] -> Html ()
protopapers pubs | length pubs == 0 = mempty
protopapers pubs = do
    dt_ "Published in"
    dd_ [class_ "ref"] $ if length pubs == 1
        then p_ $ bibentry $ head pubs
        else ol_ $ forM_ pubs (li_ . bibentry)

-- |Protocol description
protodesc :: Protocol -> Html ()
protodesc Protocol { pdescription = Nothing } = mempty
protodesc Protocol { pdescription = Just desc } = p_ $ toHtml desc

protorelated :: Database -> Protocol -> Html ()
protorelated _ p | null $ prelated p = mempty
protorelated db p =
    let
        algos = dalgos db
        lookup k = M.lookup k algos >>= \y -> return (k, y)
        rel = catMaybes $ map lookup $ prelated p
    in do
        dt_ "Related"
        dd_ [class_ "related"] $ ul_ [class_ "list-inline"] $ forM_ rel $
            \(ident, x) -> li_ [class_ "list-inline-item"] $ a_ [href_ (T.pack $ '#':ident)] $ toHtml $ pname x

-- |One protocol
protoentry :: Database -> (String, Protocol) -> Html ()
protoentry db (ident, p) =
    let
        pubs = catMaybes $ protoPublications db p
        firstpub = safeHead pubs
        field key = firstpub >>= (return . E.fields) >>= lookup key
        bodyIdent = concat [ident, "-body"]
    in
        section_ [
            id_ $ T.pack ident
            , class_ "protocol card"
            , data_ "rank" (T.pack $ show $ prank p)
            ] $ do
            div_ [class_ "card-header", role_ "tab"] $ do
                h3_ [class_ "name h5"] $ do
                    a_ [href_ $ T.pack $ '#':ident, title_ "permalink", data_ "toggle" "collapse", data_ "parent" "#protocols", data_ "target" $ T.pack $ '#':bodyIdent, class_ "collapsed dropdown-toggle"] $ toHtml $ pname p
                    " "
                    maybe "" (small_ [class_ "longname text-muted"] . toHtml) $ plongname p
            div_ [class_ "collapse", id_ $ T.pack bodyIdent, role_ "tabpanel"] $ do
                div_ [class_ "card-body"] $ do
                    protodesc p
                    dl_ $ do
                        protopapers pubs
                        protofeatures db p
                        protorelated db p

extcss url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

extjs :: T.Text -> Html ()
extjs url = script_ [type_ "text/javascript", charset_ "utf8", src_ url] ("" :: T.Text)

-- | Try very hard to find an appropriate URL for the bibentry, DOIs are prefered
bibentryurl bib = safeHead $ catMaybes [doi, url]
    where
        fields = E.fields bib
        doi = lookup "doi" fields >>= return . resolveDoi
        url = lookup "url" fields

-- | Format bibliography/references item
bibentry :: E.T -> Html ()
bibentry bib = do
    let
        fields = E.fields bib
        htmlLookup k = maybeToHtml $ lookup k fields
    maybe
        (span_ [class_ "title"] $ htmlLookup "title")
        (\x -> a_ [href_ $ T.pack $ x, class_ "title"] $ htmlLookup "title")
        (bibentryurl bib)
    ", "
    span_ [class_ "author"] $ htmlLookup "author"
    ", "
    span_ [class_ "year"] $ maybe mempty (toHtml . show) $ publicationYear bib
    " "
    button_ [type_ "button", class_ "btn btn-light btn-sm", data_ "toggle" "popover", title_ "BibTeX", data_ "content" $ T.pack $ entry bib] "BibTeX"

-- | References section
references :: [E.T] -> Html ()
references attrib = div_ $ do
        h3_ [id_ "references"] "References"
        ol_ $ forM_ attrib (li_ . bibentry)

-- | What is this?!
introduction :: Database -> Html ()
introduction db = let
        (firstyear, lastyear) = minMaxPublicationYears db
    in div_ $ do
        h1_ [class_ "display-3"] "comatose"
        p_ [class_ "lead"] $ do
            "The comprehensive MAC taxonomy database (comatose) is a collection of "
            toHtml $ show $ algorithmCount db
            " wireless media/medium access protocols published between "
            toHtml $ show firstyear
            " and "
            toHtml $ show lastyear
            "."
        p_ [class_ "lead"] $ a_ [class_ "btn btn-primary btn-lg", href_ "#about", role_ "button"] "Learn more"

featuresFilter :: Database -> Html ()
featuresFilter db = select_ [multiple_ "", id_ "filter-feature", class_ "form-control", placeholder_ "Filter by feature or name"] $ forM_ (M.toList $ getFeaturesByLevel db 0) $ \(baseident, basefeature) -> do
    optgroup_ [label_ $ T.pack $ fname basefeature] $ do
        forM_ (M.toList $ getFeaturesByBase db baseident) $ \(ident, feature) -> do
            option_ [value_ $ T.pack $ concat ["tag:", ident]] $ toHtml $ fname feature

-- |List of protocol features
features :: Database -> Html ()
features db = dl_ [class_ "row learnmore-features"] $ forM_ (M.toList $ getFeaturesByLevel db 0) $ \(baseident, basefeature) -> do
        dt_ [class_ "col-sm-3"] $ h4_ $ toHtml $ fname basefeature
        dd_ [class_ "col-sm-9"] $ do
            maybe mempty (p_ . toHtml) $ fdescription basefeature
            dl_ $ forM_ (M.toList $ getFeaturesByBase db baseident) $ \(ident, feature) -> do
                dt_ $ toHtml $ fname feature
                maybe mempty (dd_ . toHtml) $ fdescription feature

about :: Database -> [E.T] -> Html ()
about db attrib = let
        (firstyear, lastyear) = minMaxPublicationYears db
        categoryCount = M.size $ getFeaturesByLevel db 0
    in do
    div_ [id_ "about"] $ do
        h2_ "About"
        p_ "Since the publication of the ALOHA MAC protocol in 1970 the scientific community has proposed a large number of wireless medium access control (MAC) protocols.  This results in two issues.  Firstly, name collisions: Most of the single-letter abbreviations like L MAC, M MAC, O MAC and so on are already used multiple times, some up to five times. Even two-letter abbreviations like AS MAC are used three times. This makes it harder than necessary to distinguish different protocols purely based on their name. Secondly, it is very likely that some of these publications are (unintentional) reinventions of previous protocols."
        figure_ [id_ "pubHistogram"] $ do
            div_ $ script_ $ T.concat ["let yearHistData = ", decodeUtf8 $ BS.toStrict $ encode $ publicationYearHist db, ";"]
            figcaption_ "Publication count by year"
        p_ $ do
            a_ [href_ "#references"] "Surveys"
            " are usually limited to a small subset of protocols due to time constraints and their target medium, printed journals, which limits the page count.  They are static in two ways: Once published they cannot be modified or updated.  And secondly printed surveys cannot provide interactivity like feature-based filtering and searching.  Additionally their results are not reusable and extendable since they are not machine-readable."
        p_ $ do
            "The comprehensive MAC taxonomy database (comatose), aims to fix these problems. It lists most known scientific MAC protocol proposals including their short and long name, a description and refererences the publication it originated from. Its code is public and open source and thus can be updated whenever new research appears. The database is machine-readable and searchable by humans through a browser interface. It also assigns tags or features to each protocol to aid finding protocols with specific properties. These features are grouped into "
            toHtml $ show categoryCount
            " categories. For some categories features are mutually exclusive."
        features db
        h3_ "Implementation"
        p_ $ do
            "Comatose uses two separate databases.  The first one contains basic information about a protocol, like name, description and features, in a "
            a_ [href_ "http://yaml.org/"] "YAML"
            " file.  This file format is human and machine-readable at the same time and thus easy to maintain.  Additional software like an SQL database server is not required.  The second database is a standard BibTeX file.  Since TeX is used for a lot of scientific publications these records usually exist already and can be copied, as well as reused for new publications.  Therefore, both databases should provide value beyond the scope of this project."

        h3_ "Contributing"
        p_ $ do
            "As mentioned earlier, this database is not an exhaustive list of MAC protocols as long as new protocols are invented.  Due to the large number of protocols listed some of them are still lacking descriptions and tags. If you want to help "
            a_ [href_ "mailto:lars+comatose@6xq.net"] "send an email with your suggestions"
            " or clone the repository from "
            a_ [href_ "https://github.com/PromyLOPh/comatose"] "GitHub"
            ", edit the database and create a pull request."
        h3_ "Acknowledgements"
        p_ $ do
            "This work has been partly funded by the "
            a_ [href_ "https://www.bmbf.de/en/index.html"] "German Federal Ministry of Education and Research (BMBF)"
            " within the project “TreuFunk” (grant number 16KIS0236)."
            img_ [src_ "bmbf_logo_fremdpublikation.svg", class_ "bmbflogo" ]
        references (sortBy (compare `on` publicationYear) attrib)
    
-- | The list of protocols
protocols :: Database -> Html ()
protocols db = section_ [id_ "protocols", role_ "tablist"] $ forM_ (M.toList $ dalgos db) (protoentry db)

-- |Page template
page db attrib = doctypehtml_ $ do
    head_ $ do
        title_ "comatose – Comprehensive MAC Taxonomy Database"
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        -- external libraries
        let
            bootstrapVersion = "4.0.0-beta.3"
            popperVersion = "1.12.9"
            selectizeVersion = "0.12.4"
            jqueryVersion = "3.2.1"
            bokehVersion = "0.12.13"
        extjs $ T.concat ["https://code.jquery.com/jquery-", jqueryVersion, ".min.js"]
        extjs $ T.concat ["https://cdnjs.cloudflare.com/ajax/libs/popper.js/", popperVersion, "/umd/popper.min.js"]
        extjs $ T.concat ["https://maxcdn.bootstrapcdn.com/bootstrap/", bootstrapVersion, "/js/bootstrap.min.js"]
        extcss $ T.concat ["https://maxcdn.bootstrapcdn.com/bootstrap/", bootstrapVersion, "/css/bootstrap.min.css"]
        extjs $ T.concat ["https://cdnjs.cloudflare.com/ajax/libs/selectize.js/", selectizeVersion, "/js/standalone/selectize.min.js"]
        extcss $ T.concat ["https://cdnjs.cloudflare.com/ajax/libs/selectize.js/", selectizeVersion, "/css/selectize.min.css"]
        extcss $ T.concat ["https://cdnjs.cloudflare.com/ajax/libs/selectize.js/", selectizeVersion, "/css/selectize.bootstrap3.min.css"]
        extcss $ T.concat ["https://cdn.pydata.org/bokeh/release/bokeh-", bokehVersion, ".min.css"]
        extjs $ T.concat ["https://cdn.pydata.org/bokeh/release/bokeh-", bokehVersion, ".min.js"]
        extjs $ T.concat ["https://cdn.pydata.org/bokeh/release/bokeh-api-", bokehVersion, ".min.js"]
        extcss "style.css"
    body_ $ do
        nav_ [class_ "navbar navbar-expand-md navbar-dark bg-dark fixed-top"] $ do
            span_ [class_ "navbar-brand"] "comatose"
            button_ [class_ "navbar-toggler", type_ "button", data_ "toggle" "collapse", data_ "target" "#navbarSupportedContent"] $ span_ [class_ "navbar-toggler-icon"] mempty
            div_ [class_ "collapse navbar-collapse", id_ "navbarSupportedContent"] $ do
                ul_ [class_ "navbar-nav mr-auto"] mempty
                form_ [id_ "protosort", class_ "navbar-nav form-inline my-2 my-lg-0", action_ "#"] $ do
                    featuresFilter db
                    " "
                    select_ [id_ "sort", class_ "form-control"] $ do
                        option_ [value_ "name"] "Name"
                        option_ [value_ "year"] "Year"
                        option_ [value_ "rank"] "Rank"
        div_ [class_ "jumbotron" ] $ section_ [class_ "container"] $ do
            introduction db
        div_ [class_ "container"] $ protocols db
        div_ [class_ "container"] $ do
            hr_ []
            about db attrib
        extjs "script.js"

-- |Render page
render f db attribution = renderToFile f (page db attribution)

