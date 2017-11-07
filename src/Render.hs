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
    span_ [class_ "year"] $ htmlLookup "year"
    " "
    button_ [type_ "button", class_ "btn btn-light btn-sm", data_ "toggle" "popover", title_ "BibTeX", data_ "content" $ T.pack $ entry bib] "BibTeX"

-- | References section
references :: [E.T] -> Html ()
references attrib = div_ $ do
        h2_ [id_ "references"] "References"
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
            toHtml firstyear
            " and "
            toHtml lastyear
            "."
        p_ [class_ "lead"] $ a_ [class_ "btn btn-primary btn-lg", href_ "#about", role_ "button", data_ "toggle" "modal", id_ "learnmore"] "Learn more"

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
    div_ [class_ "modal fade", id_ "about"]
        $ div_ [class_ "modal-dialog"]
        $ div_ [class_ "modal-content"] $ do
            div_ [class_ "modal-header"] $ do
                h2_ [class_ "modal-title"] "About"
                button_ [type_ "button", class_ "close", data_ "dismiss" "modal"] "×"
            div_ [class_ "modal-body"] $ do
                p_ "In recent years the scientific community has proposed a surprisingly large number of wireless medium access (MAC) protocols. That number is still climbing year by year, rendering classic surveys outdated rather quick. Additionally the sheer number of protocols results in name collisions, often making it harder than necessary to identify which protocol exactly is referenced by just looking at its name. Ordinary surveys also cannot provide interactivity like feature-based filtering and searching. Its results are not reusable easily since they are not machine-readable."
                p_ $ do
                    "This comprehensive MAC taxonomy database (comatose), aims to fix most of these problems. It lists most known scientific MAC protocol proposals and is not limited to a subset with specific properties. The list includes the protocol’s short and long name, a description, as well as references to the publication it originated from. It also introduces a taxonomy. Some of its terminology is based on or inspired by "
                    a_ [href_ "#references"] "existing surveys"
                    ". Features are grouped into "
                    toHtml $ show categoryCount
                    " categories and those within the same category can be mutually exclusive. Below is a list of all features in use."
                features db
                h2_ "Implementation"
                p_ $ do
                    "comatose uses two databases. The first one contains features and protocols in a "
                    a_ [href_ "http://yaml.org/"] "YAML"
                    " file. It is human and machine-readable at the same time and thus easy to edit. Also it does not require additional software like a SQL database server. This first database links protocols to publications with a second database. That one is just a standard BibTeX file. Since TeX is used for a lot of scientific publications these records usually exist already and can be copied, as well as reused for new publications. Therefore both should databases provide value beyond the scope of this project."
                p_ "This very page is generated with a HTML renderer written in Haskell. It reads both databases and transforms them into a single-page HTML document.  Additional JavaScript code provides client-side filtering and searching."
                h2_ "Contributing"
                p_ $ do
                    "As mentioned above this database is not complete yet and will never be, as long as new protocols are invented. Descriptions and feature tags are missing for a lot of protocols due to lack of time. If you want to help send an email with your suggestions to "
                    a_ [href_ "mailto:lars+comatose@6xq.net"] "lars+comatose@6xq.net"
                    " or clone the repository from "
                    a_ [href_ "https://github.com/PromyLOPh/comatose"] "GitHub"
                    ", edit the database and create a pull request."
                h2_ "Acknowledgements"
                p_ $ do
                    "This database is part of a project funded by the "
                    a_ [href_ "https://www.bmbf.de/en/index.html"] "Federal Ministry of Education and Research"
                    " from 2015 to 2017."
                references (sortBy (compare `on` lookup "year" . E.fields) attrib)
    
-- | The list of protocols
protocols :: Database -> Html ()
protocols db = section_ [id_ "protocols", role_ "tablist"] $ forM_ (M.toList $ dalgos db) (protoentry db)

-- |Page template
page db attrib = doctypehtml_ $ do
    head_ $ do
        title_ "comatose"
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        extjs "https://code.jquery.com/jquery-3.2.1.min.js"
        extjs "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js"
        extjs "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/js/bootstrap.min.js"
        extcss "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css"
        extjs "https://cdnjs.cloudflare.com/ajax/libs/selectize.js/0.12.4/js/standalone/selectize.min.js"
        extcss "https://cdnjs.cloudflare.com/ajax/libs/selectize.js/0.12.4/css/selectize.min.css"
        extcss "https://cdnjs.cloudflare.com/ajax/libs/selectize.js/0.12.4/css/selectize.bootstrap3.min.css"
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
            about db attrib
        div_ [class_ "container"] $ protocols db
        extjs "script.js"

-- |Render page
render f db attribution = renderToFile f (page db attribution)

