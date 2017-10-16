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
resolveDoi q = "http://doi.org/" ++ q

-- |List of protocol features
protofeatures :: Database -> Protocol -> Html ()
protofeatures _ p | (M.size $ pfeatures p) == 0 = mempty
protofeatures db p = do
    dt_ "Features"
    dd_ $ ul_ [class_ "features list-inline"] $ forM_ (sort $ M.keys $ pfeatures p) (\x -> li_ [data_ "id" (T.pack x), class_ "list-inline-item"] $ toHtml $ maybe ("" :: String) fname $ M.lookup x (dfeatures db))

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
    in
        section_ [
            id_ $ T.pack ident
            , class_ "protocol"
            , data_ "rank" (T.pack $ show $ prank p)
            ] $ do
            h3_ [class_ "name"] $ do
                a_ [href_ (T.pack $ '#':ident), title_ "permalink"] $ toHtml $ pname p
                " "
                maybe "" (small_ [class_ "longname"] . toHtml) $ plongname p
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

-- | References section
references :: [E.T] -> Html ()
references attrib = section_ $ do
        h2_ "References"
        ol_ $ forM_ attrib (li_ . bibentry)

-- | What is this?!
introduction :: Database -> Html ()
introduction db =
    let
        algocount = M.size $ dalgos db
        pubyears = catMaybes $ map (lookup "year" . E.fields) $ dpublications db
        firstyear = foldr min (head pubyears) (tail pubyears)
        lastyear = foldr max (head pubyears) (tail pubyears)
    in section_ [class_ "container"] $ do
        h1_ [class_ "display-3"] "comatose"
        p_ $ do
            "The comprehensive MAC taxonomy database (comatose) is a collection of "
            toHtml $ show algocount
            " wireless media/medium access protocols published between "
            toHtml firstyear
            " and "
            toHtml lastyear
            "."

-- |List of protocol features
features :: Database -> Html ()
features db =
    section_ [id_ "features"] $ do
        h2_ "Features"
        p_ "This section presents so-called “features” that are assigned to each protocol."
        forM_ (M.toList $ getFeaturesByLevel db 0) $ \(baseident, basefeature) -> do
            let featureanchor = "feature-" ++ baseident in do
                h3_ [id_ $ T.pack featureanchor] $ a_ [href_ $ T.pack $ '#':featureanchor] $ toHtml $ fname basefeature
                maybe mempty (p_ . toHtml) $ fdescription basefeature
                dl_ $ forM_ (M.toList $ getFeaturesByBase db baseident) $ \(ident, feature) -> do
                    dt_ [class_ "form-inline"] $ let i = T.pack ("filter-feature-" ++ ident) in do
                        input_ [type_ "checkbox", id_ i, class_ "filter-feature", value_ (T.pack ident)]
                        " "
                        label_ [for_ i] $ toHtml $ fname feature
                    maybe mempty (dd_ . toHtml) $ fdescription feature

-- | The list of protocols
protocols :: Database -> Html ()
protocols db = section_ [id_ "protocols"] $ do
    h2_ "Protocols"
    forM_ (M.toList $ dalgos db) (protoentry db)

-- |Page template
page db attrib = doctypehtml_ $ do
    head_ $ do
        title_ "comatose"
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        extjs "https://code.jquery.com/jquery-3.2.1.min.js"
        extcss "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css"
        extjs "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/js/bootstrap.min.js"
        extcss "style.css"
    body_ $ do
        nav_ [class_ "navbar navbar-expand-md navbar-dark bg-dark fixed-top"] $ do
            div_ [class_ "collapse navbar-collapse"] $ do
                span_ [class_ "navbar-brand"] "comatose"
                ul_ [class_ "navbar-nav mr-auto"] $ do
                    li_ [class_ "nav-item" ] $ a_ [class_ "nav-link", href_ "#features"] "Features"
                    li_ [class_ "nav-item" ] $ a_ [class_ "nav-link", href_ "#protocols"] "Protocols"
                form_ [id_ "protosort", class_ "form-inline my-2 my-lg-0"] $ do
                    input_ [id_ "filter", type_ "search", class_ "form-control mr-sm-2", placeholder_ "Filter by name"]
                    " "
                    label_ [for_ "sort"] "Sort by"
                    " "
                    select_ [id_ "sort", class_ "form-control"] $ do
                        option_ [value_ "name"] "Name"
                        option_ [value_ "year"] "Year"
                        option_ [value_ "rank"] "Rank"
        div_ [class_ "jumbotron" ] $ introduction db
        div_ [class_ "container"] $ do
            features db
            protocols db
            references (sortBy (compare `on` lookup "year" . E.fields) attrib)
        extjs "script.js"

-- |Render page
render f db attribution = renderToFile f (page db attribution)

