{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Yaml
import Data.List (nub, sort, sortBy)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Aeson as A
import Text.BibTeX.Parse
import Text.BibTeX.Entry as E
import qualified Data.Map as M
import Text.Parsec.Error
import Text.ParserCombinators.Parsec.Prim
import qualified Data.ByteString.Lazy as BS
import System.FilePath ((<.>), splitExtension)
import System.Directory (copyFile)
import Network.URI (isReserved, escapeURIString)
import Lucid

import Paths_comatose

data Protocol = Protocol {
	  pname :: String
	, plongname :: Maybe String
	, pdescription :: Maybe String
	, pref :: [String]
	, pfeatures :: M.Map String (Maybe String)
	} deriving Show

data Database = Database {
	  dalgos :: M.Map String Protocol
	, dfeatures :: M.Map String Feature
	, dpublications :: [T]
	} deriving Show

data Feature = Feature {
	  fname :: String
	, fdescription :: Maybe String
	} deriving Show

type FeatureList = M.Map String Feature

instance FromJSON Protocol where
	parseJSON (Object v) = Protocol
		<$> v .: "name"
		<*> v .:? "longname"
		<*> v .:? "description"
		<*> v .:? "ref" .!= []
		<*> v .:? "features" .!= M.empty
	parseJSON _          = mzero

instance FromJSON Database where
	parseJSON (Object v) = Database
		<$> v .: "algos"
		<*> v .: "features"
		<*> pure []
	parseJSON _          = mzero

instance FromJSON Feature where
	parseJSON (Object v) = Feature
		<$> v .: "name"
		<*> v .:? "description"
	parseJSON _          = mzero

safeHead [] = Nothing
safeHead (x:_) = Just x

findPublication db ident = safeHead $ filter (\x -> ident == E.identifier x) $ dpublications db

-- |Get a list of all publications referenced by proto @p@
protoPublications db p = map (findPublication db) (pref p)

-- |Find all referenced features’ names
referencedFeatures db = nub $ sort $ concat $ map (M.keys . pfeatures) $ M.elems $ dalgos db

readDb :: String -> IO Database
readDb f = do
	yamlres <- decodeFileEither f
	--print yamlres
	let
		(Right yamldb) = yamlres
		(basename, _) = splitExtension f
	bibres <- parseFromFile file (basename <.> "bib")
	--print bibres
	let (Right bibdb) = bibres
	return yamldb { dpublications = bibdb }

maybeToHtml = maybe (toHtml ("" :: String)) toHtml

head' [] = Nothing
head' (x:_) = Just x

scholarSearch q = "http://scholar.google.com/scholar?q=" ++ escapeURIString isReserved q
resolveDoi q = "http://doi.org/" ++ q

protofeatures :: Database -> Protocol -> Html ()
protofeatures _ p | (M.size $ pfeatures p) == 0 = mempty
protofeatures db p = do
	dt_ "Features"
	dd_ $ ul_ [class_ "features"] $ forM_ (sort $ M.keys $ pfeatures p) (\x -> li_ $ toHtml $ maybe ("" :: String) fname $ M.lookup x (dfeatures db))

protopapers :: [T] -> Html ()
protopapers pubs | length pubs == 0 = mempty
protopapers pubs = do
	dt_ "Published in"
	dd_ $ if length pubs == 1
		then p_ $ bibentry $ head pubs
		else ol_ $ forM_ pubs (li_ . bibentry)

protoentry :: Database -> (String, Protocol) -> Html ()
protoentry db (ident, p) =
	let
		pubs = catMaybes $ protoPublications db p
		firstpub = head' pubs
		field key = firstpub >>= (return . E.fields) >>= lookup key
	in
		section_ [
			id_ $ T.pack ident
			, class_ "protocol"
			, data_ "name" (T.pack $ pname p)
			, data_ "year" (maybe "" T.pack $ field "year")
			] $ do
			h3_ $ do
				toHtml $ pname p
				" "
				maybe "" (small_ . toHtml) $ plongname p
				" "
				small_ $ a_ [href_ (T.pack $ '#':ident), title_ "permalink", class_ "permalink"] "¶"
			dl_ $ do
				protopapers pubs
				protofeatures db p

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
	let fields = E.fields bib
	a_ [href_ $ T.pack $ maybe "" id $ bibentryurl bib] $ maybeToHtml $ lookup "title" fields
	", "
	maybeToHtml $ lookup "author" fields
	", "
	maybeToHtml $ lookup "year" fields

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
	in section_ $ do
		p_ $ do
			"The comprehensive MAC taxonomy database (comatose) is a collection of "
			toHtml $ show algocount
			" wireless media/medium access protocols published between "
			toHtml firstyear
			" and "
			toHtml lastyear
			"."

-- | The list of protocols
protocols :: Database -> Html ()
protocols db = section_ [id_ "protocols"] $ do
	h2_ "Protocols"
	div_ [id_ "protosort", class_ "form-inline"] $ do
		label_ [for_ "filter"] "Filter"
		" "
		input_ [id_ "filter", type_ "text", class_ "form-control"]
		" "
		label_ [for_ "sort"] "Sort by"
		" "
		select_ [id_ "sort", class_ "form-control"] $ do
			option_ [value_ "name"] "Name"
			option_ [value_ "year"] "Year"
	forM_ (M.toList $ dalgos db) (protoentry db)

page db attrib = doctypehtml_ $ do
	head_ $ do
		title_ "comatose"
		meta_ [charset_ "utf-8"]
		extjs "https://code.jquery.com/jquery-1.11.2.min.js"
		extcss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"
		extcss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"
		extjs "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"
		extcss "style.css"
	body_ $ do
		div_ [class_ "container"] $ do
			div_ [class_ "page-header"] $ do
				h1_ "comatose"
			introduction db
			protocols db
			references (sortBy (compare `on` lookup "year" . E.fields) attrib)
		extjs "script.js"

render f db attribution = renderToFile f (page db attribution)

readAttributions = getDataFileName "data/attribution.bib" >>= parseFromFile file

copyDataFile source dest = getDataFileName source >>= (\x -> copyFile x dest)

main = do
	db <- getDataFileName "data/db.yaml" >>= readDb
	(Right attribution) <- readAttributions
	render "_build/index.html" db attribution
	copyDataFile "data/style.css" "_build/style.css"
	copyDataFile "data/script.js" "_build/script.js"

