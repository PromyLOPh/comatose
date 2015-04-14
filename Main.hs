{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Yaml
import Data.List (nub, sort)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.BibTeX.Parse
import Text.BibTeX.Entry as E
import qualified Data.Map as M
import Text.Parsec.Error
import Text.ParserCombinators.Parsec.Prim
import qualified Data.ByteString.Lazy as BS
import System.FilePath ((<.>), splitExtension)
import Lucid

import Paths_comatose

data Protocol = Protocol {
	  pname :: Maybe String
	, pabbrv :: Maybe String
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
		<$> v .:? "name"
		<*> v .:? "abbrv"
		<*> v .:? "ref" .!= []
		<*> v .:?  "features" .!= M.empty
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

protoentry :: Database -> [String] -> (String, Protocol) -> Html ()
protoentry db featurekeys (ident, p) = tr_ $ do
	let
		pubs = protoPublications db p
		firstpub = join (head' pubs)
		field key = firstpub >>= (return . E.fields) >>= lookup key
	td_ $ maybeToHtml $ pname p
	td_ $ maybeToHtml $ pabbrv p
	td_ $ maybeToHtml $ field "year"
	forM_ featurekeys (\x -> td_ $ toHtml $ maybe ("" :: String) (const "✓") $ M.lookup x (pfeatures p))

extcss url = link_ [rel_ "stylesheet", type_ "text/css", href_ url]

extjs :: T.Text -> Html ()
extjs url = script_ [type_ "text/javascript", charset_ "utf8", src_ url] ("" :: T.Text)

page db = doctypehtml_ $ do
	head_ $ do
		title_ "comatose"
		meta_ [charset_ "utf-8"]
		extjs "https://code.jquery.com/jquery-1.10.2.min.js"
		extcss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap.min.css"
		extcss "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/css/bootstrap-theme.min.css"
		extjs "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.2/js/bootstrap.min.js"
		extcss "https://cdn.datatables.net/1.10.5/css/jquery.dataTables.css"
		extjs "https://cdn.datatables.net/1.10.5/js/jquery.dataTables.js"
		extcss "https://cdn.datatables.net/plug-ins/f2c75b7247b/integration/bootstrap/3/dataTables.bootstrap.css"
		extjs "https://cdn.datatables.net/plug-ins/f2c75b7247b/integration/bootstrap/3/dataTables.bootstrap.js"
	body_ $ do
		div_ [class_ "container"] $ do
			div_ [class_ "page-header"] $ do
				h1_ "comatose"
				h2_ "COmprehensive MAc TaxonOmy databaSE"
			table_ [id_ "algo", class_ "table-striped"] $ do
				let featurekeys = referencedFeatures db
				thead_ $ do
					tr_ $ do
						th_ "Name"
						th_ "Abbrv"
						th_ "Year"
						th_ [colspan_ (T.pack $ show $ length featurekeys)] "Features"
					tr_ $ do
						th_ ""
						th_ ""
						th_ ""
						forM_ featurekeys (\x -> maybe (th_ "") (th_ . toHtml . fname) $ M.lookup x (dfeatures db))
				tbody_ $ forM_ (M.toList $ dalgos db) (protoentry db featurekeys)
		script_ "$(document).ready( function () { $('#algo').DataTable( { paging: false, \"columnDefs\": [ ] } ); } );"

render f db = renderToFile f (page db)

main = getDataFileName "data/db.yaml" >>= readDb >>= render "comatose.html"

