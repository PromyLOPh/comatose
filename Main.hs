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
import Text.Printf (printf)
import qualified Data.ByteString.Lazy as BS
import System.FilePath ((<.>), splitExtension)
import System.Directory (copyFile)
import Network.URI (isReserved, escapeURIString)
import Lucid

import Paths_comatose

-- |A MAC protocol/algorithm
data Protocol = Protocol {
	-- |Short protocol name, usually its abbreviation
	  pname :: String
	-- |Long protocol name
	, plongname :: Maybe String
	-- |Free-text protocol description
	, pdescription :: Maybe String
	-- |List of publication references describing this protocol
	, pref :: [String]
	-- |Feature references of this protocol
	, pfeatures :: M.Map String (Maybe String)
	-- |List of references to other protocols, usually from the paper’s
	-- “related work” section
	, prelated :: [String]
	-- |Relevance of this protocol, calculated (pun intended)
	, prank :: Float
	} deriving Show

-- |A MAC protocol feature
data Feature = Feature {
	-- |Referenceable name
	  fname :: String
	-- |Its description
	, fdescription :: Maybe String
	} deriving Show

-- |The whole database
data Database = Database {
	-- |Key-value pair for each protocol
	  dalgos :: M.Map String Protocol
	-- |Global list of available features
	, dfeatures :: M.Map String Feature
	-- |Global list of available publications
	, dpublications :: [T]
	} deriving Show

instance FromJSON Protocol where
	parseJSON (Object v) = Protocol
		<$> v .: "name"
		<*> v .:? "longname"
		<*> v .:? "description"
		<*> v .:? "ref" .!= []
		<*> v .:? "features" .!= M.empty
		<*> v .:? "related" .!= []
		<*> return 0
	parseJSON _          = mzero

instance FromJSON Feature where
	parseJSON (Object v) = Feature
		<$> v .: "name"
		<*> v .:? "description"
	parseJSON _          = mzero

instance FromJSON Database where
	parseJSON (Object v) = Database
		<$> v .: "algos"
		<*> v .: "features"
		<*> pure []
	parseJSON _          = mzero

-- |Safe head function
safeHead [] = Nothing
safeHead (x:_) = Just x

-- |Find publication with identifier `ident`
findPublication db ident = safeHead $ filter (\x -> ident == E.identifier x) $ dpublications db

-- |Get a list of all publications referenced by proto @p@
protoPublications db p = map (findPublication db) (pref p)

-- |Find all referenced features’ names
referencedFeatures db = nub $ sort $ concat $ map (M.keys . pfeatures) $ M.elems $ dalgos db

-- |Read protocol and bib database from file
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
	return $ calcRank $ yamldb { dpublications = bibdb }

-- |Protocol rank/popularity, uses the pagerank algorithm
calcRank db =
	let
		initalgos = M.mapWithKey initial $ dalgos db
		n = fromIntegral $ M.size $ dalgos db
		d = 0.85
		initial ident p = p { prank = 1.0/n }
		-- Get all incoming references
		pincoming algos ident = filter (\(_, x) -> ident `elem` prelated x) $ M.toList algos
		-- Calculate new rank for p
		modify algos ident p = p { prank = (1-d)/n + d*sum (map inrank (pincoming algos ident)) }
		-- Incoming rank for p
		inrank (_, p) = prank p / fromIntegral (length (prelated p))
		absdiff :: M.Map String Protocol -> (String, Protocol) -> Float
		absdiff b (ident, p) = abs (prank (b M.! ident) - prank p)
		rankdiff :: M.Map String Protocol -> M.Map String Protocol -> Float
		rankdiff a b = 1/n * M.foldlWithKey (\x ident p -> x + absdiff b (ident, p)) 0 a
		iterateUntil fiter fcmp a = let b = fiter a in if fcmp a b then iterateUntil fiter fcmp b else b
		run algos = M.mapWithKey (modify algos) algos
		stop prevalgos algos = rankdiff prevalgos algos > 0.00001
	in db { dalgos = (iterateUntil run stop initalgos) }

maybeToHtml = maybe (toHtml ("" :: String)) toHtml

scholarSearch q = "http://scholar.google.com/scholar?q=" ++ escapeURIString isReserved q
resolveDoi q = "http://doi.org/" ++ q

-- |List of protocol features
protofeatures :: Database -> Protocol -> Html ()
protofeatures _ p | (M.size $ pfeatures p) == 0 = mempty
protofeatures db p = do
	dt_ "Features"
	dd_ $ ul_ [class_ "features"] $ forM_ (sort $ M.keys $ pfeatures p) (\x -> li_ $ toHtml $ maybe ("" :: String) fname $ M.lookup x (dfeatures db))

-- |List of protocol publications
protopapers :: [T] -> Html ()
protopapers pubs | length pubs == 0 = mempty
protopapers pubs = do
	dt_ "Published in"
	dd_ $ if length pubs == 1
		then p_ $ bibentry $ head pubs
		else ol_ $ forM_ pubs (li_ . bibentry)

-- |Protocol description
protodesc :: Protocol -> Html ()
protodesc Protocol { pdescription = Nothing } = mempty
protodesc Protocol { pdescription = Just desc } = do
	dt_ "Description"
	dd_ $ p_ $ toHtml desc

protorelated :: Database -> Protocol -> Html ()
protorelated _ p | null $ prelated p = mempty
protorelated db p =
	let
		algos = dalgos db
		lookup k = M.lookup k algos >>= \y -> return (k, y)
		rel = catMaybes $ map lookup $ prelated p
	in do
		dt_ "Related"
		dd_ [class_ "related"] $ ul_ $ forM_ rel $
			\(ident, x) -> li_ $ a_ [href_ (T.pack $ '#':ident)] $ toHtml $ pname x

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
			, data_ "name" (T.pack $ pname p)
			, data_ "longname" (maybe "" T.pack $ plongname p)
			, data_ "author" (maybe "" T.pack $ field "author")
			, data_ "year" (maybe "" T.pack $ field "year")
			, data_ "rank" (T.pack $ show $ prank p)
			] $ do
			h3_ $ do
				a_ [href_ (T.pack $ '#':ident), title_ "permalink", class_ "permalink"] $ toHtml $ pname p
				" "
				maybe "" (small_ . toHtml) $ plongname p
			dl_ $ do
				protopapers pubs
				protodesc p
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
		input_ [id_ "filter", type_ "search", class_ "form-control"]
		" "
		label_ [for_ "sort"] "Sort by"
		" "
		select_ [id_ "sort", class_ "form-control"] $ do
			option_ [value_ "name"] "Name"
			option_ [value_ "year"] "Year"
			option_ [value_ "rank"] "Rank"
	forM_ (M.toList $ dalgos db) (protoentry db)

-- |Page template
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

-- |Render page
render f db attribution = renderToFile f (page db attribution)

-- |Read attributions from bibtex file
readAttributions = getDataFileName "data/attribution.bib" >>= parseFromFile file

copyDataFile source dest = getDataFileName source >>= (\x -> copyFile x dest)

main = do
	db <- getDataFileName "data/db.yaml" >>= readDb
	(Right attribution) <- readAttributions
	render "_build/index.html" db attribution
	copyDataFile "data/style.css" "_build/style.css"
	copyDataFile "data/script.js" "_build/script.js"

