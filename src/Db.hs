{-# LANGUAGE OverloadedStrings #-}
module Db where

import System.FilePath ((<.>), splitExtension)
import Data.List (nub, sort, sortBy, isPrefixOf)
import Control.Monad
import Data.Yaml
import Text.Parsec.Error
import Text.ParserCombinators.Parsec.Prim
import Text.BibTeX.Parse
import Text.BibTeX.Entry as E
import qualified Data.Aeson as A
import qualified Data.Map as M

import Util

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

-- |Find publication with identifier `ident`
findPublication db ident = safeHead $ filter (\x -> ident == E.identifier x) $ dpublications db

-- |Get a list of all publications referenced by proto @p@
protoPublications db p = map (findPublication db) (pref p)

-- |Find all referenced features’ names
referencedFeatures db = nub $ sort $ concat $ map (M.keys . pfeatures) $ M.elems $ dalgos db

-- |Get all features at `level`, starting with 0
getFeaturesByLevel db level = M.filterWithKey (\k v -> countDots k == level) $ dfeatures db
    where
        countDots = length . filter ((==) '.')

-- |Get features by `base` feature
getFeaturesByBase db base = M.filterWithKey (\k v -> (base ++ ".") `isPrefixOf` k) $ dfeatures db

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

