module Main (main) where

import Text.ParserCombinators.Parsec.Prim
import Text.BibTeX.Parse
import System.Directory (copyFile)

import Render (render)
import Db (readDb)

import Paths_comatose

-- |Read attributions from bibtex file
readAttributions = getDataFileName "data/attribution.bib" >>= parseFromFile file

copyDataFile source dest = getDataFileName source >>= (\x -> copyFile x dest)

main = do
    db <- getDataFileName "data/db.yaml" >>= readDb
    (Right attribution) <- readAttributions
    render "_build/index.html" db attribution
    copyDataFile "data/style.css" "_build/style.css"
    copyDataFile "data/script.js" "_build/script.js"

