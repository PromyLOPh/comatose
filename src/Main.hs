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
    copyDataFile "data/bmbf_logo_fremdpublikation.svg" "_build/bmbf_logo_fremdpublikation.svg"

