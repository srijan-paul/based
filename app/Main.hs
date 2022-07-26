module Main where

import DataSet (loadData)
import Preprocess (parseCorpus)
import Prelude

main :: IO ()
main = do
  print $ parseCorpus loadData
