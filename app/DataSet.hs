module DataSet (loadData, Corpus, Label) where

-- Loads the training data from the CSDMC2010SPAM corpus
-- For now though, this is a stub module that returns dummy data :p

data Label = Spam | Ham
  deriving (Show)

type Corpus = [(String, Label)]

loadData :: Corpus
loadData =
  [ ("free robux", Spam),
    ("newsletter update", Ham),
    ("hot singles in your area", Spam)
  ]
