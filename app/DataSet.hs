module DataSet where

-- Loads the training data from the CSDMC2010SPAM corpus
-- For now though, this is a stub module that returns dummy data :p

data Label = Spam | Ham
  deriving (Show)

loadData :: [(String, Label)]
loadData =
  [ ("free robux", Spam),
    ("newsletter update", Ham),
    ("hot singles in your area", Spam)
  ]
