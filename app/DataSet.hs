module DataSet (loadData, Corpus, MailLabel(..)) where

-- Loads the training data from the CSDMC2010SPAM corpus
-- For now though, this is a stub module that returns dummy data :p

data MailLabel = Spam | Ham
  deriving (Show, Eq)

type Corpus = [(String, MailLabel)]

loadData :: Corpus
loadData =
  [ ("free hot robux", Spam),
    ("newsletter update", Ham),
    ("hot singles in your area", Spam)
  ]
