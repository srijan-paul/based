{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DataSet (loadData, loadDummyData, Corpus, MailLabel (..), splitDataSet) where

import Control.Applicative (liftA2)
import Control.Exception (IOException, catch)
import Control.Monad (MonadPlus (mzero))
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Bitraversable as Bifunctor
import Data.Csv (FromField (..), HasHeader (HasHeader), decode)
import Data.Encoding.UTF8
import Data.List (partition)
import Data.Maybe (Maybe (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist)
import System.Directory.Recursive (getFilesRecursive)
import System.FilePath ((</>))

-- Loads the training data from the CSDMC2010SPAM corpus
-- For now though, this is a stub module that returns dummy data :p

data MailLabel = Spam | Ham
  deriving (Show, Eq)

instance FromField MailLabel where
  parseField s
    | s == "spam" = pure Spam
    | s == "ham" = pure Ham
    | otherwise = mzero

type CorpusItem = (String, MailLabel)

type Corpus = [CorpusItem]

type CSVEntry = (Int, MailLabel, String, Int)

loadData :: String -> IO Corpus
loadData dirPath = do
  let (spamDir, hamDir) = (dirPath </> "spam", dirPath </> "ham")
  dirsExist <- liftA2 (&&) (doesDirectoryExist spamDir) (doesDirectoryExist hamDir)
  spamCorpus <- readCorpus Spam spamDir
  hamCorpus <- readCorpus Ham hamDir
  return (spamCorpus ++ hamCorpus)
  where
    readFileSafe :: FilePath -> IO String
    readFileSafe path = catch (T.unpack <$> TIO.readFile path) (\e -> return $ "ERROR: " ++ show (e :: IOException))

    readCorpus :: MailLabel -> FilePath -> IO Corpus
    readCorpus label dir = do
      filePaths <- getFilesRecursive dir
      contents <- mapM readFileSafe filePaths
      return $ map (,label) contents

-- where
--   readFilesFromDir :: String -> MailLabel -> ([String], MailLabel)
--   readFilesFromDir path label = getFiles

-- csvText <- BL.readFile dirPath
-- let parsedCsv = decode HasHeader csvText :: Either String (V.Vector CSVEntry)
-- return $
--   either (const Nothing) (Just . map convert . V.toList) parsedCsv
-- where
--   convert (_, label, s, _) = (s, label)

--- | Splits a data set into two parts for training and testing.
--- | Ensures equal equal distribution of spam and ham in both parts.
--- | Returns a tuple (TrainData, TestData)
splitDataSet :: Float -> Corpus -> (Corpus, Corpus)
splitDataSet testRatio' corpus =
  let (spamPart, hamPart) = partition ((== Spam) . snd) corpus
      testRatio =
        if testRatio' >= 0.99 || testRatio' < 0.01
          then 0.5
          else testRatio'
      (testSpam, trainSpam) = split testRatio spamPart
      (testHam, trainHam) = split testRatio hamPart
   in (testSpam ++ testHam, trainSpam ++ trainHam)
  where
    split ratio xs =
      let splitIndex = round $ ratio * fromIntegral (length xs)
       in splitAt splitIndex xs

loadDummyData :: IO Corpus
loadDummyData =
  return
    [ ("free robux", Spam),
      ("newsletter update", Ham),
      ("hot singles in your area", Spam)
    ]
