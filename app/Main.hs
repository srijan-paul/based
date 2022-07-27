module Main where

import qualified Data.Bifunctor as Bifunctor
import Data.List as List (foldl, partition, (++))
import qualified Data.Map as Map
import DataSet (MailLabel (..), loadData)
import Preprocess (ParsedCorpus, parseCorpus)
import Prelude

type FreqMap = Map.Map String Int

getWordFreqs :: [String] -> Map.Map String Int
getWordFreqs = List.foldl addToMap Map.empty
  where
    addToMap :: Map.Map String Int -> String -> Map.Map String Int
    addToMap map word = Map.insertWith (+) word 1 map

getLabelledFreqs :: ParsedCorpus -> (FreqMap, FreqMap)
getLabelledFreqs corpus =
    -- 1. Split the corpus into two parts, one with spam and the other with ham
  let (spamData, hamData) = List.partition ((== Spam) . snd) corpus
    -- 2. Collect all words from each labelled corpus into a list of strings.
      (spamWords, hamWords) = mapPairs collectWords (spamData, hamData)
    -- 3. Get the frequency of each word for a specific label.
   in mapPairs getWordFreqs (spamWords, hamWords)
  where
    -- Extract out all words from a parsed corpus
    collectWords :: ParsedCorpus -> [String]
    collectWords = List.foldl (\acc cur -> acc ++ fst cur) []

    -- Apply a function to both elements of a homogenous tuple.
    mapPairs :: (a -> b) -> (a, a) -> (b, b)
    mapPairs f (a, b) = (f a, f b)

main :: IO ()
main = do
  let parsedCorpus = parseCorpus loadData
      (spamWordFreqs, hamWordFreqs) = getLabelledFreqs parsedCorpus
   in print (spamWordFreqs, hamWordFreqs)
