module Main where

import qualified Data.Bifunctor as Bifunctor
import Data.List as List (filter, foldl, length, map, partition, (++))
import DataSet (MailLabel (..), loadData)
import Preprocess (ParsedCorpus, parseCorpus, preprocessString)
import Prelude

-- Apply a function to both elements of a homogenous tuple.
mapPairs :: (a -> b) -> (a, a) -> (b, b)
mapPairs f (a, b) = (f a, f b)

float :: Int -> Float
float = fromIntegral

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

priori :: ParsedCorpus -> (Float, Float)
priori corpus =
  let (spamCount, hamCount) = mapPairs float (countLabel Spam, countLabel Ham)
      n = spamCount + hamCount
   in mapPairs (/ n) (spamCount, hamCount)
  where
    countLabel :: MailLabel -> Int
    countLabel label = length $ filter ((== label) . snd) corpus

getClassProbs :: ParsedCorpus -> String -> (Float, Float)
getClassProbs corpus str =
  let (prioriS, prioriH) = priori corpus
      n = float $ length corpus
      tokens = preprocessString str
      pSpamOfWords = map ((/ prioriS) . (/ n) . getCountUnderLabel Spam) tokens
      pHamOfWords = map ((/ prioriH) . (/ n) . getCountUnderLabel Ham) tokens
      (postS, postH) =
        Bifunctor.bimap
          (* prioriS)
          (* prioriH)
          (mapPairs product (pSpamOfWords, pHamOfWords))
   in (postS, postH)
  where
    getCountUnderLabel :: MailLabel -> String -> Float
    getCountUnderLabel label word = float $ foldl combine 1 corpus
      where
        combine acc (s, label') =
          if label == label'
            then acc + count word s
            else acc

classifyMessage :: ParsedCorpus -> String -> MailLabel
classifyMessage corpus s =
  let (probSpam, probHam) = getClassProbs corpus s
   in if probSpam > probHam
        then Spam
        else Ham

main :: IO ()
main = do
  let parsedCorpus = parseCorpus loadData
      c = getClassProbs parsedCorpus "free neighborhood 123 ad robux"
   in print c
