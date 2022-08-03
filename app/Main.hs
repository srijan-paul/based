module Main where

import qualified Data.Bifunctor as Bifunctor
import Data.List as List (filter, foldl, length, map, partition, (++))
import qualified Data.Map as M
import DataSet (Corpus, MailLabel (..), loadData, loadDummyData, splitDataSet)
import Preprocess (ParsedCorpus, parseCorpus, preprocessString)
import Prelude

type FreqOfWord = M.Map String Int

type Prob = Float

data Model = Model
  { trainSize :: Float,
    prioriS :: Float,
    prioriH :: Float,
    countInSpam :: FreqOfWord,
    countInHam :: FreqOfWord
  }
  deriving (Show)

-- Apply a function to both elements of a homogenous tuple.
mapPairs :: (a -> b) -> (a, a) -> (b, b)
mapPairs f (a, b) = (f a, f b)

float :: Int -> Float
float = fromIntegral

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

makeFreqMap :: MailLabel -> ParsedCorpus -> FreqOfWord
makeFreqMap label corpus =
  let wordsWithLabel = foldl1 (++) $ map fst $ filter ((== label) . snd) corpus
   in foldl addWordToMap M.empty wordsWithLabel
  where
    addWordToMap map word = M.insertWith (+) word 1 map

priori :: ParsedCorpus -> (Float, Float)
priori corpus =
  let (spamCount, hamCount) = mapPairs float (countLabel Spam, countLabel Ham)
      n = spamCount + hamCount
   in mapPairs (/ n) (spamCount, hamCount)
  where
    countLabel :: MailLabel -> Int
    countLabel label = length $ filter ((== label) . snd) corpus

getClassProbs :: Model -> String -> (Float, Float)
getClassProbs model str =
  let (priorS, priorH) = (prioriS model, prioriS model)
      n = trainSize model
      tokens = preprocessString str
      pSpamOfWords = map ((/ priorS) . (/ n) . getCountUnderLabel Spam) tokens
      pHamOfWords = map ((/ priorH) . (/ n) . getCountUnderLabel Ham) tokens
      (postS, postH) =
        Bifunctor.bimap
          (* priorS)
          (* priorH)
          (mapPairs product (pSpamOfWords, pHamOfWords))
   in (postS, postH)
  where
    getCountUnderLabel :: MailLabel -> String -> Float
    getCountUnderLabel label word =
      let map = if label == Spam then countInSpam model else countInHam model
       in float $ 1 + M.findWithDefault 0 word map

classifyMessage :: Model -> String -> MailLabel
classifyMessage model s =
  let (probSpam, probHam) = getClassProbs model s
   in if probSpam > probHam
        then Spam
        else Ham

train :: ParsedCorpus -> Model
train corpus =
  let (prioriS, prioriH) = priori corpus
      trainSize = float $ length corpus
      spamFreqMap = makeFreqMap Spam corpus
      hamFreqMap = makeFreqMap Ham corpus
   in Model
        { prioriS = prioriS,
          prioriH = prioriH,
          trainSize = trainSize,
          countInSpam = spamFreqMap,
          countInHam = hamFreqMap
        }

loadCorpus :: Float -> IO (Corpus, Corpus)
loadCorpus ratio = do
  rawCorpus <- loadData "data"
  return $ splitDataSet ratio rawCorpus

findAccuracy :: Model -> Corpus -> Float
findAccuracy model corpus =
  let (correct, incorrect) =
        mapPairs
          List.length
          $ List.partition (== True) (map isPredictionCorrect corpus)
   in float correct / float (correct + incorrect) * 100.0
  where
    isPredictionCorrect (s, label) = label == classifyMessage model s

main :: IO ()
main = do
  (trainSet, testSet) <- loadCorpus 0.9
  print $ mapPairs length (trainSet, testSet)
  let model = parseCorpus trainSet
  print "tch"
  -- print model

-- print $ findAccuracy model testSet

-- d <- loadDummyData
-- print $ getClassProbs (train $ parseCorpus d) "free"
