module Preprocess (parseCorpus, ParsedCorpus (..), preprocessString) where

import qualified Data.Bifunctor as Bifunctor
import Data.Char (isSpace, toLower)
import DataSet (Corpus, MailLabel (..))
import Data.List (intercalate)

isWordDelimiter :: Char -> Bool
isWordDelimiter x = isSpace x || x `elem` punctuations
  where
    punctuations = ",.!?;:'\"&-+-()[]=_"

-- | Return a list of words in a string.
-- __Examples:__
--
-- >>> tokenize "Hey! click here for free coupons :)"
-- ["Hey","click","here","for","free","coupons"]
tokenize :: String -> [String]
tokenize s = case dropWhile isWordDelimiter s of
  "" -> []
  remainingStr -> word : tokenize s'
    where
      (word, s') = break isWordDelimiter remainingStr

stopWords :: [String]
stopWords =
  [ "i",
    "me",
    "my",
    "myself",
    "we",
    "our",
    "ours",
    "ourselves",
    "you",
    "your",
    "yours",
    "yourself",
    "yourselves",
    "he",
    "him",
    "his",
    "himself",
    "she",
    "her",
    "hers",
    "herself",
    "it",
    "its",
    "itself",
    "they",
    "them",
    "their",
    "theirs",
    "themselves",
    "what",
    "which",
    "who",
    "whom",
    "this",
    "that",
    "these",
    "those",
    "am",
    "is",
    "are",
    "was",
    "were",
    "be",
    "been",
    "being",
    "have",
    "has",
    "had",
    "having",
    "do",
    "does",
    "did",
    "doing",
    "a",
    "an",
    "the",
    "and",
    "but",
    "if",
    "or",
    "because",
    "as",
    "until",
    "while",
    "of",
    "at",
    "by",
    "for",
    "with",
    "about",
    "against",
    "between",
    "into",
    "through",
    "during",
    "before",
    "after",
    "above",
    "below",
    "to",
    "from",
    "up",
    "down",
    "in",
    "out",
    "on",
    "off",
    "over",
    "under",
    "again",
    "further",
    "then",
    "once",
    "here",
    "there",
    "when",
    "where",
    "why",
    "how",
    "all",
    "any",
    "both",
    "each",
    "few",
    "more",
    "most",
    "other",
    "some",
    "such",
    "no",
    "nor",
    "not",
    "only",
    "own",
    "same",
    "so",
    "than",
    "too",
    "very",
    "s",
    "t",
    "can",
    "will",
    "just",
    "don",
    "should",
    "now"
  ]

removeStopWords :: [String] -> [String]
removeStopWords = filter (\x -> notElem x stopWords && (length x > 2))

preprocessString :: String -> [String]
preprocessString = makeStringLower . removeStopWords . tokenize
  where
    makeStringLower = map (map toLower)

type ParsedCorpus = [([String], MailLabel)]

-- | Preprocess the string by extracting out words and removing stop words and punctuation
-- __Examples:__
--
-- >>> parseCorpus [("hot singles in your area", Spam)]
-- [(["hot","singles","area"],Spam)]
parseCorpus :: Corpus -> ParsedCorpus
parseCorpus = map (Bifunctor.first preprocessString)
