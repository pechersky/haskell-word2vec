module Lib
    ( 
      VocabWord
    , VocabSentence
    , Vocabulary
    , scan_vocab
    , merge_vocab
    , empty_vocab
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict.Merge as MM
import qualified Data.Text as T

type VocabWord = T.Text
type VocabSentence = [VocabWord]

type Vocabulary = M.Map VocabWord Int
  
scan_vocab :: (Foldable f) => f VocabSentence -> Vocabulary
scan_vocab sentences = foldr (addWord) empty_vocab (concat sentences)
  where
    addWord word = M.insertWith (+) word 1

merge_vocab :: Vocabulary -> Vocabulary -> Vocabulary
merge_vocab = MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched mergeWords)
  where
    mergeWords word = (+)

empty_vocab :: Vocabulary
empty_vocab = M.empty