{-# LANGUAGE FlexibleContexts #-}

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
import           System.Log.Simple

type VocabWord = T.Text
type VocabSentence = [VocabWord]

type Vocabulary = M.Map VocabWord Int

sentenceToVocabulary :: VocabSentence -> Vocabulary
sentenceToVocabulary = foldr merge_vocab empty_vocab . (fmap mapWord)
  where
    mapWord word = M.singleton word 1

{-scan_vocab :: (Foldable f) => f VocabSentence -> Vocabulary-}
scan_vocab sentences = do
  debug "collecting all words and their counts"
  return $ foldr mapWord empty_vocab $ concat sentences
    where
      mapWord word = M.insertWith (+) word 1
-- fmap $ foldr (liftM2 merge_vocab) (return empty_vocab) $ fmap (fmap
-- scan_vocab) lin

merge_vocab :: Vocabulary -> Vocabulary -> Vocabulary
merge_vocab = MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched mergeWords)
  where
    mergeWords word = (+)

empty_vocab :: Vocabulary
empty_vocab = M.empty
