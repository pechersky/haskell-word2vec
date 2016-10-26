{-# LANGUAGE FlexibleContexts #-}

module Lib
    (
      VocabWord
    , VocabSentence
    , Vocabulary
    , scan_vocab
    , merge_vocab
    , empty_vocab
    , scale_vocab
    -- , threshold
    , _rf
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Map.Strict.Merge as MM
import qualified Data.Text as T
import           Control.Monad as CM
import           System.Log.Simple

type VocabWord = T.Text
type VocabSentence = [VocabWord]

type Vocabulary = M.Map VocabWord Int
type TrimRule = VocabWord -> Int -> Int -> Bool

_rf = (runBaseLogger (Lvl, Msg) . runWriterLoggerT)

sentenceToVocabulary :: VocabSentence -> Vocabulary
sentenceToVocabulary = foldr merge_vocab empty_vocab . (fmap mapWord)
  where
    mapWord word = M.singleton word 1

scan_vocab :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m) 
  => [VocabSentence] 
  -> Int 
  -> Maybe TrimRule 
  -> Maybe Int
  -> m Vocabulary
scan_vocab sentences progress_per trim_rule max_vocab_size = do
  debug "collecting all words and their counts"
  -- return $ foldr mapWord empty_vocab $ concat sentences
  (voc, _, total_words, sentence_no) <- CM.foldM go (empty_vocab, initMinReduce, initTotalWords, 0) $ (zip [0, 1..]) sentences
  debug $ foldr (++) "" ["collected ", show (wordTypes voc), " word types from a corpus of ", show (total_words + numWords voc), " raw words and ", show (sentence_no + 1), " sentences"]
  return voc
    where
      go tup@(acc, min_reduce, total_words, _) (sent_ix, sent) = do
        when ((sent_ix `mod` progress_per) == 0) $ debug $ foldr (++) "" ["PROGRESS: at sentence ", show sent_ix, ", processed ", show (numWords acc), " words, keeping ", show (wordTypes acc), " word types"]
        case max_vocab_size of
          Nothing -> return (unprunedVocab, min_reduce, total_words, sent_ix)
          Just maxVSize -> case (wordTypes acc > maxVSize) of
            True -> do
              (prunedVocab, filteredWordNum) <- prune_vocab unprunedVocab min_reduce trim_rule
              return (prunedVocab, min_reduce+1, total_words+filteredWordNum, sent_ix)
            False -> return (unprunedVocab, min_reduce, total_words, sent_ix)
        where
          unprunedVocab = foldr mapWord acc sent
      mapWord word = M.insertWith (+) word 1
      numWords = M.foldr (+) 0
      wordTypes = M.size
      initMinReduce = 1
      initTotalWords = 0
      
prune_vocab :: (Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m) 
  => Vocabulary 
  -> Int 
  -> Maybe TrimRule 
  -> m (Vocabulary, Int)       
prune_vocab vocab min_reduce trim_rule = do
  debug $ foldr (++) "" ["pruned out ", show (old_len - newLen), " tokens with count <=", show min_reduce, " (before ", show old_len, ", after ", show newLen, ")"]
  return (newVocab, result)
    where
      old_len = M.size vocab
      (newVocab, filtVocab) = M.partitionWithKey (\k v -> keep_vocab_item k v min_reduce trim_rule) vocab
      result = M.foldr (+) 0 filtVocab
      newLen = M.size newVocab

keep_vocab_item :: VocabWord 
                -> Int 
                -> Int 
                -> Maybe TrimRule 
                -> Bool
keep_vocab_item word count min_count trim_rule = case trim_rule of
  Nothing -> default_res
  Just f -> f word count min_count
  where 
    default_res = count >= min_count

-- fmap $ foldr (liftM2 merge_vocab) (return empty_vocab) $ fmap (fmap
-- scan_vocab) lin

merge_vocab :: Vocabulary -> Vocabulary -> Vocabulary
merge_vocab = MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched mergeWords)
  where
    mergeWords word = (+)

empty_vocab :: Vocabulary
empty_vocab = M.empty

scale_vocab :: (Show a, Floating a, RealFrac a, Monad m, MonadRecord (Data Lvl, (Data Msg, ())) m) 
  => Vocabulary
  -> Vocabulary
  -> Int
  -> Maybe a
  -> Bool
  -> Bool
  -> Maybe TrimRule 
  -> Bool
  -> m (M.Map VocabWord (Int, a))
scale_vocab raw_vocab vocab min_count sample dry_run keep_raw_vocab trim_rule update = do
  case update of
    False -> do
      debug $ "Loading a fresh vocabulary"
      debug $ foldr (++) "" ["min_count=", show min_count, " retains ", show retainWordCount, " unique words (",  show retain_unique_pct, "% of original ", show original_unique_total, ", drops ", show dropWordCount, ")"]
      debug $ foldr (++) "" ["min_count=", show min_count, " retains ", show retainWordTotal, " word corpus (", show retain_pct, "% of original ", show original_total, ", drops ", show dropWordTotal, ")"]
    True -> do
      debug $ "Updating model with new vocabulary"
      debug $ foldr (++) "" ["New added ", show addWordCount, " unique words (", show new_unique_pct, "% of original ", show original_unique_total, ") and increased the count of ", show preWordCount, " pre-existing words (", show pre_exist_unique_pct, "% of original ", show original_unique_total, ")"]
  debug $ foldr (++) "" ["sample=", show sample, " downsamples ", show downsample_unique, " most-common words"]
  debug $ foldr (++) "" ["downsampling leaves estimated ", show downsample_total, "word corpus (", show downsample_pct, "% of prior ", show retain_total, ")"]
  return $ MM.merge MM.dropMissing MM.dropMissing (MM.zipWithMatched (\k -> (,))) retainVocab sampleIntVocab
  where 
    (retainVocab, dropVocab) = M.partitionWithKey (\k v -> keep_vocab_item k v min_count trim_rule) raw_vocab
    keyValCountVocab = M.foldrWithKey' (\k v (kacc, vacc) -> (kacc + 1, vacc+v)) (0 :: Integer,0)
    (retainWordCount, retainWordTotal) = keyValCountVocab retainVocab
    (dropWordCount, dropWordTotal) = keyValCountVocab dropVocab
    original_unique_total = retainWordCount + dropWordCount
    original_total = retainWordTotal + dropWordTotal
    retain_unique_pct = (fromIntegral retainWordCount) * 100 / (fromIntegral . toInteger $ max original_unique_total 1)
    retain_total = retainWordTotal
    retain_pct = (fromIntegral $ retainWordTotal) * 100 / (fromIntegral . toInteger $ max original_total 1)
    
    preExistVocab = M.intersection retainVocab vocab
    addVocab = M.difference retainVocab vocab
    (preWordCount, preWordTotal) = keyValCountVocab preExistVocab
    (addWordCount, addWordTotal) = keyValCountVocab addVocab
    pre_exist_unique_pct = (fromIntegral preWordCount) * 100 / (fromIntegral . toInteger $ max original_unique_total 1)
    new_unique_pct = (fromIntegral addWordCount) * 100 / (fromIntegral . toInteger $ max original_unique_total 1)
    
    threshold_count = case sample of
      Nothing -> (fromIntegral . toInteger) retain_total
      Just sampleval -> case (sampleval < 1.0) of
        True -> sampleval * ((fromIntegral . toInteger) retain_total)
        False -> (fromIntegral . round) ((sampleval * (3 + sqrt 5)) / 2)
    
    retainNumVocab = M.map (fromIntegral . toInteger) retainVocab
    word_probability val = min ((sqrt(val / threshold_count) + 1) * (threshold_count / val)) 1.0
    downSampledVocab = M.map (word_probability) retainNumVocab
    multipliedVocab = M.unionWith (*) downSampledVocab retainNumVocab
    downsample_unique = M.foldr (+) 0.0 $ M.filter (< 1.0) downSampledVocab
    downsample_total = M.foldr (+) 0.0 multipliedVocab
    downsample_pct = (downsample_total) * 100 / (fromIntegral . toInteger $ max retain_total 1)
    sampleIntVocab = M.map (fromIntegral . round . (*(2**32))) downSampledVocab
