{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Main where

import Lib
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import           "zip-conduit" Codec.Archive.Zip
import           Data.Text.Internal as DT
import qualified Data.Text as T
import qualified Data.List as L
import           Hakyll.Web.Html (stripTags)

fpath = "unlabeledTrainData.tsv.zip"

main :: IO ()
main = do
  corpus <- getLines
    {-.| CL.mapM scan_vocab-}
    {-.| CL.fold merge_vocab empty_vocab-}
  (liftIO . print) $ corpus
  return ()

getLines = withArchive fpath $ do
  n:_ <- entryNames
  lines <- sourceEntry n
    $ CT.decode CT.utf8
    .| firstLines 30
    .| columnex
    .| transf
    .| CL.consume
  return lines

firstLines :: Monad m => Int -> Conduit Text m Text
firstLines n = CT.lines
    =$ (CL.drop 1 >> CL.map id)
    =$ CL.isolate n

columnex :: Monad m => Conduit Text m Text
columnex = CL.map $ second
  where second = fmap (\(_:x:_) -> x) $ T.splitOn "\t"

-- transf = CL.map $ (map L.head) . L.group . L.sort . T.unpack . (foldl1 T.append) . concat . (fmap T.words) . sentences . T.toLower . clean . stripTagsText
-- (fmap (VocabSentence . (fmap VocabWord))) .

transf :: Monad m => Conduit Text m [VocabSentence]
transf = CL.map $ (fmap T.words) . sentences . T.toLower . removeapostrophe . clean . stripTagsText
  where
    sentences = T.split (\x -> or $ fmap (x==) sentenceTerms)
    sentenceTerms = ['.', '?', '!']
    clean = flip (foldr (\x -> T.replace x (T.pack " "))) (fmap T.pack punctuation)
    punctuation = ["\"", "\\", "/", "-", ",", ":", ";", "(", ")", "&", "%"]
    removeapostrophe = flip (foldr (\x -> T.replace x (T.pack ""))) (fmap T.pack ["'"])
    stripTagsText = T.pack . stripTags . T.unpack

printSink :: (Show a, MonadIO m) => Sink a m ()
printSink = CL.mapM_ (liftIO . print)
