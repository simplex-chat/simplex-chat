{-# LANGUAGE OverloadedStrings #-}

module ByteStringBatcherTests where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.List.NonEmpty (fromList)
import Simplex.Chat.ByteStringBatcher
import Simplex.Chat.Protocol (maxChatMsgSize)
import Test.Hspec

byteStringBatcherTests :: Spec
byteStringBatcherTests = describe "ByteStringBatcher tests" $ do
  testBatchingCorrectness
  it "image x.msg.new and x.msg.file.descr should fit into single batch" testImageFitsSingleBatch

testBatchingCorrectness :: Spec
testBatchingCorrectness = describe "correctness tests" $ do
  runBatcherTest 8 ["a"] [] ["a"]
  runBatcherTest 8 ["a", "b"] [] ["[a,b]"]
  runBatcherTest 8 ["a", "b", "c"] [] ["[a,b,c]"]
  runBatcherTest 8 ["a", "bb", "c"] [] ["[a,bb,c]"]
  runBatcherTest 8 ["a", "b", "c", "d"] [] ["[a,b,c]", "d"]
  runBatcherTest 8 ["a", "bb", "c", "d"] [] ["[a,bb,c]", "d"]
  runBatcherTest 8 ["a", "bb", "c", "de"] [] ["[a,bb,c]", "de"]
  runBatcherTest 8 ["a", "b", "c", "d", "e"] [] ["[a,b,c]", "[d,e]"]
  runBatcherTest 8 ["aaaaa"] [] ["aaaaa"]
  runBatcherTest 8 ["8aaaaaaa"] [] ["8aaaaaaa"]
  runBatcherTest 8 ["aaaa", "bbbb"] [] ["aaaa", "bbbb"]
  runBatcherTest 8 ["aa", "bbb", "cc", "dd"] [] ["[aa,bbb]", "[cc,dd]"]
  runBatcherTest 8 ["9aaaaaaaa"] ["9aaaaaaaa"] []
  runBatcherTest 8 ["aaaaa", "bbb", "cc"] [] ["aaaaa", "[bbb,cc]"]
  runBatcherTest 8 ["8aaaaaaa", "bbb", "cc"] [] ["8aaaaaaa", "[bbb,cc]"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc"] ["9aaaaaaaa"] ["[bbb,cc]"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc", "dd"] ["9aaaaaaaa"] ["[bbb,cc]", "dd"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc", "dd", "e"] ["9aaaaaaaa"] ["[bbb,cc]", "[dd,e]"]
  runBatcherTest 8 ["bbb", "cc", "aaaaa"] [] ["[bbb,cc]", "aaaaa"]
  runBatcherTest 8 ["bbb", "cc", "8aaaaaaa"] [] ["[bbb,cc]", "8aaaaaaa"]
  runBatcherTest 8 ["bbb", "cc", "9aaaaaaaa"] ["9aaaaaaaa"] ["[bbb,cc]"]
  runBatcherTest 8 ["bbb", "cc", "dd", "9aaaaaaaa"] ["9aaaaaaaa"] ["[bbb,cc]", "dd"]
  runBatcherTest 8 ["bbb", "cc", "dd", "e", "9aaaaaaaa"] ["9aaaaaaaa"] ["[bbb,cc]", "[dd,e]"]
  runBatcherTest 8 ["bbb", "cc", "aaaaa", "dd"] [] ["[bbb,cc]", "aaaaa", "dd"]
  runBatcherTest 8 ["bbb", "cc", "aaaaa", "dd", "e"] [] ["[bbb,cc]", "aaaaa", "[dd,e]"]
  runBatcherTest 8 ["bbb", "cc", "8aaaaaaa", "dd"] [] ["[bbb,cc]", "8aaaaaaa", "dd"]
  runBatcherTest 8 ["bbb", "cc", "8aaaaaaa", "dd", "e"] [] ["[bbb,cc]", "8aaaaaaa", "[dd,e]"]
  runBatcherTest 8 ["bbb", "cc", "9aaaaaaaa"] ["9aaaaaaaa"] ["[bbb,cc]"]
  runBatcherTest 8 ["bbb", "cc", "9aaaaaaaa", "dd"] ["9aaaaaaaa"] ["[bbb,cc]", "dd"]
  runBatcherTest 8 ["bbb", "cc", "9aaaaaaaa", "dd", "e"] ["9aaaaaaaa"] ["[bbb,cc]", "[dd,e]"]
  runBatcherTest 8 ["9aaaaaaaa", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] []
  runBatcherTest 8 ["8aaaaaaa", "9aaaaaaaa", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["8aaaaaaa"]
  runBatcherTest 8 ["9aaaaaaaa", "8aaaaaaa", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["8aaaaaaa"]
  runBatcherTest 8 ["9aaaaaaaa", "10aaaaaaaa", "8aaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["8aaaaaaa"]
  runBatcherTest 8 ["bb", "cc", "dd", "9aaaaaaaa", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["[bb,cc]", "dd"]
  runBatcherTest 8 ["bb", "cc", "9aaaaaaaa", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["[bb,cc]", "dd"]
  runBatcherTest 8 ["bb", "9aaaaaaaa", "cc", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["bb", "9aaaaaaaa", "cc", "10aaaaaaaa", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "cc", "dd"]
  runBatcherTest 8 ["9aaaaaaaa", "bb", "cc", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["[bb,cc]", "dd"]
  runBatcherTest 8 ["9aaaaaaaa", "bb", "10aaaaaaaa", "cc", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["9aaaaaaaa", "10aaaaaaaa", "bb", "cc", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["[bb,cc]", "dd"]

testImageFitsSingleBatch :: IO ()
testImageFitsSingleBatch = do
  -- 14000 (limit for encoded image used in UI)
  -- + 300 (remaining x.msg.new metadata, rounded up, actual example was 266)
  let xMsgNewRoundedSize = 14300
  -- size of x.msg.file.descr body for a file of size
  -- 261_120 bytes (MAX_IMAGE_SIZE in UI), rounded up, example was 743
  let descrRoundedSize = 800

  let xMsgNewStr = L.replicate xMsgNewRoundedSize 1
      descrStr = L.replicate descrRoundedSize 2

  runBatcherTest' maxChatMsgSize [xMsgNewStr, descrStr] [] ["[" <> xMsgNewStr <> "," <> descrStr <> "]"]

runBatcherTest :: Int64 -> [L.ByteString] -> [L.ByteString] -> [L.ByteString] -> SpecWith ()
runBatcherTest batchLenLimit bStrs expectedLargeStrs expectedBatchedStrs =
  it
    ( (show bStrs <> ", limit " <> show batchLenLimit <> ": should return ")
        <> (show (length expectedLargeStrs) <> " large, ")
        <> (show (length expectedBatchedStrs) <> " batches")
    )
    (runBatcherTest' batchLenLimit bStrs expectedLargeStrs expectedBatchedStrs)

runBatcherTest' :: Int64 -> [L.ByteString] -> [L.ByteString] -> [L.ByteString] -> IO ()
runBatcherTest' batchLenLimit bStrs expectedLargeStrs expectedBatchedStrs = do
  let (largeStrs, batches) = partitionBatches $ batchByteStringObjects batchLenLimit (fromList bStrs)
      batchedStrs = map (\(BSBatch batchBuilder _) -> BB.toLazyByteString batchBuilder) batches
  largeStrs `shouldBe` expectedLargeStrs
  batchedStrs `shouldBe` expectedBatchedStrs
