{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module MessageBatching (batchingTests) where

import Crypto.Number.Serialize (os2ip)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Either (partitionEithers)
import Data.Int (Int64)
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Messages.Batch
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Messages (SndMessage (..))
import Simplex.Chat.Protocol (SharedMsgId (..), maxChatMsgSize)
import Test.Hspec

batchingTests :: Spec
batchingTests = describe "message batching tests" $ do
  testBatchingCorrectness
  it "image x.msg.new and x.msg.file.descr should fit into single batch" testImageFitsSingleBatch

instance IsString SndMessage where
  fromString s = SndMessage {msgId, sharedMsgId = SharedMsgId "", msgBody = LB.fromStrict s'}
    where
      s' = encodeUtf8 $ T.pack s
      msgId = fromInteger $ os2ip s'

deriving instance Eq SndMessage

instance IsString ChatError where
  fromString s = ChatError $ CEInternalError ("large message " <> show msgId)
    where
      s' = encodeUtf8 $ T.pack s
      msgId = fromInteger (os2ip s') :: Int64

testBatchingCorrectness :: Spec
testBatchingCorrectness = describe "correctness tests" $ do
  runBatcherTest 8 ["a"] [] ["a"]
  runBatcherTest 8 ["a", "b"] [] ["[a,b]"]
  runBatcherTest 8 ["a", "b", "c"] [] ["[a,b,c]"]
  runBatcherTest 8 ["a", "bb", "c"] [] ["[a,bb,c]"]
  runBatcherTest 8 ["a", "b", "c", "d"] [] ["a", "[b,c,d]"]
  runBatcherTest 8 ["a", "bb", "c", "d"] [] ["a", "[bb,c,d]"]
  runBatcherTest 8 ["a", "bb", "c", "de"] [] ["[a,bb]", "[c,de]"]
  runBatcherTest 8 ["a", "b", "c", "d", "e"] [] ["[a,b]", "[c,d,e]"]
  runBatcherTest 8 ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] [] ["a", "[b,c,d]", "[e,f,g]", "[h,i,j]"]
  runBatcherTest 8 ["aaaaa"] [] ["aaaaa"]
  runBatcherTest 8 ["8aaaaaaa"] [] ["8aaaaaaa"]
  runBatcherTest 8 ["aaaa", "bbbb"] [] ["aaaa", "bbbb"]
  runBatcherTest 8 ["aa", "bbb", "cc", "dd"] [] ["[aa,bbb]", "[cc,dd]"]
  runBatcherTest 8 ["aa", "bbb", "cc", "dd", "eee", "fff", "gg", "hh"] [] ["aa", "[bbb,cc]", "[dd,eee]", "fff", "[gg,hh]"]
  runBatcherTest 8 ["9aaaaaaaa"] ["9aaaaaaaa"] []
  runBatcherTest 8 ["aaaaa", "bbb", "cc"] [] ["aaaaa", "[bbb,cc]"]
  runBatcherTest 8 ["8aaaaaaa", "bbb", "cc"] [] ["8aaaaaaa", "[bbb,cc]"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc"] ["9aaaaaaaa"] ["[bbb,cc]"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc", "dd"] ["9aaaaaaaa"] ["bbb", "[cc,dd]"]
  runBatcherTest 8 ["9aaaaaaaa", "bbb", "cc", "dd", "e"] ["9aaaaaaaa"] ["[bbb,cc]", "[dd,e]"]
  runBatcherTest 8 ["bbb", "cc", "aaaaa"] [] ["[bbb,cc]", "aaaaa"]
  runBatcherTest 8 ["bbb", "cc", "8aaaaaaa"] [] ["[bbb,cc]", "8aaaaaaa"]
  runBatcherTest 8 ["bbb", "cc", "9aaaaaaaa"] ["9aaaaaaaa"] ["[bbb,cc]"]
  runBatcherTest 8 ["bbb", "cc", "dd", "9aaaaaaaa"] ["9aaaaaaaa"] ["bbb", "[cc,dd]"]
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
  runBatcherTest 8 ["bb", "cc", "dd", "9aaaaaaaa", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["bb", "cc", "9aaaaaaaa", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["[bb,cc]", "dd"]
  runBatcherTest 8 ["bb", "9aaaaaaaa", "cc", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["bb", "9aaaaaaaa", "cc", "10aaaaaaaa", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "cc", "dd"]
  runBatcherTest 8 ["9aaaaaaaa", "bb", "cc", "dd", "10aaaaaaaa"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["9aaaaaaaa", "bb", "10aaaaaaaa", "cc", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]
  runBatcherTest 8 ["9aaaaaaaa", "10aaaaaaaa", "bb", "cc", "dd"] ["9aaaaaaaa", "10aaaaaaaa"] ["bb", "[cc,dd]"]

testImageFitsSingleBatch :: IO ()
testImageFitsSingleBatch = do
  -- 14000 (limit for encoded image used in UI)
  -- + 300 (remaining x.msg.new metadata, rounded up, actual example was 266)
  let xMsgNewRoundedSize = 14300
  -- size of x.msg.file.descr body for a file of size
  -- 261_120 bytes (MAX_IMAGE_SIZE in UI), rounded up, example was 743
  let descrRoundedSize = 800

  let xMsgNewStr = LB.replicate xMsgNewRoundedSize 1
      descrStr = LB.replicate descrRoundedSize 2
      msg s = SndMessage {msgId = 0, sharedMsgId = SharedMsgId "", msgBody = s}
      batched = "[" <> xMsgNewStr <> "," <> descrStr <> "]"

  runBatcherTest' maxChatMsgSize [msg xMsgNewStr, msg descrStr] [] [batched]

runBatcherTest :: Int64 -> [SndMessage] -> [ChatError] -> [LB.ByteString] -> Spec
runBatcherTest maxLen msgs expectedErrors expectedBatches =
  it
    ( (show (map (\SndMessage {msgBody} -> msgBody) msgs) <> ", limit " <> show maxLen <> ": should return ")
        <> (show (length expectedErrors) <> " large, ")
        <> (show (length expectedBatches) <> " batches")
    )
    (runBatcherTest' maxLen msgs expectedErrors expectedBatches)

runBatcherTest' :: Int64 -> [SndMessage] -> [ChatError] -> [LB.ByteString] -> IO ()
runBatcherTest' maxLen msgs expectedErrors expectedBatches = do
  let (errors, batches) = partitionEithers $ batchMessages maxLen msgs
      batchedStrs = map (\(MsgBatch builder _) -> toLazyByteString builder) batches
  testErrors errors `shouldBe` testErrors expectedErrors
  batchedStrs `shouldBe` expectedBatches
  where
    testErrors = map (\case ChatError (CEInternalError s) -> Just s; _ -> Nothing)
