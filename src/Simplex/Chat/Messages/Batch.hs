{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Messages.Batch
  ( MsgBatch (..),
    batchMessages,
  )
where

import Data.ByteString.Builder (Builder, charUtf8, lazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Messages

data MsgBatch = MsgBatch Builder [SndMessage]
  deriving (Show)

-- | Batches [SndMessage] into batches of ByteString builders in form of JSON arrays.
-- Does not check if the resulting batch is a valid JSON.
-- If a single element is passed, it is returned as is (a JSON string).
-- If an element exceeds maxLen, it is returned as ChatError.
batchMessages :: Int64 -> [SndMessage] -> [Either ChatError MsgBatch]
batchMessages maxLen msgs =
  let (batches, batch, _, n) = foldr addToBatch ([], [], 0, 0) msgs
   in if n == 0 then batches else msgBatch batch : batches
  where
    msgBatch batch = Right (MsgBatch (encodeMessages batch) batch)
    addToBatch :: SndMessage -> ([Either ChatError MsgBatch], [SndMessage], Int64, Int) -> ([Either ChatError MsgBatch], [SndMessage], Int64, Int)
    addToBatch msg@SndMessage {msgBody} (batches, batch, len, n)
      | batchLen <= maxLen = (batches, msg : batch, len', n + 1)
      | msgLen <= maxLen = (batches', [msg], msgLen, 1)
      | otherwise = (errLarge msg : (if n == 0 then batches else batches'), [], 0, 0)
      where
        msgLen = LB.length msgBody
        batches' = msgBatch batch : batches
        len'
          | n == 0 = msgLen
          | otherwise = msgLen + len + 1 -- 1 accounts for comma
        batchLen
          | n == 0 = len'
          | otherwise = len' + 2 -- 2 accounts for opening and closing brackets
        errLarge SndMessage {msgId} = Left $ ChatError $ CEInternalError ("large message " <> show msgId)

encodeMessages :: [SndMessage] -> Builder
encodeMessages = \case
  [] -> mempty
  [msg] -> encodeMsg msg
  (msg : msgs) -> charUtf8 '[' <> encodeMsg msg <> mconcat [charUtf8 ',' <> encodeMsg msg' | msg' <- msgs] <> charUtf8 ']'
  where
    encodeMsg SndMessage {msgBody} = lazyByteString msgBody
