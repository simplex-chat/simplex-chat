{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Messages.Batch
  ( MsgBatch (..),
    batchMessages,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Messages

data MsgBatch = MsgBatch ByteString [SndMessage]

-- | Batches SndMessages in [Either ChatError SndMessage] into batches of ByteStrings in form of JSON arrays.
-- Preserves original errors in the list.
-- Does not check if the resulting batch is a valid JSON.
-- If a single element is passed, it is returned as is (a JSON string).
-- If an element exceeds maxLen, it is returned as ChatError.
batchMessages :: Int -> [Either ChatError SndMessage] -> [Either ChatError MsgBatch]
batchMessages maxLen = addBatch . foldr addToBatch ([], [], 0, 0)
  where
    msgBatch batch = Right (MsgBatch (encodeMessages batch) batch)
    addToBatch :: Either ChatError SndMessage -> ([Either ChatError MsgBatch], [SndMessage], Int, Int) -> ([Either ChatError MsgBatch], [SndMessage], Int, Int)
    addToBatch (Left err) acc = (Left err : addBatch acc, [], 0, 0) -- step over original error
    addToBatch (Right msg@SndMessage {msgBody}) acc@(batches, batch, len, n)
      | batchLen <= maxLen = (batches, msg : batch, len', n + 1)
      | msgLen <= maxLen = (addBatch acc, [msg], msgLen, 1)
      | otherwise = (errLarge msg : addBatch acc, [], 0, 0)
      where
        msgLen = B.length msgBody
        len'
          | n == 0 = msgLen
          | otherwise = msgLen + len + 1 -- 1 accounts for comma
        batchLen
          | n == 0 = len'
          | otherwise = len' + 2 -- 2 accounts for opening and closing brackets
        errLarge SndMessage {msgId} = Left $ ChatError $ CEInternalError ("large message " <> show msgId)
    addBatch :: ([Either ChatError MsgBatch], [SndMessage], Int, Int) -> [Either ChatError MsgBatch]
    addBatch (batches, batch, _, n) = if n == 0 then batches else msgBatch batch : batches
    encodeMessages :: [SndMessage] -> ByteString
    encodeMessages = \case
      [] -> mempty
      [msg] -> body msg
      msgs -> B.concat ["[", B.intercalate "," (map body msgs), "]"]
    body SndMessage {msgBody} = msgBody
