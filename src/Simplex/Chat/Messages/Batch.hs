{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Messages.Batch
  ( MsgBatch (..),
    BatchMode (..),
    batchMessages,
    batchDeliveryTasks1,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Delivery
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Types (VersionRangeChat)
import Simplex.Messaging.Encoding (Large (..), smpEncodeList)

data BatchMode = BMJson | BMBinary
  deriving (Eq, Show)

data MsgBatch = MsgBatch ByteString [SndMessage]

-- | Batches SndMessages in [Either ChatError SndMessage] into batches of ByteStrings.
-- BMJson mode: JSON arrays like [msg1,msg2,...]
-- BMBinary mode: Binary format =<count>(<len:2><body>)*
-- Preserves original errors in the list.
-- Does not check if the resulting batch is a valid JSON.
-- If a single element is passed, it is returned as is (a JSON string).
-- If an element exceeds maxLen, it is returned as ChatError.
-- TODO [member keys] signatures will also be encoded by batcher.
batchMessages :: BatchMode -> Int -> [Either ChatError SndMessage] -> [Either ChatError MsgBatch]
batchMessages mode maxLen = addBatch . foldr addToBatch ([], [], 0, 0)
  where
    addToBatch :: Either ChatError SndMessage -> ([Either ChatError MsgBatch], [SndMessage], Int, Int) -> ([Either ChatError MsgBatch], [SndMessage], Int, Int)
    addToBatch (Left err) acc = (Left err : addBatch acc, [], 0, 0) -- step over original error
    addToBatch (Right msg@SndMessage {msgBody}) acc@(batches, batch, len, n)
      | batchLen mode len' n' <= maxLen = (batches, msg : batch, len', n')
      | msgLen <= maxLen = (addBatch acc, [msg], msgLen, 1)
      | otherwise = (errLarge msg : addBatch acc, [], 0, 0)
      where
        msgLen = B.length msgBody
        len' = len + msgLen
        n' = n + 1
        errLarge SndMessage {msgId} = Left $ ChatError $ CEInternalError ("large message " <> show msgId)
    addBatch :: ([Either ChatError MsgBatch], [SndMessage], Int, Int) -> [Either ChatError MsgBatch]
    addBatch (batches, batch, _, n)
      | n == 0 = batches
      | otherwise =
          let encoded = encodeBatch mode (map body batch)
           in Right (MsgBatch encoded batch) : batches
    body SndMessage {msgBody} = msgBody

-- | Batches delivery tasks into (batch, [taskIds], [largeTaskIds]).
-- Always uses binary batch format for relay groups.
-- TODO [member keys] signatures will also be encoded here.
batchDeliveryTasks1 :: VersionRangeChat -> Int -> NonEmpty MessageDeliveryTask -> (ByteString, [Int64], [Int64])
batchDeliveryTasks1 vr maxLen = toResult . foldl' addToBatch ([], [], [], 0, 0) . L.toList
  where
    addToBatch :: ([ByteString], [Int64], [Int64], Int, Int) -> MessageDeliveryTask -> ([ByteString], [Int64], [Int64], Int, Int)
    addToBatch (msgBodies, taskIds, largeTaskIds, len, n) task
      -- too large: skip msgBody, record taskId in largeTaskIds
      | msgLen > maxLen = (msgBodies, taskIds, taskId : largeTaskIds, len, n)
      -- fits: include in batch
      | batchLen BMBinary len' (n + 1) <= maxLen = (msgBody : msgBodies, taskId : taskIds, largeTaskIds, len', n + 1)
      -- doesn't fit: stop adding further messages
      | otherwise = (msgBodies, taskIds, largeTaskIds, len, n)
      where
        MessageDeliveryTask {taskId, fwdSender, brokerTs, chatMessage} = task
        msgBody =
          let (memberId_, memberName_) = case fwdSender of
                FwdMember mid mname -> (Just mid, Just mname)
                FwdChannel -> (Nothing, Nothing)
              fwdEvt = XGrpMsgForward memberId_ memberName_ chatMessage brokerTs
              cm = ChatMessage {chatVRange = vr, msgId = Nothing, chatMsgEvent = fwdEvt}
           in chatMsgToBody cm
        msgLen = B.length msgBody
        len' = len + msgLen
    toResult :: ([ByteString], [Int64], [Int64], Int, Int) -> (ByteString, [Int64], [Int64])
    toResult (msgBodies, taskIds, largeTaskIds, _, _) =
      let encoded = encodeBatch BMBinary (reverse msgBodies)
       in (encoded, reverse taskIds, reverse largeTaskIds)

encodeBatch :: BatchMode -> [ByteString] -> ByteString
encodeBatch _ [] = mempty
encodeBatch _ [msg] = msg
encodeBatch BMJson msgs = B.concat ["[", B.intercalate "," msgs, "]"]
encodeBatch BMBinary msgs = B.cons '=' $ smpEncodeList (map Large msgs)

-- Returns length the batch would have if encoded
-- `len` - the total length of all `n` content elements
batchLen :: BatchMode -> Int -> Int -> Int
batchLen _ _ 0 = 0
batchLen _ len 1 = len
batchLen BMJson len n = len + n + 1 -- (n - 1) commas + 2 brackets
batchLen BMBinary len n = len + n * 2 + 2 -- length prefixes + '=' + count
