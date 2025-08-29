{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Messages.Batch
  ( MsgBatch (..),
    batchMessages,
    batchDeliveryTasks1,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as L
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

-- | Batches delivery tasks into (batch, [taskIds], [largeTaskIds]).
batchDeliveryTasks1 :: Int -> NonEmpty MessageForwardTask -> (ByteString, [Int64], [Int64])
batchDeliveryTasks1 maxLen = toResult . foldl' addToBatch ([], [], [], 0, 0) . L.toList
  where
    addToBatch :: (ByteString, [Int64], [Int64], Int, Int) -> MessageForwardTask -> (ByteString, [Int64], [Int64], Int, Int)
    addToBatch (bodies, taskIds, largeTaskIds, len, n) task
      -- too large: skip body, record taskId in largeTaskIds
      | msgLen > maxLen = (bodies, taskIds, taskId : largeTaskIds, len, n)
      -- fits: include in batch
      | batchLen <= maxLen = (body : bodies, taskId : taskIds, largeTaskIds, len', n + 1)
      -- doesn’t fit: stop adding further messages
      | otherwise = (bodies, taskIds, largeTaskIds, len, n)
      where
        MessageForwardTask {taskId, senderMemberId, senderMemberName, brokerTs, chatMessage, messageFromChannel} = task
        -- TODO [channels fwd] handle messageFromChannel (null memberId in XGrpMsgForward)
        body =
          let fwdEvt = XGrpMsgForward senderMemberId senderMemberName chatMessage brokerTs
              cm = ChatMessage {chatVRange = vr, msgId = Nothing, chatMsgEvent = fwdEvt}
            in chatMsgToBody cm
        msgLen = B.length msgBody
        len'
          | n == 0 = msgLen
          | otherwise = msgLen + len + 1 -- 1 accounts for comma
        batchLen
          | n == 0 = len'
          | otherwise = len' + 2 -- 2 accounts for opening and closing brackets
    toResult :: ([ByteString], [Int64], [Int64], Int, Int) -> (ByteString, [Int64], [Int64])
    toResult (bodies, taskIds, largeTaskIds, _, _) =
      (encodeMessagesBodies (reverse bodies), reverse taskIds, reverse largeTaskIds)
    encodeMessages :: [ByteString] -> ByteString
    encodeMessages = \case
      [] -> mempty
      [msg] -> msg
      msgs -> B.concat ["[", B.intercalate "," msgs, "]"]
