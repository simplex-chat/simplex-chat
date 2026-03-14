{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Messages.Batch
  ( MsgBatch (..),
    BatchMode (..),
    encodeBatchElement,
    encodeFwdElement,
    encodeBinaryBatch,
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
import Simplex.Messaging.Encoding (Large (..), smpEncode, smpEncodeList)

data BatchMode = BMJson | BMBinary
  deriving (Eq, Show)

-- | Encode a batch element with optional signature prefix.
-- Dual of elementP's 'S'/'{'cases.
encodeBatchElement :: Maybe SignedMsg -> ByteString -> ByteString
encodeBatchElement Nothing body = body
encodeBatchElement (Just SignedMsg {chatBinding, msgSignatures}) body =
  "S" <> smpEncode (chatBinding, msgSignatures) <> body

data MsgBatch = MsgBatch ByteString [SndMessage]

-- | Batches SndMessages in [Either ChatError SndMessage] into batches of ByteStrings.
-- BMJson mode: JSON arrays like [msg1,msg2,...]
-- BMBinary mode: Binary format =<count>(<len:2><body>)*
-- Preserves original errors in the list.
-- If a single element is passed, it is returned as is.
-- If an element exceeds maxLen, it is returned as ChatError.
-- Elements are encoded with signature prefix via encodeBatchElement.
batchMessages :: BatchMode -> Int -> [Either ChatError SndMessage] -> [Either ChatError MsgBatch]
batchMessages mode maxLen = addBatch . foldr addToBatch ([], [], [], 0, 0)
  where
    addToBatch :: Either ChatError SndMessage -> ([Either ChatError MsgBatch], [ByteString], [SndMessage], Int, Int) -> ([Either ChatError MsgBatch], [ByteString], [SndMessage], Int, Int)
    addToBatch (Left err) acc = (Left err : addBatch acc, [], [], 0, 0) -- step over original error
    addToBatch (Right msg@SndMessage {msgBody, signedMsg_}) acc@(batches, bodies, msgs, len, n)
      | batchLen mode len' n' <= maxLen = (batches, body : bodies, msg : msgs, len', n')
      | msgLen <= maxLen = (addBatch acc, [body], [msg], msgLen, 1)
      | otherwise = (errLarge msg : addBatch acc, [], [], 0, 0)
      where
        body = encodeBatchElement signedMsg_ msgBody
        msgLen = B.length body
        len' = len + msgLen
        n' = n + 1
        errLarge SndMessage {msgId} = Left $ ChatError $ CEInternalError ("large message " <> show msgId)
    addBatch :: ([Either ChatError MsgBatch], [ByteString], [SndMessage], Int, Int) -> [Either ChatError MsgBatch]
    addBatch (batches, bodies, msgs, _, n)
      | n == 0 = batches
      | otherwise =
          let encoded = encodeBatch mode bodies
           in Right (MsgBatch encoded msgs) : batches

-- | Batches delivery tasks into (batch, [taskIds], [largeTaskIds]).
-- Always uses binary batch format for relay groups.
batchDeliveryTasks1 :: VersionRangeChat -> Int -> NonEmpty MessageDeliveryTask -> (ByteString, [Int64], [Int64])
batchDeliveryTasks1 _vr maxLen = toResult . foldl' addToBatch ([], [], [], 0, 0) . L.toList
  where
    addToBatch :: ([ByteString], [Int64], [Int64], Int, Int) -> MessageDeliveryTask -> ([ByteString], [Int64], [Int64], Int, Int)
    addToBatch (msgBodies, taskIds, largeTaskIds, len, n) task
      -- too large: skip, record taskId in largeTaskIds
      | msgLen > maxLen = (msgBodies, taskIds, taskId : largeTaskIds, len, n)
      -- fits: include in batch
      -- batch overhead: '=' + count (2) + 2-byte length prefix per element
      | len' + (n + 1) * 2 + 2 <= maxLen = (fwdBody : msgBodies, taskId : taskIds, largeTaskIds, len', n + 1)
      -- doesn't fit: stop adding further messages
      | otherwise = (msgBodies, taskIds, largeTaskIds, len, n)
      where
        MessageDeliveryTask {taskId, fwdSender, brokerTs = fwdBrokerTs, msgBody, signedMsg_} = task
        fwdBody = encodeFwdElement GrpMsgForward {fwdSender, fwdBrokerTs} signedMsg_ msgBody
        msgLen = B.length fwdBody
        len' = len + msgLen
    toResult :: ([ByteString], [Int64], [Int64], Int, Int) -> (ByteString, [Int64], [Int64])
    toResult (msgBodies, taskIds, largeTaskIds, _, _) =
      let encoded = encodeBinaryBatch (reverse msgBodies)
       in (encoded, reverse taskIds, reverse largeTaskIds)

-- | Encode a batch element for relay groups: F<GrpMsgForward>[S<sigs>]<body>.
encodeFwdElement :: GrpMsgForward -> Maybe SignedMsg -> ByteString -> ByteString
encodeFwdElement fwd signedMsg_ body =
  "F" <> smpEncode fwd <> encodeBatchElement signedMsg_ body

encodeBatch :: BatchMode -> [ByteString] -> ByteString
encodeBatch _ [] = mempty
encodeBatch _ [msg] = msg
encodeBatch BMJson msgs = B.concat ["[", B.intercalate "," msgs, "]"]
encodeBatch BMBinary msgs = B.cons '=' $ smpEncodeList (map Large msgs)

-- Always uses batch format (no single-element shortcut) since elements may have F prefix.
encodeBinaryBatch :: [ByteString] -> ByteString
encodeBinaryBatch [] = mempty
encodeBinaryBatch msgs = B.cons '=' $ smpEncodeList (map Large msgs)

-- Returns length the batch would have if encoded.
-- `len` - the total length of all `n` encoded elements (including signature prefixes)
batchLen :: BatchMode -> Int -> Int -> Int
batchLen _ _ 0 = 0
batchLen _ len 1 = len
batchLen BMJson len n = len + n + 1 -- (n - 1) commas + 2 brackets
batchLen BMBinary len n = len + n * 2 + 2 -- 2-byte length prefix per element + '=' + count
