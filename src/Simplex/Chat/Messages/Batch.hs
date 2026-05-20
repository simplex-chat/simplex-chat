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
    prependBatchElement,
    packIntoBody,
    packIntoBatches,
    maxBatchElementSize,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, ord)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (foldl', sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..))
import Simplex.Chat.Delivery
import Simplex.Chat.Messages
import Simplex.Chat.Protocol
import Simplex.Chat.Types (GroupMember, VersionRangeChat)
import Simplex.Messaging.Encoding (Large (..), smpEncode, smpEncodeList)

data BatchMode = BMJson | BMBinary
  deriving (Eq, Show)

-- | Encode a batch element with optional signature prefix.
-- Dual of elementP's '/'/'{'cases.
encodeBatchElement :: Maybe SignedMsg -> ByteString -> ByteString
encodeBatchElement Nothing body = body
encodeBatchElement (Just SignedMsg {chatBinding, signatures}) body =
  "/" <> smpEncode (chatBinding, signatures) <> body

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

-- | Batches delivery tasks into (batch, accepted, large).
-- Always uses binary batch format for relay groups.
batchDeliveryTasks1 :: VersionRangeChat -> Int -> NonEmpty MessageDeliveryTask -> (ByteString, [MessageDeliveryTask], [MessageDeliveryTask])
batchDeliveryTasks1 _vr maxLen = toResult . foldl' addToBatch ([], [], [], 0, 0) . L.toList
  where
    addToBatch :: ([ByteString], [MessageDeliveryTask], [MessageDeliveryTask], Int, Int) -> MessageDeliveryTask -> ([ByteString], [MessageDeliveryTask], [MessageDeliveryTask], Int, Int)
    addToBatch (msgBodies, accepted, large, len, n) task
      -- too large: skip, record in large
      | msgLen > maxLen = (msgBodies, accepted, task : large, len, n)
      -- fits: include in batch
      -- batch overhead: '=' + count (2) + 2-byte length prefix per element
      | len' + (n + 1) * 2 + 2 <= maxLen = (msgBody : msgBodies, task : accepted, large, len', n + 1)
      -- doesn't fit: stop adding further messages
      | otherwise = (msgBodies, accepted, large, len, n)
      where
        MessageDeliveryTask {fwdSender, brokerTs = fwdBrokerTs, verifiedMsg} = task
        msgBody = encodeFwdElement GrpMsgForward {fwdSender, fwdBrokerTs} verifiedMsg
        msgLen = B.length msgBody
        len' = len + msgLen
    toResult :: ([ByteString], [MessageDeliveryTask], [MessageDeliveryTask], Int, Int) -> (ByteString, [MessageDeliveryTask], [MessageDeliveryTask])
    toResult (msgBodies, accepted, large, _, _) =
      let encoded = encodeBinaryBatch (reverse msgBodies)
       in (encoded, reverse accepted, reverse large)

-- | Encode a batch element for relay groups: ><GrpMsgForward>[/<sigs>]<body>.
encodeFwdElement :: GrpMsgForward -> VerifiedMsg 'Json -> ByteString
encodeFwdElement fwd verifiedMsg = ">" <> smpEncode fwd <> encodeBatchElement signedMsg_ msgBody
  where
    (_, signedMsg_, msgBody) = verifiedMsgParts verifiedMsg

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

-- | Largest element that fits a singleton 'encodeBinaryBatch' inside an
-- agent SMP message: '=' + count(1) + Word16 length prefix(2) = 4 bytes
-- of framing on top of the element.
maxBatchElementSize :: Int
maxBatchElementSize = maxEncodedMsgLength - 4

-- | Prepend one element to an existing binary-batch body without re-parsing
-- the rest. Body format: '=' <count:1B> ( <len:Word16> <element> )*.
-- 'error' on malformed input or count overflow — invariant assertions;
-- packers below enforce the byte budget that keeps this unreachable.
prependBatchElement :: ByteString -> ByteString -> ByteString
prependBatchElement element body
  | B.null body = encodeBinaryBatch [element]
  | B.head body /= '=' = error "prependBatchElement: invalid batch body (missing '=' prefix)"
  | B.length body < 2 = error "prependBatchElement: invalid batch body (missing count byte)"
  | oldCount >= 255 = error "prependBatchElement: batch element count overflow"
  | otherwise =
      let newCount = chr (oldCount + 1)
          encodedElement = smpEncode (Large element)
          rest = B.drop 2 body
       in B.cons '=' (B.cons newCount (encodedElement <> rest))
  where
    oldCount = ord (B.index body 1)

-- | Greedy-pack elements into 'body' smallest-first while the result fits
-- 'maxLen'. Returns (extBody, accepted, overflow): the senders whose
-- profile is now inline AND the labeled elements that did not fit.
-- Per-element budget: 2-byte Word16 length prefix plus the element bytes.
packIntoBody :: Int -> ByteString -> [(GroupMember, ByteString)] -> (ByteString, [GroupMember], [(GroupMember, ByteString)])
packIntoBody maxLen body labeled =
  let (b, accepted, overflow) = foldl' step (body, [], []) (sortBy (compare `on` (B.length . snd)) labeled)
   in (b, reverse accepted, reverse overflow)
  where
    step (b, accepted, overflow) (s, e)
      | B.length b + 2 + B.length e <= maxLen = (prependBatchElement e b, s : accepted, overflow)
      | otherwise = (b, accepted, (s, e) : overflow)

-- | Greedy-pack elements into one or more batches, each bounded by 'maxLen'.
-- Returns each batch paired with the senders whose profile it carries, so
-- the worker's recipient-misses-sender check uses 'GroupMember' identity
-- rather than encoded bytes. Ascending size keeps small-only batches
-- together.
packIntoBatches :: Int -> [(GroupMember, ByteString)] -> [(ByteString, [GroupMember])]
packIntoBatches maxLen = go . sortBy (compare `on` (B.length . snd))
  where
    go [] = []
    go ((s, e) : rest) =
      let (taken, left) = takeFitting [(s, e)] (singletonLen e) rest
          batch = encodeBinaryBatch (reverse (map snd taken))
       in (batch, reverse (map fst taken)) : go left
    -- A one-element batch is encodeBinaryBatch [e] = '=' + 1-byte count +
    -- 2-byte length + e bytes = B.length e + 4. Subsequent additions cost
    -- 2 + B.length e (length prefix only — '=' and count are already in).
    singletonLen e = B.length e + 4
    takeFitting acc _ [] = (acc, [])
    takeFitting acc accLen ((s, e) : rest)
      | accLen + 2 + B.length e <= maxLen =
          takeFitting ((s, e) : acc) (accLen + 2 + B.length e) rest
      | otherwise = (acc, (s, e) : rest)
