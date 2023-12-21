{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.ByteStringBatcher
  ( HasByteString (..),
    BSBatch (..),
    BSBatcherOutput (..),
    batchByteStringObjects,
    partitionBatches,
  )
where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L

class HasByteString a where
  getByteString :: a -> L.ByteString

instance HasByteString L.ByteString where
  getByteString = id

data HasByteString a => BSBatch a = BSBatch BB.Builder [a]

data HasByteString a => BSBatcherOutput a
  = BatcherOutputBatch (BSBatch a)
  | BatcherOutputLarge a

-- | Batches instances of HasByteString into batches of ByteString builders in form of JSON arrays.
-- Does not check if the resulting batch is a valid JSON. If it is required,
-- getByteString should return ByteString encoded JSON object.
-- If a single element is passed, it is returned in form of JSON object instead.
-- If an element exceeds batchLenLimit, it is returned as BatcherOutputLarge.
batchByteStringObjects :: forall a. HasByteString a => Int64 -> NonEmpty a -> [BSBatcherOutput a]
batchByteStringObjects batchLenLimit = reverse . mkBatch []
  where
    mkBatch :: [BSBatcherOutput a] -> NonEmpty a -> [BSBatcherOutput a]
    mkBatch batches objs =
      let (batch, objs_) = encodeBatch mempty 0 0 [] objs
          batches' = batch : batches
       in maybe batches' (mkBatch batches') objs_
    encodeBatch :: BB.Builder -> Int64 -> Int -> [a] -> NonEmpty a -> (BSBatcherOutput a, Maybe (NonEmpty a))
    encodeBatch builder len cnt batchedObjs remainingObjs@(obj :| objs_)
      -- batched string fits
      | len' <= maxSize' =
          case L.nonEmpty objs_ of
            Just objs' -> encodeBatch builder' len' cnt' batchedObjs' objs'
            Nothing -> completeBatchLastStrFits
      -- batched string doesn't fit
      | cnt == 0 = (BatcherOutputLarge obj, L.nonEmpty objs_)
      | otherwise = completeBatchStrDoesntFit
      where
        bStr = getByteString obj
        cnt' = cnt + 1
        (len', builder')
          | cnt' == 1 =
              ( LB.length bStr, -- initially len = 0
                BB.lazyByteString bStr
              )
          | cnt' == 2 =
              ( len + LB.length bStr + 2, -- for opening bracket "[" and comma ","
                "[" <> builder <> "," <> BB.lazyByteString bStr
              )
          | otherwise =
              ( len + LB.length bStr + 1, -- for comma ","
                builder <> "," <> BB.lazyByteString bStr
              )
        maxSize'
          | cnt' == 1 = batchLenLimit
          | otherwise = batchLenLimit - 1 -- for closing bracket "]"
        batchedObjs' :: [a]
        batchedObjs' = obj : batchedObjs
        completeBatchLastStrFits :: (BSBatcherOutput a, Maybe (NonEmpty a))
        completeBatchLastStrFits =
          (BatcherOutputBatch $ BSBatch completeBuilder (reverse batchedObjs'), Nothing)
          where
            completeBuilder
              | cnt' == 1 = builder' -- if last string fits, we look at current cnt'
              | otherwise = builder' <> "]"
        completeBatchStrDoesntFit :: (BSBatcherOutput a, Maybe (NonEmpty a))
        completeBatchStrDoesntFit =
          (BatcherOutputBatch $ BSBatch completeBuilder (reverse batchedObjs), Just remainingObjs)
          where
            completeBuilder
              | cnt == 1 = builder -- if string doesn't fit, we look at previous cnt
              | otherwise = builder <> "]"

-- | Partitions list of batcher outputs into lists of batches and large objects.
partitionBatches :: forall a. HasByteString a => [BSBatcherOutput a] -> ([a], [BSBatch a])
partitionBatches = foldr partition' ([], [])
  where
    partition' :: BSBatcherOutput a -> ([a], [BSBatch a]) -> ([a], [BSBatch a])
    partition' (BatcherOutputBatch bStrBatch) (largeBStrs, bStrBatches) = (largeBStrs, bStrBatch : bStrBatches)
    partition' (BatcherOutputLarge largeBStr) (largeBStrs, bStrBatches) = (largeBStr : largeBStrs, bStrBatches)
