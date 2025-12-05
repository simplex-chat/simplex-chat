{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Types.MemberRelations
  ( IntroductionDirection (..),
    MemberRelation (..),
    toIntroductionInt,
    fromIntroductionInt,
    toRelationInt,
    fromRelationInt,
    getRelation,
    getRelation',
    getRelationsIndexes,
    setRelation,
    setRelations,
    setRelationConnected,
    setNewRelation,
    setNewRelations,
  )
where

import Control.Monad
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (toForeignPtr, unsafeCreate)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

data IntroductionDirection
  = IDSubjectIntroduced -- Member described by vector (subject member, vector "owner") is introduced to member referenced in vector
  | IDReferencedIntroduced -- Member referenced in vector is introduced to subject member
  deriving (Eq, Show)

toIntroductionInt :: IntroductionDirection -> Word8
toIntroductionInt = \case
  IDSubjectIntroduced -> 0
  IDReferencedIntroduced -> 1

fromIntroductionInt :: Word8 -> IntroductionDirection
fromIntroductionInt = \case
  0 -> IDSubjectIntroduced
  1 -> IDReferencedIntroduced
  _ -> IDSubjectIntroduced

data MemberRelation
  = MRNew
  | MRIntroduced
  | MRSubjectConnected -- Subject member notified about connection to referenced member
  | MRReferencedConnected -- Referenced member notified about connection to subject member
  | MRConnected -- Both members notified about connection
  deriving (Eq, Ord, Show)

toRelationInt :: MemberRelation -> Word8
toRelationInt = \case
  MRNew -> 0
  MRIntroduced -> 1
  MRSubjectConnected -> 2
  MRReferencedConnected -> 3
  MRConnected -> 4

fromRelationInt :: Word8 -> MemberRelation
fromRelationInt = \case
  0 -> MRNew
  1 -> MRIntroduced
  2 -> MRSubjectConnected
  3 -> MRReferencedConnected
  4 -> MRConnected
  _ -> MRNew

-- Bit layout: 4 reserved | 1 direction | 3 status

-- | Get the relation status of a member at a given index from the relations vector.
-- Returns 'MRNew' if the vector is not long enough (lazy initialization).
getRelation :: Int64 -> ByteString -> MemberRelation
getRelation i v = snd $ getRelation' i v

-- | Get both direction and status of a member at a given index from the relations vector.
-- Returns (IDSubjectIntroduced, MRNew) if the vector is not long enough (lazy initialization).
getRelation' :: Int64 -> ByteString -> (IntroductionDirection, MemberRelation)
getRelation' i v
  | i < 0 || fromIntegral i >= B.length v = (IDSubjectIntroduced, MRNew)
  | otherwise =
      let b = v `B.index` fromIntegral i
       in (fromIntroductionInt $ (b .&. directionMask) `shiftR` 3, fromRelationInt $ b .&. statusMask)

-- | Get the indexes of members that satisfy the given relation predicate.
getRelationsIndexes :: (MemberRelation -> Bool) -> ByteString -> [Int64]
getRelationsIndexes p v = [i | i <- [0 .. fromIntegral (B.length v) - 1], p (getRelation i v)]

-- | Set the relation status of a member at a given index in the relations vector.
-- Preserves the introduction direction. Expands the vector lazily if needed.
setRelation :: Int64 -> MemberRelation -> ByteString -> ByteString
setRelation i r v
  | i >= 0 = setRelations [(i, r)] v
  | otherwise = v

-- | Set multiple relation statuses at once.
-- Preserves the introduction direction. Expands the vector lazily if needed.
setRelations :: [(Int64, MemberRelation)] -> ByteString -> ByteString
setRelations relations v = setRelations' [(i, Nothing, r) | (i, r) <- relations] v

-- | Set relation to connected state based on passed status and current status.
-- newStatus should be MRSubjectConnected or MRReferencedConnected, otherwise returns vector unchanged.
-- Logic:
-- - if newStatus is complementary to oldStatus -> set MRConnected
-- - if newStatus > oldStatus (by enum order) -> set newStatus
-- - otherwise don't update
setRelationConnected :: Int64 -> MemberRelation -> ByteString -> ByteString
setRelationConnected i newStatus v
  | newStatus /= MRSubjectConnected && newStatus /= MRReferencedConnected = v
  | otherwise = case status' of
      Nothing -> v
      Just s -> setRelation i s v
  where
    oldStatus = getRelation i v
    status' = case (oldStatus, newStatus) of
      -- complementary statuses -> MRConnected
      (MRSubjectConnected, MRReferencedConnected) -> Just MRConnected
      (MRReferencedConnected, MRSubjectConnected) -> Just MRConnected
      -- newStatus > oldStatus -> set newStatus
      _ | newStatus > oldStatus -> Just newStatus
        | otherwise -> Nothing

-- | Set a new relation with both direction and status at a given index.
-- Expands the vector lazily if needed.
setNewRelation :: Int64 -> IntroductionDirection -> MemberRelation -> ByteString -> ByteString
setNewRelation i dir r v
  | i >= 0 = setNewRelations [(i, dir, r)] v
  | otherwise = v

-- | Set multiple new relations with both direction and status at once.
-- Expands the vector lazily if needed.
setNewRelations :: [(Int64, IntroductionDirection, MemberRelation)] -> ByteString -> ByteString
setNewRelations relations v = setRelations' [(i, Just dir, r) | (i, dir, r) <- relations] v

-- | Internal function to set relations with optional direction update.
-- If direction is Nothing, preserves existing direction; if Just, sets new direction.
setRelations' :: [(Int64, Maybe IntroductionDirection, MemberRelation)] -> ByteString -> ByteString
setRelations' [] v = v
setRelations' relations v =
  let (fp, off, len) = toForeignPtr v
      newLen = max len $ fromIntegral $ maximum (map (\(i, _, _) -> i) relations) + 1
   in unsafeCreate newLen $ \ptr -> do
        withForeignPtr fp $ \vPtr -> copyBytes ptr (vPtr `plusPtr` off) len
        when (newLen > len) $ fillBytes (ptr `plusPtr` len) 0 (newLen - len)
        forM_ relations $ \(ix, dir_, status) -> when (ix >= 0) $ do
          let i = fromIntegral ix
          b <- peekByteOff ptr i
          let b' = (b .&. complement statusMask) .|. toRelationInt status
              b'' = case dir_ of
                Just dir -> (b' .&. complement directionMask) .|. (toIntroductionInt dir `shiftL` 3)
                Nothing -> b'
          pokeByteOff ptr i b''

statusMask :: Word8
statusMask = 0x07 -- bits 0-2

directionMask :: Word8
directionMask = 0x08 -- bit 3
