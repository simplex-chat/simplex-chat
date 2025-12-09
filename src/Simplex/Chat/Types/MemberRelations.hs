{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Types.MemberRelations
  ( IntroductionDirection (..),
    MemberRelation (..),
    toIntroDirInt,
    fromIntroDirInt,
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

toIntroDirInt :: IntroductionDirection -> Word8
toIntroDirInt = \case
  IDSubjectIntroduced -> 0
  IDReferencedIntroduced -> 1

fromIntroDirInt :: Word8 -> IntroductionDirection
fromIntroDirInt = \case
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
       in (fromIntroDirInt $ (b .&. directionMask) `shiftR` 3, fromRelationInt $ b .&. statusMask)

-- | Get the indexes of members with the given relation status from the relations vector.
getRelationsIndexes :: MemberRelation -> ByteString -> [Int64]
getRelationsIndexes r v = [i | i <- [0 .. fromIntegral (B.length v) - 1], getRelation i v == r]

-- | Set the relation status of a member at a given index in the relations vector.
-- Preserves the introduction direction. Expands the vector lazily if needed.
setRelation :: Int64 -> MemberRelation -> ByteString -> ByteString
setRelation i r v
  | i >= 0 = setRelations [(i, r)] v
  | otherwise = v

-- | Set multiple relation statuses at once.
-- Preserves the introduction direction. Expands the vector lazily if needed.
setRelations :: [(Int64, MemberRelation)] -> ByteString -> ByteString
setRelations = setRelations_ $ \r b -> (b .&. complement statusMask) .|. toRelationInt r

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
  | i >= 0 = setNewRelations [(i, (dir, r))] v
  | otherwise = v

-- | Set multiple new relations with both direction and status at once.
-- Expands the vector lazily if needed.
setNewRelations :: [(Int64, (IntroductionDirection, MemberRelation))] -> ByteString -> ByteString
setNewRelations = setRelations_ $ \(dir, r) b -> (b .&. relationMask) .|. (toIntroDirInt dir `shiftL` 3) .|. toRelationInt r
  where
    relationMask = complement (statusMask .|. directionMask)

setRelations_ :: (r -> Word8 -> Word8) -> [(Int64, r)] -> ByteString -> ByteString
setRelations_ _ [] v = v
setRelations_ updateByte relations v =
  let (fp, off, len) = toForeignPtr v
      newLen = max len $ fromIntegral $ maximum (map fst relations) + 1
   in unsafeCreate newLen $ \ptr -> do
        withForeignPtr fp $ \vPtr -> copyBytes ptr (vPtr `plusPtr` off) len
        when (newLen > len) $ fillBytes (ptr `plusPtr` len) 0 (newLen - len)
        forM_ relations $ \(ix, r) -> when (ix >= 0) $
          let i = fromIntegral ix
           in pokeByteOff ptr i . updateByte r =<< peekByteOff ptr i

statusMask :: Word8
statusMask = 0x07 -- bits 0-2

directionMask :: Word8
directionMask = 0x08 -- bit 3
