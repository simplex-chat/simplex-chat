{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Types.MemberRelations
  ( MemberRelation (..),
    getRelation,
    setRelation,
    setRelations,
  )
where

import Control.Monad
import Data.Bits ((.&.), (.|.), complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (toForeignPtr, unsafeCreate)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

data MemberRelation
  = MRNew
  | MRIntroduced
  | MRConnected
  deriving (Eq, Show)

toRelationInt :: MemberRelation -> Word8
toRelationInt = \case
  MRNew -> 0
  MRIntroduced -> 1
  MRConnected -> 2

fromRelationInt :: Word8 -> MemberRelation
fromRelationInt = \case
  0 -> MRNew
  1 -> MRIntroduced
  2 -> MRConnected
  _ -> MRNew

-- | Get the relation status of a member at a given index from the relations vector.
-- Returns 'MRNew' if the vector is not long enough (lazy initialization).
getRelation :: Int64 -> ByteString -> MemberRelation
getRelation i v
  | i < 0 || fromIntegral i >= B.length v = MRNew
  | otherwise = fromRelationInt $ (v `B.index` fromIntegral i) .&. relationMask


-- | Set the relation status of a member at a given index in the relations vector.
-- Expands the vector lazily if needed (padding with zeros for 'MRNew' relation).
setRelation :: Int64 -> MemberRelation -> ByteString -> ByteString
setRelation i r v
  | i >= 0 = setRelations [(i, r)] v
  | otherwise = v

-- | Set multiple relations at once.
-- Expands the vector lazily if needed (padding with zeros for 'MRNew' relation).
setRelations :: [(Int64, MemberRelation)] -> ByteString -> ByteString
setRelations [] v = v
setRelations relations v =
  let (fp, off, len) = toForeignPtr v
      newLen = max len $ fromIntegral $ maximum (map fst relations) + 1
   in unsafeCreate newLen $ \ptr -> do
        withForeignPtr fp $ \vPtr -> copyBytes ptr (vPtr `plusPtr` off) len
        when (newLen > len) $ fillBytes (ptr `plusPtr` len) 0 (newLen - len)
        forM_ relations $ \(ix, r) -> when (ix >= 0) $ do
          let i = fromIntegral ix
          b <- peekByteOff ptr i
          let b' = (b .&. complement relationMask) .|. toRelationInt r
          pokeByteOff ptr i b'

relationMask :: Word8
relationMask = 0x07 -- reserving 3 bits
