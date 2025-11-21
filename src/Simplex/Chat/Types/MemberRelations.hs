{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Types.MemberRelations
  ( MemberRelation (..),
    getRelation,
    setRelation,
    setRelations,
  )
where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.List (foldl', sortOn)
import Data.Word (Word8)

data MemberRelation
  = MRNew
  | MRIntroduced
  | MRConnected
  deriving (Eq, Show)

toRelationInt :: MemberRelation -> Int
toRelationInt = \case
  MRNew -> 0
  MRIntroduced -> 1
  MRConnected -> 2

fromRelationInt :: Int -> MemberRelation
fromRelationInt = \case
  0 -> MRNew
  1 -> MRIntroduced
  2 -> MRConnected
  _ -> MRNew

-- | Calculate byte index and bit offset within that byte for a given index in group.
-- Each byte stores 4 relations (2 bits each), so bit offset is 0, 2, 4, or 6.
indexPosition :: Int64 -> (Int, Int)
indexPosition indexInGroup =
  let (byteIndex, bitPosition) = fromIntegral indexInGroup `divMod` 4
   in (byteIndex, bitPosition * 2)

-- | Update 2 bits at a specific offset in a byte with a new relation value.
updateByte :: Word8 -> Int -> MemberRelation -> Word8
updateByte byte bitOffset relation =
  let mask = complement (0x03 `shiftL` bitOffset)
      relationBits = fromIntegral (toRelationInt relation) `shiftL` bitOffset
   in (byte .&. mask) .|. relationBits

-- | Get the relation status of a member at a given index from the relations vector.
-- Returns 'MRNew' if the vector is not long enough (lazy initialization).
getRelation :: Int64 -> ByteString -> MemberRelation
getRelation indexInGroup vector =
  case B.indexMaybe vector byteIndex of
    Nothing -> MRNew
    Just byte ->
      let relationBits = fromIntegral $ (byte `shiftR` bitOffset) .&. 0x03
       in fromRelationInt relationBits
  where
    (byteIndex, bitOffset) = indexPosition indexInGroup

-- | Set the relation status of a member at a given index in the relations vector.
-- Expands the vector lazily if needed (padding with zeros for 'MRNew' relation).
setRelation :: Int64 -> MemberRelation -> ByteString -> ByteString
setRelation indexInGroup relation vector
  | indexInGroup < 0 = vector
  | otherwise =
      let (byteIndex, bitOffset) = indexPosition indexInGroup
          requiredLength = byteIndex + 1
          expanded = expandVector vector requiredLength
          byte = B.index expanded byteIndex
          newByte = updateByte byte bitOffset relation
       in B.concat [B.take byteIndex expanded, B.singleton newByte, B.drop (byteIndex + 1) expanded]

-- | Set multiple relations at once. More efficient than calling setRelation multiple times.
-- Sorts by index first for O(k log k + n) complexity instead of O(k*n).
setRelations :: [(Int64, MemberRelation)] -> ByteString -> ByteString
setRelations [] vector = vector
setRelations relations vector =
  let sorted = sortOn fst relations  -- Sort once by index
      maxIndex = fst (last sorted)
      (maxByteIndex, _) = indexPosition maxIndex
      requiredLength = maxByteIndex + 1
      expanded = expandVector vector requiredLength
   in B.pack $ updateBytes 0 (B.unpack expanded) sorted
  where
    updateBytes :: Int -> [Word8] -> [(Int64, MemberRelation)] -> [Word8]
    updateBytes _ [] _ = []
    updateBytes _ bytes [] = bytes
    updateBytes byteIdx (byte : bytes) changes@((idx, _) : _) =
      let (bIdx, _) = indexPosition idx
       in if bIdx == byteIdx
            then
              -- Collect all changes for this byte
              let (byteChanges, remaining) = span (\(i, _) -> fst (indexPosition i) == byteIdx) changes
                  newByte = foldl' (\b (i, r) -> updateByte b (snd $ indexPosition i) r) byte byteChanges
               in newByte : updateBytes (byteIdx + 1) bytes remaining
            else byte : updateBytes (byteIdx + 1) bytes changes

-- | Expand vector to required length, padding with zero bytes (representing 'MRNew' relation).
expandVector :: ByteString -> Int -> ByteString
expandVector vector requiredLength
  | currentLength >= requiredLength = vector
  | otherwise = B.append vector (B.replicate (requiredLength - currentLength) 0)
  where
    currentLength = B.length vector
