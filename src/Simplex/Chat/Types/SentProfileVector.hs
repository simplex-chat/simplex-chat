module Simplex.Chat.Types.SentProfileVector
  ( isProfileSentTo,
    markSentPositions,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Simplex.Chat.Types.MemberRelations (updateByteVector)

-- Sent-profile vector: one byte per member index. 0 = profile not sent, 1 = sent.
-- Reads past the end return False (profile not sent yet). Writes past the end
-- grow the vector and zero-fill the gap (see updateByteVector).

isProfileSentTo :: ByteString -> Int64 -> Bool
isProfileSentTo v idx
  | idx < 0 || fromIntegral idx >= B.length v = False
  | otherwise = B.index v (fromIntegral idx) /= 0

markSentPositions :: [Int64] -> ByteString -> ByteString
markSentPositions positions =
  updateByteVector (\() _ -> 1) [(i, ()) | i <- positions]
