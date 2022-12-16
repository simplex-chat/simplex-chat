module Simplex.Chat.Util
  ( diffInMicros,
  )
where

import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (UTCTime, diffUTCTime)

diffInMicros :: UTCTime -> UTCTime -> Int
diffInMicros a b = (`div` 1000000) $ diffInPicos a b

diffInPicos :: UTCTime -> UTCTime -> Int
diffInPicos a b = fromInteger . fromPico . nominalDiffTimeToSeconds $ diffUTCTime a b

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
