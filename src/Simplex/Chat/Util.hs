{-# LANGUAGE NumericUnderscores #-}

module Simplex.Chat.Util
  ( diffInMicros,
    diffInSeconds,
  )
where

import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock (UTCTime, diffUTCTime)

diffInSeconds :: UTCTime -> UTCTime -> Int
diffInSeconds a b = (`div` 1000000_000000) $ diffInPicos a b

diffInMicros :: UTCTime -> UTCTime -> Int
diffInMicros a b = (`div` 1000000) $ diffInPicos a b

diffInPicos :: UTCTime -> UTCTime -> Int
diffInPicos a b = fromInteger . fromPico . nominalDiffTimeToSeconds $ diffUTCTime a b

fromPico :: Pico -> Integer
fromPico (MkFixed i) = i
