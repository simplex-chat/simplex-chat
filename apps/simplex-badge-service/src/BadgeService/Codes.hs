{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.Codes
  ( issueOneCode,
    singleUse,
  )
where

import BadgeService.Store (insertBadgeCode)
import BadgeService.Store.Invoices (truncateToSecond)
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Badges (BadgeType)
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeHash, randomBadgeCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus)
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatController (..))

singleUse :: Int
singleUse = 1

-- | The code table keeps only the hash, so the caller must deliver the code.
issueOneCode :: ChatController -> BadgeType -> Int -> BadgeCodePaymentStatus -> Int -> IO (Either String (BadgeCode, Int64))
issueOneCode cc badgeType months paymentStatus redeemLimit = do
  code <- randomBadgeCode $ random cc
  now <- truncateToSecond <$> getCurrentTime
  fmap (code,) <$> withDB' "issueBadgeCode" cc (\db -> insertBadgeCode db (badgeCodeHash code) badgeType months paymentStatus redeemLimit now)
