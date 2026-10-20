{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Badges.StorePlan (storePlan) where

import Data.Text (Text)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.PaymentService.Types (PaymentProvider (..))

-- | What a subscription charge grants, from the plan it was for: Apple's product id, or Play's base
-- plan, which only Play's subscription record names. The months are the plan's, never the period's length.
storePlan :: PaymentProvider -> Text -> Maybe (BadgeType, Int)
storePlan provider planId = case (provider, planId) of
  (PPApple, "SUBSCR_BADGE_SUPPORTER_MONTH_01") -> Just (BTSupporter, 1)
  (PPApple, "SUBSCR_BADGE_SUPPORTER_YEAR_01") -> Just (BTSupporter, 12)
  (PPApple, "SUBSCR_BADGE_LEGEND_MONTH_01") -> Just (BTLegend, 1)
  (PPApple, "SUBSCR_BADGE_LEGEND_YEAR_01") -> Just (BTLegend, 12)
  (PPGoogle, "subscr-badge-supporter-month-02") -> Just (BTSupporter, 1)
  (PPGoogle, "subscr-badge-supporter-year-01") -> Just (BTSupporter, 12)
  (PPGoogle, "subscr-badge-legend-month-01") -> Just (BTLegend, 1)
  (PPGoogle, "subscr-badge-legend-year-01") -> Just (BTLegend, 12)
  _ -> Nothing
