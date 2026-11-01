{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Badges.StorePlan (storePlan, playBasePlan) where

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
  (PPGoogle, _) -> snd <$> playBasePlan planId
  _ -> Nothing

-- | A Play base plan's subscription product, and what the plan grants. Play keeps a base plan id unique
-- only within its subscription, so a plan is one of these only under its own product.
playBasePlan :: Text -> Maybe (Text, (BadgeType, Int))
playBasePlan = \case
  "subscr-badge-supporter-month-02" -> Just ("subscr_badge_supporter_01", (BTSupporter, 1))
  "subscr-badge-supporter-year-01" -> Just ("subscr_badge_supporter_01", (BTSupporter, 12))
  "subscr-badge-legend-month-01" -> Just ("subscr_badge_legend_01", (BTLegend, 1))
  "subscr-badge-legend-year-01" -> Just ("subscr_badge_legend_01", (BTLegend, 12))
  _ -> Nothing
