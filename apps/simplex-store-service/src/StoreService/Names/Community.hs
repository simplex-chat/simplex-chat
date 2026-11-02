{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Community
  ( communityFor,
    termsFor,
    communityJSON,
    termsJSON,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Simplex.Messaging.SimplexName (LabelHash (..))
import StoreService.Config (NamesConfig (..), TermsConfig (..))

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} (LabelHash h) = M.lookup h nCommunity

-- | The suffix records that the buyer agreed to the community wording, under the same version.
termsFor :: NamesConfig -> LabelHash -> Text
termsFor cfg@NamesConfig {nTerms = TermsConfig {tVersion}} lh = maybe tVersion (const $ tVersion <> communityTermsSuffix) (communityFor cfg lh)

communityTermsSuffix :: Text
communityTermsSuffix = "/community"

communityJSON :: NamesConfig -> LabelHash -> J.Value
communityJSON cfg lh = maybe J.Null (\t -> J.object ["for" .= t]) (communityFor cfg lh)

termsJSON :: NamesConfig -> J.Value
termsJSON NamesConfig {nTerms = TermsConfig {tVersion, tUrl}} = J.object ["version" .= tVersion, "url" .= tUrl]
