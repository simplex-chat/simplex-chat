{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Community
  ( communityFor,
    termsFor,
    agreedTermsHash,
    communityJSON,
    termsJSON,
    nameCommunityTerms,
    AcceptanceAsked (..),
    currentAcceptance,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..), TermsConfig (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Contracts (Contracts, bonaFideName, communityTermsHash, namePolicy, policyNonce)

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} (LabelHash h) = M.lookup h nCommunity

-- | A community name's order signs the community terms' hash after the version, so the order records which text was agreed to.
termsFor :: NamesConfig -> Maybe ByteString -> Text
termsFor NamesConfig {nTerms = TermsConfig {tVersion}} = maybe tVersion (\h -> tVersion <> communityTermsSuffix <> T.drop 2 (hex h))

-- | The community terms hash an order signed, Nothing for the terms of a name not listed.
agreedTermsHash :: Text -> Maybe ByteString
agreedTermsHash terms = T.stripPrefix communityTermsSuffix (snd $ T.breakOn communityTermsSuffix terms) >>= bytes32FromHex . ("0x" <>)

communityTermsSuffix :: Text
communityTermsSuffix = "/community/"

-- | Null for plain terms. The page links the text the service serves from the name policy.
communityJSON :: NamesConfig -> LabelHash -> Maybe ByteString -> J.Value
communityJSON cfg lh = \case
  Nothing -> J.Null
  Just h -> J.object ["for" .= communityFor cfg lh, "terms" .= J.object ["url" .= ("/api/community-terms/" <> hex h), "hash" .= hex h]]

termsJSON :: NamesConfig -> J.Value
termsJSON NamesConfig {nTerms = TermsConfig {tVersion, tUrl}} = J.object ["version" .= tVersion, "url" .= tUrl]

-- | Nothing for a name the policy does not list; else the community terms it is sold under.
nameCommunityTerms :: Contracts -> LabelHash -> IO (Either Text (Maybe ByteString))
nameCommunityTerms ct lh = runExceptT $
  withExceptT tshow (ExceptT $ bonaFideName ct lh) >>= \case
    False -> pure Nothing
    True -> Just <$> withExceptT tshow (ExceptT $ communityTermsHash ct)

-- | What the page signs to accept the community terms of a name the policy lists.
data AcceptanceAsked = AcceptanceAsked {acPolicy :: Address, acTermsHash :: ByteString, acNonce :: Word64}
  deriving (Eq, Show)

-- | The acceptance the owner signs for a name the policy lists, Nothing for another name; the view returns it for the
-- page to sign, and the reveal is checked against it.
currentAcceptance :: Contracts -> LabelHash -> Address -> IO (Either Text (Maybe AcceptanceAsked))
currentAcceptance ct lh owner = runExceptT $ ExceptT (nameCommunityTerms ct lh) >>= traverse acceptance
  where
    acceptance acTermsHash = do
      acPolicy <- withExceptT tshow $ ExceptT $ namePolicy ct
      acNonce <- withExceptT tshow $ ExceptT $ policyNonce ct owner
      pure AcceptanceAsked {acPolicy, acTermsHash, acNonce}
