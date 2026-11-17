{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Community
  ( communityFor,
    termsFor,
    communityJSON,
    yearsFor,
    termsJSON,
    nameTerms,
    readableTerms,
    AcceptanceAsked (..),
    currentAcceptance,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (ChainError (..))
import StoreService.Names.Contracts (Contracts, Terms (..), TermsRefusal (..), communityName, concurrent, kindTerms, namePolicy, policyNonce, termsLinks, termsText, together)
import StoreService.Names.Registration (AgreedTerms (..), CommunityName, TermsHash)

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} lh = M.lookup lh nCommunity

termsFor :: CommunityName -> Terms -> AgreedTerms
termsFor community Terms {tmHash} = AgreedTerms {atCommunityName = community, atTermsHash = tmHash}

-- | Null for plain terms.
communityJSON :: NamesConfig -> LabelHash -> CommunityName -> J.Value
communityJSON cfg lh community = if community then J.object ["for" .= communityFor cfg lh, "years" .= communityYears] else J.Null

-- | The paid years of a community name, whatever years its code or [names] years gives.
communityYears :: Int
communityYears = 2

yearsFor :: CommunityName -> Int -> Int
yearsFor community years = if community then communityYears else years

termsJSON :: Maybe Terms -> J.Value
termsJSON = maybe J.Null $ \Terms {tmLink, tmHash} -> J.object ["hash" .= tmHash, "link" .= tmLink]

-- | Whether the policy lists the name, and the terms it is sold under: the community terms for a listed name, else the
-- general terms.
nameTerms :: Contracts -> LabelHash -> IO (Either ChainError (CommunityName, Maybe Terms))
nameTerms ct lh = together $ (\community links -> (community, kindTerms community links)) <$> concurrent (communityName ct lh) <*> concurrent (termsLinks ct)

readableTerms :: Contracts -> Maybe Terms -> IO (Either (Maybe Int) Terms)
readableTerms ct = maybe (pure $ Left Nothing) $ \t -> bimap waitOf (const t) <$> termsText ct t
  where
    waitOf (TRWait seconds) = Just seconds

-- | What the page signs to accept the terms of the name's kind.
data AcceptanceAsked = AcceptanceAsked {acCommunity :: CommunityName, acPolicy :: Address, acTermsHash :: TermsHash, acNonce :: Word64}
  deriving (Eq, Show)

currentAcceptance :: Contracts -> LabelHash -> Address -> IO (Either ChainError AcceptanceAsked)
currentAcceptance ct lh owner = runExceptT $ do
  (acCommunity, terms) <- ExceptT $ nameTerms ct lh
  Terms {tmHash = acTermsHash} <- maybe (throwError $ ChainUnavailable "the name policy has no terms of this kind that the service takes") pure terms
  acPolicy <- ExceptT $ namePolicy ct
  acNonce <- ExceptT $ policyNonce ct owner
  pure AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce}
