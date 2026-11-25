{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Names.Community
  ( Community (..),
    TermsLink (..),
    communityFor,
    termsFor,
    communityOf,
    yearsFor,
    termsLink,
    nameTerms,
    readableTerms,
    AcceptanceAsked (..),
    currentAcceptance,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Word (Word64)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (LabelHash)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (ChainError (..))
import StoreService.Names.Contracts (Contracts, Terms (..), TermsRefusal (..), communityName, concurrent, kindTerms, namePolicy, policyNonce, termsLinks, termsText, together)
import StoreService.Names.Registration (CommunityName, TermsHash)
import StoreService.Names.Signatures (AgreedTerms (AgreedTerms))

data Community = Community {for :: Maybe Text, years :: Int}
  deriving (Eq, Show)

data TermsLink = TermsLink {link :: Text, hash :: TermsHash}
  deriving (Eq, Show)

$(JQ.deriveToJSON defaultJSON ''Community)

$(JQ.deriveToJSON defaultJSON ''TermsLink)

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} lh = M.lookup lh nCommunity
{-# INLINE communityFor #-}

termsFor :: CommunityName -> Terms -> AgreedTerms
termsFor community Terms {tmHash} = AgreedTerms community tmHash
{-# INLINE termsFor #-}

-- | Nothing for plain terms.
communityOf :: NamesConfig -> LabelHash -> CommunityName -> Maybe Community
communityOf cfg lh community = if community then Just Community {for = communityFor cfg lh, years = communityYears} else Nothing

-- | The paid years of a community name, whatever years its code or [names] years gives.
communityYears :: Int
communityYears = 2

yearsFor :: CommunityName -> Int -> Int
yearsFor community years = if community then communityYears else years

termsLink :: Terms -> TermsLink
termsLink Terms {tmLink, tmHash} = TermsLink {link = tmLink, hash = tmHash}
{-# INLINE termsLink #-}

-- | Whether the policy lists the name, and the terms it is sold under: the community terms for a listed name, else the
-- general terms.
nameTerms :: Contracts -> LabelHash -> IO (Either ChainError (CommunityName, Maybe Terms))
nameTerms ct lh = together $ (\community links -> (community, kindTerms community links)) <$> concurrent (communityName ct lh) <*> concurrent (termsLinks ct)

readableTerms :: Contracts -> Maybe Terms -> IO (Either (Maybe Int) Terms)
readableTerms ct = maybe (pure $ Left Nothing) $ \t -> bimap waitOf (const t) <$> termsText ct t
  where
    waitOf (TRWait seconds) = Just seconds

-- | What the page signs to accept the terms of the name's kind.
data AcceptanceAsked = AcceptanceAsked {community :: CommunityName, policy :: Address, termsHash :: TermsHash, nonce :: Word64}
  deriving (Eq, Show)

currentAcceptance :: Contracts -> LabelHash -> Address -> IO (Either ChainError AcceptanceAsked)
currentAcceptance ct lh owner = runExceptT $ do
  (community, terms) <- ExceptT $ nameTerms ct lh
  Terms {tmHash = termsHash} <- maybe (throwError $ ChainUnavailable "the name policy has no terms of this kind that the service takes") pure terms
  policy <- ExceptT $ namePolicy ct
  nonce <- ExceptT $ policyNonce ct owner
  pure AcceptanceAsked {community, policy, termsHash, nonce}

$(JQ.deriveToJSON defaultJSON ''AcceptanceAsked)
