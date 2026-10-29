{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Community
  ( communityFor,
    termsFor,
    agreedTermsHash,
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
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..))
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Abi (bytes32FromHex, hex)
import StoreService.Names.Chain (ChainError (..))
import StoreService.Names.Contracts (Contracts, Terms (..), TermsRefusal (..), bonaFideName, concurrent, namePolicy, policyNonce, termsLinks, termsOf, termsText, together)

communityFor :: NamesConfig -> LabelHash -> Maybe Text
communityFor NamesConfig {nCommunity} (LabelHash h) = M.lookup h nCommunity

-- | The prefix records that the buyer agreed to the community name terms, whose hash follows it.
termsFor :: Bool -> Terms -> Text
termsFor listed Terms {tmHash} = (if listed then communityTermsPrefix else "") <> T.drop 2 (hex tmHash)

-- | The community terms hash an order signed, Nothing for plain terms.
agreedTermsHash :: Text -> Maybe ByteString
agreedTermsHash terms = case T.splitOn "/" terms of
  ["community", h] -> hashOf h
  [_version, "community", h] -> hashOf h
  _ -> Nothing
  where
    hashOf = bytes32FromHex . ("0x" <>)

communityTermsPrefix :: Text
communityTermsPrefix = "community/"

-- | Null for plain terms.
communityJSON :: NamesConfig -> LabelHash -> Bool -> J.Value
communityJSON cfg lh listed = if listed then J.object ["for" .= communityFor cfg lh, "years" .= communityYears] else J.Null

-- | The paid years of a community name, whatever years its code or [names] years gives.
communityYears :: Int
communityYears = 2

yearsFor :: Bool -> Int -> Int
yearsFor listed years = if listed then communityYears else years

termsJSON :: Maybe Terms -> J.Value
termsJSON = maybe J.Null $ \Terms {tmLink, tmHash} -> J.object ["hash" .= hex tmHash, "link" .= tmLink]

-- | Whether the policy lists the name, and the terms it is sold under: the community name terms for a listed name, else the
-- name terms; Nothing until the owner sets them.
nameTerms :: Contracts -> LabelHash -> IO (Either ChainError (Bool, Maybe Terms))
nameTerms ct lh = together $ kindTerms <$> concurrent (bonaFideName ct lh) <*> concurrent (termsLinks ct)
  where
    kindTerms listed (nameLink, communityLink) = (listed, termsOf $ if listed then communityLink else nameLink)

readableTerms :: Contracts -> Maybe Terms -> IO (Either (Maybe Int) Terms)
readableTerms ct = maybe (pure $ Left Nothing) $ \t -> bimap waitOf (const t) <$> termsText ct t
  where
    waitOf (TRWait seconds) = Just seconds
    waitOf TRLink = Nothing

-- | What the page signs to accept the terms of the name's kind.
data AcceptanceAsked = AcceptanceAsked {acCommunity :: Bool, acPolicy :: Address, acTermsHash :: ByteString, acNonce :: Word64}
  deriving (Eq, Show)

-- | Fails while the terms of the name's kind are not set.
currentAcceptance :: Contracts -> LabelHash -> Address -> IO (Either ChainError AcceptanceAsked)
currentAcceptance ct lh owner = runExceptT $ do
  (acCommunity, terms) <- ExceptT $ nameTerms ct lh
  Terms {tmHash = acTermsHash} <- maybe (throwError $ ChainUnavailable "the name policy has not set these terms yet") pure terms
  acPolicy <- ExceptT $ namePolicy ct
  acNonce <- ExceptT $ policyNonce ct owner
  pure AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce}
