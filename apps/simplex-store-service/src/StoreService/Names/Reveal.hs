{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The reveal's checks, in the contract's order: the signed label, the order's own label and links,
-- the commitment, the owner's intent, the terms acceptance, and the links' kinds last.
module StoreService.Names.Reveal
  ( RevealRefusal (..),
    RevealRequest (..),
    isRepeat,
    checkReveal,
    linksValid,
    intentOf,
  )
where

import Control.Logger.Simple (logWarn)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (nameLengthInt)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Chain (ChainSeconds, Hex32, chainSeconds)
import StoreService.Names.Community (AcceptanceAsked (..), currentAcceptance)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Orders (boundedInteger, hexSignature, jsonField)
import StoreService.Names.Registration (LinkKind (..), RegisterIntent (..), Registration (..), TermsAcceptance (..), makeCommitment, nameLink, policyDomain, sponsorDomain, termsKind, validLabel, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Store (NameRegistration (..), NameReveal (..))

data RevealRefusal = RVBadRequest Text | RVInvalidLink | RVChainUnavailable
  deriving (Eq, Show)

data RevealRequest = RevealRequest
  { rqLabel :: Text,
    rqSignature :: RecoverableSignature,
    rqSecret :: Hex32,
    rqContactLink :: Maybe Text,
    rqChannelLink :: Maybe Text,
    rqNonce :: Word64,
    rqDeadline :: ChainSeconds,
    rqAcceptance :: TermsAcceptance
  }

instance J.FromJSON RevealRequest where
  parseJSON = J.withObject "reveal" $ \o -> do
    rqLabel <- o .: "label"
    rqSignature <- jsonField o "signature" hexSignature
    rqSecret <- o .: "secret"
    records <- o .: "records"
    rqContactLink <- records .:? "contact"
    rqChannelLink <- records .:? "channel"
    rqNonce <- jsonField o "nonce" boundedInteger
    rqDeadline <- jsonField o "deadline" boundedInteger
    rqAcceptance <- jsonField o "acceptance" $ J.withObject "acceptance" $ \a -> do
      taTermsHash <- a .: "termsHash"
      taNonce <- jsonField a "nonce" boundedInteger
      taSignature <- jsonField a "signature" hexSignature
      pure TermsAcceptance {taTermsHash, taNonce, taSignature}
    pure RevealRequest {rqLabel, rqSignature, rqSecret, rqContactLink, rqChannelLink, rqNonce, rqDeadline, rqAcceptance}

isRepeat :: RevealRequest -> NameReveal -> Bool
isRepeat RevealRequest {rqLabel, rqSignature} NameReveal {rvLabel, rvSignature} = rqLabel == rvLabel && rqSignature == rvSignature

checkReveal :: NamesConfig -> Contracts -> UTCTime -> NameRegistration -> ChainSeconds -> RevealRequest -> IO (Either RevealRefusal NameReveal)
checkReveal cfg ct now r revealBefore request = runExceptT $ do
  rv <- liftEither $ committedReveal cfg r request
  ExceptT $ checkIntent cfg ct now revealBefore (intentOf cfg r rv)
  ExceptT $ checkAcceptance cfg ct r rv
  pure rv

committedReveal :: NamesConfig -> NameRegistration -> RevealRequest -> Either RevealRefusal NameReveal
committedReveal cfg@NamesConfig {nTLD} r@NameRegistration {nrLabelHash, nrLength, nrCommitment} RevealRequest {rqLabel, rqSignature, rqSecret, rqContactLink, rqChannelLink, rqNonce, rqDeadline, rqAcceptance} = do
  label <- first (RVBadRequest . ("the label is not a name: " <>) . tshow) $ validLabel nTLD rqLabel
  unless (labelHash label == nrLabelHash && T.length label == nameLengthInt nrLength) $ Left $ RVBadRequest "the label is not the order's"
  let rv = NameReveal {rvLabel = label, rvSecret = rqSecret, rvContactLink = rqContactLink, rvChannelLink = rqChannelLink, rvNonce = rqNonce, rvDeadline = rqDeadline, rvSignature = rqSignature, rvAcceptance = rqAcceptance}
  unless (makeCommitment (riRegistration $ intentOf cfg r rv) == nrCommitment) $ Left $ RVBadRequest "the reveal is not the order's commitment"
  pure rv

-- | The deadline and the owner's signature first, so a request nobody signed reads nothing from the chain.
-- A deadline before revealBefore would expire while the commitment can still register.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> ChainSeconds -> RegisterIntent -> IO (Either RevealRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now revealBefore intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce, riDeadline}
  | riDeadline < revealBefore = pure $ Left $ RVBadRequest "the intent's deadline is before revealBefore"
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) (chainSeconds now) intent >>= \case
        Left e -> pure $ Left $ RVBadRequest $ T.pack e
        Right () ->
          sponsorNonce ct rOwner >>= \case
            Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
            Right nonce -> pure $ if nonce == riNonce then Right () else Left $ RVBadRequest "the intent's nonce is not the sponsor's next"

-- | After the intent, so only a signed request reads the policy. Every name needs its owner's signature of the acceptance
-- its view returns, with the intent's deadline.
checkAcceptance :: NamesConfig -> Contracts -> NameRegistration -> NameReveal -> IO (Either RevealRefusal ())
checkAcceptance NamesConfig {nChain = ChainConfig {cChainId}} ct NameRegistration {nrLabelHash, nrOwner, nrCommitment} NameReveal {rvDeadline, rvAcceptance = a@TermsAcceptance {taTermsHash, taNonce}} =
  currentAcceptance ct nrLabelHash nrOwner >>= \case
    Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's terms acceptance could not be read: " <> tshow e)
    Right AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce}
      | taTermsHash == acTermsHash && taNonce == acNonce -> first (RVBadRequest . T.pack) <$> verifyAcceptance (policyDomain cChainId acPolicy) (termsKind acCommunity) nrOwner nrCommitment rvDeadline a
      | otherwise -> pure $ Left $ RVBadRequest "the acceptance is not of the terms and nonce the policy asks"

linksValid :: NameReveal -> Bool
linksValid NameReveal {rvContactLink, rvChannelLink} = all (ofKind LKContact) rvContactLink && all (ofKind LKChannel) rvChannelLink
  where
    ofKind kind link = (fst <$> nameLink link) == Just kind

intentOf :: NamesConfig -> NameRegistration -> NameReveal -> RegisterIntent
intentOf NamesConfig {nTLD, nChain = ChainConfig {cResolver}} NameRegistration {nrOwner, nrDuration} NameReveal {rvLabel, rvSecret, rvContactLink, rvChannelLink, rvNonce, rvDeadline, rvSignature} =
  RegisterIntent {riRegistration = registration, riNonce = rvNonce, riDeadline = rvDeadline, riSignature = rvSignature}
  where
    registration = Registration {rLabel = rvLabel, rTLD = nTLD, rOwner = nrOwner, rDuration = nrDuration, rSecret = rvSecret, rResolver = cResolver, rContactLink = rvContactLink, rChannelLink = rvChannelLink}
