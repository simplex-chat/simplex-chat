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
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Chain (ChainSeconds, chainSeconds)
import StoreService.Names.Community (AcceptanceAsked (..), currentAcceptance)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Orders (boundedInteger, hexHash, hexSignature, jsonField)
import StoreService.Names.Registration (LinkKind (..), RegisterIntent (..), Registration (..), TermsAcceptance (..), makeCommitment, nameLink, policyDomain, sponsorDomain, termsKind, validLabel, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Store (NameRegistration (..), NameReveal (..))

data RevealRefusal = RVBadRequest | RVInvalidLink | RVChainUnavailable
  deriving (Eq, Show)

-- | The links of each key as sent, however many; at most one per key is a check of its own.
data RevealRequest = RevealRequest
  { rqLabel :: Text,
    rqSignature :: RecoverableSignature,
    rqSecret :: ByteString,
    rqContactLinks :: [Text],
    rqChannelLinks :: [Text],
    rqNonce :: Word64,
    rqDeadline :: ChainSeconds,
    rqAcceptance :: TermsAcceptance
  }

instance J.FromJSON RevealRequest where
  parseJSON = J.withObject "reveal" $ \o -> do
    rqLabel <- o .: "label"
    rqSignature <- jsonField o "signature" hexSignature
    rqSecret <- jsonField o "secret" hexHash
    records <- o .: "records"
    rqContactLinks <- records .: "contact"
    rqChannelLinks <- records .: "channel"
    rqNonce <- jsonField o "nonce" boundedInteger
    rqDeadline <- jsonField o "deadline" boundedInteger
    rqAcceptance <- jsonField o "acceptance" $ J.withObject "acceptance" $ \a -> do
      taTermsHash <- jsonField a "termsHash" hexHash
      taNonce <- jsonField a "nonce" boundedInteger
      taSignature <- jsonField a "signature" hexSignature
      pure TermsAcceptance {taTermsHash, taNonce, taSignature}
    pure RevealRequest {rqLabel, rqSignature, rqSecret, rqContactLinks, rqChannelLinks, rqNonce, rqDeadline, rqAcceptance}

isRepeat :: RevealRequest -> NameReveal -> Bool
isRepeat RevealRequest {rqLabel, rqSignature} NameReveal {rvLabel, rvSignature} = rqLabel == rvLabel && rqSignature == rvSignature

checkReveal :: NamesConfig -> Contracts -> UTCTime -> NameRegistration -> ChainSeconds -> RevealRequest -> IO (Either RevealRefusal NameReveal)
checkReveal cfg ct now r revealBefore request = runExceptT $ do
  rv <- liftEither $ committedReveal cfg r request
  ExceptT $ checkIntent cfg ct now revealBefore (intentOf cfg r rv)
  ExceptT $ checkAcceptance cfg ct r rv
  pure rv

committedReveal :: NamesConfig -> NameRegistration -> RevealRequest -> Either RevealRefusal NameReveal
committedReveal cfg@NamesConfig {nTLD} r@NameRegistration {nrLabelHash, nrLength, nrCommitment} RevealRequest {rqLabel, rqSignature, rqSecret, rqContactLinks, rqChannelLinks, rqNonce, rqDeadline, rqAcceptance} = do
  label <- first (const RVBadRequest) $ validLabel nTLD rqLabel
  unless (labelHash label == nrLabelHash && T.length label == nrLength) $ Left RVBadRequest
  rvContactLink <- oneLink rqContactLinks
  rvChannelLink <- oneLink rqChannelLinks
  let rv = NameReveal {rvLabel = label, rvSecret = rqSecret, rvContactLink, rvChannelLink, rvNonce = rqNonce, rvDeadline = rqDeadline, rvSignature = rqSignature, rvAcceptance = rqAcceptance}
  unless (makeCommitment (riRegistration $ intentOf cfg r rv) == nrCommitment) $ Left RVBadRequest
  pure rv
  where
    oneLink = \case
      [] -> Right Nothing
      [link] -> Right $ Just link
      _ -> Left RVInvalidLink

-- | The deadline and the owner's signature first, so a request nobody signed reads nothing from the chain.
-- A deadline before revealBefore would expire while the commitment can still register.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> ChainSeconds -> RegisterIntent -> IO (Either RevealRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now revealBefore intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce, riDeadline}
  | riDeadline < revealBefore = pure $ Left RVBadRequest
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) (chainSeconds now) intent >>= \case
        Left _ -> pure $ Left RVBadRequest
        Right () ->
          sponsorNonce ct rOwner >>= \case
            Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
            Right nonce -> pure $ if nonce == riNonce then Right () else Left RVBadRequest

-- | After the intent, so only a signed request reads the policy. Every name needs its owner's signature of the acceptance
-- its view returns, with the intent's deadline.
checkAcceptance :: NamesConfig -> Contracts -> NameRegistration -> NameReveal -> IO (Either RevealRefusal ())
checkAcceptance NamesConfig {nChain = ChainConfig {cChainId}} ct NameRegistration {nrLabelHash, nrOwner, nrCommitment} NameReveal {rvDeadline, rvAcceptance = a@TermsAcceptance {taTermsHash, taNonce}} =
  currentAcceptance ct nrLabelHash nrOwner >>= \case
    Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's terms acceptance could not be read: " <> tshow e)
    Right AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce}
      | taTermsHash == acTermsHash && taNonce == acNonce -> first (const RVBadRequest) <$> verifyAcceptance (policyDomain cChainId acPolicy) (termsKind acCommunity) nrOwner nrCommitment rvDeadline a
      | otherwise -> pure $ Left RVBadRequest

linksValid :: NameReveal -> Bool
linksValid NameReveal {rvContactLink, rvChannelLink} = all (ofKind LKContact) rvContactLink && all (ofKind LKChannel) rvChannelLink
  where
    ofKind kind link = (fst <$> nameLink link) == Just kind

intentOf :: NamesConfig -> NameRegistration -> NameReveal -> RegisterIntent
intentOf NamesConfig {nTLD, nChain = ChainConfig {cResolver}} NameRegistration {nrOwner, nrDuration} NameReveal {rvLabel, rvSecret, rvContactLink, rvChannelLink, rvNonce, rvDeadline, rvSignature} =
  RegisterIntent {riRegistration = registration, riNonce = rvNonce, riDeadline = rvDeadline, riSignature = rvSignature}
  where
    registration = Registration {rLabel = rvLabel, rTLD = nTLD, rOwner = nrOwner, rDuration = nrDuration, rSecret = rvSecret, rResolver = cResolver, rContactLink = rvContactLink, rChannelLink = rvChannelLink}
