{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The reveal's checks, in the contract's order: the signed label, the order's own label and links,
-- the commitment, the owner's intent, the community terms acceptance, and the links' kinds last.
module StoreService.Names.Reveal
  ( RevealRefusal (..),
    SignedLabel (..),
    RevealFields (..),
    parseSignedLabel,
    parseRevealFields,
    isRepeat,
    committedRegistration,
    checkIntent,
    checkAcceptance,
    linksValid,
    revealOf,
    intentOf,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Log (logWarn)
import StoreService.Names.Abi (bytes32FromHex)
import StoreService.Names.Community (nameCommunityTerms, termsErrorText)
import StoreService.Names.Contracts (Contracts, namePolicy, policyNonce, sponsorNonce)
import StoreService.Names.Label (validLabel)
import StoreService.Names.Links (LinkKind (..), linkKindOf)
import StoreService.Names.Orders (integerField, textField)
import StoreService.Names.Registration (RegisterIntent (..), Registration (..), TermsAcceptance (..), makeCommitment, policyDomain, sponsorDomain, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Signatures (parseSignature)
import StoreService.Names.Store (NameRegistration (..), NameReveal (..))

data RevealRefusal = RVBadRequest | RVInvalidLink | RVChainUnavailable
  deriving (Eq, Show)

-- | What an identical repeat is compared on.
data SignedLabel = SignedLabel {slLabel :: Text, slSignature :: RecoverableSignature}

data RevealFields = RevealFields
  { rfSecret :: ByteString,
    rfContactLinks :: [Text],
    rfChannelLinks :: [Text],
    rfNonce :: Word64,
    rfDeadline :: Word64,
    rfAcceptance :: Maybe TermsAcceptance
  }

-- | The signature's format and low s, and the label as text.
parseSignedLabel :: J.Object -> Maybe SignedLabel
parseSignedLabel o = do
  slSignature <- either (const Nothing) Just . parseSignature =<< textField o "signature"
  slLabel <- textField o "label"
  pure SignedLabel {slLabel, slSignature}

-- | The links of each key as sent, however many; at most one per key is a check of its own.
parseRevealFields :: J.Object -> Maybe RevealFields
parseRevealFields o = do
  rfSecret <- bytes32FromHex =<< textField o "secret"
  rfNonce <- integerField =<< KM.lookup "nonce" o
  rfDeadline <- integerField =<< KM.lookup "deadline" o
  records <- case KM.lookup "records" o of
    Just (J.Object records) -> Just records
    _ -> Nothing
  rfContactLinks <- links "contact" records
  rfChannelLinks <- links "channel" records
  rfAcceptance <- case KM.lookup "acceptance" o of
    Nothing -> Just Nothing
    Just (J.Object a) -> Just <$> acceptance a
    _ -> Nothing
  pure RevealFields {rfSecret, rfContactLinks, rfChannelLinks, rfNonce, rfDeadline, rfAcceptance}
  where
    acceptance a = do
      taTermsHash <- bytes32FromHex =<< textField a "termsHash"
      taNonce <- integerField =<< KM.lookup "nonce" a
      taSignature <- either (const Nothing) Just . parseSignature =<< textField a "signature"
      pure TermsAcceptance {taTermsHash, taNonce, taSignature}
    links key records = case KM.lookup key records of
      Just (J.Array items) -> mapM text $ toList items
      _ -> Nothing
    text = \case
      J.String t -> Just t
      _ -> Nothing

isRepeat :: SignedLabel -> NameReveal -> Bool
isRepeat SignedLabel {slLabel, slSignature} NameReveal {rvLabel, rvSignature} = slLabel == rvLabel && slSignature == rvSignature

-- | The registration the reveal commits to, rebuilt from the order's stored owner and duration and the default resolver.
committedRegistration :: NamesConfig -> NameRegistration -> SignedLabel -> RevealFields -> Either RevealRefusal Registration
committedRegistration NamesConfig {nTLD, nChain = ChainConfig {cResolver}} NameRegistration {nrLabelHash, nrLength, nrOwner, nrDuration, nrCommitment} SignedLabel {slLabel} RevealFields {rfSecret, rfContactLinks, rfChannelLinks} = do
  label <- first (const RVBadRequest) $ validLabel nTLD slLabel
  unless (labelHash label == nrLabelHash && T.length label == nrLength) $ Left RVBadRequest
  rContactLink <- oneLink rfContactLinks
  rChannelLink <- oneLink rfChannelLinks
  let r = Registration {rLabel = label, rTLD = nTLD, rOwner = nrOwner, rDuration = nrDuration, rSecret = rfSecret, rResolver = cResolver, rContactLink, rChannelLink}
  unless (makeCommitment r == nrCommitment) $ Left RVBadRequest
  pure r
  where
    oneLink = \case
      [] -> Right Nothing
      [link] -> Right $ Just link
      _ -> Left RVInvalidLink

-- | The deadline and the owner's signature first, so a request nobody signed reads nothing from the chain.
-- A deadline before revealBefore would expire while the commitment can still register.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> Maybe Word64 -> RegisterIntent -> IO (Either RevealRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now revealBefore intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce, riDeadline}
  | maybe True (riDeadline <) revealBefore = pure $ Left RVBadRequest
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) (floor $ utcTimeToPOSIXSeconds now) intent >>= \case
        Left _ -> pure $ Left RVBadRequest
        Right () ->
          sponsorNonce ct rOwner >>= \case
            Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
            Right nonce -> pure $ if nonce == riNonce then Right () else Left RVBadRequest

-- | After the intent, so only a signed request reads the policy. A name the policy lists needs its owner's acceptance of the
-- current community terms by the policy's nonce, with the intent's deadline; another name takes none.
checkAcceptance :: NamesConfig -> Contracts -> NameRegistration -> Word64 -> Maybe TermsAcceptance -> IO (Either RevealRefusal ())
checkAcceptance cfg@NamesConfig {nChain = ChainConfig {cChainId}} ct NameRegistration {nrLabelHash, nrOwner, nrCommitment} deadline acceptance = runExceptT $ do
  terms <- chainRead "community terms" termsErrorText $ nameCommunityTerms cfg ct nrLabelHash
  case (terms, acceptance) of
    (Nothing, Nothing) -> pure ()
    (Just current, Just a@TermsAcceptance {taTermsHash, taNonce}) -> do
      policy <- chainRead "name policy" tshow $ namePolicy ct
      withExceptT (const RVBadRequest) $ ExceptT $ verifyAcceptance (policyDomain cChainId policy) nrOwner nrCommitment deadline a
      nonce <- chainRead "policy nonce" tshow $ policyNonce ct nrOwner
      unless (taTermsHash == current && taNonce == nonce) $ throwError RVBadRequest
    _ -> throwError RVBadRequest
  where
    chainRead what err a =
      ExceptT $
        a >>= \case
          Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's " <> what <> " read failed: " <> err e)
          Right v -> pure $ Right v

linksValid :: Registration -> Bool
linksValid Registration {rContactLink, rChannelLink} = all (ofKind LKContact) rContactLink && all (ofKind LKChannel) rChannelLink
  where
    ofKind kind link = linkKindOf link == Just kind

revealOf :: RegisterIntent -> Maybe TermsAcceptance -> NameReveal
revealOf RegisterIntent {riRegistration = Registration {rLabel, rSecret, rContactLink, rChannelLink}, riNonce, riDeadline, riSignature} rvAcceptance =
  NameReveal {rvLabel = rLabel, rvSecret = rSecret, rvContactLink = rContactLink, rvChannelLink = rChannelLink, rvNonce = riNonce, rvDeadline = riDeadline, rvSignature = riSignature, rvAcceptance}

-- | The intent an accepted reveal stored, as the register sends it.
intentOf :: NamesConfig -> NameRegistration -> NameReveal -> RegisterIntent
intentOf NamesConfig {nTLD, nChain = ChainConfig {cResolver}} NameRegistration {nrOwner, nrDuration} NameReveal {rvLabel, rvSecret, rvContactLink, rvChannelLink, rvNonce, rvDeadline, rvSignature} =
  RegisterIntent {riRegistration = registration, riNonce = rvNonce, riDeadline = rvDeadline, riSignature = rvSignature}
  where
    registration = Registration {rLabel = rvLabel, rTLD = nTLD, rOwner = nrOwner, rDuration = nrDuration, rSecret = rvSecret, rResolver = cResolver, rContactLink = rvContactLink, rChannelLink = rvChannelLink}
