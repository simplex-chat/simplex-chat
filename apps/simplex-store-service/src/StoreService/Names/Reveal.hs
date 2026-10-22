{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The reveal's checks, in the contract's order: the signed label, the order's own label and links,
-- the commitment, the owner's intent, and the links' kinds last.
module StoreService.Names.Reveal
  ( RevealRefusal (..),
    SignedLabel (..),
    RevealFields (..),
    parseSignedLabel,
    parseRevealFields,
    isRepeat,
    committedRegistration,
    checkIntent,
    linksValid,
    revealOf,
  )
where

import Control.Monad (unless)
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
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Label (validLabel)
import StoreService.Names.Links (LinkKind (..), linkKindOf)
import StoreService.Names.Orders (integerField)
import StoreService.Names.Registration (RegisterIntent (..), Registration (..), makeCommitment, sponsorDomain, verifyRegisterIntent)
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
    rfDeadline :: Word64
  }

-- | The signature's format and low s, and the label as text.
parseSignedLabel :: J.Object -> Maybe SignedLabel
parseSignedLabel o = do
  slSignature <- either (const Nothing) Just . parseSignature =<< textField "signature" o
  slLabel <- textField "label" o
  pure SignedLabel {slLabel, slSignature}

-- | The links of each key as sent, however many; at most one per key is a check of its own.
parseRevealFields :: J.Object -> Maybe RevealFields
parseRevealFields o = do
  rfSecret <- bytes32FromHex =<< textField "secret" o
  rfNonce <- integerField =<< KM.lookup "nonce" o
  rfDeadline <- integerField =<< KM.lookup "deadline" o
  records <- case KM.lookup "records" o of
    Just (J.Object records) -> Just records
    _ -> Nothing
  rfContactLinks <- links "contact" records
  rfChannelLinks <- links "channel" records
  pure RevealFields {rfSecret, rfContactLinks, rfChannelLinks, rfNonce, rfDeadline}
  where
    links key records = case KM.lookup key records of
      Just (J.Array items) -> mapM text $ toList items
      _ -> Nothing
    text = \case
      J.String t -> Just t
      _ -> Nothing

textField :: J.Key -> J.Object -> Maybe Text
textField k o = case KM.lookup k o of
  Just (J.String t) -> Just t
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

-- | The deadline not past and the owner's signature first, so a request nobody signed reads nothing from the chain.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> RegisterIntent -> IO (Either RevealRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce} =
  verifyRegisterIntent (sponsorDomain cChainId cSponsor) (floor $ utcTimeToPOSIXSeconds now) intent >>= \case
    Left _ -> pure $ Left RVBadRequest
    Right () ->
      sponsorNonce ct rOwner >>= \case
        Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
        Right nonce -> pure $ if nonce == riNonce then Right () else Left RVBadRequest

linksValid :: Registration -> Bool
linksValid Registration {rContactLink, rChannelLink} = all (ofKind LKContact) rContactLink && all (ofKind LKChannel) rChannelLink
  where
    ofKind kind link = linkKindOf link == Just kind

revealOf :: RegisterIntent -> NameReveal
revealOf RegisterIntent {riRegistration = Registration {rLabel, rSecret, rContactLink, rChannelLink}, riNonce, riDeadline, riSignature} =
  NameReveal {rvLabel = rLabel, rvSecret = rSecret, rvContactLink = rContactLink, rvChannelLink = rChannelLink, rvNonce = riNonce, rvDeadline = riDeadline, rvSignature = riSignature}
