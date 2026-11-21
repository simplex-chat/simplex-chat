{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The reveal's checks, in the contract's order: the signed label, the order's own label and links,
-- the commitment, the owner's intent, the terms acceptance, and the links' kinds last.
module StoreService.Names.Reveal
  ( RevealRequest (..),
    isRepeat,
    checkReveal,
    orderReveal,
    refusedLink,
    intentOf,
  )
where

import Control.Logger.Simple (logWarn)
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (nameLengthInt)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.SystemTime (SystemSeconds, roundedToUTCTime)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Chain (Hex32)
import StoreService.Names.Orders (OrderRefusal (..), signedRequestSeconds)
import StoreService.Names.Community (AcceptanceAsked (..), currentAcceptance)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Label (validLabel)
import StoreService.Names.Registration (LinkKind (..), RegisterIntent (..), Registration (..), TermsAcceptance (..), makeCommitment, nameLink, policyDomain, sponsorDomain, termsKind, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store (NameReveal (..))

data RevealRequest = RevealRequest
  { label :: Text,
    signature :: RecoverableSignature,
    secret :: Hex32,
    records :: RevealLinks,
    nonce :: Word64,
    deadline :: SystemSeconds,
    acceptance :: TermsAcceptance
  }

data RevealLinks = RevealLinks {contact :: Maybe Text, channel :: Maybe Text}

$(JQ.deriveFromJSON defaultJSON ''RevealLinks)

$(JQ.deriveFromJSON defaultJSON ''RevealRequest)

isRepeat :: RevealRequest -> NameReveal -> Bool
isRepeat RevealRequest {label, signature} NameReveal {rvLabel, rvSignature} = label == rvLabel && signature == rvSignature

checkReveal :: NamesConfig -> Contracts -> UTCTime -> Address -> OrderFields -> UTCTime -> RevealRequest -> IO (Either OrderRefusal NameReveal)
checkReveal cfg ct now owner fields earliest request = runExceptT $ do
  rv <- liftEither $ committedReveal cfg owner fields request
  ExceptT $ checkIntent cfg ct now earliest (intentOf cfg owner (ofDuration fields) rv)
  ExceptT $ checkAcceptance cfg ct owner fields rv
  pure rv

orderReveal :: NamesConfig -> Contracts -> UTCTime -> Address -> OrderFields -> RevealRequest -> IO (Either OrderRefusal NameReveal)
orderReveal cfg ct now owner fields request =
  checkReveal cfg ct now owner fields now request <&> (>>= \rv -> maybe (Right rv) (Left . ORInvalidLink) (refusedLink rv))

committedReveal :: NamesConfig -> Address -> OrderFields -> RevealRequest -> Either OrderRefusal NameReveal
committedReveal cfg@NamesConfig {nTLD} owner OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment} RevealRequest {label = sentLabel, signature, secret, records = RevealLinks {contact, channel}, nonce, deadline, acceptance} = do
  label <- first (ORBadRequest . ("the label is not a name: " <>) . tshow) $ validLabel nTLD sentLabel
  unless (labelHash label == ofLabelHash && T.length label == nameLengthInt ofLength) $ Left $ ORBadRequest "the label is not the order's"
  let rv = NameReveal {rvLabel = label, rvSecret = secret, rvContactLink = contact, rvChannelLink = channel, rvNonce = nonce, rvDeadline = deadline, rvSignature = signature, rvAcceptance = acceptance}
  unless (makeCommitment (riRegistration $ intentOf cfg owner ofDuration rv) == ofCommitment) $ Left $ ORBadRequest "the reveal is not the order's commitment"
  pure rv

-- | The deadline and the owner's signature first, so a request nobody signed reads nothing from the chain.
-- A deadline before revealBefore would expire while the commitment can still register.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> UTCTime -> RegisterIntent -> IO (Either OrderRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now earliest intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce, riDeadline}
  | roundedToUTCTime riDeadline < earliest = pure $ Left $ ORBadRequest "the intent's deadline is before revealBefore"
  | roundedToUTCTime riDeadline > addUTCTime (fromInteger $ signatureSeconds + signedRequestSeconds) now = pure $ Left $ ORBadRequest "the intent's deadline is more than a week ahead"
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) now intent >>= \case
        Left e -> pure $ Left $ ORBadRequest $ T.pack e
        Right () ->
          sponsorNonce ct rOwner >>= \case
            Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
            Right nonce -> pure $ if nonce == riNonce then Right () else Left $ ORBadRequest "the intent's nonce is not the sponsor's next"

-- | After the intent, so only a signed request reads the policy. Every name needs its owner's signature of the acceptance
-- its view returns, with the intent's deadline.
checkAcceptance :: NamesConfig -> Contracts -> Address -> OrderFields -> NameReveal -> IO (Either OrderRefusal ())
checkAcceptance NamesConfig {nChain = ChainConfig {cChainId}} ct owner OrderFields {ofLabelHash, ofCommitment} NameReveal {rvDeadline, rvAcceptance = a@TermsAcceptance {termsHash = signedHash, nonce = signedNonce}} =
  currentAcceptance ct ofLabelHash owner >>= \case
    Left e -> Left ORNameCheckUnavailable <$ logWarn ("names: a reveal's terms acceptance could not be read: " <> tshow e)
    Right AcceptanceAsked {community, policy, termsHash, nonce}
      | signedHash == termsHash && signedNonce == nonce -> first (ORBadRequest . T.pack) <$> verifyAcceptance (policyDomain cChainId policy) (termsKind community) owner ofCommitment rvDeadline a
      | otherwise -> pure $ Left $ ORBadRequest "the acceptance is not of the terms and nonce the policy asks"

signatureSeconds :: Integer
signatureSeconds = 7 * 86400

refusedLink :: NameReveal -> Maybe LinkKind
refusedLink NameReveal {rvContactLink, rvChannelLink} =
  listToMaybe [kind | (kind, Just link) <- [(LKContact, rvContactLink), (LKChannel, rvChannelLink)], (fst <$> nameLink link) /= Just kind]

intentOf :: NamesConfig -> Address -> Word64 -> NameReveal -> RegisterIntent
intentOf NamesConfig {nTLD, nChain = ChainConfig {cResolver}} owner duration NameReveal {rvLabel, rvSecret, rvContactLink, rvChannelLink, rvNonce, rvDeadline, rvSignature} =
  RegisterIntent {riRegistration = registration, riNonce = rvNonce, riDeadline = rvDeadline, riSignature = rvSignature}
  where
    registration = Registration {rLabel = rvLabel, rTLD = nTLD, rOwner = owner, rDuration = duration, rSecret = rvSecret, rResolver = cResolver, rContactLink = rvContactLink, rChannelLink = rvChannelLink}
