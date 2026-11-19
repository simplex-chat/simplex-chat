{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The reveal's checks, in the contract's order: the signed label, the order's own label and links,
-- the commitment, the owner's intent, the terms acceptance, and the links' kinds last.
module StoreService.Names.Reveal
  ( RevealRefusal (..),
    RevealRequest (..),
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
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (nameLengthInt)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Chain (ChainSeconds, Hex32, chainSeconds, unixSeconds)
import StoreService.Names.Orders (OrderRefusal (..), signedRequestSeconds)
import StoreService.Names.Community (AcceptanceAsked (..), currentAcceptance)
import StoreService.Names.Contracts (Contracts, sponsorNonce)
import StoreService.Names.Registration (LinkKind (..), OrderFields (..), RegisterIntent (..), Registration (..), TermsAcceptance (..), makeCommitment, nameLink, policyDomain, sponsorDomain, termsKind, validLabel, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Store (NameReveal (..))

data RevealRefusal = RVBadRequest Text | RVInvalidLink LinkKind | RVChainUnavailable
  deriving (Eq, Show)

data RevealRequest = RevealRequest
  { rqLabel :: Text,
    rqSignature :: RecoverableSignature,
    rqSecret :: Hex32,
    rqRecords :: RevealLinks,
    rqNonce :: Word64,
    rqDeadline :: ChainSeconds,
    rqAcceptance :: TermsAcceptance
  }

data RevealLinks = RevealLinks {rlContact :: Maybe Text, rlChannel :: Maybe Text}

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "rl"} ''RevealLinks)

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "rq"} ''RevealRequest)

isRepeat :: RevealRequest -> NameReveal -> Bool
isRepeat RevealRequest {rqLabel, rqSignature} NameReveal {rvLabel, rvSignature} = rqLabel == rvLabel && rqSignature == rvSignature

checkReveal :: NamesConfig -> Contracts -> UTCTime -> Address -> OrderFields -> ChainSeconds -> RevealRequest -> IO (Either RevealRefusal NameReveal)
checkReveal cfg ct now owner fields earliest request = runExceptT $ do
  rv <- liftEither $ committedReveal cfg owner fields request
  ExceptT $ checkIntent cfg ct now earliest (intentOf cfg owner (ofDuration fields) rv)
  ExceptT $ checkAcceptance cfg ct owner fields rv
  pure rv

orderReveal :: NamesConfig -> Contracts -> UTCTime -> Address -> OrderFields -> RevealRequest -> IO (Either OrderRefusal NameReveal)
orderReveal cfg ct now owner fields request =
  checkReveal cfg ct now owner fields (chainSeconds now) request <&> \case
    Right rv -> maybe (Right rv) (Left . ORInvalidLink) (refusedLink rv)
    Left (RVBadRequest reason) -> Left $ ORBadRequest reason
    Left (RVInvalidLink kind) -> Left $ ORInvalidLink kind
    Left RVChainUnavailable -> Left ORNameCheckUnavailable

committedReveal :: NamesConfig -> Address -> OrderFields -> RevealRequest -> Either RevealRefusal NameReveal
committedReveal cfg@NamesConfig {nTLD} owner OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment} RevealRequest {rqLabel, rqSignature, rqSecret, rqRecords = RevealLinks {rlContact, rlChannel}, rqNonce, rqDeadline, rqAcceptance} = do
  label <- first (RVBadRequest . ("the label is not a name: " <>) . tshow) $ validLabel nTLD rqLabel
  unless (labelHash label == ofLabelHash && T.length label == nameLengthInt ofLength) $ Left $ RVBadRequest "the label is not the order's"
  let rv = NameReveal {rvLabel = label, rvSecret = rqSecret, rvContactLink = rlContact, rvChannelLink = rlChannel, rvNonce = rqNonce, rvDeadline = rqDeadline, rvSignature = rqSignature, rvAcceptance = rqAcceptance}
  unless (makeCommitment (riRegistration $ intentOf cfg owner ofDuration rv) == ofCommitment) $ Left $ RVBadRequest "the reveal is not the order's commitment"
  pure rv

-- | The deadline and the owner's signature first, so a request nobody signed reads nothing from the chain.
-- A deadline before revealBefore would expire while the commitment can still register.
checkIntent :: NamesConfig -> Contracts -> UTCTime -> ChainSeconds -> RegisterIntent -> IO (Either RevealRefusal ())
checkIntent NamesConfig {nChain = ChainConfig {cChainId, cSponsor}} ct now earliest intent@RegisterIntent {riRegistration = Registration {rOwner}, riNonce, riDeadline}
  | riDeadline < earliest = pure $ Left $ RVBadRequest "the intent's deadline is before revealBefore"
  | toInteger riDeadline > unixSeconds now + signatureSeconds + signedRequestSeconds = pure $ Left $ RVBadRequest "the intent's deadline is more than a week ahead"
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) (chainSeconds now) intent >>= \case
        Left e -> pure $ Left $ RVBadRequest $ T.pack e
        Right () ->
          sponsorNonce ct rOwner >>= \case
            Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's nonce read failed: " <> tshow e)
            Right nonce -> pure $ if nonce == riNonce then Right () else Left $ RVBadRequest "the intent's nonce is not the sponsor's next"

-- | After the intent, so only a signed request reads the policy. Every name needs its owner's signature of the acceptance
-- its view returns, with the intent's deadline.
checkAcceptance :: NamesConfig -> Contracts -> Address -> OrderFields -> NameReveal -> IO (Either RevealRefusal ())
checkAcceptance NamesConfig {nChain = ChainConfig {cChainId}} ct owner OrderFields {ofLabelHash, ofCommitment} NameReveal {rvDeadline, rvAcceptance = a@TermsAcceptance {taTermsHash, taNonce}} =
  currentAcceptance ct ofLabelHash owner >>= \case
    Left e -> Left RVChainUnavailable <$ logWarn ("names: a reveal's terms acceptance could not be read: " <> tshow e)
    Right AcceptanceAsked {acCommunity, acPolicy, acTermsHash, acNonce}
      | taTermsHash == acTermsHash && taNonce == acNonce -> first (RVBadRequest . T.pack) <$> verifyAcceptance (policyDomain cChainId acPolicy) (termsKind acCommunity) owner ofCommitment rvDeadline a
      | otherwise -> pure $ Left $ RVBadRequest "the acceptance is not of the terms and nonce the policy asks"

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
