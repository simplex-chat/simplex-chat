{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Names.View
  ( RegistrationView (..),
    PaidWith (..),
    BalanceView (..),
    PhaseView (..),
    Signing (..),
    ToSign (..),
    NameRefusal (..),
    registrationView,
    phaseView,
    readToSign,
    toRegistrationView,
    balanceView,
    addressText,
    lapsedAt,
    lapseMoment,
    registrationVersion,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Logger.Simple (logWarn)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.SystemTime (SystemSeconds, roundedToUTCTime)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (Hex32 (..), TxHash (..))
import StoreService.Names.Community (AcceptanceAsked, Community, TermsLink, communityOf, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (DisputeDays, LinkKind)
import StoreService.Names.Signatures (AgreedTerms (..))
import StoreService.Names.Store (Balance (..), Commit (..), FailReason, InvoiceClose, LateWait, NameRegistration (..), NameReveal (..), OrderPayment (..), OrderPhase (..), TxHistory (..), getBalance, getRegistration, lateWait, orderLive, paymentInvoice)

data PaidWith
  = PWInvoice {method :: Text}
  | PWCode
  deriving (Eq, Show)

data BalanceView = BalanceView {amount :: USDCents, refunded :: USDCents}
  deriving (Eq, Show)

data PhaseView
  = PVUnpaid
  | PVClosed {closedBy :: InvoiceClose}
  | PVQueued
  | PVCommitting
  | PVCommitted {revealAfter :: SystemSeconds, revealBefore :: SystemSeconds, signature :: Signing}
  | PVRegistering
  | PVRegistered {expiresAt :: SystemSeconds, disputeUntil :: Maybe SystemSeconds}
  | PVTaken
  | PVFailed {reason :: FailReason, heldUntil :: Maybe UTCTime}
  | PVEnded
  deriving (Eq, Show)

data Signing = SAwaiting {toSign :: ToSign} | SSigned | SLapsed
  deriving (Eq, Show)

-- | What the owner signs with the reveal; unread when either chain read failed.
data ToSign = TSUnread | TSRead {intentNonce :: Word64, acceptance :: AcceptanceAsked}
  deriving (Eq, Show)

data RegistrationView = RegistrationView
  { vwId :: Text,
    vwVersion :: Int64,
    vwPhase :: PhaseView,
    vwLabelHash :: Hex32,
    vwLabel :: Maybe Text,
    vwOwner :: Address,
    vwCommitment :: Hex32,
    vwSealed :: Text,
    vwYears :: Int,
    vwDuration :: Word64,
    vwPrice :: USDCents,
    vwPayment :: PaidWith,
    vwDisputeDays :: DisputeDays,
    vwBalance :: Maybe BalanceView,
    vwLatePayment :: Maybe LateWait,
    vwCommitTx :: Maybe TxHash,
    vwRegisterTx :: Maybe TxHash,
    vwCommunity :: Maybe Community,
    vwTerms :: AgreedTerms
  }
  deriving (Eq, Show)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "PW") ''PaidWith)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False} ''BalanceView)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "TS") ''ToSign)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "S") ''Signing)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "PV") ''PhaseView)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False, J.fieldLabelModifier = dropPrefix "vw"} ''RegistrationView)

data NameRefusal
  = NRBadRequest
  | NRInvalidName
  | NRInvalidLink {kind :: Maybe LinkKind}
  | NRNotFound
  | NRInternal
  | NROrderExists
  | NRNameTaken
  | NRNameCheckUnavailable
  | NRCatalogChanged {price :: USDCents}
  | NRTermsChanged {terms :: TermsLink, community :: Maybe Community}
  | NRCodesOnly
  | NRDowntime
  | NRCodeInvalid
  | NRCodeUsed
  | NRCodeReserved
  | NRCodeExpired
  | NRCodeNotCovering
  | NRPaymentPending
  | NRRegistrationChanged {registration :: RegistrationView}
  | NRStaleNonce
  | NRRecordReverted
  | NREditLimit {nextAt :: UTCTime}
  | NRRateLimited {retryAfter :: Int}
  | NRProviderUnavailable
  | NRTermsUnavailable {wait :: Maybe Int}
  | NREditPending
  deriving (Eq, Show)

$(JQ.deriveToJSON (taggedObjectJSON $ J.camelTo2 '_' . dropPrefix "NR") {J.omitNothingFields = False} ''NameRefusal)

-- | The balance, and what a closed order's late money waits on.
registrationView :: DBStore -> NamesConfig -> Contracts -> UTCTime -> NameRegistration -> IO RegistrationView
registrationView st cfg ct now r = do
  (balance, late) <- withTransaction st $ \db -> do
    balance <- getBalance db (nrId r)
    late <- maybe (pure Nothing) (lateWait db) (paymentInvoice $ nrPayment r)
    pure (balance, late)
  toRegistrationView cfg r balance late <$> phaseView ct now r

-- | The chain is read only while a committed order awaits its owner's signature.
phaseView :: Contracts -> UTCTime -> NameRegistration -> IO PhaseView
phaseView ct now r@NameRegistration {nrPhase, nrReveal} = case nrPhase of
  OUnpaid _ -> pure PVUnpaid
  OClosed by -> pure PVClosed {closedBy = by}
  OQueued -> pure PVQueued
  OCommitting _ -> pure PVCommitting
  OCommitted Commit {cmRevealAfter, cmRevealBefore} -> PVCommitted cmRevealAfter cmRevealBefore <$> signing
  ORegistering {} -> pure PVRegistering
  ORegistered expiry disputeEnd _ -> pure PVRegistered {expiresAt = expiry, disputeUntil = disputeEnd}
  OTaken _ -> pure PVTaken
  OFailed _ why -> pure PVFailed {reason = why, heldUntil = if orderLive r then addMonths 1 <$> nrSelectedAt r else Nothing}
  OEnded {} -> pure PVEnded
  where
    signing
      | isJust nrReveal = pure SSigned
      | lapsedAt now r = pure SLapsed
      | otherwise = SAwaiting <$> readToSign ct ("the view of registration " <> rid) nrOwner nrLabelHash
    NameRegistration {nrId = RegistrationId rid, nrOwner, nrLabelHash} = r

readToSign :: Contracts -> Text -> Address -> LabelHash -> IO ToSign
readToSign ct what owner lh = do
  deadline <- deadlineIn $ ctDeadline ct
  withSlot deadline (ctViewSlots ct) (concurrently (sponsorNonce ct owner) (currentAcceptance ct lh owner)) >>= \case
    Just (Right nonce, Right asked) -> pure TSRead {intentNonce = nonce, acceptance = asked}
    Just (nonceRead, askedRead) -> TSUnread <$ unread ("could not read what its owner signs: " <> T.intercalate "; " [tshow e | Left e <- [() <$ nonceRead, () <$ askedRead]])
    Nothing -> TSUnread <$ unread "read the chain in no slot within its deadline"
  where
    unread reason = logWarn $ "names: " <> what <> " " <> reason

toRegistrationView :: NamesConfig -> NameRegistration -> Maybe Balance -> Maybe LateWait -> PhaseView -> RegistrationView
toRegistrationView cfg r balance late phase =
  RegistrationView
    { vwId = rid,
      vwVersion = nrVersion,
      vwPhase = phase,
      vwLabelHash = Hex32 lh,
      vwLabel = if isJust nrSettledAt then rvLabel <$> nrReveal else Nothing,
      vwOwner = nrOwner,
      vwCommitment = nrCommitment,
      vwSealed = nrSealed,
      vwYears = nrYears,
      vwDuration = nrDuration,
      vwPrice = nrPrice,
      vwPayment = paidWith nrPayment,
      vwDisputeDays = nrDisputeDays,
      vwBalance = balanceView <$> balance,
      vwLatePayment = late,
      vwCommitTx = thCommitTx,
      vwRegisterTx = thRegisterTx,
      vwCommunity = communityOf cfg (nrLabelHash r) (communityName nrAgreedTerms),
      vwTerms = nrAgreedTerms
    }
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrPayment, nrDisputeDays, nrAgreedTerms, nrHistory = TxHistory {thCommitTx, thRegisterTx}, nrSettledAt} = r

paidWith :: OrderPayment -> PaidWith
paidWith = \case
  OPInvoice m _ -> PWInvoice {method = textEncode m}
  OPCode _ -> PWCode

balanceView :: Balance -> BalanceView
balanceView Balance {blAmount, blRefunded} = BalanceView {amount = blAmount, refunded = blRefunded}

-- | EIP-55, as the page compares owners.
addressText :: Address -> Text
addressText = safeDecodeUtf8 . strEncode

-- | Versions start at 1, so a registration deleted during a hold reads as changed.
registrationVersion :: DBStore -> RegistrationId -> IO Int64
registrationVersion st rid = maybe 0 nrVersion <$> withTransaction st (`getRegistration` rid)

-- | Past revealBefore with no reveal, the commitment can no longer register.
lapsedAt :: UTCTime -> NameRegistration -> Bool
lapsedAt now = maybe False (now >) . lapseMoment

lapseMoment :: NameRegistration -> Maybe UTCTime
lapseMoment = \case
  NameRegistration {nrPhase = OCommitted Commit {cmRevealBefore}, nrReveal = Nothing} -> Just $ roundedToUTCTime cmRevealBefore
  _ -> Nothing
