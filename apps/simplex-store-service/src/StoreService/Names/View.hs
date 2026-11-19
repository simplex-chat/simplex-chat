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
    Successor (..),
    NameRefusal (..),
    refusalResponse,
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
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Network.HTTP.Types (status400, status403, status404, status405, status409, status429, status500, status503, status504)
import Network.Wai (Response, responseLBS)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Chat.PaymentService.Types (InvoiceId)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (strEncode, textEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (ChainSeconds, Hex32 (..), TxHash (..), chainTime)
import StoreService.Names.Community (AcceptanceAsked, Community, TermsLink, communityOf, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (AgreedTerms (..), LinkKind, RegistrationId (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceUse (..), Commit (..), FailReason, HoldState, InvoiceClose, LateWait, NameRegistration (..), NameReveal (..), OrderPayment (..), OrderPhase (..), TxHistory (..), getBalance, getRegistration, heldBy, lateWait, orderLive, paymentInvoice)
import StoreService.Web.Http (apiHeaders)

data PaidWith
  = PWInvoice {method :: Text}
  | PWDifference {method :: Text, fromBalance :: USDCents}
  | PWBalance {fromBalance :: USDCents}
  | PWCode
  deriving (Eq, Show)

-- | The holder is not named, so an order's link discloses no other order. An overdue hold reads as none.
data BalanceView = BalanceView {amount :: USDCents, hold :: Maybe HoldState, refunded :: USDCents}
  deriving (Eq, Show)

data PhaseView
  = PVUnpaid
  | PVClosed {closedBy :: InvoiceClose}
  | PVQueued
  | PVCommitting
  | PVCommitted {revealAfter :: ChainSeconds, revealBefore :: ChainSeconds, signature :: Signing}
  | PVRegistering
  | PVRegistered {expiresAt :: ChainSeconds}
  | PVTaken
  | PVFailed {reason :: FailReason, heldUntil :: Maybe UTCTime}
  deriving (Eq, Show)

data Signing = SAwaiting {toSign :: ToSign} | SSigned | SLapsed
  deriving (Eq, Show)

-- | What the owner signs with the reveal; unread when either chain read failed.
data ToSign = TSUnread | TSRead {intentNonce :: Word64, acceptance :: AcceptanceAsked}
  deriving (Eq, Show)

data Successor = Successor {scRegistrationId :: Text, scInvoiceId :: Maybe InvoiceId}
  deriving (Eq, Show)

data RegistrationView = RegistrationView
  { vwId :: Text,
    vwVersion :: Int64,
    vwMoves :: Int,
    vwLinksRefused :: Maybe LinkKind,
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
    vwBalance :: Maybe BalanceView,
    vwLatePayment :: Maybe LateWait,
    vwCommitTx :: Maybe TxHash,
    vwRegisterTx :: Maybe TxHash,
    vwCommunity :: Maybe Community,
    vwTerms :: AgreedTerms,
    vwReplacedBy :: Maybe Successor
  }
  deriving (Eq, Show)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "PW") ''PaidWith)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False} ''BalanceView)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "TS") ''ToSign)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "S") ''Signing)

$(JQ.deriveToJSON (taggedObjectJSON $ dropPrefix "PV") ''PhaseView)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False, J.fieldLabelModifier = dropPrefix "sc"} ''Successor)

$(JQ.deriveToJSON defaultJSON {J.omitNothingFields = False, J.fieldLabelModifier = dropPrefix "vw"} ''RegistrationView)

data NameRefusal
  = NRBadRequest
  | NRInvalidName
  | NRInvalidLink {rfKind :: Maybe LinkKind}
  | NRNotFound
  | NRMethodNotAllowed
  | NRInternal
  | NROrderExists
  | NRNameTaken
  | NRNameCheckUnavailable
  | NRCatalogChanged {rfPrice :: USDCents}
  | NRPriceTooHigh {rfPrice :: USDCents}
  | NRBalanceChanged {rfBalance :: Maybe BalanceView}
  | NRTermsChanged {rfTerms :: TermsLink, rfCommunity :: Maybe Community}
  | NRCodesOnly
  | NRCodeInvalid
  | NRCodeUsed
  | NRCodeReserved
  | NRCodeExpired
  | NRCodeNotCovering
  | NRPaymentPending
  | NRMoveLimit {rfRegistration :: RegistrationView}
  | NRRegistrationChanged {rfRegistration :: RegistrationView}
  | NRStaleNonce
  | NRRecordReverted
  | NREditLimit {rfNextAt :: UTCTime}
  | NRRateLimited {rfRetryAfter :: Int}
  | NRProviderUnavailable
  | NRTermsUnavailable {rfWait :: Maybe Int}
  | NREditPending
  deriving (Eq, Show)

$(JQ.deriveToJSON (taggedObjectJSON $ J.camelTo2 '_' . dropPrefix "NR") {J.omitNothingFields = False, J.fieldLabelModifier = dropPrefix "rf"} ''NameRefusal)

refusalResponse :: NameRefusal -> Response
refusalResponse r = responseLBS status (retryAfterHeader <> apiHeaders) (J.encode r)
  where
    status = case r of
      NRBadRequest -> status400
      NRInvalidName -> status400
      NRInvalidLink {} -> status400
      NRNotFound -> status404
      NRMethodNotAllowed -> status405
      NRInternal -> status500
      NROrderExists -> status409
      NRNameTaken -> status409
      NRNameCheckUnavailable -> status503
      NRCatalogChanged {} -> status409
      NRPriceTooHigh {} -> status409
      NRBalanceChanged {} -> status409
      NRTermsChanged {} -> status409
      NRCodesOnly -> status403
      NRCodeInvalid -> status400
      NRCodeUsed -> status409
      NRCodeReserved -> status409
      NRCodeExpired -> status409
      NRCodeNotCovering -> status409
      NRPaymentPending -> status409
      NRMoveLimit {} -> status409
      NRRegistrationChanged {} -> status409
      NRStaleNonce -> status409
      NRRecordReverted -> status409
      NREditLimit {} -> status409
      NRRateLimited {} -> status429
      NRProviderUnavailable -> status503
      NRTermsUnavailable {} -> status503
      NREditPending -> status504
    retryAfterHeader = case r of
      NRRateLimited {rfRetryAfter} -> [("Retry-After", B.pack $ show rfRetryAfter)]
      NRTermsUnavailable {rfWait = Just seconds} -> [("Retry-After", B.pack $ show seconds)]
      _ -> []

-- | The balance as it reads at the given time, when a hold may have become overdue, and what a closed order's late money waits on.
registrationView :: DBStore -> NamesConfig -> Contracts -> UTCTime -> NameRegistration -> IO RegistrationView
registrationView st cfg ct now r = do
  (balance, late) <- withTransaction st $ \db -> do
    balance <- getBalance db (nrId r) now
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
  ORegistered expiry _ -> pure PVRegistered {expiresAt = expiry}
  OTaken -> pure PVTaken
  OFailed _ why -> pure PVFailed {reason = why, heldUntil = if orderLive r then addMonths 1 <$> nrPaidAt r else Nothing}
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
      vwMoves = nrMoves,
      vwLinksRefused = snd <$> nrLinksRefused,
      vwPhase = phase,
      vwLabelHash = Hex32 lh,
      vwLabel = rvLabel <$> nrReveal,
      vwOwner = nrOwner,
      vwCommitment = nrCommitment,
      vwSealed = nrSealed,
      vwYears = nrYears,
      vwDuration = nrDuration,
      vwPrice = nrPrice,
      vwPayment = paidWith nrPayment,
      vwBalance = balanceView <$> balance,
      vwLatePayment = late,
      vwCommitTx = thCommitTx,
      vwRegisterTx = thRegisterTx,
      vwCommunity = communityOf cfg (nrLabelHash r) (atCommunityName nrAgreedTerms),
      vwTerms = nrAgreedTerms,
      vwReplacedBy = (\(RegistrationId s, invId) -> Successor {scRegistrationId = s, scInvoiceId = invId}) <$> nrReplacedBy
    }
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrMoves, nrLabelHash = LabelHash lh, nrReveal, nrOwner, nrCommitment, nrSealed} = r
    NameRegistration {nrYears, nrDuration, nrPrice, nrPayment, nrAgreedTerms, nrLinksRefused, nrHistory = TxHistory {thCommitTx, thRegisterTx}, nrReplacedBy} = r

paidWith :: OrderPayment -> PaidWith
paidWith = \case
  OPInvoice m _ -> PWInvoice {method = textEncode m}
  OPDifference BalanceUse {buUsed} m _ -> PWDifference {method = textEncode m, fromBalance = buUsed}
  OPBalance BalanceUse {buUsed} -> PWBalance {fromBalance = buUsed}
  OPCode _ -> PWCode

balanceView :: Balance -> BalanceView
balanceView b@Balance {blAmount, blRefunded} = BalanceView {amount = blAmount, hold = bhState <$> heldBy b, refunded = blRefunded}

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
  NameRegistration {nrPhase = OCommitted Commit {cmRevealBefore}, nrReveal = Nothing} -> Just $ chainTime cmRevealBefore
  _ -> Nothing
