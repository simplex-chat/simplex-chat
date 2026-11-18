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
    registrationView,
    phaseView,
    readToSign,
    toRegistrationView,
    balanceView,
    phaseText,
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
import StoreService.Names.Community (AcceptanceAsked, Community, communityOf, currentAcceptance)
import StoreService.Names.Contracts (Contracts (ctDeadline, ctViewSlots), sponsorNonce)
import StoreService.Names.Profiles (deadlineIn, withSlot)
import StoreService.Names.Registration (AgreedTerms (..), RegistrationId (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceUse (..), Commit (..), FailReason, HoldState, InvoiceClose, LateWait, NamePhase (..), NameRegistration (..), NameReveal (..), OrderPayment (..), OrderPhase (..), TxHistory (..), getBalance, getRegistration, heldBy, lateWait, namePhase, paymentInvoice)

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
  | PVFailed {reason :: FailReason}
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
    vwLinksRefused :: Bool,
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
  OFailed _ why -> pure PVFailed {reason = why}
  where
    signing
      | isJust nrReveal = pure SSigned
      | lapsedAt now r = pure SLapsed
      | otherwise = SAwaiting <$> readToSign ct r

readToSign :: Contracts -> NameRegistration -> IO ToSign
readToSign ct NameRegistration {nrId = RegistrationId rid, nrOwner, nrLabelHash} = do
  deadline <- deadlineIn $ ctDeadline ct
  withSlot deadline (ctViewSlots ct) (concurrently (sponsorNonce ct nrOwner) (currentAcceptance ct nrLabelHash nrOwner)) >>= \case
    Just (Right nonce, Right asked) -> pure TSRead {intentNonce = nonce, acceptance = asked}
    Just (nonceRead, askedRead) -> TSUnread <$ unread ("could not read what its owner signs: " <> T.intercalate "; " [tshow e | Left e <- [() <$ nonceRead, () <$ askedRead]])
    Nothing -> TSUnread <$ unread "read the chain in no slot within its deadline"
  where
    unread reason = logWarn $ "names: the view of registration " <> rid <> " " <> reason

toRegistrationView :: NamesConfig -> NameRegistration -> Maybe Balance -> Maybe LateWait -> PhaseView -> RegistrationView
toRegistrationView cfg r balance late phase =
  RegistrationView
    { vwId = rid,
      vwVersion = nrVersion,
      vwMoves = nrMoves,
      vwLinksRefused = isJust nrLinksRefusedAt,
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
    NameRegistration {nrYears, nrDuration, nrPrice, nrPayment, nrAgreedTerms, nrLinksRefusedAt, nrHistory = TxHistory {thCommitTx, thRegisterTx}, nrReplacedBy} = r

paidWith :: OrderPayment -> PaidWith
paidWith = \case
  OPInvoice m _ -> PWInvoice {method = textEncode m}
  OPDifference BalanceUse {buUsed} m _ -> PWDifference {method = textEncode m, fromBalance = buUsed}
  OPBalance BalanceUse {buUsed} -> PWBalance {fromBalance = buUsed}
  OPCode _ -> PWCode

balanceView :: Balance -> BalanceView
balanceView b@Balance {blAmount, blRefunded} = BalanceView {amount = blAmount, hold = bhState <$> heldBy b, refunded = blRefunded}

phaseText :: OrderPhase -> Text
phaseText = \case
  OClosed _ -> textEncode PUnpaid
  phase -> textEncode $ namePhase phase

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
