{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module StoreService.Names.Store
  ( CodeId,
    RecordEditId,
    OrderHash,
    CodeHash,
    BodyHash,
    NamePhase (..),
    TxRef (..),
    Commit (..),
    InvoiceClose (..),
    OrderPhase (..),
    namePhase,
    phaseCommit,
    TxHistory (..),
    noHistory,
    FailReason (..),
    OrderPayment (..),
    paymentInvoice,
    NameReveal (..),
    NameRegistration (..),
    NewRegistration (..),
    PaidOrder (..),
    CodeInsertRefused (..),
    VersionWrite (..),
    LiveOrder (..),
    OrderTerms (..),
    Balance (..),
    BalanceLink (..),
    LateCredit (..),
    CreditAmount (..),
    creditCents,
    LateCreditState (..),
    LateMoney (..),
    LateFigures (..),
    LatePending (..),
    LateAlert (..),
    LateSupport (..),
    LateWait (..),
    CleanupKept (..),
    BrokenInvariant (..),
    PrepaidCode (..),
    NewPrepaidCode (..),
    CmdSource (..),
    CodeHashTaken (..),
    TxFor (..),
    RegistrationTx (..),
    TxStatus (..),
    TxSettlement (..),
    TxState (..),
    NameTransaction (..),
    NewTransaction (..),
    RecordChange (..),
    SignedChange (..),
    EditOutcome (..),
    EditState (..),
    RecordEdit (..),
    NewRecordEdit (..),
    NamesCounts (..),
    findLimit,
    orderHash,
    orderLive,
    newRegistrationId,
    createPaidRegistration,
    createCodeRegistration,
    getRegistration,
    getRegistrationByOrderHash,
    getLiveOrders,
    closeOverdue,
    updateRegistration,
    selectRegistration,
    findRegistrations,
    labelHolder,
    setSettledAt,
    unregisteredPaidBefore,
    monthCandidates,
    markRead,
    claimUnregisteredAlert,
    AttemptStage (..),
    NameAttempt (..),
    recordAttempt,
    openInvoicesOfHeldName,
    activeRegistrations,
    unpaidCleanupCandidates,
    deleteUnpaidRegistration,
    getBalance,
    balanceByReference,
    balanceLink,
    creditBalance,
    spendBalance,
    refundBalance,
    recordReversal,
    takeBackBalance,
    lockRegistration,
    reversalRecorded,
    registerTxSent,
    registerTxPending,
    lateCreditState,
    creditLatePayment,
    latePending,
    uncreditedLateInvoices,
    lateAlertDue,
    claimLateAlert,
    setLateSupport,
    lateWait,
    insertPrepaidCode,
    getPrepaidCode,
    groupNameCodesIssuedSince,
    getPrepaidCodeById,
    reservePrepaidCode,
    releasePrepaidCode,
    spendLandedPrepaidCode,
    revokePrepaidCode,
    insertTransaction,
    recordTransactionOutcome,
    pendingTransactions,
    registrationTransactions,
    editTransactions,
    insertRecordEdit,
    getRecordEdit,
    getRecordEditByBody,
    settleRecordEdit,
    pendingEditIds,
    deleteUnsentEdit,
    relayedEditTimes,
    getTermsText,
    setTermsText,
    namesCounts,
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameLength, nameLength, nameLengthInt)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.PaymentService.Types (CryptoCurrency, CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider, ServicePaymentMethod)
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..), FromField (..), ToField (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding (Encoding (..), Large (..))
import Simplex.Messaging.Encoding.String (TextEncoding (..), textToJSON)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds)
import Simplex.Messaging.Util (maybeFirstRow, maybeFirstRow', tshow)
import StoreService.Names.Chain (Hex32 (..), TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (Commitment, LinkKind, TermsAcceptance (..), TermsHash)
import StoreService.Names.Revert (RevertName)
import StoreService.Names.Signatures (AgreedTerms, OrderFields (..))
import StoreService.Providers (Rate, Reversal (..), Uncreditable (..), Units)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (StoreDecodeError (..), executeChanging, lockInvoice, mkQuery, newInvoiceId, note, prefixQuery, unfundedQuery)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, ToRow, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, ToRow, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

type CodeId = Int64

type RecordEditId = Int64

type OrderHash = Hex32

type CodeHash = Hex32

type BodyHash = Hex32

data NamePhase
  = PUnpaid
  | PClosed
  | PQueued
  | PCommitting
  | PCommitted
  | PRegistering
  | PRegistered
  | PTaken
  | PFailed
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding NamePhase where
  textEncode = \case
    PUnpaid -> "unpaid"
    PClosed -> "closed"
    PQueued -> "queued"
    PCommitting -> "committing"
    PCommitted -> "committed"
    PRegistering -> "registering"
    PRegistered -> "registered"
    PTaken -> "taken"
    PFailed -> "failed"
  textDecode = \case
    "unpaid" -> Just PUnpaid
    "closed" -> Just PClosed
    "queued" -> Just PQueued
    "committing" -> Just PCommitting
    "committed" -> Just PCommitted
    "registering" -> Just PRegistering
    "registered" -> Just PRegistered
    "taken" -> Just PTaken
    "failed" -> Just PFailed
    _ -> Nothing

instance FromField NamePhase where fromField = fromTextField_ textDecode

instance ToField NamePhase where toField = toField . textEncode

data TxRef = TxRef {txHash :: TxHash, txBlock :: Word64}
  deriving (Eq, Show)

data Commit = Commit
  { cmTx :: Maybe TxRef,
    cmRevealAfter :: SystemSeconds,
    cmRevealBefore :: SystemSeconds
  }
  deriving (Eq, Show)

data InvoiceClose = ICExpired | ICCancelled | ICReversed
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding InvoiceClose where
  textEncode = \case
    ICExpired -> "expired"
    ICCancelled -> "cancelled"
    ICReversed -> "reversed"
  textDecode = \case
    "expired" -> Just ICExpired
    "cancelled" -> Just ICCancelled
    "reversed" -> Just ICReversed
    _ -> Nothing

instance FromField InvoiceClose where fromField = fromTextField_ textDecode

instance ToField InvoiceClose where toField = toField . textEncode

data OrderPhase
  = OUnpaid InvoiceId
  | OClosed InvoiceClose
  | OQueued
  | OCommitting TxHash
  | OCommitted Commit
  | ORegistering Commit TxHash
  | ORegistered SystemSeconds UTCTime
  | OTaken
  | OFailed UTCTime FailReason
  deriving (Eq, Show)

namePhase :: OrderPhase -> NamePhase
namePhase = \case
  OUnpaid _ -> PUnpaid
  OClosed _ -> PClosed
  OQueued -> PQueued
  OCommitting _ -> PCommitting
  OCommitted _ -> PCommitted
  ORegistering {} -> PRegistering
  ORegistered {} -> PRegistered
  OTaken -> PTaken
  OFailed {} -> PFailed

phaseCommit :: OrderPhase -> Maybe Commit
phaseCommit = \case
  OCommitted c -> Just c
  ORegistering c _ -> Just c
  _ -> Nothing

data TxHistory = TxHistory {thCommitTx :: Maybe TxHash, thRegisterTx :: Maybe TxHash}
  deriving (Eq, Show)

noHistory :: TxHistory
noHistory = TxHistory {thCommitTx = Nothing, thRegisterTx = Nothing}

data FailReason = FRRegistryRefused | FRSignatureRefused | FRMonthEnded | FRUnread
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding FailReason where
  textEncode = \case
    FRRegistryRefused -> "registry_refused"
    FRSignatureRefused -> "signature_refused"
    FRMonthEnded -> "month_ended"
    FRUnread -> "unread"
  textDecode = \case
    "registry_refused" -> Just FRRegistryRefused
    "signature_refused" -> Just FRSignatureRefused
    "month_ended" -> Just FRMonthEnded
    "unread" -> Just FRUnread
    _ -> Nothing

instance FromField FailReason where fromField = fromTextField_ textDecode

instance ToField FailReason where toField = toField . textEncode

data OrderPayment
  = OPInvoice ServicePaymentMethod InvoiceId
  | OPCode CodeId
  deriving (Eq, Show)

paymentInvoice :: OrderPayment -> Maybe InvoiceId
paymentInvoice = \case
  OPInvoice _ invId -> Just invId
  OPCode _ -> Nothing

-- | The name, links and owner's intent, all set by an accepted reveal and cleared together.
data NameReveal = NameReveal
  { rvLabel :: Text,
    rvSecret :: Hex32,
    rvContactLink :: Maybe Text,
    rvChannelLink :: Maybe Text,
    rvNonce :: Word64,
    rvDeadline :: SystemSeconds,
    rvSignature :: RecoverableSignature,
    -- | the owner's acceptance of the terms of the name's kind
    rvAcceptance :: TermsAcceptance
  }
  deriving (Eq, Show)

data NameRegistration = NameRegistration
  { nrId :: RegistrationId,
    nrVersion :: Int64,
    nrLabelHash :: LabelHash,
    nrLength :: NameLength,
    nrDuration :: Word64,
    nrOwner :: Address,
    nrCommitment :: Commitment,
    nrSealed :: Text,
    nrAgreedTerms :: AgreedTerms,
    nrPhase :: OrderPhase,
    nrYears :: Int,
    nrPrice :: USDCents,
    nrPayment :: OrderPayment,
    nrReveal :: Maybe NameReveal,
    nrHistory :: TxHistory,
    nrOrderHash :: OrderHash,
    nrCreatedAt :: UTCTime,
    nrUpdatedAt :: UTCTime,
    nrReadAt :: UTCTime,
    nrSettledAt :: Maybe UTCTime,
    nrSelectedAt :: Maybe UTCTime,
    nrLastAttempt :: Maybe NameAttempt
  }
  deriving (Eq, Show)

data NewRegistration = NewRegistration
  { nrgId :: RegistrationId,
    nrgOwner :: Address,
    nrgOrder :: OrderFields,
    nrgReveal :: NameReveal,
    nrgYears :: Int,
    nrgOrderHash :: OrderHash
  }

-- | The invoice row is inserted by the caller, in the same transaction and before the registration.
data PaidOrder = PaidOrder
  { poMethod :: ServicePaymentMethod,
    poPrice :: USDCents,
    poInvoiceId :: InvoiceId
  }

data CodeInsertRefused = CIRLiveOrder | CIRCodeNotReservable
  deriving (Eq, Show)

-- | NotLive: the write would make a closed order live. LiveOrderExists: another live order holds its commitment, or its owner and label hash.
data VersionWrite = Written Int64 | VersionChanged | NotLive | LiveOrderExists
  deriving (Eq, Show)

-- | A live order an order request meets; overdue when its open invoice is past its expiry time.
data LiveOrder = LiveOrder
  { loRegistration :: NameRegistration,
    loOverdue :: Bool
  }
  deriving (Eq, Show)

-- | What an order pays with, as its hash covers it.
data OrderTerms = OTInvoice ServicePaymentMethod USDCents | OTCode CodeHash

-- | Cents left to spend, and over the balance's life the cents credited and refunded.
data Balance = Balance
  { blRegistration :: RegistrationId,
    blAmount :: USDCents,
    blCredited :: USDCents,
    blRefunded :: USDCents,
    blRefundedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

-- | The order of a balance and the money paid for it. lkPaid counts only an order that went past unpaid.
data BalanceLink = BalanceLink
  { lkRegistration :: RegistrationId,
    lkOwner :: Address,
    lkPayment :: OrderPayment,
    lkProviderRef :: Maybe Text,
    lkPaid :: USDCents,
    lkLateCredits :: [LateCredit]
  }
  deriving (Eq, Show)

-- | Money paid on a name invoice after its order closed, credited to that order's balance.
data LateCredit = LateCredit
  { lcInvoice :: InvoiceId,
    lcCreditNo :: Int,
    lcRegistration :: RegistrationId,
    lcAmount :: CreditAmount
  }
  deriving (Eq, Show)

-- | Card cents, or crypto units credited in cents at the rate used, the lower of the invoice's and the current one.
data CreditAmount
  = CACard USDCents
  | CACrypto {caUnits :: Units, caInvoiceRate :: Rate, caCurrentRate :: Rate, caRateUsed :: Rate, caCents :: USDCents}
  deriving (Eq, Show)

creditCents :: CreditAmount -> USDCents
creditCents = \case
  CACard cents -> cents
  CACrypto {caCents} -> caCents

-- | A name invoice's payment figures, what was credited from it, and its order's phase.
-- lsSupport: why its money will not be credited by itself, once known.
-- | A closed order's late money, or the extra crypto of an invoice that paid its order.
data LateMoney = LMLate | LMOverpaid
  deriving (Eq, Show)

data LateCreditState = LateCreditState
  { lsInvoice :: InvoiceId,
    lsRegistration :: RegistrationId,
    lsMoney :: LateMoney,
    lsClosed :: Bool,
    lsSupport :: Maybe LateSupport,
    lsReceivedCents :: USDCents,
    lsFigures :: LateFigures,
    lsInvoiceAmount :: USDCents,
    lsCreditedCents :: USDCents,
    lsCredits :: Int,
    lsPaymentCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

-- | Card money counted in cents; crypto in units, with the invoice's rate and invoiced units it is credited at.
data LateFigures
  = LFCard {lfConfirmedCents :: USDCents}
  | LFCrypto
      { lfCoin :: CryptoCurrency,
        lfReceivedUnits :: Maybe Units,
        lfConfirmedUnits :: Maybe Units,
        lfInvoicedUnits :: Maybe Units,
        lfInvoiceRate :: Maybe Rate,
        lfCreditedUnits :: Units
      }
  deriving (Eq, Show)

-- | Why a closed order's money is not all credited: Monero confirming with a zero figure, confirmed money not credited yet,
-- money received and not confirmed, or crypto received that could not be counted in units.
data LatePending = LPConfirming | LPUncredited | LPUnconfirmed | LPUncounted
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding LatePending where
  textEncode = \case
    LPConfirming -> "confirming"
    LPUncredited -> "uncredited"
    LPUnconfirmed -> "unconfirmed"
    LPUncounted -> "uncounted"
  textDecode = \case
    "confirming" -> Just LPConfirming
    "uncredited" -> Just LPUncredited
    "unconfirmed" -> Just LPUnconfirmed
    "uncounted" -> Just LPUncounted
    _ -> Nothing

instance FromField LatePending where fromField = fromTextField_ textDecode

-- | What a closed order's late money waits on, for the view: confirmation, the credit, or support.
data LateWait = LWConfirming | LWCrediting | LWSupport
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding LateWait where
  textEncode = \case
    LWConfirming -> "confirming"
    LWCrediting -> "crediting"
    LWSupport -> "support"
  textDecode = \case
    "confirming" -> Just LWConfirming
    "crediting" -> Just LWCrediting
    "support" -> Just LWSupport
    _ -> Nothing

instance ToJSON LateWait where toJSON = textToJSON

-- | Why a closed order's money will not be credited by itself: the provider's reason it is uncreditable, a current rate below the floor,
-- or unconfirmed money alerted past the read window.
data LateSupport = LSUncreditable Uncreditable | LSRateFloor | LSUnconfirmed
  deriving (Eq, Show)

instance TextEncoding LateSupport where
  textEncode = \case
    LSUncreditable UCHighSpeed -> "high_speed"
    LSUncreditable UCUncounted -> "units"
    LSUncreditable UCNoConfirmationShown -> "no_confirmation"
    LSRateFloor -> "rate_floor"
    LSUnconfirmed -> "unconfirmed"
  textDecode = \case
    "high_speed" -> Just $ LSUncreditable UCHighSpeed
    "units" -> Just $ LSUncreditable UCUncounted
    "no_confirmation" -> Just $ LSUncreditable UCNoConfirmationShown
    "rate_floor" -> Just LSRateFloor
    "unconfirmed" -> Just LSUnconfirmed
    _ -> Nothing

instance FromField LateSupport where fromField = fromTextField_ textDecode

instance ToField LateSupport where toField = toField . textEncode

-- | A closed order's pending money to alert on, with the received figure the alert names and the one the last alert named.
data LateAlert = LateAlert
  { laState :: LateCreditState,
    laProviderRef :: Text,
    laPending :: LatePending,
    laReceived :: Maybe Int64,
    laAlertedAt :: Maybe UTCTime,
    laAlertedReceived :: Maybe Int64
  }
  deriving (Eq, Show)

-- | Why the cleanup deletes no unpaid order: a payment was recorded for it, or it is gone.
data CleanupKept = CKPaid | CKGone
  deriving (Eq, Show)

newtype BrokenInvariant = BrokenInvariant Text
  deriving (Show)

instance E.Exception BrokenInvariant

data PrepaidCode = PrepaidCode
  { pcId :: CodeId,
    pcCodeHash :: CodeHash,
    pcMinLength :: Int,
    pcYears :: Int,
    pcPaymentStatus :: BadgeCodePaymentStatus,
    pcReservedBy :: Maybe RegistrationId,
    pcReservedAt :: Maybe UTCTime,
    pcSpentAt :: Maybe UTCTime,
    pcRevokedAt :: Maybe UTCTime,
    pcExpiresAt :: Maybe UTCTime,
    pcCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

data NewPrepaidCode = NewPrepaidCode
  { npcCodeHash :: CodeHash,
    npcMinLength :: Int,
    npcYears :: Int,
    npcPaymentStatus :: BadgeCodePaymentStatus,
    npcExpiresAt :: Maybe UTCTime,
    npcIssuer :: CmdSource
  }

data CmdSource = FromConsole | FromGroup
  deriving (Eq, Show)

data CodeHashTaken = CodeHashTaken
  deriving (Eq, Show)

data TxFor = TxRegistration RegistrationTx RegistrationId | TxEdit RecordEditId
  deriving (Eq, Show)

data RegistrationTx = RTCommit | RTAccept | RTRegister
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding RegistrationTx where
  textEncode = \case
    RTCommit -> "commit"
    RTAccept -> "accept"
    RTRegister -> "register"
  textDecode = \case
    "commit" -> Just RTCommit
    "accept" -> Just RTAccept
    "register" -> Just RTRegister
    _ -> Nothing

instance FromField RegistrationTx where fromField = fromTextField_ textDecode

instance ToField RegistrationTx where toField = toField . textEncode

data AttemptStage = ASDryRun | ASSent
  deriving (Eq, Show)

instance TextEncoding AttemptStage where
  textEncode = \case
    ASDryRun -> "dry_run"
    ASSent -> "sent"
  textDecode = \case
    "dry_run" -> Just ASDryRun
    "sent" -> Just ASSent
    _ -> Nothing

instance FromField AttemptStage where fromField = fromTextField_ textDecode

instance ToField AttemptStage where toField = toField . textEncode

data NameAttempt = NameAttempt
  { naTx :: RegistrationTx,
    naStage :: AttemptStage,
    naReason :: Text,
    naAt :: UTCTime
  }
  deriving (Eq, Show)

-- | A dropped transaction has no receipt and its registrar nonce is used.
data TxStatus = TSPending | TSMined | TSReverted | TSDropped
  deriving (Eq, Show, Enum, Bounded)

data TxSettlement = TSMinedAt Word64 | TSRevertedAt Word64 | TSDroppedTx
  deriving (Eq, Show)

data TxState = TxPending | TxSettled TxSettlement
  deriving (Eq, Show)

instance TextEncoding TxStatus where
  textEncode = \case
    TSPending -> "pending"
    TSMined -> "mined"
    TSReverted -> "reverted"
    TSDropped -> "dropped"
  textDecode = \case
    "pending" -> Just TSPending
    "mined" -> Just TSMined
    "reverted" -> Just TSReverted
    "dropped" -> Just TSDropped
    _ -> Nothing

instance FromField TxStatus where fromField = fromTextField_ textDecode

instance ToField TxStatus where toField = toField . textEncode

data NameTransaction = NameTransaction
  { ntId :: Int64,
    ntFor :: TxFor,
    ntRaw :: ByteString,
    ntHash :: TxHash,
    ntNonce :: Word64,
    ntSentAt :: UTCTime,
    ntState :: TxState
  }
  deriving (Eq, Show)

data NewTransaction = NewTransaction
  { ntxRaw :: ByteString,
    ntxHash :: TxHash,
    ntxNonce :: Word64
  }

-- | A text record set to a value, "" removing it.
data RecordChange = RCSetText LinkKind Text
  deriving (Eq, Show)

data EditOutcome
  = -- | mined, Nothing when found applied with the applying transaction unknown
    EOApplied (Maybe TxHash)
  | -- | the revert's name when known; the nonce is free again
    EOReverted (Maybe RevertName)
  deriving (Eq, Show)

data EditState = ESPending | ESSettled EditOutcome
  deriving (Eq, Show)

-- | A change the owner signed for the resolver's nonce it names.
data SignedChange = SignedChange
  { scChange :: RecordChange,
    scNonce :: Word64,
    scDeadline :: SystemSeconds,
    scSignature :: RecoverableSignature
  }
  deriving (Eq, Show)

-- | One request's changes, relayed in one transaction, their nonces consecutive in this order.
data RecordEdit = RecordEdit
  { reId :: RecordEditId,
    reLabelHash :: LabelHash,
    reOwner :: Address,
    reChanges :: NonEmpty SignedChange,
    reBodyHash :: BodyHash,
    reState :: EditState,
    reCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

data NewRecordEdit = NewRecordEdit
  { nreLabelHash :: LabelHash,
    nreOwner :: Address,
    nreChanges :: NonEmpty SignedChange,
    nreBodyHash :: BodyHash
  }

data NamesCounts = NamesCounts
  { ncPhases :: [(NamePhase, Maybe FailReason, Int)],
    ncQueueOldestChange :: Maybe UTCTime,
    ncPendingTransactions :: Int,
    ncOldestPending :: Maybe UTCTime,
    ncOpenInvoices :: Int,
    ncPendingEdits :: Int,
    ncOldestEdit :: Maybe UTCTime,
    ncOldestUnregisteredPaid :: Maybe UTCTime
  }
  deriving (Eq, Show)

findLimit :: Int
findLimit = 50

initialVersion :: Int64
initialVersion = 1

-- | No two different orders encode the same.
orderHash :: Address -> OrderFields -> OrderTerms -> OrderHash
orderHash owner OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} terms =
  Hex32 $ sha256Hash $ smpEncode (tag, lh, num (nameLengthInt ofLength), int64 ofDuration, unAddress owner, unHex32 ofCommitment, Large $ encodeUtf8 ofSealed, textEncode ofAgreedTerms) <> payment
  where
    (tag, payment) = case terms of
      OTInvoice method price -> ("name-invoice" :: ByteString, smpEncode (textEncode method, num price))
      OTCode (Hex32 codeHash) -> ("name-code", smpEncode codeHash)
    num :: Integral a => a -> Int64
    num = fromIntegral

newRegistrationId :: IO RegistrationId
newRegistrationId = (\(InvoiceId t) -> RegistrationId t) <$> newInvoiceId

-- | False when the registration would be a second live one. A refusal leaves its writes for the caller to roll back.
createPaidRegistration :: DB.Connection -> NewRegistration -> PaidOrder -> UTCTime -> IO Bool
createPaidRegistration db new@NewRegistration {nrgId = RegistrationId rid} PaidOrder {poMethod, poPrice, poInvoiceId = InvoiceId invId} now = do
  inserted <- insertRegistration db new PUnpaid poPrice (Just poMethod) Nothing now
  when inserted $
    DB.execute
      db
      (mkQuery "INSERT INTO @name_invoices (invoice_id, registration_id, created_at) VALUES (?,?,?)")
      (invId, rid, now)
  pure inserted

-- | Reserving the code is the conditional update of the transaction that creates the registration. A refusal leaves its writes for
-- the caller to roll back.
createCodeRegistration :: DB.Connection -> NewRegistration -> PrepaidCode -> UTCTime -> IO (Either CodeInsertRefused ())
createCodeRegistration db new@NewRegistration {nrgId = regId} PrepaidCode {pcId} now = do
  inserted <- insertRegistration db new PQueued 0 Nothing (Just pcId) now
  if not inserted
    then pure $ Left CIRLiveOrder
    else
      reservePrepaidCode db pcId regId now >>= \case
        True -> pure $ Right ()
        False -> pure $ Left CIRCodeNotReservable

-- | False when the registration would be a second live one for its commitment, or for its owner and label hash.
insertRegistration :: DB.Connection -> NewRegistration -> NamePhase -> USDCents -> Maybe ServicePaymentMethod -> Maybe CodeId -> UTCTime -> IO Bool
insertRegistration db NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder, nrgReveal, nrgYears, nrgOrderHash} phase price method codeId now =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        INSERT INTO @name_registrations
          (registration_id, version, label_hash, length, duration, owner, commitment, sealed, phase, years,
           price, method, name_prepaid_code_id, live, order_hash, created_at, updated_at, agreed_terms, settled_at, selected_at, read_at,
           label, secret, contact_link, channel_link, intent_nonce, intent_deadline, intent_signature, accept_terms, accept_nonce, accept_signature)
        VALUES (?,?,?,?,?,?,?,?,?,?, ?,?,?,?,?,?,?, ?,?,?,?, ?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      )
      ( (rid, initialVersion, ofLabelHash, nameLengthInt ofLength, int64 ofDuration, nrgOwner, ofCommitment, ofSealed, phase, nrgYears)
          :. (price, method, codeId, BI True, nrgOrderHash, now, now)
          :. (ofAgreedTerms, placedAt, placedAt, now)
          :. revealRow (Just nrgReveal)
      )
  where
    OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} = nrgOrder
    placedAt = if phase == PQueued then Just now else Nothing

registrationColumns :: Query
registrationColumns =
  [sql|
    SELECT r.registration_id, r.version, r.label_hash, r.length, r.duration, r.owner, r.commitment, r.sealed, r.phase, r.years,
      r.price, r.method, ni.invoice_id, r.name_prepaid_code_id, r.label, r.secret, r.contact_link, r.channel_link, r.intent_nonce, r.intent_deadline,
      r.intent_signature, r.commit_tx, r.commit_block, r.register_tx, r.reveal_after, r.reveal_before, r.expires_at, r.registered_at, r.failed_at, r.fail_reason,
      r.closed_by, r.order_hash, r.created_at, r.updated_at, r.agreed_terms, r.accept_terms,
      r.accept_nonce, r.accept_signature, r.read_at, r.settled_at, r.selected_at, r.last_attempt_tx, r.last_attempt_stage, r.last_attempt_reason, r.last_attempt_at
  |]

registrationFrom :: Query
registrationFrom =
  " "
    <> prefixQuery
      [sql|
    FROM @name_registrations r
    LEFT JOIN @name_invoices ni ON ni.registration_id = r.registration_id
  |]

selectRegistrations :: Query
selectRegistrations = registrationColumns <> registrationFrom

type RegistrationRow =
  (Text, Int64, LabelHash, Int, Int64, Address, Commitment, Text, NamePhase, Int)
    :. (USDCents, Maybe ServicePaymentMethod, Maybe Text, Maybe Int64, Maybe Text, Maybe Hex32, Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)
    :. (Maybe RecoverableSignature, Maybe TxHash, Maybe Int64, Maybe TxHash, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime, Maybe UTCTime, Maybe FailReason)
    :. (Maybe InvoiceClose, OrderHash, UTCTime, UTCTime, AgreedTerms, Maybe TermsHash)
    :. (Maybe Int64, Maybe RecoverableSignature, UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe RegistrationTx, Maybe AttemptStage, Maybe Text, Maybe UTCTime)

toRegistration :: RegistrationRow -> Either Text NameRegistration
toRegistration
  ( (rid, nrVersion, nrLabelHash, len, duration, nrOwner, nrCommitment, nrSealed, phase, nrYears)
      :. (nrPrice, method_, invoiceId_, codeId_, label_, secret_, nrContactLink, nrChannelLink, nonce_, deadline_)
      :. (signature_, commitTx, commitBlock, registerTx, revealAfter, revealBefore, expiresAt, registeredAt, failedAt, failReason)
      :. (closedBy, nrOrderHash, nrCreatedAt, nrUpdatedAt, nrAgreedTerms, acceptTerms_)
      :. (acceptNonce_, acceptSignature_, nrReadAt, nrSettledAt, nrSelectedAt, attemptTx_, attemptStage_, attemptReason_, attemptAt_)
    ) = do
    nrLength <- note ("name_registrations.length: " <> rid) $ nameLength len
    let commitOf after before = Commit {cmTx = TxRef <$> commitTx <*> (word64 <$> commitBlock), cmRevealAfter = RoundedSystemTime after, cmRevealBefore = RoundedSystemTime before}
        noCommit = isNothing revealAfter && isNothing revealBefore && isNothing commitBlock
        noRegistered = isNothing expiresAt && isNothing registeredAt
        noFailure = isNothing failedAt && isNothing failReason
        noClose = isNothing closedBy
        ownCommit = isJust commitTx == isJust commitBlock
        fitting p fits = if fits then Right p else Left $ "name_registrations " <> textEncode phase <> ": " <> rid
    nrPhase <- case phase of
      PUnpaid -> maybe (Left $ "name_registrations unpaid without an invoice: " <> rid) (\invId -> fitting (OUnpaid $ InvoiceId invId) (noCommit && noRegistered && noFailure && noClose)) invoiceId_
      PClosed -> maybe (Left $ "name_registrations closed without closed_by: " <> rid) (\c -> fitting (OClosed c) (noCommit && noRegistered && noFailure)) closedBy
      PQueued -> fitting OQueued (noCommit && noRegistered && noFailure && noClose)
      PCommitting -> maybe (Left $ "name_registrations committing without commit_tx: " <> rid) (\tx -> fitting (OCommitting tx) (noCommit && noRegistered && noFailure && noClose)) commitTx
      PCommitted -> case (revealAfter, revealBefore) of
        (Just after, Just before) -> fitting (OCommitted $ commitOf after before) (ownCommit && noRegistered && noFailure && noClose)
        _ -> Left $ "name_registrations committed without reveal times: " <> rid
      PRegistering -> case (revealAfter, revealBefore, registerTx) of
        (Just after, Just before, Just tx) -> fitting (ORegistering (commitOf after before) tx) (ownCommit && noRegistered && noFailure && noClose)
        _ -> Left $ "name_registrations registering without reveal times or register_tx: " <> rid
      PRegistered -> case (expiresAt, registeredAt) of
        (Just expiry, Just at) -> fitting (ORegistered (RoundedSystemTime expiry) at) (noCommit && noFailure && noClose)
        _ -> Left $ "name_registrations registered without expires_at or registered_at: " <> rid
      PTaken -> fitting OTaken (noCommit && noRegistered && noFailure && noClose)
      PFailed -> case (failedAt, failReason) of
        (Just at, Just reason) -> fitting (OFailed at reason) (noCommit && noRegistered && noClose)
        _ -> Left $ "name_registrations failed without failed_at or fail_reason: " <> rid
    nrPayment <- case (method_, invoiceId_, codeId_) of
      (Just m, Just invId, Nothing) -> Right $ OPInvoice m (InvoiceId invId)
      (Nothing, Nothing, Just codeId) -> Right $ OPCode codeId
      _ -> Left $ "name_registrations payment: " <> rid
    nrReveal <- case (label_, secret_, nonce_, deadline_, signature_) of
      (Nothing, Nothing, Nothing, Nothing, Nothing) -> Right Nothing
      (Just rvLabel, Just rvSecret, Just nonce, Just deadline, Just rvSignature) -> do
        rvAcceptance <- case (acceptTerms_, acceptNonce_, acceptSignature_) of
          (Just termsHash, Just n, Just signature) -> Right TermsAcceptance {termsHash, nonce = word64 n, signature}
          _ -> Left $ "name_registrations acceptance: " <> rid
        Right $ Just NameReveal {rvLabel, rvSecret, rvContactLink = nrContactLink, rvChannelLink = nrChannelLink, rvNonce = word64 nonce, rvDeadline = RoundedSystemTime deadline, rvSignature, rvAcceptance}
      _ -> Left $ "name_registrations reveal: " <> rid
    nrLastAttempt <- case (attemptTx_, attemptStage_, attemptReason_, attemptAt_) of
      (Nothing, Nothing, Nothing, Nothing) -> Right Nothing
      (Just naTx, Just naStage, Just naReason, Just naAt) -> Right $ Just NameAttempt {naTx, naStage, naReason, naAt}
      _ -> Left $ "name_registrations last attempt: " <> rid
    pure
      NameRegistration
        { nrId = RegistrationId rid,
          nrVersion,
          nrLabelHash,
          nrLength,
          nrDuration = word64 duration,
          nrOwner,
          nrCommitment,
          nrSealed,
          nrAgreedTerms,
          nrPhase,
          nrYears,
          nrPrice,
          nrPayment,
          nrReveal,
          nrHistory = TxHistory {thCommitTx = commitTx, thRegisterTx = registerTx},
          nrOrderHash,
          nrCreatedAt,
          nrUpdatedAt,
          nrReadAt,
          nrSettledAt,
          nrSelectedAt,
          nrLastAttempt
        }

getRegistration :: DB.Connection -> RegistrationId -> IO (Maybe NameRegistration)
getRegistration db (RegistrationId rid) =
  firstDecoded toRegistration $ DB.query db (selectRegistrations <> " WHERE r.registration_id = ?") (Only rid)

-- | The newest stored order, live or not, with exactly these fields.
getRegistrationByOrderHash :: DB.Connection -> OrderHash -> IO (Maybe NameRegistration)
getRegistrationByOrderHash db hash =
  firstDecoded toRegistration $
    DB.query db (selectRegistrations <> " WHERE r.order_hash = ? ORDER BY r.created_at DESC, r.name_registration_id DESC LIMIT 1") (Only hash)

-- | Overdue whatever status the poller has marked, as long as the invoice is not paid and holds no money, as the sweep decides.
getLiveOrders :: DB.Connection -> Commitment -> Address -> LabelHash -> UTCTime -> IO [LiveOrder]
getLiveOrders db commitment owner lh now =
  DB.query
    db
    ( registrationColumns
        <> ", CASE WHEN"
        <> overdueOrder "r"
        <> " THEN 1 ELSE 0 END"
        <> registrationFrom
        <> mkQuery " LEFT JOIN @invoices i ON i.invoice_id = ni.invoice_id"
        <> " WHERE r.live = 1 AND (r.commitment = ? OR (r.owner = ? AND r.label_hash = ?))"
    )
    (PUnpaid, ISPaid, now, commitment, owner, lh)
    >>= decodeAll toLiveOrder
  where
    toLiveOrder (row :. Only (BI loOverdue)) = (\loRegistration -> LiveOrder {loRegistration, loOverdue}) <$> toRegistration row

-- | The sweep's own check, on the invoice joined as i.
unfunded :: Query
unfunded = unfundedQuery "i.invoice_id"

-- | Run in the transaction that inserts the next order, so the unique constraints let it in; the registration's new version.
-- Nothing when the order changed since it was read, or its invoice is no longer open, not past its expiry, or holds money: the caller rolls back,
-- since the invoice is written before the registration, as settlement, cancel and the sweep write them.
closeOverdue :: DB.Connection -> NameRegistration -> UTCTime -> IO (Maybe Int64)
closeOverdue db r@NameRegistration {nrId = RegistrationId rid, nrVersion, nrPayment} now = do
  -- locked first, so the funding check below, a later statement, sees a payment a concurrent settlement committed
  mapM_ (\invId -> lockInvoice db invId now) $ paymentInvoice nrPayment
  expired <-
    executeChanging
      db
      ( prefixQuery
          [sql|
          UPDATE @invoices SET status = ?, updated_at = ?
          WHERE status = ? AND invoice_id IN (
            SELECT i.invoice_id FROM @name_invoices ni
            JOIN @name_registrations r ON r.registration_id = ni.registration_id
            JOIN @invoices i ON i.invoice_id = ni.invoice_id
            WHERE ni.registration_id = ? AND r.version = ? AND r.live = 1 AND i.expires_at < ?
        |]
          <> unfunded
          <> " )"
      )
      (ISExpired, now, ISOpen, rid, nrVersion, now)
  if expired > 0
    then
      updateRegistration db r {nrPhase = OClosed ICExpired, nrReveal = Nothing} now >>= \case
        Written v -> pure $ Just v
        _ -> pure Nothing
    else pure Nothing

-- | Whether an order can still progress to a registration for its name. The unique live indexes allow one such order per commitment,
-- and one per owner and label hash, so a wallet can order a name again once its registration is done.
-- An unpaid order is live while its invoice is open; a taken order, its worth kept as a balance, is live again only by a selection of a name.
orderLive :: NameRegistration -> Bool
orderLive NameRegistration {nrPhase} = case nrPhase of
  OUnpaid _ -> True
  OClosed _ -> False
  OQueued -> True
  OCommitting _ -> True
  OCommitted _ -> True
  ORegistering {} -> True
  OFailed _ FRMonthEnded -> False
  OFailed {} -> True
  OTaken -> False
  ORegistered {} -> False

-- | Writes every changeable field of a registration read at its nrVersion, unless it changed since, with live as orderLive decides.
-- A closed order is never made live again, and a write that would make the order meet another live one changes nothing.
updateRegistration :: DB.Connection -> NameRegistration -> UTCTime -> IO VersionWrite
updateRegistration db r now = do
  changed <-
    executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_registrations
        SET version = version + 1, label_hash = ?, length = ?, duration = ?, commitment = ?, sealed = ?, agreed_terms = ?, phase = ?, price = ?,
          label = ?, secret = ?, contact_link = ?, channel_link = ?, intent_nonce = ?, intent_deadline = ?, intent_signature = ?,
          accept_terms = ?, accept_nonce = ?, accept_signature = ?, commit_tx = ?, commit_block = ?, register_tx = ?, reveal_after = ?, reveal_before = ?, expires_at = ?,
          registered_at = ?, failed_at = ?, fail_reason = ?, closed_by = ?, live = ?, updated_at = ?
        WHERE registration_id = ? AND version = ?
          AND (? = 0 OR ((live = 1 OR phase = ?) AND NOT EXISTS (
            SELECT 1 FROM @name_registrations o
            WHERE o.live = 1 AND o.registration_id <> ? AND (o.commitment = ? OR (o.owner = ? AND o.label_hash = ?))
          )))
      |]
      )
      ( (nrLabelHash, nameLengthInt nrLength, int64 nrDuration, nrCommitment, nrSealed, nrAgreedTerms, namePhase nrPhase, nrPrice)
          :. revealRow nrReveal
          :. (commitTx, int64 <$> commitBlock, registerTx, roundedSeconds <$> revealAfter, roundedSeconds <$> revealBefore, roundedSeconds <$> expiresAt)
          :. (registeredAt, failedAt, failReason, closedBy, BI live, now)
          :. (rid, nrVersion, BI live, PTaken, rid, nrCommitment, nrOwner, nrLabelHash)
      )
  if changed > 0 then pure $ Written (nrVersion + 1) else refusal
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrLabelHash, nrLength, nrDuration, nrOwner, nrCommitment, nrSealed, nrAgreedTerms, nrPhase, nrPrice, nrReveal} = r
    NameRegistration {nrHistory = TxHistory {thCommitTx, thRegisterTx}} = r
    (commitBlock, revealAfter, revealBefore) = case phaseCommit nrPhase of
      Just Commit {cmTx, cmRevealAfter, cmRevealBefore} -> (txBlock <$> cmTx, Just cmRevealAfter, Just cmRevealBefore)
      Nothing -> (Nothing, Nothing, Nothing)
    commitTx = case nrPhase of
      OCommitting tx -> Just tx
      _ -> maybe thCommitTx (fmap txHash . cmTx) (phaseCommit nrPhase)
    registerTx = case nrPhase of
      ORegistering _ tx -> Just tx
      _ -> thRegisterTx
    (expiresAt, registeredAt, failedAt, failReason, closedBy) = case nrPhase of
      ORegistered expiry at -> (Just expiry, Just at, Nothing, Nothing, Nothing)
      OFailed at reason -> (Nothing, Nothing, Just at, Just reason, Nothing)
      OClosed closed -> (Nothing, Nothing, Nothing, Nothing, Just closed)
      _ -> (Nothing, Nothing, Nothing, Nothing, Nothing)
    live = orderLive r
    refusal =
      DB.query db (mkQuery "SELECT version, live, phase FROM @name_registrations WHERE registration_id = ?") (Only rid) >>= \case
        [(version, BI storedLive, storedPhase)] | version == nrVersion -> pure $ if storedLive || storedPhase == PTaken then LiveOrderExists else NotLive
        _ -> pure VersionChanged

type RevealRow = (Maybe Text, Maybe Hex32, Maybe Text, Maybe Text, Maybe Int64, Maybe Int64, Maybe RecoverableSignature) :. (Maybe TermsHash, Maybe Int64, Maybe RecoverableSignature)

revealRow :: Maybe NameReveal -> RevealRow
revealRow rv =
  (rvLabel <$> rv, rvSecret <$> rv, rvContactLink =<< rv, rvChannelLink =<< rv, int64 . rvNonce <$> rv, roundedSeconds . rvDeadline <$> rv, rvSignature <$> rv)
    :. (termsHash <$> acceptance, int64 . nonce <$> acceptance, signature <$> acceptance)
  where
    acceptance = rvAcceptance <$> rv

-- | A name selected for the order, with the terms agreed for it and its reveal: queued with the transactions, reveal times and failed attempts cleared,
-- its month and 3-hour alert counted from the selection. The order hash stays the original order's, so a resend of that order still finds it.
selectRegistration :: DB.Connection -> NameRegistration -> OrderFields -> NameReveal -> UTCTime -> IO VersionWrite
selectRegistration db r@NameRegistration {nrId = RegistrationId rid} OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} reveal now =
  updateRegistration
    db
    r
      { nrLabelHash = ofLabelHash,
        nrLength = ofLength,
        nrDuration = ofDuration,
        nrCommitment = ofCommitment,
        nrSealed = ofSealed,
        nrAgreedTerms = ofAgreedTerms,
        nrPhase = OQueued,
        nrReveal = Just reveal,
        nrHistory = noHistory
      }
    now
    >>= \case
      w@(Written _) -> w <$ DB.execute db (mkQuery "UPDATE @name_registrations SET failed_dry_runs = 0, failed_sends = 0, last_attempt_tx = NULL, last_attempt_stage = NULL, last_attempt_reason = NULL, last_attempt_at = NULL, selected_at = ?, unregistered_alerted_at = NULL WHERE registration_id = ?") (now, rid)
      w -> pure w

-- | Newest first: findLimit of them, then every older one holding a balance above 0, since a balance never expires.
findRegistrations :: DB.Connection -> Address -> IO [NameRegistration]
findRegistrations db owner = do
  newest <- DB.query db (selectRegistrations <> " WHERE r.owner = ?" <> newestFirst <> " LIMIT ?") (owner, findLimit) >>= decodeAll toRegistration
  holding <- DB.query db (selectRegistrations <> " WHERE r.owner = ?" <> withBalance <> newestFirst) (Only owner) >>= decodeAll toRegistration
  pure $ newest <> filter ((`notElem` map nrId newest) . nrId) holding
  where
    newestFirst = " ORDER BY r.created_at DESC, r.name_registration_id DESC"
    withBalance = mkQuery " AND EXISTS (SELECT 1 FROM @name_balances b WHERE b.registration_id = r.registration_id AND b.amount > 0)"

labelHolder :: DB.Connection -> LabelHash -> IO (Maybe RegistrationId)
labelHolder db lh =
  maybeFirstRow (RegistrationId . fromOnly) $
    DB.query db (mkQuery "SELECT registration_id FROM @name_registrations WHERE label_hash = ? AND live = 1 AND phase <> ? LIMIT 1") (lh, PUnpaid)

setSettledAt :: DB.Connection -> RegistrationId -> UTCTime -> IO ()
setSettledAt db (RegistrationId rid) settledAt =
  DB.execute db (mkQuery "UPDATE @name_registrations SET settled_at = ?, selected_at = ? WHERE registration_id = ? AND settled_at IS NULL") (settledAt, settledAt, rid)

unregisteredPaidBefore :: DB.Connection -> UTCTime -> IO [(RegistrationId, NamePhase, UTCTime)]
unregisteredPaidBefore db cutoff =
  map (\(rid, phase, selectedAt) -> (RegistrationId rid, phase, selectedAt))
    <$> DB.query
      db
      (mkQuery "SELECT registration_id, phase, selected_at FROM @name_registrations WHERE phase IN (?,?,?,?,?) AND selected_at < ? AND unregistered_alerted_at IS NULL ORDER BY selected_at, registration_id")
      (PQueued, PCommitting, PCommitted, PRegistering, PFailed, cutoff)

markRead :: DB.Connection -> RegistrationId -> UTCTime -> UTCTime -> IO ()
markRead db (RegistrationId rid) before now =
  DB.execute db (mkQuery "UPDATE @name_registrations SET read_at = ? WHERE registration_id = ? AND read_at <= ?") (now, rid, before)

monthCandidates :: DB.Connection -> UTCTime -> IO [(RegistrationId, UTCTime)]
monthCandidates db cutoff =
  map (\(rid, selectedAt) -> (RegistrationId rid, selectedAt))
    <$> DB.query
      db
      (mkQuery "SELECT registration_id, selected_at FROM @name_registrations WHERE live = 1 AND phase IN (?,?,?) AND selected_at <= ? ORDER BY selected_at, registration_id")
      (PQueued, PCommitted, PFailed, cutoff)

claimUnregisteredAlert :: DB.Connection -> RegistrationId -> UTCTime -> IO Bool
claimUnregisteredAlert db (RegistrationId rid) now =
  (> 0) <$> executeChanging db (mkQuery "UPDATE @name_registrations SET unregistered_alerted_at = ? WHERE registration_id = ? AND unregistered_alerted_at IS NULL") (now, rid)

recordAttempt :: DB.Connection -> RegistrationId -> Bool -> NameAttempt -> IO Int
recordAttempt db (RegistrationId rid) counted NameAttempt {naTx, naStage, naReason, naAt} =
  maybeFirstRow' 0 fromOnly $
    DB.query
      db
      (mkQuery $ "UPDATE @name_registrations SET " <> failures <> " = " <> failures <> " + ?, last_attempt_tx = ?, last_attempt_stage = ?, last_attempt_reason = ?, last_attempt_at = ? WHERE registration_id = ? RETURNING " <> failures)
      (if counted then 1 else 0 :: Int, naTx, naStage, naReason, naAt, rid)
  where
    failures = case naStage of
      ASDryRun -> "failed_dry_runs"
      ASSent -> "failed_sends"

openInvoicesOfHeldName :: DB.Connection -> RegistrationId -> IO [(InvoiceId, PaymentProvider, Text)]
openInvoicesOfHeldName db (RegistrationId rid) =
  map (\(invId, provider, ref) -> (InvoiceId invId, provider, ref))
    <$> DB.query
      db
      ( prefixQuery
          [sql|
        SELECT i.invoice_id, i.provider, i.provider_ref
        FROM @name_registrations r
        JOIN @name_registrations o ON o.label_hash = r.label_hash AND o.registration_id <> r.registration_id
        JOIN @name_invoices ni ON ni.registration_id = o.registration_id
        JOIN @invoices i ON i.invoice_id = ni.invoice_id
        WHERE r.registration_id = ? AND r.phase IN (?,?,?,?) AND o.phase = ? AND i.status = ?
        ORDER BY i.created_at, i.invoice_id
      |]
      )
      (rid, PQueued, PCommitting, PCommitted, PRegistering, PUnpaid, ISOpen)

-- | The registrations a worker moves on, oldest first, by id alone, so a row that does not decode stops only its own worker.
activeRegistrations :: DB.Connection -> IO [RegistrationId]
activeRegistrations db =
  map (RegistrationId . fromOnly)
    <$> DB.query
      db
      (mkQuery "SELECT registration_id FROM @name_registrations WHERE phase IN (?,?,?,?) ORDER BY created_at, registration_id")
      (PQueued, PCommitting, PCommitted, PRegistering)

-- | Unpaid orders whose invoice expired before the cutoff, without a recorded payment.
unpaidCleanupCandidates :: DB.Connection -> UTCTime -> IO [(RegistrationId, InvoiceId, Text)]
unpaidCleanupCandidates db expiredBefore =
  map (\(rid, invId, ref) -> (RegistrationId rid, InvoiceId invId, ref))
    <$> DB.query
      db
      ( prefixQuery
          [sql|
        SELECT r.registration_id, ni.invoice_id, i.provider_ref
        FROM @name_registrations r
        JOIN @name_invoices ni ON ni.registration_id = r.registration_id
        JOIN @invoices i ON i.invoice_id = ni.invoice_id
        WHERE r.phase IN (?, ?) AND i.expires_at < ?
          AND NOT EXISTS (SELECT 1 FROM @payments p WHERE p.invoice_id = ni.invoice_id)
        ORDER BY i.expires_at, r.registration_id
      |]
      )
      (PUnpaid, PClosed, expiredBefore)

-- | Deletes an unpaid registration and its invoice, unless a payment was recorded for it.
deleteUnpaidRegistration :: DB.Connection -> RegistrationId -> IO (Either CleanupKept ())
deleteUnpaidRegistration db (RegistrationId rid) =
  DB.query db (mkQuery "SELECT ni.invoice_id FROM @name_registrations r LEFT JOIN @name_invoices ni ON ni.registration_id = r.registration_id WHERE r.registration_id = ?") (Only rid) >>= \case
    [] -> pure $ Left CKGone
    invoiceIds -> do
      deleted <-
        executeChanging
          db
          ( prefixQuery
              [sql|
            DELETE FROM @name_registrations
            WHERE registration_id = ? AND phase IN (?, ?)
              AND NOT EXISTS (
                SELECT 1 FROM @name_invoices ni
                JOIN @payments p ON p.invoice_id = ni.invoice_id
                WHERE ni.registration_id = ?
              )
          |]
          )
          (rid, PUnpaid, PClosed, rid)
      if deleted > 0
        then Right () <$ mapM_ (DB.execute db (mkQuery "DELETE FROM @invoices WHERE invoice_id = ?") . Only) [invId | Only (Just invId) <- invoiceIds :: [Only (Maybe Text)]]
        else pure $ Left CKPaid

-- | The order of the alias with invoice i is overdue: unpaid, its invoice past its expiry, not paid, and holding no money.
-- Takes the unpaid phase, the paid status and the time.
overdueOrder :: Query -> Query
overdueOrder alias = " " <> alias <> ".phase = ? AND i.status <> ? AND i.expires_at < ?" <> unfunded

getBalance :: DB.Connection -> RegistrationId -> IO (Maybe Balance)
getBalance db (RegistrationId rid) =
  maybeFirstRow toBalance $
    DB.query db (mkQuery "SELECT registration_id, amount, credited, refunded, refunded_at FROM @name_balances WHERE registration_id = ?") (Only rid)
  where
    toBalance (registration, blAmount, blCredited, blRefunded, blRefundedAt) = Balance {blRegistration = RegistrationId registration, blAmount, blCredited, blRefunded, blRefundedAt}

-- | The order a support reference names, by its registration id or its name invoice's id, with its balance.
balanceByReference :: DB.Connection -> Text -> IO (Maybe (RegistrationId, Maybe Balance))
balanceByReference db ref =
  DB.query
    db
    (mkQuery "SELECT registration_id FROM @name_registrations WHERE registration_id = ? UNION ALL SELECT registration_id FROM @name_invoices WHERE invoice_id = ?")
    (ref, ref)
    >>= \case
      Only rid : _ -> (\balance -> Just (RegistrationId rid, balance)) <$> getBalance db (RegistrationId rid)
      [] -> pure Nothing

balanceLink :: DB.Connection -> RegistrationId -> IO (Maybe BalanceLink)
balanceLink db rid = getRegistration db rid >>= mapM paidBy
  where
    paidBy NameRegistration {nrPhase, nrOwner, nrPayment} = do
      let pastUnpaid = namePhase nrPhase `notElem` [PUnpaid, PClosed]
      (lkPaid, lkProviderRef, lkLateCredits) <- case paymentInvoice nrPayment of
        Just invId -> (,,) <$> (if pastUnpaid then invoiceAmount db invId else pure 0) <*> providerRef db invId <*> lateCredits db invId
        Nothing -> pure (0, Nothing, [])
      pure BalanceLink {lkRegistration = rid, lkOwner = nrOwner, lkPayment = nrPayment, lkProviderRef, lkPaid, lkLateCredits}

invoiceAmount :: DB.Connection -> InvoiceId -> IO USDCents
invoiceAmount db (InvoiceId invId) =
  maybeFirstRow' 0 fromOnly $ DB.query db (mkQuery "SELECT amount FROM @invoices WHERE invoice_id = ?") (Only invId)

providerRef :: DB.Connection -> InvoiceId -> IO (Maybe Text)
providerRef db (InvoiceId invId) =
  maybeFirstRow fromOnly $ DB.query db (mkQuery "SELECT provider_ref FROM @invoices WHERE invoice_id = ?") (Only invId)

lateCredits :: DB.Connection -> InvoiceId -> IO [LateCredit]
lateCredits db (InvoiceId invId) =
  DB.query
    db
    (mkQuery "SELECT invoice_id, credit_no, registration_id, units, invoice_rate, current_rate, rate_used, cents FROM @name_late_credits WHERE invoice_id = ? ORDER BY credit_no")
    (Only invId)
    >>= decodeAll toLateCredit
  where
    toLateCredit (inv, lcCreditNo, reg, units_, invoiceRate_, currentRate_, rateUsed_, cents) =
      (\lcAmount -> LateCredit {lcInvoice = InvoiceId inv, lcCreditNo, lcRegistration = RegistrationId reg, lcAmount}) <$> case (units_, invoiceRate_, currentRate_, rateUsed_) of
        (Nothing, Nothing, Nothing, Nothing) -> Right $ CACard cents
        (Just caUnits, Just caInvoiceRate, Just caCurrentRate, Just caRateUsed) -> Right CACrypto {caUnits, caInvoiceRate, caCurrentRate, caRateUsed, caCents = cents}
        _ -> Left $ "name_late_credits figures: " <> inv

-- | Adds to the order's balance, making it on its first credit, unless a payment of the order was reversed.
creditBalance :: DB.Connection -> RegistrationId -> USDCents -> UTCTime -> IO ()
creditBalance db (RegistrationId rid) cents now =
  DB.execute
    db
    ( prefixQuery
        [sql|
      INSERT INTO @name_balances (registration_id, amount, credited, refunded, created_at, updated_at)
      SELECT ?, ?, ?, 0, ?, ?
      WHERE NOT EXISTS (
        SELECT 1 FROM @name_invoices ni
        WHERE ni.registration_id = ? AND ni.reversal_count > 0
      )
      ON CONFLICT (registration_id) DO UPDATE SET
        amount = @name_balances.amount + excluded.amount,
        credited = @name_balances.credited + excluded.credited,
        updated_at = excluded.updated_at
    |]
    )
    (rid, cents, cents, now, now, rid)

-- | The price, above 0, where the amount holds it.
spendBalance :: DB.Connection -> RegistrationId -> USDCents -> UTCTime -> IO Bool
spendBalance db (RegistrationId rid) price now =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_balances SET amount = amount - ?, updated_at = ? WHERE registration_id = ? AND amount >= ? AND ? > 0")
      (price, now, rid, price, price)

-- | All of the balance, where it is the cents given and above 0; refunded counts every refund.
refundBalance :: DB.Connection -> RegistrationId -> USDCents -> UTCTime -> IO Bool
refundBalance db (RegistrationId rid) cents now =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_balances SET amount = 0, refunded = refunded + ?, refunded_at = ?, updated_at = ?
        WHERE registration_id = ? AND amount = ? AND amount > 0
      |]
      )
      (cents, now, now, rid, cents)

recordReversal :: DB.Connection -> Text -> InvoiceId -> Reversal -> IO (Maybe USDCents)
recordReversal db event (InvoiceId invId) reversal =
  DB.query db (mkQuery "SELECT last_reversal_event, reversed_cents FROM @name_invoices WHERE invoice_id = ?") (Only invId) >>= \case
    [(lastEvent, reversedBefore)] | lastEvent /= Just event -> do
      let cents = case reversal of
            RVDispute _ (CurrencyAmount amount) -> fromIntegral amount
            RVRefund _ (CurrencyAmount total) -> max 0 $ fromIntegral total - reversedBefore
      DB.execute
        db
        (mkQuery "UPDATE @name_invoices SET reversal_count = reversal_count + 1, last_reversal_event = ?, last_reversal_cents = ?, reversed_cents = reversed_cents + ? WHERE invoice_id = ?")
        (event, cents, cents, invId)
      pure $ Just cents
    _ -> pure Nothing

takeBackBalance :: DB.Connection -> InvoiceId -> RegistrationId -> USDCents -> UTCTime -> IO USDCents
takeBackBalance db invId@(InvoiceId inv) (RegistrationId rid) cents now = do
  invoiced <- invoiceAmount db invId
  reversed <- maybeFirstRow' 0 fromOnly $ DB.query db (mkQuery "SELECT reversed_cents FROM @name_invoices WHERE invoice_id = ?") (Only inv)
  DB.execute db (mkQuery "UPDATE @name_balances SET amount = amount WHERE registration_id = ?") (Only rid)
  amount <- maybeFirstRow' 0 fromOnly $ DB.query db (mkQuery "SELECT amount FROM @name_balances WHERE registration_id = ?") (Only rid)
  let taken = max 0 $ minimum [cents, invoiced - (reversed - cents), amount]
  when (taken > 0) $ DB.execute db (mkQuery "UPDATE @name_balances SET amount = amount - ?, updated_at = ? WHERE registration_id = ?") (taken, now, rid)
  pure taken

lockRegistration :: DB.Connection -> NameRegistration -> IO Bool
lockRegistration db NameRegistration {nrId = RegistrationId rid, nrVersion} =
  (> 0) <$> executeChanging db (mkQuery "UPDATE @name_registrations SET version = version WHERE registration_id = ? AND version = ?") (rid, nrVersion)

reversalRecorded :: DB.Connection -> RegistrationId -> IO Bool
reversalRecorded db (RegistrationId rid) =
  not . null <$> (DB.query db (mkQuery "SELECT 1 FROM @name_invoices WHERE registration_id = ? AND reversal_count > 0 LIMIT 1") (Only rid) :: IO [Only Int])

registerTxSent :: DB.Connection -> RegistrationId -> IO Bool
registerTxSent db (RegistrationId rid) =
  not . null <$> (DB.query db (mkQuery "SELECT 1 FROM @name_registrations r WHERE r.registration_id = ? AND (r.register_tx IS NOT NULL OR EXISTS (SELECT 1 FROM @name_transactions t WHERE t.registration_id = r.registration_id AND t.kind = ? AND t.status IN (?,?)))") (rid, TKRegistration RTRegister, TSPending, TSMined) :: IO [Only Int])

registerTxPending :: DB.Connection -> RegistrationId -> IO Bool
registerTxPending db (RegistrationId rid) =
  not . null <$> (DB.query db (mkQuery "SELECT 1 FROM @name_transactions WHERE registration_id = ? AND kind = ? AND status = ? LIMIT 1") (rid, TKRegistration RTRegister, TSPending) :: IO [Only Int])

-- | A name invoice with its order, its invoice row and its payment row.
lateFrom :: Query
lateFrom =
  mkQuery $
    " FROM @name_invoices ni"
      <> " JOIN @name_registrations r ON r.registration_id = ni.registration_id"
      <> " JOIN @invoices i ON i.invoice_id = ni.invoice_id"
      <> " JOIN @payments p ON p.invoice_id = ni.invoice_id"

-- | Its order closed unpaid, whatever the invoice's status, since settlement queues only a live order; takes the closed phase.
closedUnpaid :: Query
closedUnpaid = " r.phase = ? AND r.closed_by <> " <> textLiteral ICReversed

-- | Its order paid by it, with crypto received above what it asked; takes the unpaid and closed phases. The last term matches the partial index of
-- overpaid payments, so the poller's pass reads only those.
overpaidOrder :: Query
overpaidOrder = " r.phase NOT IN (?, ?) AND" <> cryptoInvoice <> " AND p.received_units > p.invoiced_units"

-- | Confirmed crypto above what the invoice asked, not all credited.
uncreditedExtra :: Query
uncreditedExtra = " p.confirmed_units - p.invoiced_units >" <> creditedTotal "units"

cardInvoice :: Query
cardInvoice = " i.payment_crypto_currency IS NULL"

cryptoInvoice :: Query
cryptoInvoice = " i.payment_crypto_currency IS NOT NULL"

-- | No credit rows sum to 0, not NULL.
creditedTotal :: Query -> Query
creditedTotal column = " (SELECT " <> integerSum ("COALESCE(SUM(c." <> column <> "), 0)") <> mkQuery " FROM @name_late_credits c WHERE c.invoice_id = ni.invoice_id)"

integerSum :: Query -> Query
#if defined(dbPostgres)
-- Postgres sums a BIGINT as NUMERIC, which does not decode into an integer
integerSum s = "CAST(" <> s <> " AS BIGINT)"
#else
integerSum = id
#endif

-- | The first alert's old marks are NULL, which = never matches.
notDistinctFrom :: Query
#if defined(dbPostgres)
notDistinctFrom = " IS NOT DISTINCT FROM "
#else
notDistinctFrom = " IS "
#endif

-- | Confirmed money not all credited: cents for card, units for crypto.
uncreditedMoney :: Query
uncreditedMoney =
  " ((" <> cardInvoice <> " AND p.confirmed_amount >" <> creditedTotal "cents" <> ")"
    <> " OR (" <> cryptoInvoice <> " AND p.confirmed_units >" <> creditedTotal "units" <> "))"

-- | Why a closed order's money is not all credited, by the first case that holds, else NULL.
-- Confirmed money waiting comes first, so its alert never asks for a refund by hand.
pendingCase :: Query
pendingCase =
  " CASE WHEN" <> uncreditedMoney <> " THEN " <> textLiteral LPUncredited
    <> " WHEN COALESCE(p.amount, 0) = 0 AND (p.crypto_paid IS NOT NULL OR p.paid_in_full = 1) THEN " <> textLiteral LPConfirming
    <> " WHEN" <> cardInvoice <> " AND COALESCE(p.amount, 0) > p.confirmed_amount THEN " <> textLiteral LPUnconfirmed
    <> " WHEN p.received_units > COALESCE(p.confirmed_units, 0) THEN " <> textLiteral LPUnconfirmed
    <> " WHEN" <> cryptoInvoice <> " AND p.received_units IS NULL AND COALESCE(p.amount, 0) > 0 THEN " <> textLiteral LPUncounted
    <> " END"

-- | Why an overpaid invoice's extra is not all credited: confirmed extra not credited yet, or received units above the confirmed ones,
-- which overpaidOrder already holds above the invoiced ones.
overpaidPendingCase :: Query
overpaidPendingCase =
  " CASE WHEN" <> uncreditedExtra <> " THEN " <> textLiteral LPUncredited
    <> " WHEN p.received_units > COALESCE(p.confirmed_units, 0) THEN " <> textLiteral LPUnconfirmed
    <> " END"

textLiteral :: TextEncoding a => a -> Query
textLiteral p = fromString $ "'" <> T.unpack (textEncode p) <> "'"

-- | What an alert names as received: units for crypto, and cents for card or for crypto that could not be counted in units.
receivedFigure :: Query
receivedFigure = " CASE WHEN" <> cardInvoice <> " THEN p.amount ELSE COALESCE(p.received_units, p.amount) END"

lateColumns :: Query
lateColumns =
  "SELECT ni.invoice_id, ni.registration_id, CASE WHEN r.phase IN (" <> textLiteral PUnpaid <> ", " <> textLiteral PClosed <> ") THEN 1 ELSE 0 END, CASE WHEN r.phase = " <> textLiteral PClosed
    <> " AND r.closed_by <> " <> textLiteral ICReversed <> " THEN 1 ELSE 0 END, i.payment_crypto_currency, ni.late_support, COALESCE(p.amount, 0), p.confirmed_amount,"
    <> " p.received_units, p.confirmed_units, p.invoice_rate,"
    <> creditedTotal "units"
    <> ","
    <> creditedTotal "cents"
    <> mkQuery ", (SELECT COUNT(*) FROM @name_late_credits c WHERE c.invoice_id = ni.invoice_id), p.created_at, i.amount, p.invoiced_units"

type LateRow = (Text, Text, BoolInt, BoolInt, Maybe CryptoCurrency, Maybe LateSupport, USDCents, USDCents, Maybe Int64, Maybe Int64) :. (Maybe Rate, Int64, USDCents, Int, UTCTime, USDCents, Maybe Int64)

toLateState :: LateRow -> Either Text LateCreditState
toLateState
  ( (inv, reg, BI late, BI lsClosed, coin_, lsSupport, lsReceivedCents, confirmedCents, lfReceivedUnits, lfConfirmedUnits)
      :. (lfInvoiceRate, lfCreditedUnits, lsCreditedCents, lsCredits, lsPaymentCreatedAt, lsInvoiceAmount, lfInvoicedUnits)
    ) = do
    lsFigures <- case coin_ of
      Nothing -> Right LFCard {lfConfirmedCents = confirmedCents}
      Just lfCoin -> Right LFCrypto {lfCoin, lfReceivedUnits, lfConfirmedUnits, lfInvoicedUnits, lfInvoiceRate, lfCreditedUnits}
    pure LateCreditState {lsInvoice = InvoiceId inv, lsRegistration = RegistrationId reg, lsMoney = if late then LMLate else LMOverpaid, lsClosed, lsSupport, lsReceivedCents, lsFigures, lsInvoiceAmount, lsCreditedCents, lsCredits, lsPaymentCreatedAt}

-- | Nothing without a payment row.
lateCreditState :: DB.Connection -> InvoiceId -> IO (Maybe LateCreditState)
lateCreditState db (InvoiceId invId) = firstDecoded toLateState $ DB.query db (lateColumns <> lateFrom <> " WHERE ni.invoice_id = ?") (Only invId)

-- | Run after the invoice's lock. The order's balance is credited only when the credit's row is new, so a taken credit number changes neither.
creditLatePayment :: DB.Connection -> LateCredit -> UTCTime -> IO (Maybe Int64)
creditLatePayment db LateCredit {lcInvoice = InvoiceId invId, lcCreditNo, lcRegistration = regId@(RegistrationId rid), lcAmount} now = do
  version <- maybeFirstRow fromOnly $ DB.query db (mkQuery "UPDATE @name_registrations SET version = version + 1 WHERE registration_id = ? RETURNING version") (Only rid)
  inserted <-
    executeChanging
      db
      ( prefixQuery
          [sql|
        INSERT INTO @name_late_credits (invoice_id, credit_no, registration_id, units, invoice_rate, current_rate, rate_used, cents, created_at)
        VALUES (?,?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      )
      (invId, lcCreditNo, rid, units, invoiceRate, currentRate, rateUsed, creditCents lcAmount, now)
  if inserted > 0 then version <$ creditBalance db regId (creditCents lcAmount) now else pure Nothing
  where
    (units, invoiceRate, currentRate, rateUsed) = case lcAmount of
      CACard _ -> (Nothing, Nothing, Nothing, Nothing)
      CACrypto {caUnits, caInvoiceRate, caCurrentRate, caRateUsed} -> (Just caUnits, Just caInvoiceRate, Just caCurrentRate, Just caRateUsed)

-- | Why the closed order's money on the invoice is not all credited; the one rule of the pending log and the alert.
latePending :: DB.Connection -> InvoiceId -> IO (Maybe LatePending)
latePending db (InvoiceId invId) =
  maybeFirstRow' Nothing fromOnly $
    DB.query db ("SELECT" <> pendingCase <> lateFrom <> " WHERE ni.invoice_id = ? AND" <> closedUnpaid) (invId, PClosed)

-- | Nothing once all of the closed order's money is credited, whatever support reason was given before.
lateWait :: DB.Connection -> InvoiceId -> IO (Maybe LateWait)
lateWait db (InvoiceId invId) =
  maybeFirstRow' Nothing waitOf $
    DB.query db ("SELECT" <> pendingCase <> ", ni.late_support" <> lateFrom <> " WHERE ni.invoice_id = ? AND" <> closedUnpaid) (invId, PClosed)
  where
    waitOf :: (Maybe LatePending, Maybe LateSupport) -> Maybe LateWait
    waitOf = \case
      (Nothing, _) -> Nothing
      (Just _, Just _) -> Just LWSupport
      (Just LPUncounted, Nothing) -> Just LWSupport
      (Just LPUncredited, Nothing) -> Just LWCrediting
      (Just LPConfirming, Nothing) -> Just LWConfirming
      (Just LPUnconfirmed, Nothing) -> Just LWConfirming

-- | Closed orders' invoices with confirmed money not all credited, then overpaid invoices with confirmed extra not all credited, for the
-- poller's credit pass; none a rate below the floor sent to support.
uncreditedLateInvoices :: DB.Connection -> IO [(InvoiceId, Text)]
uncreditedLateInvoices db = (<>) <$> uncredited closedUnpaid (Only PClosed) uncreditedMoney <*> uncredited overpaidOrder (PUnpaid, PClosed) uncreditedExtra
  where
    uncredited :: ToRow p => Query -> p -> Query -> IO [(InvoiceId, Text)]
    uncredited order phases money =
      map (\(invId, ref) -> (InvoiceId invId, ref))
        <$> DB.query
          db
          ("SELECT ni.invoice_id, i.provider_ref" <> lateFrom <> " WHERE" <> order <> " AND" <> money <> " AND (ni.late_support IS NULL OR ni.late_support <> ?) ORDER BY p.created_at, ni.invoice_id")
          (phases :. Only LSRateFloor)

-- | Pending money of closed orders, then of overpaid invoices, whose payment row was created before the cutoff, never alerted, or alerted
-- for less money than now received.
lateAlertDue :: DB.Connection -> UTCTime -> IO [LateAlert]
lateAlertDue db cutoff = (<>) <$> alertsDue closedUnpaid (Only PClosed) pendingCase <*> alertsDue overpaidOrder (PUnpaid, PClosed) overpaidPendingCase
  where
    alertsDue :: ToRow p => Query -> p -> Query -> IO [LateAlert]
    alertsDue order phases pending =
      DB.query
        db
        ( lateColumns
            <> ","
            <> pending
            <> ","
            <> receivedFigure
            <> ", ni.late_alerted_at, ni.late_alerted_units, i.provider_ref"
            <> lateFrom
            <> " WHERE"
            <> order
            <> " AND p.created_at < ? AND"
            <> pending
            <> " IS NOT NULL AND (ni.late_alerted_at IS NULL OR"
            <> receivedFigure
            <> " > COALESCE(ni.late_alerted_units, -1)) ORDER BY p.created_at, ni.invoice_id"
        )
        (phases :. Only cutoff)
        >>= decodeAll toAlert
    toAlert :: LateRow :. (Maybe LatePending, Maybe Int64, Maybe UTCTime, Maybe Int64, Text) -> Either Text LateAlert
    toAlert (row :. (pending_, laReceived, laAlertedAt, laAlertedReceived, laProviderRef)) = case pending_ of
      Just laPending -> (\laState -> LateAlert {laState, laProviderRef, laPending, laReceived, laAlertedAt, laAlertedReceived}) <$> toLateState row
      Nothing -> Left "name invoice alert without pending money"

-- | Marks the alert sent, with the support reason it gives unless one is set, unless another instance marked it since it was read;
-- only the instance that marks it logs it.
claimLateAlert :: DB.Connection -> LateAlert -> Maybe LateSupport -> UTCTime -> IO Bool
claimLateAlert db LateAlert {laState = LateCreditState {lsInvoice = InvoiceId invId}, laReceived, laAlertedAt, laAlertedReceived} support now =
  (> 0)
    <$> executeChanging
      db
      ( mkQuery "UPDATE @name_invoices SET late_alerted_at = ?, late_alerted_units = ?, late_support = COALESCE(late_support, ?)"
          <> " WHERE invoice_id = ? AND late_alerted_at"
          <> notDistinctFrom
          <> "? AND late_alerted_units"
          <> notDistinctFrom
          <> "?"
      )
      (now, laReceived, support, invId, laAlertedAt, laAlertedReceived)

-- | The first reason given stays.
setLateSupport :: DB.Connection -> InvoiceId -> LateSupport -> IO ()
setLateSupport db (InvoiceId invId) support =
  DB.execute db (mkQuery "UPDATE @name_invoices SET late_support = COALESCE(late_support, ?) WHERE invoice_id = ?") (support, invId)

insertPrepaidCode :: DB.Connection -> NewPrepaidCode -> UTCTime -> IO (Either CodeHashTaken CodeId)
insertPrepaidCode db NewPrepaidCode {npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, npcExpiresAt, npcIssuer} now = do
  inserted <-
    executeChanging
      db
      ( prefixQuery
          [sql|
        INSERT INTO @name_prepaid_codes (code_hash, min_length, years, code_payment_status, expires_at, created_at, issued_in_group)
        VALUES (?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      )
      (npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, npcExpiresAt, now, BI (npcIssuer == FromGroup))
  if inserted > 0 then Right <$> insertedRowId db else pure $ Left CodeHashTaken

prepaidCodeColumns :: Query
prepaidCodeColumns =
  prefixQuery
    [sql|
    SELECT name_prepaid_code_id, code_hash, min_length, years, code_payment_status, reserved_by, reserved_at, spent_at, revoked_at, expires_at, created_at
    FROM @name_prepaid_codes
  |]

type PrepaidCodeRow = (Int64, CodeHash, Int, Int, BadgeCodePaymentStatus, Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime) :. Only UTCTime

toPrepaidCode :: PrepaidCodeRow -> PrepaidCode
toPrepaidCode ((pcId, pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt) :. Only pcCreatedAt) =
  PrepaidCode {pcId, pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcReservedBy = RegistrationId <$> reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt, pcCreatedAt}

groupNameCodesIssuedSince :: DB.Connection -> UTCTime -> IO Int
groupNameCodesIssuedSince db since = maybeFirstRow' 0 fromOnly (DB.query db (mkQuery "SELECT COUNT(*) FROM @name_prepaid_codes WHERE created_at >= ? AND issued_in_group = 1") (Only since))

getPrepaidCode :: DB.Connection -> CodeHash -> IO (Maybe PrepaidCode)
getPrepaidCode db codeHash =
  maybeFirstRow toPrepaidCode $ DB.query db (prepaidCodeColumns <> " WHERE code_hash = ?") (Only codeHash)

getPrepaidCodeById :: DB.Connection -> CodeId -> IO PrepaidCode
getPrepaidCodeById db codeId =
  maybeFirstRow toPrepaidCode (DB.query db (prepaidCodeColumns <> " WHERE name_prepaid_code_id = ?") (Only codeId))
    >>= maybe (E.throwIO $ BrokenInvariant $ "the prepaid code " <> tshow codeId <> " of an order is missing") pure

-- | Only an unused code: not reserved, spent or revoked, paid or free, and not expired.
reservePrepaidCode :: DB.Connection -> CodeId -> RegistrationId -> UTCTime -> IO Bool
reservePrepaidCode db codeId (RegistrationId rid) now =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_prepaid_codes SET reserved_by = ?, reserved_at = ?
        WHERE name_prepaid_code_id = ? AND reserved_by IS NULL AND spent_at IS NULL AND revoked_at IS NULL
          AND code_payment_status <> ? AND (expires_at IS NULL OR expires_at > ?)
      |]
      )
      (rid, now, codeId, CPSUnpaid, now)

releasePrepaidCode :: DB.Connection -> CodeId -> RegistrationId -> IO Bool
releasePrepaidCode db codeId (RegistrationId rid) =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_prepaid_codes SET reserved_by = NULL, reserved_at = NULL
        WHERE name_prepaid_code_id = ? AND reserved_by = ? AND spent_at IS NULL
      |]
      )
      (codeId, rid)

-- | For a registration whose register went through: spends the code it holds, or one released and still usable; False otherwise.
spendLandedPrepaidCode :: DB.Connection -> CodeId -> RegistrationId -> UTCTime -> IO Bool
spendLandedPrepaidCode db codeId (RegistrationId rid) now =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_prepaid_codes SET reserved_by = ?, reserved_at = COALESCE(reserved_at, ?), spent_at = ?
        WHERE name_prepaid_code_id = ? AND spent_at IS NULL
          AND (reserved_by = ? OR (reserved_by IS NULL AND revoked_at IS NULL AND code_payment_status <> ? AND (expires_at IS NULL OR expires_at > ?)))
      |]
      )
      (rid, now, now, codeId, rid, CPSUnpaid, now)

-- | A reserved code can be revoked, and its order keeps it; a spent one cannot. With the code as it reads after.
revokePrepaidCode :: DB.Connection -> CodeHash -> UTCTime -> IO (RevokeResult, Maybe PrepaidCode)
revokePrepaidCode db codeHash now = do
  revoked <-
    executeChanging
      db
      (mkQuery "UPDATE @name_prepaid_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND spent_at IS NULL")
      (now, codeHash)
  (\code -> (maybe NoSuchCode (result revoked) code, code)) <$> getPrepaidCode db codeHash
  where
    result revoked PrepaidCode {pcId, pcRevokedAt}
      | revoked > 0 = Revoked pcId
      | isJust pcRevokedAt = AlreadyRevoked pcId
      | otherwise = AlreadyRedeemed

data TxKind = TKRegistration RegistrationTx | TKEdit
  deriving (Eq, Show)

instance TextEncoding TxKind where
  textEncode = \case
    TKRegistration step -> textEncode step
    TKEdit -> "edit"
  textDecode = \case
    "edit" -> Just TKEdit
    t -> TKRegistration <$> textDecode t

instance FromField TxKind where fromField = fromTextField_ textDecode

instance ToField TxKind where toField = toField . textEncode

txKind :: TxFor -> (TxKind, Maybe Text, Maybe RecordEditId)
txKind = \case
  TxRegistration step (RegistrationId rid) -> (TKRegistration step, Just rid, Nothing)
  TxEdit editId -> (TKEdit, Nothing, Just editId)

-- | Saved once signed and before it is sent, so a restart can rebroadcast it.
insertTransaction :: DB.Connection -> TxFor -> NewTransaction -> UTCTime -> IO Int64
insertTransaction db for NewTransaction {ntxRaw, ntxHash, ntxNonce} now = do
  let (kind, rid, editId) = txKind for
  DB.execute
    db
    ( prefixQuery
        [sql|
      INSERT INTO @name_transactions
        (registration_id, name_record_edit_id, kind, raw, tx_hash, registrar_nonce, sent_at, status)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    )
    (rid, editId, kind, Binary ntxRaw, ntxHash, int64 ntxNonce, now, TSPending)
  insertedRowId db

-- | Moves a transaction on from the status the caller read, so an outcome is never overwritten unseen; False when it is not in that status.
recordTransactionOutcome :: DB.Connection -> TxHash -> TxStatus -> TxSettlement -> IO Bool
recordTransactionOutcome db hash from settlement =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_transactions SET status = ?, mined_block = ? WHERE tx_hash = ? AND status = ?")
      (status, int64 <$> block, hash, from)
  where
    (status, block) = case settlement of
      TSMinedAt b -> (TSMined, Just b)
      TSRevertedAt b -> (TSReverted, Just b)
      TSDroppedTx -> (TSDropped, Nothing)

transactionColumns :: Query
transactionColumns =
  prefixQuery
    [sql|
    SELECT name_transaction_id, kind, registration_id, name_record_edit_id, raw, tx_hash, registrar_nonce, sent_at, mined_block, status
    FROM @name_transactions
  |]

type TransactionRow = (Int64, TxKind, Maybe Text, Maybe Int64, Binary ByteString, TxHash, Int64, UTCTime, Maybe Int64, TxStatus)

toTransaction :: TransactionRow -> Either Text NameTransaction
toTransaction (ntId, kind, rid_, editId_, Binary ntRaw, ntHash, nonce, ntSentAt, block, status) = do
  ntFor <- case (kind, rid_, editId_) of
    (TKRegistration step, Just rid, Nothing) -> Right $ TxRegistration step $ RegistrationId rid
    (TKEdit, Nothing, Just editId) -> Right $ TxEdit editId
    _ -> Left $ "name_transactions.kind: " <> textEncode kind
  ntState <- case (status, word64 <$> block) of
    (TSPending, Nothing) -> Right TxPending
    (TSMined, Just b) -> Right $ TxSettled $ TSMinedAt b
    (TSReverted, Just b) -> Right $ TxSettled $ TSRevertedAt b
    (TSDropped, Nothing) -> Right $ TxSettled TSDroppedTx
    _ -> Left $ "name_transactions status and mined_block: " <> tshow ntId
  pure NameTransaction {ntId, ntFor, ntRaw, ntHash, ntNonce = word64 nonce, ntSentAt, ntState}

-- | Those without a receipt, in nonce order, for the sender to resume.
pendingTransactions :: DB.Connection -> IO [NameTransaction]
pendingTransactions db =
  DB.query db (transactionColumns <> " WHERE status = ? ORDER BY registrar_nonce, name_transaction_id") (Only TSPending)
    >>= decodeAll toTransaction

registrationTransactions :: DB.Connection -> RegistrationId -> IO [NameTransaction]
registrationTransactions db (RegistrationId rid) =
  DB.query db (transactionColumns <> " WHERE registration_id = ? ORDER BY name_transaction_id") (Only rid)
    >>= decodeAll toTransaction

editTransactions :: DB.Connection -> RecordEditId -> IO [NameTransaction]
editTransactions db editId =
  DB.query db (transactionColumns <> " WHERE name_record_edit_id = ? ORDER BY name_transaction_id") (Only editId)
    >>= decodeAll toTransaction

data EditTag = ETPending | ETMined | ETReverted
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding EditTag where
  textEncode = \case
    ETPending -> "pending"
    ETMined -> "mined"
    ETReverted -> "reverted"
  textDecode = \case
    "pending" -> Just ETPending
    "mined" -> Just ETMined
    "reverted" -> Just ETReverted
    _ -> Nothing

instance FromField EditTag where fromField = fromTextField_ textDecode

instance ToField EditTag where toField = toField . textEncode

-- | Nothing when a pending or mined edit already starts at the owner's first nonce.
insertRecordEdit :: DB.Connection -> NewRecordEdit -> UTCTime -> IO (Maybe RecordEditId)
insertRecordEdit db NewRecordEdit {nreLabelHash, nreOwner, nreChanges, nreBodyHash} now = do
  inserted <-
    executeChanging
      db
      ( prefixQuery
          [sql|
        INSERT INTO @name_record_edits (label_hash, owner, nonce, body_hash, state, created_at)
        VALUES (?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      )
      (nreLabelHash, nreOwner, int64 $ scNonce $ L.head nreChanges, nreBodyHash, ETPending, now)
  if inserted > 0
    then do
      editId <- insertedRowId db
      mapM_ (insertChange editId) nreChanges
      pure $ Just editId
    else pure Nothing
  where
    insertChange editId SignedChange {scChange = RCSetText key value, scNonce, scDeadline, scSignature} =
      DB.execute
        db
        (mkQuery "INSERT INTO @name_record_changes (name_record_edit_id, nonce, record_key, record_value, deadline, signature) VALUES (?,?,?,?,?,?)")
        (editId, int64 scNonce, textEncode key, value, roundedSeconds scDeadline, scSignature)

recordEditColumns :: Query
recordEditColumns =
  prefixQuery
    [sql|
    SELECT name_record_edit_id, label_hash, owner, body_hash, state, revert, mined_tx, created_at
    FROM @name_record_edits
  |]

type RecordEditRow = (Int64, LabelHash, Address, BodyHash, EditTag, Maybe Text, Maybe TxHash, UTCTime)

type ChangeRow = (Int64, Text, Text, Int64, RecoverableSignature)

-- | An edit's row with its changes, in nonce order.
readRecordEdit :: DB.Connection -> RecordEditRow -> IO RecordEdit
readRecordEdit db row@(reId, _, _, _, _, _, _, _) = do
  changes <- DB.query db (mkQuery "SELECT nonce, record_key, record_value, deadline, signature FROM @name_record_changes WHERE name_record_edit_id = ? ORDER BY nonce") (Only reId)
  either (E.throwIO . StoreDecodeError) pure $ toRecordEdit row changes

toRecordEdit :: RecordEditRow -> [ChangeRow] -> Either Text RecordEdit
toRecordEdit (reId, reLabelHash, reOwner, reBodyHash, state, revert, minedTx, reCreatedAt) changeRows = do
  reChanges <- note ("name_record_edits changes: " <> tshow reId) . L.nonEmpty =<< mapM toChange changeRows
  reState <- case state of
    ETPending -> Right ESPending
    ETMined -> Right $ ESSettled $ EOApplied minedTx
    ETReverted -> ESSettled . EOReverted <$> mapM (note ("name_record_edits revert: " <> tshow reId) . textDecode) revert
  pure RecordEdit {reId, reLabelHash, reOwner, reChanges, reBodyHash, reState, reCreatedAt}
  where
    toChange (nonce, key, value, deadline, scSignature) = do
      kind <- note ("name_record_changes record: " <> tshow reId) $ textDecode key
      pure SignedChange {scChange = RCSetText kind value, scNonce = word64 nonce, scDeadline = RoundedSystemTime deadline, scSignature}

firstRecordEdit :: DB.Connection -> IO [RecordEditRow] -> IO (Maybe RecordEdit)
firstRecordEdit db rows = rows >>= mapM (readRecordEdit db) . listToMaybe

getRecordEdit :: DB.Connection -> RecordEditId -> IO (Maybe RecordEdit)
getRecordEdit db editId =
  firstRecordEdit db $ DB.query db (recordEditColumns <> " WHERE name_record_edit_id = ?") (Only editId)

-- | The newest edit sent with this signed body.
getRecordEditByBody :: DB.Connection -> BodyHash -> IO (Maybe RecordEdit)
getRecordEditByBody db bodyHash =
  firstRecordEdit db $
    DB.query db (recordEditColumns <> " WHERE body_hash = ? ORDER BY name_record_edit_id DESC LIMIT 1") (Only bodyHash)

-- | Settles a pending edit; False when it was settled already.
settleRecordEdit :: DB.Connection -> RecordEditId -> EditOutcome -> IO Bool
settleRecordEdit db editId outcome =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_record_edits SET state = ?, revert = ?, mined_tx = ? WHERE name_record_edit_id = ? AND state = ?")
      (tag, revert, minedTx, editId, ETPending)
  where
    (tag, revert, minedTx) = case outcome of
      EOApplied tx -> (ETMined, Nothing, tx)
      EOReverted name -> (ETReverted, textEncode <$> name, Nothing)

pendingEditIds :: DB.Connection -> IO [RecordEditId]
pendingEditIds db = map fromOnly <$> DB.query db (mkQuery "SELECT name_record_edit_id FROM @name_record_edits WHERE state = ? ORDER BY name_record_edit_id") (Only ETPending)

deleteUnsentEdit :: DB.Connection -> RecordEditId -> IO ()
deleteUnsentEdit db editId =
  DB.execute
    db
    (mkQuery "DELETE FROM @name_record_edits WHERE name_record_edit_id = ? AND state = ? AND NOT EXISTS (SELECT 1 FROM @name_transactions WHERE name_record_edit_id = ?)")
    (editId, ETPending, editId)

-- | When each edit relayed for the name since the given time was sent, oldest first.
relayedEditTimes :: DB.Connection -> LabelHash -> UTCTime -> IO [UTCTime]
relayedEditTimes db lh since =
  map fromOnly
    <$> DB.query
      db
      (mkQuery "SELECT created_at FROM @name_record_edits WHERE label_hash = ? AND created_at > ? ORDER BY created_at")
      (lh, since)

getTermsText :: DB.Connection -> TermsHash -> IO (Maybe ByteString)
getTermsText db termsHash =
  maybeFirstRow (\(Only (Binary text)) -> text) $
    DB.query db (mkQuery "SELECT terms_text FROM @name_terms WHERE terms_hash = ?") (Only termsHash)

setTermsText :: DB.Connection -> TermsHash -> Text -> IO ()
setTermsText db termsHash text =
  DB.execute
    db
    ( prefixQuery
        [sql|
      INSERT INTO @name_terms (terms_hash, terms_text) VALUES (?,?)
      ON CONFLICT (terms_hash) DO UPDATE SET terms_text = excluded.terms_text
    |]
    )
    (termsHash, Binary $ encodeUtf8 text)

namesCounts :: DB.Connection -> IO NamesCounts
namesCounts db = do
  ncPhases <- DB.query_ db (mkQuery "SELECT phase, fail_reason, COUNT(*) FROM @name_registrations GROUP BY phase, fail_reason")
  DB.query
    db
    ( prefixQuery
        [sql|
      SELECT
        (SELECT MIN(updated_at) FROM @name_registrations WHERE phase IN (?,?,?) OR (phase = ? AND label IS NOT NULL)),
        (SELECT COUNT(DISTINCT registrar_nonce) FROM @name_transactions WHERE status = ?),
        (SELECT MIN(sent_at) FROM @name_transactions WHERE status = ?),
        (SELECT COUNT(*) FROM @name_invoices ni JOIN @invoices i ON i.invoice_id = ni.invoice_id WHERE i.status = ?),
        (SELECT COUNT(*) FROM @name_record_edits WHERE state = ?),
        (SELECT MIN(created_at) FROM @name_record_edits WHERE state = ?),
        (SELECT MIN(selected_at) FROM @name_registrations WHERE phase IN (?,?,?,?,?) AND live = 1)
    |]
    )
    ((PQueued, PCommitting, PRegistering, PCommitted, TSPending, TSPending, ISOpen, ETPending, ETPending) :. (PQueued, PCommitting, PCommitted, PRegistering, PFailed))
    >>= \case
      [(ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncPendingEdits, ncOldestEdit) :. Only ncOldestUnregisteredPaid] ->
        pure NamesCounts {ncPhases, ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncPendingEdits, ncOldestEdit, ncOldestUnregisteredPaid}
      _ -> E.throwIO $ StoreDecodeError "names counts: not one row"

int64 :: Word64 -> Int64
int64 = fromIntegral

word64 :: Int64 -> Word64
word64 = fromIntegral

decodeAll :: (row -> Either Text a) -> [row] -> IO [a]
decodeAll decode = either (E.throwIO . StoreDecodeError) pure . traverse decode

firstDecoded :: (row -> Either Text a) -> IO [row] -> IO (Maybe a)
firstDecoded decode rows = rows >>= fmap listToMaybe . decodeAll decode . take 1

$(JQ.deriveToJSON (enumJSON $ dropPrefix "P") ''NamePhase)

$(JQ.deriveToJSON (enumJSON $ dropPrefix "IC") ''InvoiceClose)

$(JQ.deriveToJSON (enumJSON $ dropPrefix "FR") ''FailReason)

$(JQ.deriveToJSON (enumJSON $ dropPrefix "RT") ''RegistrationTx)

$(JQ.deriveToJSON (enumJSON $ dropPrefix "AS") ''AttemptStage)
