{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module StoreService.Names.Store
  ( CodeId,
    RecordEditId,
    OrderHash,
    CodeHash,
    BodyHash,
    NamePhase (..),
    OrderPayment (..),
    BalanceUse (..),
    paymentInvoice,
    paymentBalance,
    NameReveal (..),
    NameRegistration (..),
    NewRegistration (..),
    PaidOrder (..),
    CodeInsertRefused (..),
    BalanceInsertRefused (..),
    VersionWrite (..),
    LiveOrder (..),
    OrderTerms (..),
    Spend (..),
    Balance (..),
    BalanceHold (..),
    HoldState (..),
    BalanceLink (..),
    LateCredit (..),
    CreditAmount (..),
    creditCents,
    LateCreditState (..),
    LateFigures (..),
    LatePending (..),
    LateAlert (..),
    LateSupport (..),
    LateWait (..),
    CleanupKept (..),
    BrokenInvariant (..),
    PrepaidCode (..),
    NewPrepaidCode (..),
    CodeIssuer (..),
    CodeHashTaken (..),
    TxFor (..),
    TxStatus (..),
    NameTransaction (..),
    NewTransaction (..),
    RecordChange (..),
    SignedChange (..),
    EditState (..),
    RecordEdit (..),
    NewRecordEdit (..),
    NamesCounts (..),
    findLimit,
    orderHash,
    orderLive,
    newRegistrationId,
    methodText,
    textMethod,
    createPaidRegistration,
    createCodeRegistration,
    createBalanceRegistration,
    getRegistration,
    getRegistrationByOrderHash,
    getLiveOrders,
    closeOverdue,
    updateRegistration,
    retargetRegistration,
    findRegistrations,
    isLabelRegistering,
    activeRegistrations,
    unpaidCleanupCandidates,
    deleteUnpaidRegistration,
    failedCodeOrdersBefore,
    heldBy,
    getBalance,
    balanceByReference,
    balanceChain,
    creditBalance,
    spendHeldBalance,
    releaseHeldBalance,
    refundBalance,
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
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (isJust, listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..), FromField (..), ToField (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Encoding (Encoding (..), Large (..))
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (maybeFirstRow, maybeFirstRow', tshow)
import StoreService.Names.Abi (addressFromBytes, hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (Commitment, TermsAcceptance (..), TermsHash)
import StoreService.Names.Signatures (OrderFields (..), parseSignature)
import StoreService.Providers (Rate, Uncreditable (..), Units)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (StoreDecodeError (..), executeChanging, invoiceStatusText, lockInvoice, mkQuery, newInvoiceId, note, prefixQuery, textToCryptoCurrency, truncateToSecond, unfundedQuery)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

type CodeId = Int64

type RecordEditId = Int64

type OrderHash = ByteString

type CodeHash = ByteString

type BodyHash = ByteString

data NamePhase
  = PUnpaid
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
    PQueued -> "queued"
    PCommitting -> "committing"
    PCommitted -> "committed"
    PRegistering -> "registering"
    PRegistered -> "registered"
    PTaken -> "taken"
    PFailed -> "failed"
  textDecode t = lookup t [(textEncode p, p) | p <- [minBound .. maxBound]]

instance FromField NamePhase where fromField = fromTextField_ textDecode

instance ToField NamePhase where toField = toField . textEncode

-- | OPDifference: an invoice of the price above the balance the order holds. OPBalance: wholly from a balance, with neither invoice nor code.
data OrderPayment
  = OPInvoice ServicePaymentMethod InvoiceId
  | OPDifference BalanceUse ServicePaymentMethod InvoiceId
  | OPBalance BalanceUse
  | OPCode CodeId
  deriving (Eq, Show)

-- | The order whose balance pays, and the cents it pays.
data BalanceUse = BalanceUse {buSource :: RegistrationId, buUsed :: Int}
  deriving (Eq, Show)

paymentInvoice :: OrderPayment -> Maybe InvoiceId
paymentInvoice = \case
  OPInvoice _ invId -> Just invId
  OPDifference _ _ invId -> Just invId
  OPBalance _ -> Nothing
  OPCode _ -> Nothing

paymentBalance :: OrderPayment -> Maybe BalanceUse
paymentBalance = \case
  OPInvoice {} -> Nothing
  OPDifference use _ _ -> Just use
  OPBalance use -> Just use
  OPCode _ -> Nothing

-- | The name, links and owner's intent, all set by an accepted reveal and cleared together.
data NameReveal = NameReveal
  { rvLabel :: Text,
    rvSecret :: ByteString,
    rvContactLink :: Maybe Text,
    rvChannelLink :: Maybe Text,
    rvNonce :: Word64,
    rvDeadline :: Word64,
    rvSignature :: RecoverableSignature,
    -- | the owner's acceptance of the terms of the name's kind
    rvAcceptance :: Maybe TermsAcceptance
  }
  deriving (Eq, Show)

data NameRegistration = NameRegistration
  { nrId :: RegistrationId,
    nrVersion :: Int64,
    nrLabelHash :: LabelHash,
    nrLength :: Int,
    nrDuration :: Word64,
    nrOwner :: Address,
    nrCommitment :: Commitment,
    nrSealed :: Text,
    nrAgreedTerms :: Text,
    nrPhase :: NamePhase,
    nrYears :: Int,
    nrPrice :: Int,
    nrPayment :: OrderPayment,
    nrReveal :: Maybe NameReveal,
    nrCommitTx :: Maybe TxHash,
    nrCommitBlock :: Maybe Word64,
    nrRegisterTx :: Maybe TxHash,
    nrRegisterBlock :: Maybe Word64,
    nrRevealAfter :: Maybe Word64,
    nrRevealBefore :: Maybe Word64,
    nrExpiresAt :: Maybe Word64,
    nrRegisteredAt :: Maybe UTCTime,
    nrFailedAt :: Maybe UTCTime,
    nrError :: Maybe Text,
    nrLive :: Bool,
    nrOrderHash :: OrderHash,
    nrMoves :: Int,
    nrLinksRefusedAt :: Maybe UTCTime,
    nrCreatedAt :: UTCTime,
    nrUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

data NewRegistration = NewRegistration
  { nrgId :: RegistrationId,
    nrgOwner :: Address,
    nrgOrder :: OrderFields,
    nrgYears :: Int,
    nrgOrderHash :: OrderHash
  }

-- | The invoice row is inserted by the caller, in the same transaction and before the registration.
data PaidOrder = PaidOrder
  { poMethod :: ServicePaymentMethod,
    poPrice :: Int,
    poInvoiceId :: InvoiceId,
    poProviderRef :: Text
  }

data CodeInsertRefused = CIRLiveOrder | CIRCodeNotReservable
  deriving (Eq, Show)

-- | BIRBalanceChanged: the source's balance no longer has the amount seen, or another order holds it.
data BalanceInsertRefused = BIRLiveOrder | BIRBalanceChanged
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
data OrderTerms = OTInvoice ServicePaymentMethod Int | OTCode CodeHash | OTBalance Spend

-- | A new order paid from a balance. Its hash covers the paying order, the price, the balance the page read,
-- and the method, present exactly when the price is more than that balance.
data Spend = Spend
  { spSource :: RegistrationId,
    spPrice :: Int,
    spBalance :: Int,
    spMethod :: Maybe ServicePaymentMethod
  }

-- | Cents left to spend, and over the balance's life the cents credited and refunded.
data Balance = Balance
  { blRegistration :: RegistrationId,
    blAmount :: Int,
    blCredited :: Int,
    blRefunded :: Int,
    blHold :: Maybe BalanceHold,
    blRefundedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)

-- | The order whose open invoice holds a balance, until that invoice expires; overdue past it with no money received.
-- Funded once money reached it: it then holds until its payment settles or the provider closes it, whatever the time.
data BalanceHold = BalanceHold
  { bhHolder :: RegistrationId,
    bhState :: HoldState
  }
  deriving (Eq, Show)

data HoldState = HSOpen UTCTime | HSFunded | HSOverdue UTCTime
  deriving (Eq, Show)

-- | An order the money of a balance came through. lkPaid and lkFromBalance count only an order that went past unpaid,
-- since closing an unpaid one released its source's balance.
data BalanceLink = BalanceLink
  { lkRegistration :: RegistrationId,
    lkOwner :: Address,
    lkPayment :: OrderPayment,
    lkProviderRef :: Maybe Text,
    lkPaid :: Int,
    lkFromBalance :: Int,
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
  = CACard Int
  | CACrypto {caUnits :: Units, caInvoiceRate :: Rate, caCurrentRate :: Rate, caRateUsed :: Rate, caCents :: Int}
  deriving (Eq, Show)

creditCents :: CreditAmount -> Int
creditCents = \case
  CACard cents -> cents
  CACrypto {caCents} -> caCents

-- | A name invoice's payment figures, what was credited from it, and its order's phase and live flag.
-- lsSupport: why its money will not be credited by itself, once known.
data LateCreditState = LateCreditState
  { lsInvoice :: InvoiceId,
    lsRegistration :: RegistrationId,
    lsPhase :: NamePhase,
    lsLive :: Bool,
    lsSupport :: Maybe LateSupport,
    lsReceivedCents :: Int,
    lsFigures :: LateFigures,
    lsInvoiceAmount :: Int,
    lsCreditedCents :: Int,
    lsCredits :: Int,
    lsPaymentCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

-- | Card money counted in cents; crypto in units, with the invoice's rate and invoiced units it is credited at.
data LateFigures
  = LFCard {lfConfirmedCents :: Int}
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
  textDecode t = lookup t [(textEncode p, p) | p <- [minBound .. maxBound]]

instance FromField LatePending where fromField = fromTextField_ textDecode

-- | What a closed order's late money waits on, for the view: confirmation, the credit, or support.
data LateWait = LWConfirming | LWCrediting | LWSupport
  deriving (Eq, Show)

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

-- | Why the cleanup deletes no unpaid order: a payment was recorded for it, it holds the balance of the given order, or it is gone.
data CleanupKept = CKPaid | CKHolder RegistrationId | CKGone
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
    npcIssuer :: CodeIssuer
  }

data CodeIssuer = IssuedInGroup | IssuedInConsole
  deriving (Eq, Show)

data CodeHashTaken = CodeHashTaken
  deriving (Eq, Show)

data TxFor = TxCommit RegistrationId | TxAccept RegistrationId | TxRegister RegistrationId | TxEdit RecordEditId
  deriving (Eq, Show)

-- | A dropped transaction has no receipt and its registrar nonce is used.
data TxStatus = TSPending | TSMined | TSReverted | TSDropped
  deriving (Eq, Show, Enum, Bounded)

instance TextEncoding TxStatus where
  textEncode = \case
    TSPending -> "pending"
    TSMined -> "mined"
    TSReverted -> "reverted"
    TSDropped -> "dropped"
  textDecode t = lookup t [(textEncode s, s) | s <- [minBound .. maxBound]]

instance FromField TxStatus where fromField = fromTextField_ textDecode

instance ToField TxStatus where toField = toField . textEncode

data NameTransaction = NameTransaction
  { ntId :: Int64,
    ntFor :: TxFor,
    ntRaw :: ByteString,
    ntHash :: TxHash,
    ntNonce :: Word64,
    ntSentAt :: UTCTime,
    ntMinedBlock :: Maybe Word64,
    ntStatus :: TxStatus
  }
  deriving (Eq, Show)

data NewTransaction = NewTransaction
  { ntxRaw :: ByteString,
    ntxHash :: TxHash,
    ntxNonce :: Word64
  }

-- | A text record set to a value, "" removing it, or every record cleared.
data RecordChange = RCSetText Text Text | RCClear
  deriving (Eq, Show)

data EditState = ESPending | ESMined (Maybe TxHash) | ESReverted (Maybe Text)
  deriving (Eq, Show)

-- | A change the owner signed for the resolver's nonce it names.
data SignedChange = SignedChange
  { scChange :: RecordChange,
    scNonce :: Word64,
    scDeadline :: Word64,
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
  { ncPhases :: [(NamePhase, Int)],
    ncQueueOldestChange :: Maybe UTCTime,
    ncPendingTransactions :: Int,
    ncOldestPending :: Maybe UTCTime,
    ncOpenInvoices :: Int,
    ncHeldBalances :: Int,
    ncHeldCents :: Int,
    ncPendingEdits :: Int,
    ncOldestEdit :: Maybe UTCTime
  }
  deriving (Eq, Show)

findLimit :: Int
findLimit = 50

initialVersion :: Int64
initialVersion = 1

-- | No two different orders encode the same.
orderHash :: Address -> OrderFields -> OrderTerms -> OrderHash
orderHash owner OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} terms =
  sha256Hash $ smpEncode (tag, lh, num ofLength, int64 ofDuration, unAddress owner, ofCommitment, Large $ encodeUtf8 ofSealed, ofAgreedTerms) <> payment
  where
    (tag, payment) = case terms of
      OTInvoice method price -> ("name-invoice" :: ByteString, smpEncode (methodText method, num price))
      OTCode codeHash -> ("name-code", smpEncode codeHash)
      OTBalance Spend {spSource = RegistrationId source, spPrice, spBalance, spMethod} -> ("name-balance", smpEncode (source, num spPrice, num spBalance, methodText <$> spMethod))
    num :: Int -> Int64
    num = fromIntegral

newRegistrationId :: IO RegistrationId
newRegistrationId = (\(InvoiceId t) -> RegistrationId t) <$> newInvoiceId

methodText :: ServicePaymentMethod -> Text
methodText = \case
  SPMCard CPStripe -> "card"
  SPMCrypto CCBtc -> "btc"
  SPMCrypto CCXmr -> "xmr"

textMethod :: Text -> Maybe ServicePaymentMethod
textMethod t = lookup t [(methodText m, m) | m <- [SPMCard CPStripe, SPMCrypto CCBtc, SPMCrypto CCXmr]]

-- | A difference order holds all of the balance seen while its invoice is open. The hold is written after the registration,
-- so the registration's row is locked before the balance's. A refusal leaves its writes for the caller to roll back.
createPaidRegistration :: DB.Connection -> NewRegistration -> Maybe BalanceUse -> PaidOrder -> UTCTime -> IO (Either BalanceInsertRefused ())
createPaidRegistration db new@NewRegistration {nrgId = regId@(RegistrationId rid)} held PaidOrder {poMethod, poPrice, poInvoiceId = InvoiceId invId, poProviderRef} now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PUnpaid poPrice (Just poMethod) Nothing held now
  if not inserted
    then pure $ Left BIRLiveOrder
    else do
      DB.execute
        db
        (mkQuery "INSERT INTO @name_invoices (invoice_id, registration_id, provider_ref, created_at) VALUES (?,?,?,?)")
        (invId, rid, poProviderRef, now)
      held' <- maybe (pure True) (\BalanceUse {buSource, buUsed} -> holdBalance db buSource buUsed regId now) held
      pure $ if held' then Right () else Left BIRBalanceChanged

-- | Reserving the code is the conditional update of the transaction that creates the registration. A refusal leaves its writes for
-- the caller to roll back.
createCodeRegistration :: DB.Connection -> NewRegistration -> PrepaidCode -> UTCTime -> IO (Either CodeInsertRefused ())
createCodeRegistration db new@NewRegistration {nrgId = regId} PrepaidCode {pcId} now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PQueued 0 Nothing (Just pcId) Nothing now
  if not inserted
    then pure $ Left CIRLiveOrder
    else
      reservePrepaidCode db pcId regId now >>= \case
        True -> pure $ Right ()
        False -> pure $ Left CIRCodeNotReservable

-- | A queued order whose price is spent from the source's balance in the same transaction, where its amount is the one seen and nothing holds it.
-- A refusal leaves its writes for the caller to roll back.
createBalanceRegistration :: DB.Connection -> NewRegistration -> BalanceUse -> Int -> UTCTime -> IO (Either BalanceInsertRefused ())
createBalanceRegistration db new use@BalanceUse {buSource, buUsed} seen now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PQueued buUsed Nothing Nothing (Just use) now
  if not inserted
    then pure $ Left BIRLiveOrder
    else
      spendBalance db buSource buUsed seen now >>= \case
        True -> pure $ Right ()
        False -> pure $ Left BIRBalanceChanged

-- | False when the registration would be a second live one for its commitment, or for its owner and label hash.
insertRegistration :: DB.Connection -> NewRegistration -> NamePhase -> Int -> Maybe ServicePaymentMethod -> Maybe CodeId -> Maybe BalanceUse -> UTCTime -> IO Bool
insertRegistration db NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder, nrgYears, nrgOrderHash} phase price method codeId fromBalance now =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        INSERT INTO @name_registrations
          (registration_id, version, label_hash, length, duration, owner, commitment, sealed, phase, years,
           price, method, name_prepaid_code_id, live, order_hash, moves, created_at, updated_at, balance_source, balance_used, agreed_terms)
        VALUES (?,?,?,?,?,?,?,?,?,?, ?,?,?,?,?,?,?,?,?,?, ?)
        ON CONFLICT DO NOTHING
      |]
      )
      ( (rid, initialVersion, Binary lh, ofLength, int64 ofDuration, Binary $ unAddress nrgOwner, Binary ofCommitment, ofSealed, phase, nrgYears)
          :. (price, methodText <$> method, codeId, BI True, Binary nrgOrderHash, 0 :: Int, now, now, (\BalanceUse {buSource = RegistrationId s} -> s) <$> fromBalance, maybe 0 buUsed fromBalance)
          :. Only ofAgreedTerms
      )
  where
    OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} = nrgOrder

registrationColumns :: Query
registrationColumns =
  [sql|
    SELECT r.registration_id, r.version, r.label_hash, r.length, r.duration, r.owner, r.commitment, r.sealed, r.phase, r.years,
      r.price, r.method, ni.invoice_id, r.name_prepaid_code_id, r.label, r.secret, r.contact_link, r.channel_link, r.intent_nonce, r.intent_deadline,
      r.intent_signature, r.commit_tx, r.commit_block, r.register_tx, r.register_block, r.reveal_after, r.reveal_before, r.expires_at, r.registered_at,
      r.failed_at, r.error, r.live, r.order_hash, r.moves, r.links_refused_at, r.created_at, r.updated_at, r.balance_source, r.balance_used,
      r.agreed_terms, r.accept_terms, r.accept_nonce, r.accept_signature
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
  (Text, Int64, Binary ByteString, Int, Int64, Binary ByteString, Binary ByteString, Text, NamePhase, Int)
    :. (Int, Maybe Text, Maybe Text, Maybe Int64, Maybe Text, Maybe (Binary ByteString), Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)
    :. (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe Int64, Maybe (Binary ByteString), Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime)
    :. (Maybe UTCTime, Maybe Text, BoolInt, Binary ByteString, Int, Maybe UTCTime, UTCTime, UTCTime, Maybe Text, Int)
    :. (Text, Maybe (Binary ByteString), Maybe Int64, Maybe (Binary ByteString))

toRegistration :: RegistrationRow -> Either Text NameRegistration
toRegistration
  ( (rid, nrVersion, Binary lh, nrLength, duration, Binary owner, Binary nrCommitment, nrSealed, nrPhase, nrYears)
      :. (nrPrice, method_, invoiceId_, codeId_, label_, secret_, nrContactLink, nrChannelLink, nonce_, deadline_)
      :. (signature_, commitTx, commitBlock, registerTx, registerBlock, revealAfter, revealBefore, expiresAt, nrRegisteredAt)
      :. (nrFailedAt, nrError, BI nrLive, Binary nrOrderHash, nrMoves, nrLinksRefusedAt, nrCreatedAt, nrUpdatedAt, source_, used)
      :. (nrAgreedTerms, acceptTerms_, acceptNonce_, acceptSignature_)
    ) = do
    nrOwner <- decodeAddress owner
    let methodOf m = note ("name_registrations.method: " <> m) (textMethod m)
        balanceUse source = BalanceUse {buSource = RegistrationId source, buUsed = used}
    nrPayment <- case (method_, invoiceId_, codeId_, source_) of
      (Just m, Just invId, Nothing, Nothing) -> (`OPInvoice` InvoiceId invId) <$> methodOf m
      (Just m, Just invId, Nothing, Just source) -> (\method -> OPDifference (balanceUse source) method (InvoiceId invId)) <$> methodOf m
      (Nothing, Nothing, Just codeId, Nothing) -> Right $ OPCode codeId
      (Nothing, Nothing, Nothing, Just source) -> Right $ OPBalance $ balanceUse source
      _ -> Left $ "name_registrations payment: " <> rid
    nrReveal <- case (label_, secret_, nonce_, deadline_, signature_) of
      (Nothing, Nothing, Nothing, Nothing, Nothing) -> Right Nothing
      (Just rvLabel, Just (Binary rvSecret), Just nonce, Just deadline, Just (Binary sig)) -> do
        rvSignature <- decodeSignature sig
        rvAcceptance <- case (acceptTerms_, acceptNonce_, acceptSignature_) of
          (Nothing, Nothing, Nothing) -> Right Nothing
          (Just (Binary taTermsHash), Just n, Just (Binary s)) -> (\taSignature -> Just TermsAcceptance {taTermsHash, taNonce = word64 n, taSignature}) <$> decodeSignature s
          _ -> Left $ "name_registrations acceptance: " <> rid
        Right $ Just NameReveal {rvLabel, rvSecret, rvContactLink = nrContactLink, rvChannelLink = nrChannelLink, rvNonce = word64 nonce, rvDeadline = word64 deadline, rvSignature, rvAcceptance}
      _ -> Left $ "name_registrations reveal: " <> rid
    pure
      NameRegistration
        { nrId = RegistrationId rid,
          nrVersion,
          nrLabelHash = LabelHash lh,
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
          nrCommitTx = txHash <$> commitTx,
          nrCommitBlock = word64 <$> commitBlock,
          nrRegisterTx = txHash <$> registerTx,
          nrRegisterBlock = word64 <$> registerBlock,
          nrRevealAfter = word64 <$> revealAfter,
          nrRevealBefore = word64 <$> revealBefore,
          nrExpiresAt = word64 <$> expiresAt,
          nrRegisteredAt,
          nrFailedAt,
          nrError,
          nrLive,
          nrOrderHash,
          nrMoves,
          nrLinksRefusedAt,
          nrCreatedAt,
          nrUpdatedAt
        }

getRegistration :: DB.Connection -> RegistrationId -> IO (Maybe NameRegistration)
getRegistration db (RegistrationId rid) =
  firstDecoded toRegistration $ DB.query db (selectRegistrations <> " WHERE r.registration_id = ?") (Only rid)

-- | The newest stored order, live or not, with exactly these fields.
getRegistrationByOrderHash :: DB.Connection -> OrderHash -> IO (Maybe NameRegistration)
getRegistrationByOrderHash db hash =
  firstDecoded toRegistration $
    DB.query db (selectRegistrations <> " WHERE r.order_hash = ? ORDER BY r.created_at DESC, r.registration_id DESC LIMIT 1") (Only $ Binary hash)

-- | Overdue whatever status the poller has marked, as long as the invoice is not paid and holds no money, as the sweep decides.
getLiveOrders :: DB.Connection -> Commitment -> Address -> LabelHash -> UTCTime -> IO [LiveOrder]
getLiveOrders db commitment owner (LabelHash lh) now =
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
    (PUnpaid, invoiceStatusText ISPaid, truncateToSecond now, Binary commitment, Binary $ unAddress owner, Binary lh)
    >>= decodeAll toLiveOrder
  where
    toLiveOrder (row :. Only (BI loOverdue)) = (\loRegistration -> LiveOrder {loRegistration, loOverdue}) <$> toRegistration row

-- | The sweep's own check, on the invoice joined as i.
unfunded :: Query
unfunded = unfundedQuery "i.invoice_id"

-- | Run in the transaction that inserts the next order, so the unique constraints let it in; the registration's new version.
-- Nothing when the order changed since it was read, or its invoice is no longer open, not past its expiry, or holds money: the caller rolls back,
-- since the invoice is written before the registration, as settlement, cancel and the sweep write them. A balance it held is released.
closeOverdue :: DB.Connection -> NameRegistration -> UTCTime -> IO (Maybe Int64)
closeOverdue db r@NameRegistration {nrId = regId@(RegistrationId rid), nrVersion, nrPayment} now = do
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
      (invoiceStatusText ISExpired, truncateToSecond now, invoiceStatusText ISOpen, rid, nrVersion, truncateToSecond now)
  if expired > 0
    then
      updateRegistration db r {nrLive = False} now >>= \case
        Written v -> Just v <$ releaseHeldBalance db regId now
        _ -> pure Nothing
    else pure Nothing

-- | Whether an order can still progress to a registration for its name. The unique live indexes allow one such order per commitment,
-- and one per owner and label hash, so a wallet can order a name again once its registration is done.
-- An unpaid order is live while its invoice is open, which nrLive carries; a taken order is final, its worth kept as a balance.
orderLive :: NameRegistration -> Bool
orderLive NameRegistration {nrPhase, nrLive} = case nrPhase of
  PUnpaid -> nrLive
  PQueued -> True
  PCommitting -> True
  PCommitted -> True
  PRegistering -> True
  PFailed -> True
  PTaken -> False
  PRegistered -> False

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
          accept_terms = ?, accept_nonce = ?, accept_signature = ?, commit_tx = ?, commit_block = ?, register_tx = ?, register_block = ?, reveal_after = ?, reveal_before = ?, expires_at = ?,
          registered_at = ?, failed_at = ?, error = ?, live = ?, moves = ?, links_refused_at = ?, updated_at = ?
        WHERE registration_id = ? AND version = ?
          AND (? = 0 OR (live = 1 AND NOT EXISTS (
            SELECT 1 FROM @name_registrations o
            WHERE o.live = 1 AND o.registration_id <> ? AND (o.commitment = ? OR (o.owner = ? AND o.label_hash = ?))
          )))
      |]
      )
      ( (Binary lh, nrLength, int64 nrDuration, Binary nrCommitment, nrSealed, nrAgreedTerms, nrPhase, nrPrice)
          :. (rvLabel <$> nrReveal, Binary . rvSecret <$> nrReveal, rvContactLink =<< nrReveal, rvChannelLink =<< nrReveal, int64 . rvNonce <$> nrReveal, int64 . rvDeadline <$> nrReveal, Binary . serializeSignature . rvSignature <$> nrReveal)
          :. (Binary . taTermsHash <$> acceptance, int64 . taNonce <$> acceptance, Binary . serializeSignature . taSignature <$> acceptance)
          :. (txHashField <$> nrCommitTx, int64 <$> nrCommitBlock, txHashField <$> nrRegisterTx, int64 <$> nrRegisterBlock, int64 <$> nrRevealAfter, int64 <$> nrRevealBefore, int64 <$> nrExpiresAt)
          :. (truncateToSecond <$> nrRegisteredAt, truncateToSecond <$> nrFailedAt, nrError, BI live, nrMoves, truncateToSecond <$> nrLinksRefusedAt, truncateToSecond now)
          :. (rid, nrVersion, BI live, rid, Binary nrCommitment, Binary $ unAddress nrOwner, Binary lh)
      )
  if changed > 0 then pure $ Written (nrVersion + 1) else refusal
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrLabelHash = LabelHash lh, nrLength, nrDuration, nrOwner, nrCommitment, nrSealed, nrAgreedTerms, nrPhase, nrPrice, nrReveal} = r
    NameRegistration {nrCommitTx, nrCommitBlock, nrRegisterTx, nrRegisterBlock, nrRevealAfter, nrRevealBefore, nrExpiresAt} = r
    NameRegistration {nrRegisteredAt, nrFailedAt, nrError, nrMoves, nrLinksRefusedAt} = r
    acceptance = rvAcceptance =<< nrReveal
    live = orderLive r
    refusal =
      DB.query db (mkQuery "SELECT version, live FROM @name_registrations WHERE registration_id = ?") (Only rid) >>= \case
        [(version, BI storedLive)] | version == nrVersion -> pure $ if storedLive then LiveOrderExists else NotLive
        _ -> pure VersionChanged

-- | Another name, or other links, with the terms agreed for it: queued again with the reveal, transactions, reveal times and link refusal cleared.
-- The order hash stays the original order's, so a resend of that order still finds it.
retargetRegistration :: DB.Connection -> NameRegistration -> OrderFields -> Int -> UTCTime -> IO VersionWrite
retargetRegistration db r OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed, ofAgreedTerms} price =
  updateRegistration
    db
    r
      { nrLabelHash = ofLabelHash,
        nrLength = ofLength,
        nrDuration = ofDuration,
        nrCommitment = ofCommitment,
        nrSealed = ofSealed,
        nrAgreedTerms = ofAgreedTerms,
        nrPrice = price,
        nrPhase = PQueued,
        nrMoves = nrMoves r + 1,
        nrReveal = Nothing,
        nrCommitTx = Nothing,
        nrCommitBlock = Nothing,
        nrRegisterTx = Nothing,
        nrRegisterBlock = Nothing,
        nrRevealAfter = Nothing,
        nrRevealBefore = Nothing,
        nrLinksRefusedAt = Nothing,
        nrFailedAt = Nothing,
        nrError = Nothing
      }

-- | Newest first: findLimit of them, then every older one holding a balance above 0, since a balance never expires.
-- Each with the order holding its balance, as getBalance reads the hold at the given time.
findRegistrations :: DB.Connection -> Address -> UTCTime -> IO [(NameRegistration, Maybe RegistrationId)]
findRegistrations db owner now = do
  newest <- DB.query db (selectHeld <> " WHERE r.owner = ?" <> newestFirst <> " LIMIT ?") (holdParams :. (Binary $ unAddress owner, findLimit)) >>= decodeAll found
  holding <- DB.query db (selectHeld <> " WHERE r.owner = ?" <> withBalance <> newestFirst) (holdParams :. Only (Binary $ unAddress owner)) >>= decodeAll found
  pure $ newest <> filter ((`notElem` map (nrId . fst) newest) . nrId . fst) holding
  where
    selectHeld =
      registrationColumns
        <> mkQuery
          ( ", (SELECT b.reserved_by FROM @name_balances b"
              <> " JOIN @name_registrations h ON h.registration_id = b.reserved_by"
              <> " JOIN @name_invoices hni ON hni.registration_id = b.reserved_by"
              <> " JOIN @invoices i ON i.invoice_id = hni.invoice_id"
          )
        <> " WHERE b.registration_id = r.registration_id AND NOT ("
        <> overdueOrder "h"
        <> "))"
        <> registrationFrom
    holdParams = (PUnpaid, invoiceStatusText ISPaid, truncateToSecond now)
    found :: RegistrationRow :. Only (Maybe Text) -> Either Text (NameRegistration, Maybe RegistrationId)
    found (row :. Only holder) = (\r -> (r, RegistrationId <$> holder)) <$> toRegistration row
    newestFirst = " ORDER BY r.created_at DESC, r.registration_id DESC"
    withBalance = mkQuery " AND EXISTS (SELECT 1 FROM @name_balances b WHERE b.registration_id = r.registration_id AND b.amount > 0)"

isLabelRegistering :: DB.Connection -> LabelHash -> IO Bool
isLabelRegistering db (LabelHash lh) =
  not . null
    <$> ( DB.query db (mkQuery "SELECT 1 FROM @name_registrations WHERE label_hash = ? AND phase = ? LIMIT 1") (Binary lh, PRegistering) ::
            IO [Only Int]
        )

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
        SELECT r.registration_id, ni.invoice_id, ni.provider_ref
        FROM @name_registrations r
        JOIN @name_invoices ni ON ni.registration_id = r.registration_id
        JOIN @invoices i ON i.invoice_id = ni.invoice_id
        WHERE r.phase = ? AND i.expires_at < ?
          AND NOT EXISTS (SELECT 1 FROM @payments p WHERE p.invoice_id = ni.invoice_id)
        ORDER BY i.expires_at, r.registration_id
      |]
      )
      (PUnpaid, truncateToSecond expiredBefore)

-- | Deletes an unpaid registration and its invoice, unless a payment was recorded for it or it holds a balance.
deleteUnpaidRegistration :: DB.Connection -> RegistrationId -> IO (Either CleanupKept ())
deleteUnpaidRegistration db (RegistrationId rid) =
  DB.query db (mkQuery "SELECT registration_id FROM @name_balances WHERE reserved_by = ?") (Only rid) >>= \case
    Only source : _ -> pure $ Left $ CKHolder $ RegistrationId source
    [] ->
      DB.query db (mkQuery "SELECT ni.invoice_id FROM @name_registrations r LEFT JOIN @name_invoices ni ON ni.registration_id = r.registration_id WHERE r.registration_id = ?") (Only rid) >>= \case
        [] -> pure $ Left CKGone
        invoiceIds -> do
          deleted <-
            executeChanging
              db
              ( prefixQuery
                  [sql|
                DELETE FROM @name_registrations
                WHERE registration_id = ? AND phase = ?
                  AND NOT EXISTS (
                    SELECT 1 FROM @name_invoices ni
                    JOIN @payments p ON p.invoice_id = ni.invoice_id
                    WHERE ni.registration_id = ?
                  )
              |]
              )
              (rid, PUnpaid, rid)
          if deleted > 0
            then Right () <$ mapM_ (DB.execute db (mkQuery "DELETE FROM @invoices WHERE invoice_id = ?") . Only) [invId | Only (Just invId) <- invoiceIds :: [Only (Maybe Text)]]
            else pure $ Left CKPaid

-- | Code orders failed before the cutoff that still hold their unspent code.
failedCodeOrdersBefore :: DB.Connection -> UTCTime -> IO [(RegistrationId, CodeId)]
failedCodeOrdersBefore db cutoff =
  map (\(rid, codeId) -> (RegistrationId rid, codeId))
    <$> DB.query
      db
      ( prefixQuery
          [sql|
        SELECT r.registration_id, c.name_prepaid_code_id
        FROM @name_registrations r
        JOIN @name_prepaid_codes c ON c.reserved_by = r.registration_id
        WHERE r.phase = ? AND r.failed_at < ? AND c.spent_at IS NULL
        ORDER BY r.failed_at, r.registration_id
      |]
      )
      (PFailed, truncateToSecond cutoff)

-- | The hold, unless it is overdue, which reads as none.
heldBy :: Balance -> Maybe BalanceHold
heldBy Balance {blHold} = case blHold of
  Just BalanceHold {bhState = HSOverdue _} -> Nothing
  _ -> blHold

-- | The order of the alias with invoice i is overdue: unpaid, its invoice past its expiry, not paid, and holding no money.
-- Takes the unpaid phase, the paid status and the time.
overdueOrder :: Query -> Query
overdueOrder alias = " " <> alias <> ".phase = ? AND i.status <> ? AND i.expires_at < ?" <> unfunded

getBalance :: DB.Connection -> RegistrationId -> UTCTime -> IO (Maybe Balance)
getBalance db (RegistrationId rid) now =
  firstDecoded toBalance $
    DB.query
      db
      ( "SELECT b.registration_id, b.amount, b.credited, b.refunded, b.reserved_by, i.expires_at,"
          <> " CASE WHEN"
          <> overdueOrder "h"
          <> " THEN 1 ELSE 0 END, CASE WHEN i.invoice_id IS NOT NULL"
          <> unfunded
          <> " THEN 0 ELSE 1 END, b.refunded_at"
          <> mkQuery
            ( " FROM @name_balances b"
                <> " LEFT JOIN @name_registrations h ON h.registration_id = b.reserved_by"
                <> " LEFT JOIN @name_invoices ni ON ni.registration_id = b.reserved_by"
                <> " LEFT JOIN @invoices i ON i.invoice_id = ni.invoice_id"
            )
          <> " WHERE b.registration_id = ?"
      )
      (PUnpaid, invoiceStatusText ISPaid, truncateToSecond now, rid)

toBalance :: (Text, Int, Int, Int, Maybe Text, Maybe UTCTime, BoolInt, BoolInt, Maybe UTCTime) -> Either Text Balance
toBalance (rid, blAmount, blCredited, blRefunded, holder_, until_, BI overdue, BI funded, blRefundedAt) = do
  blHold <- case (holder_, until_) of
    (Nothing, _) -> Right Nothing
    (Just holder, Just t) -> Right $ Just BalanceHold {bhHolder = RegistrationId holder, bhState = holdState t}
    (Just _, Nothing) -> Left $ "name_balances holder without an invoice: " <> rid
  pure Balance {blRegistration = RegistrationId rid, blAmount, blCredited, blRefunded, blHold, blRefundedAt}
  where
    holdState t
      | overdue = HSOverdue t
      | funded = HSFunded
      | otherwise = HSOpen t

-- | The order a support reference names, by its registration id or its name invoice's id, with its balance.
balanceByReference :: DB.Connection -> Text -> UTCTime -> IO (Maybe (RegistrationId, Maybe Balance))
balanceByReference db ref now =
  DB.query
    db
    (mkQuery "SELECT registration_id FROM @name_registrations WHERE registration_id = ? UNION ALL SELECT registration_id FROM @name_invoices WHERE invoice_id = ?")
    (ref, ref)
    >>= \case
      Only rid : _ -> (\balance -> Just (RegistrationId rid, balance)) <$> getBalance db (RegistrationId rid) now
      [] -> pure Nothing

-- | From the order back along balance_source to the first, following it only from orders past unpaid.
balanceChain :: DB.Connection -> RegistrationId -> IO [BalanceLink]
balanceChain db rid =
  getRegistration db rid >>= \case
    Nothing -> pure []
    Just NameRegistration {nrPhase, nrOwner, nrPayment} -> do
      let pastUnpaid = nrPhase /= PUnpaid
          used = if pastUnpaid then paymentBalance nrPayment else Nothing
      (lkPaid, lkProviderRef, lkLateCredits) <- case paymentInvoice nrPayment of
        Just invId -> (,,) <$> (if pastUnpaid then invoiceAmount db invId else pure 0) <*> providerRef db invId <*> lateCredits db invId
        Nothing -> pure (0, Nothing, [])
      let link = BalanceLink {lkRegistration = rid, lkOwner = nrOwner, lkPayment = nrPayment, lkProviderRef, lkPaid, lkFromBalance = maybe 0 buUsed used, lkLateCredits}
      (link :) <$> maybe (pure []) (balanceChain db . buSource) used

invoiceAmount :: DB.Connection -> InvoiceId -> IO Int
invoiceAmount db (InvoiceId invId) =
  maybeFirstRow' 0 fromOnly $ DB.query db (mkQuery "SELECT amount FROM @invoices WHERE invoice_id = ?") (Only invId)

providerRef :: DB.Connection -> InvoiceId -> IO (Maybe Text)
providerRef db (InvoiceId invId) =
  maybeFirstRow fromOnly $ DB.query db (mkQuery "SELECT provider_ref FROM @name_invoices WHERE invoice_id = ?") (Only invId)

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

-- | Adds to the order's balance, making it on its first credit.
creditBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO ()
creditBalance db (RegistrationId rid) cents now' =
  DB.execute
    db
    ( prefixQuery
        [sql|
      INSERT INTO @name_balances (registration_id, amount, credited, refunded, created_at, updated_at)
      VALUES (?,?,?,0,?,?)
      ON CONFLICT (registration_id) DO UPDATE SET
        amount = @name_balances.amount + excluded.amount,
        credited = @name_balances.credited + excluded.credited,
        updated_at = excluded.updated_at
    |]
    )
    (rid, cents, cents, now, now)
  where
    now = truncateToSecond now'

-- | The price, above 0, spent at once, where the amount is the one seen and nothing holds it.
spendBalance :: DB.Connection -> RegistrationId -> Int -> Int -> UTCTime -> IO Bool
spendBalance db (RegistrationId rid) price seen now =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_balances SET amount = amount - ?, updated_at = ? WHERE registration_id = ? AND amount = ? AND amount >= ? AND ? > 0 AND reserved_by IS NULL")
      (price, now, rid, seen, price, price)

-- | Held by a difference order, where the amount is the one seen, above 0, and nothing holds it.
holdBalance :: DB.Connection -> RegistrationId -> Int -> RegistrationId -> UTCTime -> IO Bool
holdBalance db (RegistrationId rid) seen (RegistrationId holder) now =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_balances SET reserved_by = ?, updated_at = ? WHERE registration_id = ? AND amount = ? AND ? > 0 AND reserved_by IS NULL")
      (holder, now, rid, seen, seen)

-- | The holder's hold spent and cleared, where this holder holds the balance and it still has the cents, above 0.
spendHeldBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO Bool
spendHeldBalance db (RegistrationId holder) used now =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_balances SET amount = amount - ?, reserved_by = NULL, updated_at = ? WHERE reserved_by = ? AND amount >= ? AND ? > 0")
      (used, truncateToSecond now, holder, used, used)

releaseHeldBalance :: DB.Connection -> RegistrationId -> UTCTime -> IO Bool
releaseHeldBalance db (RegistrationId holder) now =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_balances SET reserved_by = NULL, updated_at = ? WHERE reserved_by = ?")
      (truncateToSecond now, holder)

-- | All of the balance, where it is the cents given, above 0, and nothing holds it; refunded counts every refund.
refundBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO Bool
refundBalance db (RegistrationId rid) cents now' =
  (> 0)
    <$> executeChanging
      db
      ( prefixQuery
          [sql|
        UPDATE @name_balances SET amount = 0, refunded = refunded + ?, refunded_at = ?, updated_at = ?
        WHERE registration_id = ? AND amount = ? AND amount > 0 AND reserved_by IS NULL
      |]
      )
      (cents, now, now, rid, cents)
  where
    now = truncateToSecond now'

-- | A name invoice with its order, its invoice row and its payment row.
lateFrom :: Query
lateFrom =
  mkQuery $
    " FROM @name_invoices ni"
      <> " JOIN @name_registrations r ON r.registration_id = ni.registration_id"
      <> " JOIN @invoices i ON i.invoice_id = ni.invoice_id"
      <> " JOIN @payments p ON p.invoice_id = ni.invoice_id"

-- | Its order closed unpaid, whatever the invoice's status, since settlement queues only a live order; takes the unpaid phase.
closedUnpaid :: Query
closedUnpaid = " r.phase = ? AND r.live = 0"

-- | Its order paid by it, with crypto received above what it asked; takes the unpaid phase. The last term matches the partial index of
-- overpaid payments, so the poller's pass reads only those.
overpaidOrder :: Query
overpaidOrder = " r.phase <> ? AND" <> cryptoInvoice <> " AND p.received_units > p.invoiced_units"

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
  " CASE WHEN" <> uncreditedMoney <> " THEN " <> pendingLiteral LPUncredited
    <> " WHEN COALESCE(p.amount, 0) = 0 AND (p.crypto_paid IS NOT NULL OR p.paid_in_full = 1) THEN " <> pendingLiteral LPConfirming
    <> " WHEN" <> cardInvoice <> " AND COALESCE(p.amount, 0) > p.confirmed_amount THEN " <> pendingLiteral LPUnconfirmed
    <> " WHEN p.received_units > COALESCE(p.confirmed_units, 0) THEN " <> pendingLiteral LPUnconfirmed
    <> " WHEN" <> cryptoInvoice <> " AND p.received_units IS NULL AND COALESCE(p.amount, 0) > 0 THEN " <> pendingLiteral LPUncounted
    <> " END"

-- | Why an overpaid invoice's extra is not all credited: confirmed extra not credited yet, or received units above the confirmed ones,
-- which overpaidOrder already holds above the invoiced ones.
overpaidPendingCase :: Query
overpaidPendingCase =
  " CASE WHEN" <> uncreditedExtra <> " THEN " <> pendingLiteral LPUncredited
    <> " WHEN p.received_units > COALESCE(p.confirmed_units, 0) THEN " <> pendingLiteral LPUnconfirmed
    <> " END"

pendingLiteral :: LatePending -> Query
pendingLiteral p = fromString $ "'" <> T.unpack (textEncode p) <> "'"

-- | What an alert names as received: units for crypto, and cents for card or for crypto that could not be counted in units.
receivedFigure :: Query
receivedFigure = " CASE WHEN" <> cardInvoice <> " THEN p.amount ELSE COALESCE(p.received_units, p.amount) END"

lateColumns :: Query
lateColumns =
  "SELECT ni.invoice_id, ni.registration_id, r.phase, r.live, i.payment_crypto_currency, ni.late_support, COALESCE(p.amount, 0), p.confirmed_amount,"
    <> " p.received_units, p.confirmed_units, p.invoice_rate,"
    <> creditedTotal "units"
    <> ","
    <> creditedTotal "cents"
    <> mkQuery ", (SELECT COUNT(*) FROM @name_late_credits c WHERE c.invoice_id = ni.invoice_id), p.created_at, i.amount, p.invoiced_units"

type LateRow = (Text, Text, NamePhase, BoolInt, Maybe Text, Maybe LateSupport, Int, Int, Maybe Int64, Maybe Int64) :. (Maybe Rate, Int64, Int, Int, UTCTime, Int, Maybe Int64)

toLateState :: LateRow -> Either Text LateCreditState
toLateState
  ( (inv, reg, lsPhase, BI lsLive, coin_, lsSupport, lsReceivedCents, confirmedCents, lfReceivedUnits, lfConfirmedUnits)
      :. (lfInvoiceRate, lfCreditedUnits, lsCreditedCents, lsCredits, lsPaymentCreatedAt, lsInvoiceAmount, lfInvoicedUnits)
    ) = do
    lsFigures <- case coin_ of
      Nothing -> Right LFCard {lfConfirmedCents = confirmedCents}
      Just c -> (\lfCoin -> LFCrypto {lfCoin, lfReceivedUnits, lfConfirmedUnits, lfInvoicedUnits, lfInvoiceRate, lfCreditedUnits}) <$> note ("invoices.payment_crypto_currency: " <> c) (textToCryptoCurrency c)
    pure LateCreditState {lsInvoice = InvoiceId inv, lsRegistration = RegistrationId reg, lsPhase, lsLive, lsSupport, lsReceivedCents, lsFigures, lsInvoiceAmount, lsCreditedCents, lsCredits, lsPaymentCreatedAt}

-- | Nothing without a payment row.
lateCreditState :: DB.Connection -> InvoiceId -> IO (Maybe LateCreditState)
lateCreditState db (InvoiceId invId) = firstDecoded toLateState $ DB.query db (lateColumns <> lateFrom <> " WHERE ni.invoice_id = ?") (Only invId)

-- | Run after the invoice's lock. The order's balance is credited only when the credit's row is new, so a taken credit number changes neither.
creditLatePayment :: DB.Connection -> LateCredit -> UTCTime -> IO Bool
creditLatePayment db LateCredit {lcInvoice = InvoiceId invId, lcCreditNo, lcRegistration = regId@(RegistrationId rid), lcAmount} now' = do
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
  when (inserted > 0) $ creditBalance db regId (creditCents lcAmount) now
  pure $ inserted > 0
  where
    now = truncateToSecond now'
    (units, invoiceRate, currentRate, rateUsed) = case lcAmount of
      CACard _ -> (Nothing, Nothing, Nothing, Nothing)
      CACrypto {caUnits, caInvoiceRate, caCurrentRate, caRateUsed} -> (Just caUnits, Just caInvoiceRate, Just caCurrentRate, Just caRateUsed)

-- | Why the closed order's money on the invoice is not all credited; the one rule of the pending log and the alert.
latePending :: DB.Connection -> InvoiceId -> IO (Maybe LatePending)
latePending db (InvoiceId invId) =
  maybeFirstRow' Nothing fromOnly $
    DB.query db ("SELECT" <> pendingCase <> lateFrom <> " WHERE ni.invoice_id = ? AND" <> closedUnpaid) (invId, PUnpaid)

-- | Nothing once all of the closed order's money is credited, whatever support reason was given before.
lateWait :: DB.Connection -> InvoiceId -> IO (Maybe LateWait)
lateWait db (InvoiceId invId) =
  maybeFirstRow' Nothing waitOf $
    DB.query db ("SELECT" <> pendingCase <> ", ni.late_support" <> lateFrom <> " WHERE ni.invoice_id = ? AND" <> closedUnpaid) (invId, PUnpaid)
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
uncreditedLateInvoices db = (<>) <$> uncredited closedUnpaid uncreditedMoney <*> uncredited overpaidOrder uncreditedExtra
  where
    uncredited order money =
      map (\(invId, ref) -> (InvoiceId invId, ref))
        <$> DB.query
          db
          ("SELECT ni.invoice_id, ni.provider_ref" <> lateFrom <> " WHERE" <> order <> " AND" <> money <> " AND (ni.late_support IS NULL OR ni.late_support <> ?) ORDER BY p.created_at, ni.invoice_id")
          (PUnpaid, LSRateFloor)

-- | Pending money of closed orders, then of overpaid invoices, whose payment row was created before the cutoff, never alerted, or alerted
-- for less money than now received.
lateAlertDue :: DB.Connection -> UTCTime -> IO [LateAlert]
lateAlertDue db cutoff = (<>) <$> alertsDue closedUnpaid pendingCase <*> alertsDue overpaidOrder overpaidPendingCase
  where
    alertsDue order pending =
      DB.query
        db
        ( lateColumns
            <> ","
            <> pending
            <> ","
            <> receivedFigure
            <> ", ni.late_alerted_at, ni.late_alerted_units, ni.provider_ref"
            <> lateFrom
            <> " WHERE"
            <> order
            <> " AND p.created_at < ? AND"
            <> pending
            <> " IS NOT NULL AND (ni.late_alerted_at IS NULL OR"
            <> receivedFigure
            <> " > COALESCE(ni.late_alerted_units, -1)) ORDER BY p.created_at, ni.invoice_id"
        )
        (PUnpaid, truncateToSecond cutoff)
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
      (truncateToSecond now, laReceived, support, invId, laAlertedAt, laAlertedReceived)

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
      (Binary npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, truncateToSecond <$> npcExpiresAt, truncateToSecond now, BI (npcIssuer == IssuedInGroup))
  if inserted > 0 then Right <$> insertedRowId db else pure $ Left CodeHashTaken

prepaidCodeColumns :: Query
prepaidCodeColumns =
  prefixQuery
    [sql|
    SELECT name_prepaid_code_id, code_hash, min_length, years, code_payment_status, reserved_by, reserved_at, spent_at, revoked_at, expires_at, created_at
    FROM @name_prepaid_codes
  |]

type PrepaidCodeRow = (Int64, Binary ByteString, Int, Int, BadgeCodePaymentStatus, Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime) :. Only UTCTime

toPrepaidCode :: PrepaidCodeRow -> PrepaidCode
toPrepaidCode ((pcId, Binary pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt) :. Only pcCreatedAt) =
  PrepaidCode {pcId, pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcReservedBy = RegistrationId <$> reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt, pcCreatedAt}

groupNameCodesIssuedSince :: DB.Connection -> UTCTime -> IO Int
groupNameCodesIssuedSince db since = maybeFirstRow' 0 fromOnly (DB.query db (mkQuery "SELECT COUNT(*) FROM @name_prepaid_codes WHERE created_at >= ? AND issued_in_group = 1") (Only since))

getPrepaidCode :: DB.Connection -> CodeHash -> IO (Maybe PrepaidCode)
getPrepaidCode db codeHash =
  maybeFirstRow toPrepaidCode $ DB.query db (prepaidCodeColumns <> " WHERE code_hash = ?") (Only $ Binary codeHash)

getPrepaidCodeById :: DB.Connection -> CodeId -> IO PrepaidCode
getPrepaidCodeById db codeId =
  maybeFirstRow toPrepaidCode (DB.query db (prepaidCodeColumns <> " WHERE name_prepaid_code_id = ?") (Only codeId))
    >>= maybe (E.throwIO $ BrokenInvariant $ "the prepaid code " <> tshow codeId <> " of an order is missing") pure

-- | Only an unused code: not reserved, spent or revoked, paid or free, and not expired.
reservePrepaidCode :: DB.Connection -> CodeId -> RegistrationId -> UTCTime -> IO Bool
reservePrepaidCode db codeId (RegistrationId rid) now' =
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
  where
    now = truncateToSecond now'

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
spendLandedPrepaidCode db codeId (RegistrationId rid) now' =
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
  where
    now = truncateToSecond now'

-- | A reserved code can be revoked, and its order keeps it; a spent one cannot. With the code as it reads after.
revokePrepaidCode :: DB.Connection -> CodeHash -> UTCTime -> IO (RevokeResult, Maybe PrepaidCode)
revokePrepaidCode db codeHash now = do
  revoked <-
    executeChanging
      db
      (mkQuery "UPDATE @name_prepaid_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND spent_at IS NULL")
      (truncateToSecond now, Binary codeHash)
  (\code -> (maybe NoSuchCode (result revoked) code, code)) <$> getPrepaidCode db codeHash
  where
    result revoked PrepaidCode {pcId, pcRevokedAt}
      | revoked > 0 = Revoked pcId
      | isJust pcRevokedAt = AlreadyRevoked pcId
      | otherwise = AlreadyRedeemed

txKind :: TxFor -> (Text, Maybe Text, Maybe RecordEditId)
txKind = \case
  TxCommit (RegistrationId rid) -> ("commit", Just rid, Nothing)
  TxAccept (RegistrationId rid) -> ("accept", Just rid, Nothing)
  TxRegister (RegistrationId rid) -> ("register", Just rid, Nothing)
  TxEdit editId -> ("edit", Nothing, Just editId)

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
    (rid, editId, kind, Binary ntxRaw, txHashField ntxHash, int64 ntxNonce, truncateToSecond now, TSPending)
  insertedRowId db

-- | Moves a transaction on from the status the caller read, so an outcome is never overwritten unseen; False when it is not in that status.
recordTransactionOutcome :: DB.Connection -> TxHash -> TxStatus -> TxStatus -> Maybe Word64 -> IO Bool
recordTransactionOutcome db hash from status block =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_transactions SET status = ?, mined_block = ? WHERE tx_hash = ? AND status = ?")
      (status, int64 <$> block, txHashField hash, from)

transactionColumns :: Query
transactionColumns =
  prefixQuery
    [sql|
    SELECT name_transaction_id, kind, registration_id, name_record_edit_id, raw, tx_hash, registrar_nonce, sent_at, mined_block, status
    FROM @name_transactions
  |]

type TransactionRow = (Int64, Text, Maybe Text, Maybe Int64, Binary ByteString, Binary ByteString, Int64, UTCTime, Maybe Int64, TxStatus)

toTransaction :: TransactionRow -> Either Text NameTransaction
toTransaction (ntId, kind, rid_, editId_, Binary ntRaw, hash, nonce, ntSentAt, block, ntStatus) = do
  ntFor <- case (kind, rid_, editId_) of
    ("commit", Just rid, Nothing) -> Right $ TxCommit $ RegistrationId rid
    ("accept", Just rid, Nothing) -> Right $ TxAccept $ RegistrationId rid
    ("register", Just rid, Nothing) -> Right $ TxRegister $ RegistrationId rid
    ("edit", Nothing, Just editId) -> Right $ TxEdit editId
    _ -> Left $ "name_transactions.kind: " <> kind
  pure NameTransaction {ntId, ntFor, ntRaw, ntHash = txHash hash, ntNonce = word64 nonce, ntSentAt, ntMinedBlock = word64 <$> block, ntStatus}

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

pendingEdit :: Text
pendingEdit = "pending"

minedEdit :: Text
minedEdit = "mined"

revertedEdit :: Text
revertedEdit = "reverted"

-- | Nothing when a pending or mined edit already starts at the owner's first nonce.
insertRecordEdit :: DB.Connection -> NewRecordEdit -> UTCTime -> IO (Maybe RecordEditId)
insertRecordEdit db NewRecordEdit {nreLabelHash = LabelHash lh, nreOwner, nreChanges, nreBodyHash} now = do
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
      (Binary lh, Binary $ unAddress nreOwner, int64 $ scNonce $ L.head nreChanges, Binary nreBodyHash, pendingEdit, truncateToSecond now)
  if inserted > 0
    then do
      editId <- insertedRowId db
      mapM_ (insertChange editId) nreChanges
      pure $ Just editId
    else pure Nothing
  where
    insertChange editId SignedChange {scChange, scNonce, scDeadline, scSignature} =
      DB.execute
        db
        (mkQuery "INSERT INTO @name_record_changes (name_record_edit_id, nonce, record_key, record_value, deadline, signature) VALUES (?,?,?,?,?,?)")
        (editId, int64 scNonce, key, value, int64 scDeadline, Binary $ serializeSignature scSignature)
      where
        (key, value) = case scChange of
          RCSetText k v -> (Just k, Just v)
          RCClear -> (Nothing, Nothing)

recordEditColumns :: Query
recordEditColumns =
  prefixQuery
    [sql|
    SELECT name_record_edit_id, label_hash, owner, body_hash, state, revert, mined_tx, created_at
    FROM @name_record_edits
  |]

type RecordEditRow = (Int64, Binary ByteString, Binary ByteString, Binary ByteString, Text, Maybe Text, Maybe (Binary ByteString), UTCTime)

type ChangeRow = (Int64, Maybe Text, Maybe Text, Int64, Binary ByteString)

-- | An edit's row with its changes, in nonce order.
readRecordEdit :: DB.Connection -> RecordEditRow -> IO RecordEdit
readRecordEdit db row@(reId, _, _, _, _, _, _, _) = do
  changes <- DB.query db (mkQuery "SELECT nonce, record_key, record_value, deadline, signature FROM @name_record_changes WHERE name_record_edit_id = ? ORDER BY nonce") (Only reId)
  either (E.throwIO . StoreDecodeError) pure $ toRecordEdit row changes

toRecordEdit :: RecordEditRow -> [ChangeRow] -> Either Text RecordEdit
toRecordEdit (reId, Binary lh, Binary owner, Binary reBodyHash, state, revert, minedTx, reCreatedAt) changeRows = do
  reOwner <- decodeAddress owner
  reChanges <- note ("name_record_edits changes: " <> tshow reId) . L.nonEmpty =<< mapM toChange changeRows
  reState <- decodeState
  pure RecordEdit {reId, reLabelHash = LabelHash lh, reOwner, reChanges, reBodyHash, reState, reCreatedAt}
  where
    toChange (nonce, key_, value_, deadline, Binary sig) = do
      scSignature <- decodeSignature sig
      scChange <- case (key_, value_) of
        (Just key, Just value) -> Right $ RCSetText key value
        (Nothing, Nothing) -> Right RCClear
        _ -> Left $ "name_record_changes record: " <> tshow reId
      pure SignedChange {scChange, scNonce = word64 nonce, scDeadline = word64 deadline, scSignature}
    decodeState
      | state == pendingEdit = Right ESPending
      | state == minedEdit = Right $ ESMined $ txHash <$> minedTx
      | state == revertedEdit = Right $ ESReverted revert
      | otherwise = Left $ "name_record_edits.state: " <> state

firstRecordEdit :: DB.Connection -> IO [RecordEditRow] -> IO (Maybe RecordEdit)
firstRecordEdit db rows = rows >>= mapM (readRecordEdit db) . listToMaybe

getRecordEdit :: DB.Connection -> RecordEditId -> IO (Maybe RecordEdit)
getRecordEdit db editId =
  firstRecordEdit db $ DB.query db (recordEditColumns <> " WHERE name_record_edit_id = ?") (Only editId)

-- | The newest edit sent with this signed body.
getRecordEditByBody :: DB.Connection -> BodyHash -> IO (Maybe RecordEdit)
getRecordEditByBody db bodyHash =
  firstRecordEdit db $
    DB.query db (recordEditColumns <> " WHERE body_hash = ? ORDER BY name_record_edit_id DESC LIMIT 1") (Only $ Binary bodyHash)

-- | Settles a pending edit; False when it was settled already.
settleRecordEdit :: DB.Connection -> RecordEditId -> EditState -> IO Bool
settleRecordEdit db editId state =
  (> 0)
    <$> executeChanging
      db
      (mkQuery "UPDATE @name_record_edits SET state = ?, revert = ?, mined_tx = ? WHERE name_record_edit_id = ? AND state = ?")
      (stateText, revert, txHashField <$> minedTx, editId, pendingEdit)
  where
    (stateText, revert, minedTx) = case state of
      ESPending -> (pendingEdit, Nothing, Nothing)
      ESMined tx -> (minedEdit, Nothing, tx)
      ESReverted name -> (revertedEdit, name, Nothing)

pendingEditIds :: DB.Connection -> IO [RecordEditId]
pendingEditIds db = map fromOnly <$> DB.query db (mkQuery "SELECT name_record_edit_id FROM @name_record_edits WHERE state = ? ORDER BY name_record_edit_id") (Only pendingEdit)

deleteUnsentEdit :: DB.Connection -> RecordEditId -> IO ()
deleteUnsentEdit db editId =
  DB.execute
    db
    (mkQuery "DELETE FROM @name_record_edits WHERE name_record_edit_id = ? AND state = ? AND NOT EXISTS (SELECT 1 FROM @name_transactions WHERE name_record_edit_id = ?)")
    (editId, pendingEdit, editId)

-- | When each edit relayed for the name since the given time was sent, oldest first.
relayedEditTimes :: DB.Connection -> LabelHash -> UTCTime -> IO [UTCTime]
relayedEditTimes db (LabelHash lh) since =
  map fromOnly
    <$> DB.query
      db
      (mkQuery "SELECT created_at FROM @name_record_edits WHERE label_hash = ? AND created_at > ? ORDER BY created_at")
      (Binary lh, truncateToSecond since)

getTermsText :: DB.Connection -> TermsHash -> IO (Maybe ByteString)
getTermsText db termsHash =
  maybeFirstRow (\(Only (Binary text)) -> text) $
    DB.query db (mkQuery "SELECT terms_text FROM @name_terms WHERE terms_hash = ?") (Only $ Binary termsHash)

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
    (Binary termsHash, Binary $ encodeUtf8 text)

namesCounts :: DB.Connection -> IO NamesCounts
namesCounts db = do
  ncPhases <- DB.query_ db (mkQuery "SELECT phase, COUNT(*) FROM @name_registrations GROUP BY phase")
  DB.query
    db
    ( prefixQuery
        [sql|
      SELECT
        (SELECT MIN(updated_at) FROM @name_registrations WHERE phase IN (?,?,?) OR (phase = ? AND label IS NOT NULL)),
        (SELECT COUNT(DISTINCT registrar_nonce) FROM @name_transactions WHERE status = ?),
        (SELECT MIN(sent_at) FROM @name_transactions WHERE status = ?),
        (SELECT COUNT(*) FROM @name_invoices ni JOIN @invoices i ON i.invoice_id = ni.invoice_id WHERE i.status = ?),
        (SELECT COUNT(*) FROM @name_balances WHERE reserved_by IS NOT NULL),
        (SELECT COALESCE(SUM(amount), 0) FROM @name_balances WHERE reserved_by IS NOT NULL),
        (SELECT COUNT(*) FROM @name_record_edits WHERE state = ?),
        (SELECT MIN(created_at) FROM @name_record_edits WHERE state = ?)
    |]
    )
    (PQueued, PCommitting, PRegistering, PCommitted, TSPending, TSPending, invoiceStatusText ISOpen, pendingEdit, pendingEdit)
    >>= \case
      [(ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncHeldBalances, ncHeldCents, ncPendingEdits, ncOldestEdit)] ->
        pure NamesCounts {ncPhases, ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncHeldBalances, ncHeldCents, ncPendingEdits, ncOldestEdit}
      _ -> E.throwIO $ StoreDecodeError "names counts: not one row"

int64 :: Word64 -> Int64
int64 = fromIntegral

txHash :: Binary ByteString -> TxHash
txHash (Binary h) = TxHash h

txHashField :: TxHash -> Binary ByteString
txHashField (TxHash h) = Binary h

word64 :: Int64 -> Word64
word64 = fromIntegral

-- | Address has no exported constructor; its parser takes lowercase hex without the EIP-55 check.
decodeAddress :: ByteString -> Either Text Address
decodeAddress = note "address column" . addressFromBytes

decodeSignature :: ByteString -> Either Text RecoverableSignature
decodeSignature bs = either (Left . ("signature column: " <>) . tshow) Right $ parseSignature $ hex bs

decodeAll :: (row -> Either Text a) -> [row] -> IO [a]
decodeAll decode = either (E.throwIO . StoreDecodeError) pure . traverse decode

firstDecoded :: (row -> Either Text a) -> IO [row] -> IO (Maybe a)
firstDecoded decode rows = rows >>= fmap listToMaybe . decodeAll decode . take 1
