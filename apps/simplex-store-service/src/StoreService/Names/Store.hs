{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module StoreService.Names.Store
  ( NamePhase (..),
    RegistrationPayment (..),
    NameReveal (..),
    NameRegistration (..),
    NewRegistration (..),
    PaidOrder (..),
    RegistrationRefused (..),
    VersionWrite (..),
    LiveOrder (..),
    OrderTerms (..),
    HeldBalance (..),
    Balance (..),
    BalanceHold (..),
    BalanceLink (..),
    LateCredit (..),
    LateCreditState (..),
    LatePending (..),
    LateAlert (..),
    CleanupKept (..),
    PrepaidCode (..),
    NewPrepaidCode (..),
    CodeHashTaken (..),
    TxFor (..),
    TxStatus (..),
    NameTransaction (..),
    NewTransaction (..),
    RecordChange (..),
    EditState (..),
    RecordEdit (..),
    NewRecordEdit (..),
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
    overdueHolder,
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
    insertPrepaidCode,
    getPrepaidCode,
    getPrepaidCodeById,
    reservePrepaidCode,
    releasePrepaidCode,
    spendPrepaidCode,
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
    getRecordEditByNonce,
    settleRecordEdit,
    pendingEditIds,
    deleteUnsentEdit,
    relayedEditTimes,
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Crypto.Number.Serialize (i2ospOf_)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
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
import Simplex.Messaging.Encoding.String (TextEncoding (..), strDecode)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (maybeFirstRow, tshow)
import StoreService.Names.Abi (hex)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), parseSignature)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (StoreDecodeError (..), executeChanging, invoiceStatusText, lockInvoice, newInvoiceId, truncateToSecond, unfundedQuery)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

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

-- | RPBalance: an order paid wholly from another order's balance, with neither invoice nor code.
data RegistrationPayment
  = RPInvoice ServicePaymentMethod InvoiceId
  | RPCode Int64
  | RPBalance
  deriving (Eq, Show)

-- | The name, links and owner's intent, all set by an accepted reveal and cleared together.
data NameReveal = NameReveal
  { rvLabel :: Text,
    rvSecret :: ByteString,
    rvContactLink :: Maybe Text,
    rvChannelLink :: Maybe Text,
    rvNonce :: Word64,
    rvDeadline :: Word64,
    rvSignature :: RecoverableSignature
  }
  deriving (Eq, Show)

data NameRegistration = NameRegistration
  { nrId :: RegistrationId,
    nrVersion :: Int64,
    nrLabelHash :: LabelHash,
    nrLength :: Int,
    nrDuration :: Word64,
    nrOwner :: Address,
    nrCommitment :: ByteString,
    nrSealed :: Text,
    nrPhase :: NamePhase,
    nrYears :: Int,
    nrPrice :: Int,
    nrPayment :: RegistrationPayment,
    nrBalanceSource :: Maybe RegistrationId,
    nrBalanceUsed :: Int,
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
    nrOrderHash :: ByteString,
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
    nrgYears :: Int
  }

-- | The invoice row is inserted by the caller, in the same transaction and before the registration.
data PaidOrder = PaidOrder
  { poMethod :: ServicePaymentMethod,
    poPrice :: Int,
    poInvoiceId :: InvoiceId,
    poProviderRef :: Text
  }

-- | RRBalanceChanged: the source's balance no longer has the amount seen, or another order holds it.
data RegistrationRefused = RRLiveOrder | RRCodeNotReservable | RRBalanceChanged
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

-- | What an order pays with, as its hash covers it. OTBalance: the paying order, the price, the balance seen, and the method of a difference.
data OrderTerms = OTInvoice ServicePaymentMethod Int | OTCode ByteString | OTBalance RegistrationId Int Int (Maybe ServicePaymentMethod)

-- | The balance a difference order holds while its invoice is open: the paying order, and all of its amount.
data HeldBalance = HeldBalance {hbSource :: RegistrationId, hbAmount :: Int}

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
data BalanceHold = BalanceHold
  { bhHolder :: RegistrationId,
    bhUntil :: UTCTime,
    bhOverdue :: Bool
  }
  deriving (Eq, Show)

-- | An order the money of a balance came through. lkPaid and lkFromBalance count only an order that went past unpaid,
-- since closing an unpaid one released its source's balance.
data BalanceLink = BalanceLink
  { lkRegistration :: RegistrationId,
    lkPayment :: RegistrationPayment,
    lkPaid :: Int,
    lkFromBalance :: Int,
    lkLateCredits :: [LateCredit]
  }
  deriving (Eq, Show)

-- | Money paid on a name invoice after its order closed, credited to that order's balance. Rates are decimal text; card money has no units or rates.
data LateCredit = LateCredit
  { lcInvoice :: InvoiceId,
    lcCreditNo :: Int,
    lcRegistration :: RegistrationId,
    lcUnits :: Maybe Int64,
    lcInvoiceRate :: Maybe Text,
    lcCurrentRate :: Maybe Text,
    lcRateUsed :: Maybe Text,
    lcCents :: Int
  }
  deriving (Eq, Show)

-- | A name invoice's payment figures, what was credited from it, and its order's phase and live flag. Card money is counted in cents, crypto in units.
data LateCreditState = LateCreditState
  { lsInvoice :: InvoiceId,
    lsRegistration :: RegistrationId,
    lsPhase :: NamePhase,
    lsLive :: Bool,
    lsCard :: Bool,
    lsReceivedCents :: Int,
    lsConfirmedCents :: Int,
    lsReceivedUnits :: Maybe Int64,
    lsConfirmedUnits :: Maybe Int64,
    lsInvoiceRate :: Maybe Text,
    lsCreditedUnits :: Int64,
    lsCreditedCents :: Int,
    lsCredits :: Int,
    lsPaymentCreatedAt :: UTCTime
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

-- | A closed order's pending money to alert on, with the received figure the alert names and the one the last alert named.
data LateAlert = LateAlert
  { laState :: LateCreditState,
    laPending :: LatePending,
    laReceived :: Maybe Int64,
    laAlertedAt :: Maybe UTCTime,
    laAlertedReceived :: Maybe Int64
  }
  deriving (Eq, Show)

-- | Why the cleanup keeps an unpaid order: a payment was recorded for it, or it holds the balance of the given order.
data CleanupKept = CKPaid | CKHolder RegistrationId
  deriving (Eq, Show)

data PrepaidCode = PrepaidCode
  { pcId :: Int64,
    pcCodeHash :: ByteString,
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
  { npcCodeHash :: ByteString,
    npcMinLength :: Int,
    npcYears :: Int,
    npcPaymentStatus :: BadgeCodePaymentStatus,
    npcExpiresAt :: Maybe UTCTime
  }

data CodeHashTaken = CodeHashTaken
  deriving (Eq, Show)

data TxFor = TxCommit RegistrationId | TxRegister RegistrationId | TxEdit Int64
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

data RecordEdit = RecordEdit
  { reId :: Int64,
    reLabelHash :: LabelHash,
    reOwner :: Address,
    reNonce :: Word64,
    reChange :: RecordChange,
    reDeadline :: Word64,
    reSignature :: RecoverableSignature,
    reBodyHash :: ByteString,
    reState :: EditState,
    reCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

data NewRecordEdit = NewRecordEdit
  { nreLabelHash :: LabelHash,
    nreOwner :: Address,
    nreNonce :: Word64,
    nreChange :: RecordChange,
    nreDeadline :: Word64,
    nreSignature :: RecoverableSignature,
    nreBodyHash :: ByteString
  }

findLimit :: Int
findLimit = 50

initialVersion :: Int64
initialVersion = 1

orderHashFieldBytes :: Int
orderHashFieldBytes = 4

-- | Each field is length-prefixed, so no two different orders encode the same.
orderHash :: Address -> OrderFields -> OrderTerms -> ByteString
orderHash owner OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} terms =
  sha256Hash . B.concat . map field $ [tag, lh, num ofLength, num ofDuration, unAddress owner, ofCommitment, encodeUtf8 ofSealed] <> payment
  where
    (tag, payment) = case terms of
      OTInvoice method price -> ("name-invoice", [encodeUtf8 $ methodText method, num price])
      OTCode codeHash -> ("name-code", [codeHash])
      OTBalance (RegistrationId source) price seen method -> ("name-balance", [encodeUtf8 source, num price, num seen, maybe "" (encodeUtf8 . methodText) method])
    field bs = i2ospOf_ orderHashFieldBytes (toInteger $ B.length bs) <> bs
    num :: Show a => a -> ByteString
    num = encodeUtf8 . tshow

newRegistrationId :: IO RegistrationId
newRegistrationId = (\(InvoiceId t) -> RegistrationId t) <$> newInvoiceId

methodText :: ServicePaymentMethod -> Text
methodText = \case
  SPMCard CPStripe -> "card"
  SPMCrypto CCBtc -> "btc"
  SPMCrypto CCXmr -> "xmr"

textMethod :: Text -> Maybe ServicePaymentMethod
textMethod t = lookup t [(methodText m, m) | m <- [SPMCard CPStripe, SPMCrypto CCBtc, SPMCrypto CCXmr]]

-- | A difference order's hold is written after the registration, so the registration's row is locked before the balance's.
createPaidRegistration :: DB.Connection -> NewRegistration -> OrderTerms -> PaidOrder -> Maybe HeldBalance -> UTCTime -> IO (Either RegistrationRefused ())
createPaidRegistration db new@NewRegistration {nrgId = regId@(RegistrationId rid), nrgOwner, nrgOrder} terms PaidOrder {poMethod, poPrice, poInvoiceId = InvoiceId invId, poProviderRef} held now' = do
  let now = truncateToSecond now'
      fromBalance = (\HeldBalance {hbSource, hbAmount} -> (hbSource, hbAmount)) <$> held
  inserted <- insertRegistration db new PUnpaid poPrice (Just poMethod) Nothing fromBalance (orderHash nrgOwner nrgOrder terms) now
  if not inserted
    then pure $ Left RRLiveOrder
    else do
      DB.execute
        db
        "INSERT INTO sx_badge_service_name_invoices (invoice_id, registration_id, provider_ref, created_at) VALUES (?,?,?,?)"
        (invId, rid, poProviderRef, now)
      held' <- maybe (pure True) (\HeldBalance {hbSource, hbAmount} -> holdBalance db hbSource hbAmount regId now) held
      if held' then pure $ Right () else Left RRBalanceChanged <$ deleteRegistration db regId

-- | Reserving the code is the conditional update of the transaction that creates the registration.
createCodeRegistration :: DB.Connection -> NewRegistration -> PrepaidCode -> UTCTime -> IO (Either RegistrationRefused ())
createCodeRegistration db new@NewRegistration {nrgId = regId, nrgOwner, nrgOrder} PrepaidCode {pcId, pcCodeHash} now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PQueued 0 Nothing (Just pcId) Nothing (orderHash nrgOwner nrgOrder (OTCode pcCodeHash)) now
  if not inserted
    then pure $ Left RRLiveOrder
    else
      reservePrepaidCode db pcId regId now >>= \case
        True -> pure $ Right ()
        False -> Left RRCodeNotReservable <$ deleteRegistration db regId

-- | A queued order whose price is spent from the source's balance in the same transaction, where its amount is the one seen and nothing holds it.
createBalanceRegistration :: DB.Connection -> NewRegistration -> RegistrationId -> Int -> Int -> UTCTime -> IO (Either RegistrationRefused ())
createBalanceRegistration db new@NewRegistration {nrgId, nrgOwner, nrgOrder} source price seen now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PQueued price Nothing Nothing (Just (source, price)) (orderHash nrgOwner nrgOrder (OTBalance source price seen Nothing)) now
  if not inserted
    then pure $ Left RRLiveOrder
    else
      spendBalance db source price seen now >>= \case
        True -> pure $ Right ()
        False -> Left RRBalanceChanged <$ deleteRegistration db nrgId

-- | The name invoice row goes with it.
deleteRegistration :: DB.Connection -> RegistrationId -> IO ()
deleteRegistration db (RegistrationId rid) = DB.execute db "DELETE FROM sx_badge_service_name_registrations WHERE registration_id = ?" (Only rid)

-- | False when the registration would be a second live one for its commitment, or for its owner and label hash.
-- fromBalance: the order whose balance pays, and the cents it pays.
insertRegistration :: DB.Connection -> NewRegistration -> NamePhase -> Int -> Maybe ServicePaymentMethod -> Maybe Int64 -> Maybe (RegistrationId, Int) -> ByteString -> UTCTime -> IO Bool
insertRegistration db NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder, nrgYears} phase price method codeId fromBalance hash now =
  (> 0)
    <$> executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_registrations
          (registration_id, version, label_hash, length, duration, owner, commitment, sealed, phase, years,
           price, method, name_prepaid_code_id, live, order_hash, moves, created_at, updated_at, balance_source, balance_used)
        VALUES (?,?,?,?,?,?,?,?,?,?, ?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      ( (rid, initialVersion, Binary lh, ofLength, int64 ofDuration, Binary $ unAddress nrgOwner, Binary ofCommitment, ofSealed, phase, nrgYears)
          :. (price, methodText <$> method, codeId, BI True, Binary hash, 0 :: Int, now, now, (\(RegistrationId s, _) -> s) <$> fromBalance, maybe 0 snd fromBalance)
      )
  where
    OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} = nrgOrder

registrationColumns :: Query
registrationColumns =
  [sql|
    SELECT r.registration_id, r.version, r.label_hash, r.length, r.duration, r.owner, r.commitment, r.sealed, r.phase, r.years,
      r.price, r.method, ni.invoice_id, r.name_prepaid_code_id, r.label, r.secret, r.contact_link, r.channel_link, r.intent_nonce, r.intent_deadline,
      r.intent_signature, r.commit_tx, r.commit_block, r.register_tx, r.register_block, r.reveal_after, r.reveal_before, r.expires_at, r.registered_at,
      r.failed_at, r.error, r.live, r.order_hash, r.moves, r.links_refused_at, r.created_at, r.updated_at, r.balance_source, r.balance_used
  |]

registrationFrom :: Query
registrationFrom =
  " "
    <> [sql|
    FROM sx_badge_service_name_registrations r
    LEFT JOIN sx_badge_service_name_invoices ni ON ni.registration_id = r.registration_id
  |]

selectRegistrations :: Query
selectRegistrations = registrationColumns <> registrationFrom

type RegistrationRow =
  (Text, Int64, Binary ByteString, Int, Int64, Binary ByteString, Binary ByteString, Text, NamePhase, Int)
    :. (Int, Maybe Text, Maybe Text, Maybe Int64, Maybe Text, Maybe (Binary ByteString), Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)
    :. (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe Int64, Maybe (Binary ByteString), Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime)
    :. (Maybe UTCTime, Maybe Text, BoolInt, Binary ByteString, Int, Maybe UTCTime, UTCTime, UTCTime, Maybe Text, Int)

toRegistration :: RegistrationRow -> Either Text NameRegistration
toRegistration
  ( (rid, nrVersion, Binary lh, nrLength, duration, Binary owner, Binary nrCommitment, nrSealed, nrPhase, nrYears)
      :. (nrPrice, method_, invoiceId_, codeId_, label_, secret_, nrContactLink, nrChannelLink, nonce_, deadline_)
      :. (signature_, commitTx, commitBlock, registerTx, registerBlock, revealAfter, revealBefore, expiresAt, nrRegisteredAt)
      :. (nrFailedAt, nrError, BI nrLive, Binary nrOrderHash, nrMoves, nrLinksRefusedAt, nrCreatedAt, nrUpdatedAt, source_, nrBalanceUsed)
    ) = do
    nrOwner <- decodeAddress owner
    nrPayment <- case (method_, invoiceId_, codeId_) of
      (Just m, Just invId, Nothing) -> (`RPInvoice` InvoiceId invId) <$> note ("name_registrations.method: " <> m) (textMethod m)
      (Nothing, Nothing, Just codeId) -> Right $ RPCode codeId
      (Nothing, Nothing, Nothing) | isJust source_ -> Right RPBalance
      _ -> Left $ "name_registrations payment: " <> rid
    nrReveal <- case (label_, secret_, nonce_, deadline_, signature_) of
      (Nothing, Nothing, Nothing, Nothing, Nothing) -> Right Nothing
      (Just rvLabel, Just (Binary rvSecret), Just nonce, Just deadline, Just (Binary sig)) -> do
        rvSignature <- decodeSignature sig
        Right $ Just NameReveal {rvLabel, rvSecret, rvContactLink = nrContactLink, rvChannelLink = nrChannelLink, rvNonce = word64 nonce, rvDeadline = word64 deadline, rvSignature}
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
          nrPhase,
          nrYears,
          nrPrice,
          nrPayment,
          nrBalanceSource = RegistrationId <$> source_,
          nrBalanceUsed,
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
getRegistrationByOrderHash :: DB.Connection -> ByteString -> IO (Maybe NameRegistration)
getRegistrationByOrderHash db hash =
  firstDecoded toRegistration $
    DB.query db (selectRegistrations <> " WHERE r.order_hash = ? ORDER BY r.created_at DESC, r.registration_id DESC LIMIT 1") (Only $ Binary hash)

-- | Overdue whatever status the poller has marked, as long as the invoice is not paid and holds no money, as the sweep decides.
getLiveOrders :: DB.Connection -> ByteString -> Address -> LabelHash -> UTCTime -> IO [LiveOrder]
getLiveOrders db commitment owner (LabelHash lh) now =
  DB.query
    db
    ( registrationColumns
        <> ", CASE WHEN r.phase = ? AND i.status <> ? AND i.expires_at < ?"
        <> unfunded
        <> " THEN 1 ELSE 0 END"
        <> registrationFrom
        <> " LEFT JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id"
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
  mapM_ (\invId -> lockInvoice db invId now) $ case nrPayment of
    RPInvoice _ invId -> Just invId
    RPCode _ -> Nothing
    RPBalance -> Nothing
  expired <-
    executeChanging
      db
      ( [sql|
          UPDATE sx_badge_service_invoices SET status = ?, updated_at = ?
          WHERE status = ? AND invoice_id IN (
            SELECT i.invoice_id FROM sx_badge_service_name_invoices ni
            JOIN sx_badge_service_name_registrations r ON r.registration_id = ni.registration_id
            JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id
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
-- An unpaid order is live while its invoice is open, which nrLive carries; a paid order that lost its name can move to another one.
orderLive :: NameRegistration -> Bool
orderLive NameRegistration {nrPhase, nrPayment, nrLive} = case nrPhase of
  PUnpaid -> nrLive
  PQueued -> True
  PCommitting -> True
  PCommitted -> True
  PRegistering -> True
  PFailed -> True
  PTaken -> case nrPayment of
    RPInvoice {} -> True
    RPBalance -> True
    RPCode _ -> False
  PRegistered -> False

-- | Writes every changeable field of a registration read at its nrVersion, unless it changed since, with live as orderLive decides.
-- A closed order is never made live again, and a write that would make the order meet another live one changes nothing.
updateRegistration :: DB.Connection -> NameRegistration -> UTCTime -> IO VersionWrite
updateRegistration db r now = do
  changed <-
    executeChanging
      db
      [sql|
        UPDATE sx_badge_service_name_registrations
        SET version = version + 1, label_hash = ?, length = ?, duration = ?, commitment = ?, sealed = ?, phase = ?, price = ?,
          label = ?, secret = ?, contact_link = ?, channel_link = ?, intent_nonce = ?, intent_deadline = ?, intent_signature = ?,
          commit_tx = ?, commit_block = ?, register_tx = ?, register_block = ?, reveal_after = ?, reveal_before = ?, expires_at = ?,
          registered_at = ?, failed_at = ?, error = ?, live = ?, moves = ?, links_refused_at = ?, updated_at = ?
        WHERE registration_id = ? AND version = ?
          AND (? = 0 OR (live = 1 AND NOT EXISTS (
            SELECT 1 FROM sx_badge_service_name_registrations o
            WHERE o.live = 1 AND o.registration_id <> ? AND (o.commitment = ? OR (o.owner = ? AND o.label_hash = ?))
          )))
      |]
      ( (Binary lh, nrLength, int64 nrDuration, Binary nrCommitment, nrSealed, nrPhase, nrPrice)
          :. (rvLabel <$> nrReveal, Binary . rvSecret <$> nrReveal, rvContactLink =<< nrReveal, rvChannelLink =<< nrReveal, int64 . rvNonce <$> nrReveal, int64 . rvDeadline <$> nrReveal, Binary . serializeSignature . rvSignature <$> nrReveal)
          :. (txHashField <$> nrCommitTx, int64 <$> nrCommitBlock, txHashField <$> nrRegisterTx, int64 <$> nrRegisterBlock, int64 <$> nrRevealAfter, int64 <$> nrRevealBefore, int64 <$> nrExpiresAt)
          :. (truncateToSecond <$> nrRegisteredAt, truncateToSecond <$> nrFailedAt, nrError, BI live, nrMoves, truncateToSecond <$> nrLinksRefusedAt, truncateToSecond now)
          :. (rid, nrVersion, BI live, rid, Binary nrCommitment, Binary $ unAddress nrOwner, Binary lh)
      )
  if changed > 0 then pure $ Written (nrVersion + 1) else refusal
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrLabelHash = LabelHash lh, nrLength, nrDuration, nrOwner, nrCommitment, nrSealed, nrPhase, nrPrice, nrReveal} = r
    NameRegistration {nrCommitTx, nrCommitBlock, nrRegisterTx, nrRegisterBlock, nrRevealAfter, nrRevealBefore, nrExpiresAt} = r
    NameRegistration {nrRegisteredAt, nrFailedAt, nrError, nrMoves, nrLinksRefusedAt} = r
    live = orderLive r
    refusal =
      DB.query db "SELECT version, live FROM sx_badge_service_name_registrations WHERE registration_id = ?" (Only rid) >>= \case
        [(version, BI storedLive)] | version == nrVersion -> pure $ if storedLive then LiveOrderExists else NotLive
        _ -> pure VersionChanged

-- | Another name, or other links: queued again with the reveal, transactions, reveal times and link refusal cleared.
-- The order hash stays the original order's, so a resend of that order still finds it.
retargetRegistration :: DB.Connection -> NameRegistration -> OrderFields -> Int -> UTCTime -> IO VersionWrite
retargetRegistration db r OrderFields {ofLabelHash, ofLength, ofDuration, ofCommitment, ofSealed} price =
  updateRegistration
    db
    r
      { nrLabelHash = ofLabelHash,
        nrLength = ofLength,
        nrDuration = ofDuration,
        nrCommitment = ofCommitment,
        nrSealed = ofSealed,
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
findRegistrations :: DB.Connection -> Address -> IO [NameRegistration]
findRegistrations db owner = do
  newest <- DB.query db (selectRegistrations <> " WHERE r.owner = ?" <> newestFirst <> " LIMIT ?") (Binary $ unAddress owner, findLimit) >>= decodeAll toRegistration
  holding <- DB.query db (selectRegistrations <> " WHERE r.owner = ?" <> withBalance <> newestFirst) (Only $ Binary $ unAddress owner) >>= decodeAll toRegistration
  pure $ newest <> filter ((`notElem` map nrId newest) . nrId) holding
  where
    newestFirst = " ORDER BY r.created_at DESC, r.registration_id DESC"
    withBalance = " AND EXISTS (SELECT 1 FROM sx_badge_service_name_balances b WHERE b.registration_id = r.registration_id AND b.amount > 0)"

isLabelRegistering :: DB.Connection -> LabelHash -> IO Bool
isLabelRegistering db (LabelHash lh) =
  not . null
    <$> ( DB.query db "SELECT 1 FROM sx_badge_service_name_registrations WHERE label_hash = ? AND phase = ? LIMIT 1" (Binary lh, PRegistering) ::
            IO [Only Int]
        )

-- | The registrations a worker moves on, oldest first.
activeRegistrations :: DB.Connection -> IO [NameRegistration]
activeRegistrations db =
  DB.query db (selectRegistrations <> " WHERE r.phase IN (?,?,?,?) ORDER BY r.created_at, r.registration_id") (PQueued, PCommitting, PCommitted, PRegistering)
    >>= decodeAll toRegistration

-- | Unpaid orders whose invoice expired before the cutoff, without a recorded payment.
unpaidCleanupCandidates :: DB.Connection -> UTCTime -> IO [(RegistrationId, InvoiceId)]
unpaidCleanupCandidates db expiredBefore =
  map (\(rid, invId) -> (RegistrationId rid, InvoiceId invId))
    <$> DB.query
      db
      [sql|
        SELECT r.registration_id, ni.invoice_id
        FROM sx_badge_service_name_registrations r
        JOIN sx_badge_service_name_invoices ni ON ni.registration_id = r.registration_id
        JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id
        WHERE r.phase = ? AND i.expires_at < ?
          AND NOT EXISTS (SELECT 1 FROM sx_badge_service_payments p WHERE p.invoice_id = ni.invoice_id)
        ORDER BY i.expires_at, r.registration_id
      |]
      (PUnpaid, truncateToSecond expiredBefore)

-- | Deletes an unpaid registration and its invoice, unless a payment was recorded for it or it holds a balance.
deleteUnpaidRegistration :: DB.Connection -> RegistrationId -> IO (Either CleanupKept ())
deleteUnpaidRegistration db (RegistrationId rid) =
  DB.query db "SELECT registration_id FROM sx_badge_service_name_balances WHERE reserved_by = ?" (Only rid) >>= \case
    Only source : _ -> pure $ Left $ CKHolder $ RegistrationId source
    [] -> do
      invoiceIds <- DB.query db "SELECT invoice_id FROM sx_badge_service_name_invoices WHERE registration_id = ?" (Only rid)
      deleted <-
        executeChanging
          db
          [sql|
            DELETE FROM sx_badge_service_name_registrations
            WHERE registration_id = ? AND phase = ?
              AND NOT EXISTS (
                SELECT 1 FROM sx_badge_service_name_invoices ni
                JOIN sx_badge_service_payments p ON p.invoice_id = ni.invoice_id
                WHERE ni.registration_id = ?
              )
          |]
          (rid, PUnpaid, rid)
      if deleted > 0
        then Right () <$ mapM_ (DB.execute db "DELETE FROM sx_badge_service_invoices WHERE invoice_id = ?" . Only . fromOnly) (invoiceIds :: [Only Text])
        else pure $ Left CKPaid

-- | Code orders failed before the cutoff that still hold their unspent code.
failedCodeOrdersBefore :: DB.Connection -> UTCTime -> IO [(RegistrationId, Int64)]
failedCodeOrdersBefore db cutoff =
  map (\(rid, codeId) -> (RegistrationId rid, codeId))
    <$> DB.query
      db
      [sql|
        SELECT r.registration_id, c.name_prepaid_code_id
        FROM sx_badge_service_name_registrations r
        JOIN sx_badge_service_name_prepaid_codes c ON c.reserved_by = r.registration_id
        WHERE r.phase = ? AND r.failed_at < ? AND c.spent_at IS NULL
        ORDER BY r.failed_at, r.registration_id
      |]
      (PFailed, truncateToSecond cutoff)

-- | The hold, unless it is overdue, which reads as none.
heldBy :: Balance -> Maybe BalanceHold
heldBy Balance {blHold} = case blHold of
  Just BalanceHold {bhOverdue = False} -> blHold
  _ -> Nothing

-- | A holder is overdue as getLiveOrders reads an order overdue: its invoice past its expiry, not paid, and holding no money.
getBalance :: DB.Connection -> RegistrationId -> UTCTime -> IO (Maybe Balance)
getBalance db (RegistrationId rid) now =
  firstDecoded toBalance $
    DB.query
      db
      ( "SELECT b.registration_id, b.amount, b.credited, b.refunded, b.reserved_by, i.expires_at,"
          <> " CASE WHEN h.phase = ? AND i.status <> ? AND i.expires_at < ?"
          <> unfunded
          <> " THEN 1 ELSE 0 END, b.refunded_at"
          <> " FROM sx_badge_service_name_balances b"
          <> " LEFT JOIN sx_badge_service_name_registrations h ON h.registration_id = b.reserved_by"
          <> " LEFT JOIN sx_badge_service_name_invoices ni ON ni.registration_id = b.reserved_by"
          <> " LEFT JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id"
          <> " WHERE b.registration_id = ?"
      )
      (PUnpaid, invoiceStatusText ISPaid, truncateToSecond now, rid)

toBalance :: (Text, Int, Int, Int, Maybe Text, Maybe UTCTime, BoolInt, Maybe UTCTime) -> Either Text Balance
toBalance (rid, blAmount, blCredited, blRefunded, holder_, until_, BI overdue, blRefundedAt) = do
  blHold <- case (holder_, until_) of
    (Nothing, _) -> Right Nothing
    (Just holder, Just bhUntil) -> Right $ Just BalanceHold {bhHolder = RegistrationId holder, bhUntil, bhOverdue = overdue}
    (Just _, Nothing) -> Left $ "name_balances holder without an invoice: " <> rid
  pure Balance {blRegistration = RegistrationId rid, blAmount, blCredited, blRefunded, blHold, blRefundedAt}

-- | The order holding the balance, when its hold is overdue, for the next spend to close.
overdueHolder :: DB.Connection -> RegistrationId -> UTCTime -> IO (Maybe NameRegistration)
overdueHolder db rid now =
  getBalance db rid now >>= \case
    Just Balance {blHold = Just BalanceHold {bhHolder, bhOverdue = True}} -> getRegistration db bhHolder
    _ -> pure Nothing

-- | The order a support reference names, by its registration id or its name invoice's id, with its balance.
balanceByReference :: DB.Connection -> Text -> UTCTime -> IO (Maybe (RegistrationId, Maybe Balance))
balanceByReference db ref now =
  DB.query
    db
    "SELECT registration_id FROM sx_badge_service_name_registrations WHERE registration_id = ? UNION ALL SELECT registration_id FROM sx_badge_service_name_invoices WHERE invoice_id = ?"
    (ref, ref)
    >>= \case
      Only rid : _ -> (\balance -> Just (RegistrationId rid, balance)) <$> getBalance db (RegistrationId rid) now
      [] -> pure Nothing

-- | From the order back along balance_source to the first, following it only from orders past unpaid.
balanceChain :: DB.Connection -> RegistrationId -> IO [BalanceLink]
balanceChain db rid =
  getRegistration db rid >>= \case
    Nothing -> pure []
    Just NameRegistration {nrPhase, nrPayment, nrBalanceSource, nrBalanceUsed} -> do
      let pastUnpaid = nrPhase /= PUnpaid
      lkPaid <- case nrPayment of
        RPInvoice _ (InvoiceId invId) | pastUnpaid -> maybe 0 fromOnly . listToMaybe <$> (DB.query db "SELECT amount FROM sx_badge_service_invoices WHERE invoice_id = ?" (Only invId) :: IO [Only Int])
        _ -> pure 0
      lkLateCredits <- case nrPayment of
        RPInvoice _ invId -> lateCredits db invId
        _ -> pure []
      let link = BalanceLink {lkRegistration = rid, lkPayment = nrPayment, lkPaid, lkFromBalance = if pastUnpaid then nrBalanceUsed else 0, lkLateCredits}
      (link :) <$> maybe (pure []) (balanceChain db) (if pastUnpaid then nrBalanceSource else Nothing)

lateCredits :: DB.Connection -> InvoiceId -> IO [LateCredit]
lateCredits db (InvoiceId invId) =
  map toLateCredit
    <$> DB.query
      db
      "SELECT invoice_id, credit_no, registration_id, units, invoice_rate, current_rate, rate_used, cents FROM sx_badge_service_name_late_credits WHERE invoice_id = ? ORDER BY credit_no"
      (Only invId)
  where
    toLateCredit (inv, lcCreditNo, reg, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents) =
      LateCredit {lcInvoice = InvoiceId inv, lcCreditNo, lcRegistration = RegistrationId reg, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents}

-- | Adds to the order's balance, making it on its first credit.
creditBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO ()
creditBalance db (RegistrationId rid) cents now' =
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_name_balances (registration_id, amount, credited, refunded, created_at, updated_at)
      VALUES (?,?,?,0,?,?)
      ON CONFLICT (registration_id) DO UPDATE SET
        amount = sx_badge_service_name_balances.amount + excluded.amount,
        credited = sx_badge_service_name_balances.credited + excluded.credited,
        updated_at = excluded.updated_at
    |]
    (rid, cents, cents, now, now)
  where
    now = truncateToSecond now'

-- | The price, above 0, spent at once, where the amount is the one seen and nothing holds it.
spendBalance :: DB.Connection -> RegistrationId -> Int -> Int -> UTCTime -> IO Bool
spendBalance db (RegistrationId rid) price seen now =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_balances SET amount = amount - ?, updated_at = ? WHERE registration_id = ? AND amount = ? AND amount >= ? AND ? > 0 AND reserved_by IS NULL"
      (price, now, rid, seen, price, price)

-- | Held by a difference order, where the amount is the one seen, above 0, and nothing holds it.
holdBalance :: DB.Connection -> RegistrationId -> Int -> RegistrationId -> UTCTime -> IO Bool
holdBalance db (RegistrationId rid) seen (RegistrationId holder) now =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_balances SET reserved_by = ?, updated_at = ? WHERE registration_id = ? AND amount = ? AND ? > 0 AND reserved_by IS NULL"
      (holder, now, rid, seen, seen)

-- | The holder's hold spent and cleared, where this holder holds the balance and it still has the cents, above 0.
spendHeldBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO Bool
spendHeldBalance db (RegistrationId holder) used now =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_balances SET amount = amount - ?, reserved_by = NULL, updated_at = ? WHERE reserved_by = ? AND amount >= ? AND ? > 0"
      (used, truncateToSecond now, holder, used, used)

releaseHeldBalance :: DB.Connection -> RegistrationId -> UTCTime -> IO Bool
releaseHeldBalance db (RegistrationId holder) now =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_balances SET reserved_by = NULL, updated_at = ? WHERE reserved_by = ?"
      (truncateToSecond now, holder)

-- | All of the balance, where it is the cents given, above 0, and nothing holds it; refunded counts every refund.
refundBalance :: DB.Connection -> RegistrationId -> Int -> UTCTime -> IO Bool
refundBalance db (RegistrationId rid) cents now' =
  (> 0)
    <$> executeChanging
      db
      [sql|
        UPDATE sx_badge_service_name_balances SET amount = 0, refunded = refunded + ?, refunded_at = ?, updated_at = ?
        WHERE registration_id = ? AND amount = ? AND amount > 0 AND reserved_by IS NULL
      |]
      (cents, now, now, rid, cents)
  where
    now = truncateToSecond now'

-- | A name invoice with its order, its invoice row and its payment row.
lateFrom :: Query
lateFrom =
  " FROM sx_badge_service_name_invoices ni"
    <> " JOIN sx_badge_service_name_registrations r ON r.registration_id = ni.registration_id"
    <> " JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id"
    <> " JOIN sx_badge_service_payments p ON p.invoice_id = ni.invoice_id"

-- | Its order closed unpaid, whatever the invoice's status, since settlement queues only a live order; takes the unpaid phase.
closedUnpaid :: Query
closedUnpaid = " r.phase = ? AND r.live = 0"

cardInvoice :: Query
cardInvoice = " i.payment_crypto_currency IS NULL"

cryptoInvoice :: Query
cryptoInvoice = " i.payment_crypto_currency IS NOT NULL"

-- | No credit rows sum to 0, not NULL.
creditedTotal :: Query -> Query
creditedTotal column = " (SELECT " <> integerSum ("COALESCE(SUM(c." <> column <> "), 0)") <> " FROM sx_badge_service_name_late_credits c WHERE c.invoice_id = ni.invoice_id)"

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
pendingCase :: Query
pendingCase =
  " CASE WHEN COALESCE(p.amount, 0) = 0 AND (p.crypto_paid IS NOT NULL OR p.paid_in_full = 1) THEN " <> literal LPConfirming
    <> " WHEN" <> uncreditedMoney <> " THEN " <> literal LPUncredited
    <> " WHEN" <> cardInvoice <> " AND COALESCE(p.amount, 0) > p.confirmed_amount THEN " <> literal LPUnconfirmed
    <> " WHEN p.received_units > p.confirmed_units THEN " <> literal LPUnconfirmed
    <> " WHEN" <> cryptoInvoice <> " AND p.received_units IS NULL AND COALESCE(p.amount, 0) > 0 THEN " <> literal LPUncounted
    <> " END"
  where
    literal p = fromString $ "'" <> T.unpack (textEncode p) <> "'"

-- | What an alert names as received: units for crypto, and cents for card or for crypto that could not be counted in units.
receivedFigure :: Query
receivedFigure = " CASE WHEN" <> cardInvoice <> " THEN p.amount ELSE COALESCE(p.received_units, p.amount) END"

lateColumns :: Query
lateColumns =
  "SELECT ni.invoice_id, ni.registration_id, r.phase, r.live, CASE WHEN" <> cardInvoice <> " THEN 1 ELSE 0 END, COALESCE(p.amount, 0), p.confirmed_amount,"
    <> " p.received_units, p.confirmed_units, p.invoice_rate,"
    <> creditedTotal "units"
    <> ","
    <> creditedTotal "cents"
    <> ", (SELECT COUNT(*) FROM sx_badge_service_name_late_credits c WHERE c.invoice_id = ni.invoice_id), p.created_at"

type LateRow = (Text, Text, NamePhase, BoolInt, BoolInt, Int, Int, Maybe Int64, Maybe Int64, Maybe Text) :. (Int64, Int, Int, UTCTime)

toLateState :: LateRow -> LateCreditState
toLateState
  ( (inv, reg, lsPhase, BI lsLive, BI lsCard, lsReceivedCents, lsConfirmedCents, lsReceivedUnits, lsConfirmedUnits, lsInvoiceRate)
      :. (lsCreditedUnits, lsCreditedCents, lsCredits, lsPaymentCreatedAt)
    ) =
    LateCreditState {lsInvoice = InvoiceId inv, lsRegistration = RegistrationId reg, lsPhase, lsLive, lsCard, lsReceivedCents, lsConfirmedCents, lsReceivedUnits, lsConfirmedUnits, lsInvoiceRate, lsCreditedUnits, lsCreditedCents, lsCredits, lsPaymentCreatedAt}

-- | Nothing without a payment row.
lateCreditState :: DB.Connection -> InvoiceId -> IO (Maybe LateCreditState)
lateCreditState db (InvoiceId invId) = maybeFirstRow toLateState $ DB.query db (lateColumns <> lateFrom <> " WHERE ni.invoice_id = ?") (Only invId)

-- | Run after the invoice's lock. The order's balance is credited only when the credit's row is new, so a taken credit number changes neither.
creditLatePayment :: DB.Connection -> LateCredit -> UTCTime -> IO Bool
creditLatePayment db LateCredit {lcInvoice = InvoiceId invId, lcCreditNo, lcRegistration = regId@(RegistrationId rid), lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents} now' = do
  inserted <-
    executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_late_credits (invoice_id, credit_no, registration_id, units, invoice_rate, current_rate, rate_used, cents, created_at)
        VALUES (?,?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      (invId, lcCreditNo, rid, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents, now)
  when (inserted > 0) $ creditBalance db regId lcCents now
  pure $ inserted > 0
  where
    now = truncateToSecond now'

-- | Why the closed order's money on the invoice is not all credited; the one rule of the pending log and the alert.
latePending :: DB.Connection -> InvoiceId -> IO (Maybe LatePending)
latePending db (InvoiceId invId) =
  maybe Nothing fromOnly . listToMaybe
    <$> DB.query db ("SELECT" <> pendingCase <> lateFrom <> " WHERE ni.invoice_id = ? AND" <> closedUnpaid) (invId, PUnpaid)

-- | Closed orders' invoices with confirmed money not all credited, for the poller's credit pass.
uncreditedLateInvoices :: DB.Connection -> IO [InvoiceId]
uncreditedLateInvoices db =
  map (InvoiceId . fromOnly)
    <$> DB.query db ("SELECT ni.invoice_id" <> lateFrom <> " WHERE" <> closedUnpaid <> " AND" <> uncreditedMoney <> " ORDER BY p.created_at, ni.invoice_id") (Only PUnpaid)

-- | Pending money whose payment row was created before the cutoff, never alerted, or alerted for less money than now received.
lateAlertDue :: DB.Connection -> UTCTime -> IO [LateAlert]
lateAlertDue db cutoff =
  DB.query
    db
    ( lateColumns
        <> ","
        <> pendingCase
        <> ","
        <> receivedFigure
        <> ", ni.late_alerted_at, ni.late_alerted_units"
        <> lateFrom
        <> " WHERE"
        <> closedUnpaid
        <> " AND p.created_at < ? AND"
        <> pendingCase
        <> " IS NOT NULL AND (ni.late_alerted_at IS NULL OR"
        <> receivedFigure
        <> " > COALESCE(ni.late_alerted_units, -1)) ORDER BY p.created_at, ni.invoice_id"
    )
    (PUnpaid, truncateToSecond cutoff)
    >>= decodeAll toAlert
  where
    toAlert :: LateRow :. (Maybe LatePending, Maybe Int64, Maybe UTCTime, Maybe Int64) -> Either Text LateAlert
    toAlert (row :. (pending_, laReceived, laAlertedAt, laAlertedReceived)) = case pending_ of
      Just laPending -> Right LateAlert {laState = toLateState row, laPending, laReceived, laAlertedAt, laAlertedReceived}
      Nothing -> Left "name invoice alert without pending money"

-- | Marks the alert sent, unless another instance marked it since it was read; only the instance that marks it logs it.
claimLateAlert :: DB.Connection -> LateAlert -> UTCTime -> IO Bool
claimLateAlert db LateAlert {laState = LateCreditState {lsInvoice = InvoiceId invId}, laReceived, laAlertedAt, laAlertedReceived} now =
  (> 0)
    <$> executeChanging
      db
      ( "UPDATE sx_badge_service_name_invoices SET late_alerted_at = ?, late_alerted_units = ? WHERE invoice_id = ? AND late_alerted_at"
          <> notDistinctFrom
          <> "? AND late_alerted_units"
          <> notDistinctFrom
          <> "?"
      )
      (truncateToSecond now, laReceived, invId, laAlertedAt, laAlertedReceived)

insertPrepaidCode ::DB.Connection -> NewPrepaidCode -> UTCTime -> IO (Either CodeHashTaken Int64)
insertPrepaidCode db NewPrepaidCode {npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, npcExpiresAt} now = do
  inserted <-
    executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_prepaid_codes (code_hash, min_length, years, code_payment_status, expires_at, created_at)
        VALUES (?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      (Binary npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, truncateToSecond <$> npcExpiresAt, truncateToSecond now)
  if inserted > 0 then Right <$> insertedRowId db else pure $ Left CodeHashTaken

prepaidCodeColumns :: Query
prepaidCodeColumns =
  [sql|
    SELECT name_prepaid_code_id, code_hash, min_length, years, code_payment_status, reserved_by, reserved_at, spent_at, revoked_at, expires_at, created_at
    FROM sx_badge_service_name_prepaid_codes
  |]

type PrepaidCodeRow = (Int64, Binary ByteString, Int, Int, BadgeCodePaymentStatus, Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime) :. Only UTCTime

toPrepaidCode :: PrepaidCodeRow -> PrepaidCode
toPrepaidCode ((pcId, Binary pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt) :. Only pcCreatedAt) =
  PrepaidCode {pcId, pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcReservedBy = RegistrationId <$> reservedBy, pcReservedAt, pcSpentAt, pcRevokedAt, pcExpiresAt, pcCreatedAt}

getPrepaidCode :: DB.Connection -> ByteString -> IO (Maybe PrepaidCode)
getPrepaidCode db codeHash =
  maybeFirstRow toPrepaidCode $ DB.query db (prepaidCodeColumns <> " WHERE code_hash = ?") (Only $ Binary codeHash)

getPrepaidCodeById :: DB.Connection -> Int64 -> IO (Maybe PrepaidCode)
getPrepaidCodeById db codeId =
  maybeFirstRow toPrepaidCode $ DB.query db (prepaidCodeColumns <> " WHERE name_prepaid_code_id = ?") (Only codeId)

-- | Only an unused code: not reserved, spent or revoked, paid or free, and not expired.
reservePrepaidCode :: DB.Connection -> Int64 -> RegistrationId -> UTCTime -> IO Bool
reservePrepaidCode db codeId (RegistrationId rid) now' =
  (> 0)
    <$> executeChanging
      db
      [sql|
        UPDATE sx_badge_service_name_prepaid_codes SET reserved_by = ?, reserved_at = ?
        WHERE name_prepaid_code_id = ? AND reserved_by IS NULL AND spent_at IS NULL AND revoked_at IS NULL
          AND code_payment_status <> ? AND (expires_at IS NULL OR expires_at > ?)
      |]
      (rid, now, codeId, CPSUnpaid, now)
  where
    now = truncateToSecond now'

releasePrepaidCode :: DB.Connection -> Int64 -> RegistrationId -> IO Bool
releasePrepaidCode db codeId (RegistrationId rid) =
  (> 0)
    <$> executeChanging
      db
      [sql|
        UPDATE sx_badge_service_name_prepaid_codes SET reserved_by = NULL, reserved_at = NULL
        WHERE name_prepaid_code_id = ? AND reserved_by = ? AND spent_at IS NULL
      |]
      (codeId, rid)

-- | A spent code keeps reserved_by, naming the registration it paid for.
spendPrepaidCode :: DB.Connection -> Int64 -> RegistrationId -> UTCTime -> IO Bool
spendPrepaidCode db codeId (RegistrationId rid) now =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_prepaid_codes SET spent_at = ? WHERE name_prepaid_code_id = ? AND reserved_by = ? AND spent_at IS NULL"
      (truncateToSecond now, codeId, rid)

-- | For a registration whose register went through: spends the code it holds, or one released and still usable; False otherwise.
spendLandedPrepaidCode :: DB.Connection -> Int64 -> RegistrationId -> UTCTime -> IO Bool
spendLandedPrepaidCode db codeId (RegistrationId rid) now' =
  (> 0)
    <$> executeChanging
      db
      [sql|
        UPDATE sx_badge_service_name_prepaid_codes SET reserved_by = ?, reserved_at = COALESCE(reserved_at, ?), spent_at = ?
        WHERE name_prepaid_code_id = ? AND spent_at IS NULL
          AND (reserved_by = ? OR (reserved_by IS NULL AND revoked_at IS NULL AND code_payment_status <> ? AND (expires_at IS NULL OR expires_at > ?)))
      |]
      (rid, now, now, codeId, rid, CPSUnpaid, now)
  where
    now = truncateToSecond now'

-- | A reserved code can be revoked, and its order keeps it; a spent one cannot.
revokePrepaidCode :: DB.Connection -> ByteString -> UTCTime -> IO RevokeResult
revokePrepaidCode db codeHash now = do
  revoked <-
    executeChanging
      db
      "UPDATE sx_badge_service_name_prepaid_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND spent_at IS NULL"
      (truncateToSecond now, Binary codeHash)
  maybe NoSuchCode (result revoked) <$> getPrepaidCode db codeHash
  where
    result revoked PrepaidCode {pcId, pcRevokedAt}
      | revoked > 0 = Revoked pcId
      | isJust pcRevokedAt = AlreadyRevoked pcId
      | otherwise = AlreadyRedeemed

txKind :: TxFor -> (Text, Maybe Text, Maybe Int64)
txKind = \case
  TxCommit (RegistrationId rid) -> ("commit", Just rid, Nothing)
  TxRegister (RegistrationId rid) -> ("register", Just rid, Nothing)
  TxEdit editId -> ("edit", Nothing, Just editId)

-- | Saved once signed and before it is sent, so a restart can rebroadcast it.
insertTransaction :: DB.Connection -> TxFor -> NewTransaction -> UTCTime -> IO Int64
insertTransaction db for NewTransaction {ntxRaw, ntxHash, ntxNonce} now = do
  let (kind, rid, editId) = txKind for
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_name_transactions
        (registration_id, name_record_edit_id, kind, raw, tx_hash, registrar_nonce, sent_at, status)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (rid, editId, kind, Binary ntxRaw, txHashField ntxHash, int64 ntxNonce, truncateToSecond now, TSPending)
  insertedRowId db

-- | Moves a transaction on from the status the caller read, so an outcome is never overwritten unseen; False when it is not in that status.
recordTransactionOutcome :: DB.Connection -> TxHash -> TxStatus -> TxStatus -> Maybe Word64 -> IO Bool
recordTransactionOutcome db hash from status block =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_transactions SET status = ?, mined_block = ? WHERE tx_hash = ? AND status = ?"
      (status, int64 <$> block, txHashField hash, from)

transactionColumns :: Query
transactionColumns =
  [sql|
    SELECT name_transaction_id, kind, registration_id, name_record_edit_id, raw, tx_hash, registrar_nonce, sent_at, mined_block, status
    FROM sx_badge_service_name_transactions
  |]

type TransactionRow = (Int64, Text, Maybe Text, Maybe Int64, Binary ByteString, Binary ByteString, Int64, UTCTime, Maybe Int64, TxStatus)

toTransaction :: TransactionRow -> Either Text NameTransaction
toTransaction (ntId, kind, rid_, editId_, Binary ntRaw, hash, nonce, ntSentAt, block, ntStatus) = do
  ntFor <- case (kind, rid_, editId_) of
    ("commit", Just rid, Nothing) -> Right $ TxCommit $ RegistrationId rid
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

editTransactions :: DB.Connection -> Int64 -> IO [NameTransaction]
editTransactions db editId =
  DB.query db (transactionColumns <> " WHERE name_record_edit_id = ? ORDER BY name_transaction_id") (Only editId)
    >>= decodeAll toTransaction

pendingEdit :: Text
pendingEdit = "pending"

minedEdit :: Text
minedEdit = "mined"

revertedEdit :: Text
revertedEdit = "reverted"

-- | Nothing when a pending or mined edit already holds the owner's nonce.
insertRecordEdit :: DB.Connection -> NewRecordEdit -> UTCTime -> IO (Maybe Int64)
insertRecordEdit db NewRecordEdit {nreLabelHash = LabelHash lh, nreOwner, nreNonce, nreChange, nreDeadline, nreSignature, nreBodyHash} now = do
  let (key, value) = case nreChange of
        RCSetText k v -> (Just k, Just v)
        RCClear -> (Nothing, Nothing)
  inserted <-
    executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_record_edits
          (label_hash, owner, nonce, record_key, record_value, deadline, signature, body_hash, state, created_at)
        VALUES (?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      ( (Binary lh, Binary $ unAddress nreOwner, int64 nreNonce, key, value, int64 nreDeadline, Binary $ serializeSignature nreSignature, Binary nreBodyHash)
          :. (pendingEdit, truncateToSecond now)
      )
  if inserted > 0 then Just <$> insertedRowId db else pure Nothing

recordEditColumns :: Query
recordEditColumns =
  [sql|
    SELECT name_record_edit_id, label_hash, owner, nonce, record_key, record_value, deadline, signature, body_hash, state, revert, mined_tx, created_at
    FROM sx_badge_service_name_record_edits
  |]

type RecordEditRow =
  (Int64, Binary ByteString, Binary ByteString, Int64, Maybe Text, Maybe Text, Int64, Binary ByteString, Binary ByteString, Text)
    :. (Maybe Text, Maybe (Binary ByteString), UTCTime)

toRecordEdit :: RecordEditRow -> Either Text RecordEdit
toRecordEdit ((reId, Binary lh, Binary owner, nonce, key_, value_, deadline, Binary sig, Binary reBodyHash, state) :. (revert, minedTx, reCreatedAt)) = do
  reOwner <- decodeAddress owner
  reSignature <- decodeSignature sig
  reChange <- case (key_, value_) of
    (Just key, Just value) -> Right $ RCSetText key value
    (Nothing, Nothing) -> Right RCClear
    _ -> Left $ "name_record_edits record: " <> tshow reId
  reState <- decodeState
  pure RecordEdit {reId, reLabelHash = LabelHash lh, reOwner, reNonce = word64 nonce, reChange, reDeadline = word64 deadline, reSignature, reBodyHash, reState, reCreatedAt}
  where
    decodeState
      | state == pendingEdit = Right ESPending
      | state == minedEdit = Right $ ESMined $ txHash <$> minedTx
      | state == revertedEdit = Right $ ESReverted revert
      | otherwise = Left $ "name_record_edits.state: " <> state

getRecordEdit :: DB.Connection -> Int64 -> IO (Maybe RecordEdit)
getRecordEdit db editId =
  firstDecoded toRecordEdit $ DB.query db (recordEditColumns <> " WHERE name_record_edit_id = ?") (Only editId)

-- | The newest edit sent with this signed body.
getRecordEditByBody :: DB.Connection -> ByteString -> IO (Maybe RecordEdit)
getRecordEditByBody db bodyHash =
  firstDecoded toRecordEdit $
    DB.query db (recordEditColumns <> " WHERE body_hash = ? ORDER BY name_record_edit_id DESC LIMIT 1") (Only $ Binary bodyHash)

-- | The pending or mined edit holding the owner's nonce.
getRecordEditByNonce :: DB.Connection -> Address -> Word64 -> IO (Maybe RecordEdit)
getRecordEditByNonce db owner nonce =
  firstDecoded toRecordEdit $
    DB.query db (recordEditColumns <> " WHERE owner = ? AND nonce = ? AND state IN (?,?)") (Binary $ unAddress owner, int64 nonce, pendingEdit, minedEdit)

-- | Settles a pending edit; False when it was settled already.
settleRecordEdit :: DB.Connection -> Int64 -> EditState -> IO Bool
settleRecordEdit db editId state =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_record_edits SET state = ?, revert = ?, mined_tx = ? WHERE name_record_edit_id = ? AND state = ?"
      (stateText, revert, txHashField <$> minedTx, editId, pendingEdit)
  where
    (stateText, revert, minedTx) = case state of
      ESPending -> (pendingEdit, Nothing, Nothing)
      ESMined tx -> (minedEdit, Nothing, tx)
      ESReverted name -> (revertedEdit, name, Nothing)

pendingEditIds :: DB.Connection -> IO [Int64]
pendingEditIds db = map fromOnly <$> DB.query db "SELECT name_record_edit_id FROM sx_badge_service_name_record_edits WHERE state = ? ORDER BY name_record_edit_id" (Only pendingEdit)

deleteUnsentEdit :: DB.Connection -> Int64 -> IO ()
deleteUnsentEdit db editId =
  DB.execute
    db
    "DELETE FROM sx_badge_service_name_record_edits WHERE name_record_edit_id = ? AND state = ? AND NOT EXISTS (SELECT 1 FROM sx_badge_service_name_transactions WHERE name_record_edit_id = ?)"
    (editId, pendingEdit, editId)

-- | When each edit relayed for the name since the given time was sent, oldest first.
relayedEditTimes :: DB.Connection -> LabelHash -> UTCTime -> IO [UTCTime]
relayedEditTimes db (LabelHash lh) since =
  map fromOnly
    <$> DB.query
      db
      "SELECT created_at FROM sx_badge_service_name_record_edits WHERE label_hash = ? AND created_at > ? ORDER BY created_at"
      (Binary lh, truncateToSecond since)

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
decodeAddress bs = either (const $ Left "address column") Right $ strDecode $ convertToBase Base16 bs

decodeSignature :: ByteString -> Either Text RecoverableSignature
decodeSignature bs = either (Left . ("signature column: " <>) . tshow) Right $ parseSignature $ hex bs

note :: Text -> Maybe a -> Either Text a
note err = maybe (Left err) Right

decodeAll :: (row -> Either Text a) -> [row] -> IO [a]
decodeAll decode = either (E.throwIO . StoreDecodeError) pure . traverse decode

firstDecoded :: (row -> Either Text a) -> IO [row] -> IO (Maybe a)
firstDecoded decode rows = rows >>= fmap listToMaybe . decodeAll decode . take 1
