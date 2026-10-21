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
    createPaidRegistration,
    createCodeRegistration,
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
    insertPrepaidCode,
    getPrepaidCode,
    getPrepaidCodeById,
    reservePrepaidCode,
    releasePrepaidCode,
    spendPrepaidCode,
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
import Data.Text (Text)
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
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), parseSignature)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (StoreDecodeError (..), executeChanging, invoiceStatusText, newInvoiceId, truncateToSecond)

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
  | PCodeIssued
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
    PCodeIssued -> "code_issued"
  textDecode t = lookup t [(textEncode p, p) | p <- [minBound .. maxBound]]

instance FromField NamePhase where fromField = fromTextField_ textDecode

instance ToField NamePhase where toField = toField . textEncode

data RegistrationPayment
  = RPInvoice ServicePaymentMethod InvoiceId
  | RPCode Int64
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
    nrReveal :: Maybe NameReveal,
    nrCommitTx :: Maybe ByteString,
    nrCommitBlock :: Maybe Word64,
    nrRegisterTx :: Maybe ByteString,
    nrRegisterBlock :: Maybe Word64,
    nrRevealAfter :: Maybe Word64,
    nrRevealBefore :: Maybe Word64,
    nrExpiresAt :: Maybe Word64,
    nrIssuedCodeId :: Maybe Int64,
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

data RegistrationRefused = RRLiveOrder | RRCodeNotReservable
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
data OrderTerms = OTInvoice ServicePaymentMethod Int | OTCode ByteString

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

-- | A dropped transaction was never mined and its registrar nonce went to another one.
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
    ntHash :: ByteString,
    ntNonce :: Word64,
    ntSentAt :: UTCTime,
    ntMinedBlock :: Maybe Word64,
    ntStatus :: TxStatus
  }
  deriving (Eq, Show)

data NewTransaction = NewTransaction
  { ntxRaw :: ByteString,
    ntxHash :: ByteString,
    ntxNonce :: Word64
  }

-- | A text record set to a value, "" removing it, or every record cleared.
data RecordChange = RCSetText Text Text | RCClear
  deriving (Eq, Show)

data EditState = ESPending | ESMined (Maybe ByteString) | ESReverted (Maybe Text)
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

createPaidRegistration :: DB.Connection -> NewRegistration -> PaidOrder -> UTCTime -> IO (Either RegistrationRefused ())
createPaidRegistration db new@NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder} PaidOrder {poMethod, poPrice, poInvoiceId = InvoiceId invId, poProviderRef} now' = do
  let now = truncateToSecond now'
      hash = orderHash nrgOwner nrgOrder (OTInvoice poMethod poPrice)
  inserted <- insertRegistration db new PUnpaid poPrice (Just poMethod) Nothing hash now
  when inserted $
    DB.execute
      db
      "INSERT INTO sx_badge_service_name_invoices (invoice_id, registration_id, provider_ref, created_at) VALUES (?,?,?,?)"
      (invId, rid, poProviderRef, now)
  pure $ if inserted then Right () else Left RRLiveOrder

-- | Reserving the code is the conditional update of the transaction that creates the registration.
createCodeRegistration :: DB.Connection -> NewRegistration -> PrepaidCode -> UTCTime -> IO (Either RegistrationRefused ())
createCodeRegistration db new@NewRegistration {nrgId = regId@(RegistrationId rid), nrgOwner, nrgOrder} PrepaidCode {pcId, pcCodeHash} now' = do
  let now = truncateToSecond now'
  inserted <- insertRegistration db new PQueued 0 Nothing (Just pcId) (orderHash nrgOwner nrgOrder (OTCode pcCodeHash)) now
  if not inserted
    then pure $ Left RRLiveOrder
    else
      reservePrepaidCode db pcId regId now >>= \case
        True -> pure $ Right ()
        False -> do
          DB.execute db "DELETE FROM sx_badge_service_name_registrations WHERE registration_id = ?" (Only rid)
          pure $ Left RRCodeNotReservable

-- | False when the registration would be a second live one for its commitment, or for its owner and label hash.
insertRegistration :: DB.Connection -> NewRegistration -> NamePhase -> Int -> Maybe ServicePaymentMethod -> Maybe Int64 -> ByteString -> UTCTime -> IO Bool
insertRegistration db NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder, nrgYears} phase price method codeId hash now =
  (> 0)
    <$> executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_registrations
          (registration_id, version, label_hash, length, duration, owner, commitment, sealed, phase, years,
           price, method, name_prepaid_code_id, live, order_hash, moves, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?,?,?, ?,?,?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      ( (rid, initialVersion, Binary lh, ofLength, int64 ofDuration, Binary $ unAddress nrgOwner, Binary ofCommitment, ofSealed, phase, nrgYears)
          :. (price, methodText <$> method, codeId, BI True, Binary hash, 0 :: Int, now, now)
      )
  where
    OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} = nrgOrder

registrationColumns :: Query
registrationColumns =
  [sql|
    SELECT r.registration_id, r.version, r.label_hash, r.length, r.duration, r.owner, r.commitment, r.sealed, r.phase, r.years,
      r.price, r.method, ni.invoice_id, r.name_prepaid_code_id, r.label, r.secret, r.contact_link, r.channel_link, r.intent_nonce, r.intent_deadline,
      r.intent_signature, r.commit_tx, r.commit_block, r.register_tx, r.register_block, r.reveal_after, r.reveal_before, r.expires_at, r.issued_code_id, r.registered_at,
      r.failed_at, r.error, r.live, r.order_hash, r.moves, r.links_refused_at, r.created_at, r.updated_at
  |]

registrationFrom :: Query
registrationFrom =
  [sql|
    FROM sx_badge_service_name_registrations r
    LEFT JOIN sx_badge_service_name_invoices ni ON ni.registration_id = r.registration_id
  |]

selectRegistrations :: Query
selectRegistrations = registrationColumns <> registrationFrom

type RegistrationRow =
  (Text, Int64, Binary ByteString, Int, Int64, Binary ByteString, Binary ByteString, Text, NamePhase, Int)
    :. (Int, Maybe Text, Maybe Text, Maybe Int64, Maybe Text, Maybe (Binary ByteString), Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)
    :. (Maybe (Binary ByteString), Maybe (Binary ByteString), Maybe Int64, Maybe (Binary ByteString), Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe Int64, Maybe UTCTime)
    :. (Maybe UTCTime, Maybe Text, BoolInt, Binary ByteString, Int, Maybe UTCTime, UTCTime, UTCTime)

toRegistration :: RegistrationRow -> Either Text NameRegistration
toRegistration
  ( (rid, nrVersion, Binary lh, nrLength, duration, Binary owner, Binary nrCommitment, nrSealed, nrPhase, nrYears)
      :. (nrPrice, method_, invoiceId_, codeId_, label_, secret_, nrContactLink, nrChannelLink, nonce_, deadline_)
      :. (signature_, commitTx, commitBlock, registerTx, registerBlock, revealAfter, revealBefore, expiresAt, nrIssuedCodeId, nrRegisteredAt)
      :. (nrFailedAt, nrError, BI nrLive, Binary nrOrderHash, nrMoves, nrLinksRefusedAt, nrCreatedAt, nrUpdatedAt)
    ) = do
    nrOwner <- decodeAddress owner
    nrPayment <- case (method_, invoiceId_, codeId_) of
      (Just m, Just invId, Nothing) -> (`RPInvoice` InvoiceId invId) <$> note ("name_registrations.method: " <> m) (textMethod m)
      (Nothing, Nothing, Just codeId) -> Right $ RPCode codeId
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
          nrReveal,
          nrCommitTx = fromBinary <$> commitTx,
          nrCommitBlock = word64 <$> commitBlock,
          nrRegisterTx = fromBinary <$> registerTx,
          nrRegisterBlock = word64 <$> registerBlock,
          nrRevealAfter = word64 <$> revealAfter,
          nrRevealBefore = word64 <$> revealBefore,
          nrExpiresAt = word64 <$> expiresAt,
          nrIssuedCodeId,
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

-- | Overdue whatever status the poller has marked, as long as the invoice is not paid.
getLiveOrders :: DB.Connection -> ByteString -> Address -> LabelHash -> UTCTime -> IO [LiveOrder]
getLiveOrders db commitment owner (LabelHash lh) now =
  DB.query
    db
    ( registrationColumns
        <> ", CASE WHEN r.phase = ? AND i.status <> ? AND i.expires_at < ? THEN 1 ELSE 0 END"
        <> registrationFrom
        <> " LEFT JOIN sx_badge_service_invoices i ON i.invoice_id = ni.invoice_id"
        <> " WHERE r.live = 1 AND (r.commitment = ? OR (r.owner = ? AND r.label_hash = ?))"
    )
    (PUnpaid, invoiceStatusText ISPaid, truncateToSecond now, Binary commitment, Binary $ unAddress owner, Binary lh)
    >>= decodeAll toLiveOrder
  where
    toLiveOrder (row :. Only (BI loOverdue)) = (\loRegistration -> LiveOrder {loRegistration, loOverdue}) <$> toRegistration row

-- | Run in the transaction that inserts the next order, so the unique constraints let it in.
closeOverdue :: DB.Connection -> NameRegistration -> UTCTime -> IO VersionWrite
closeOverdue db r@NameRegistration {nrId = RegistrationId rid} now = do
  written <- updateRegistration db r {nrLive = False} now
  when (isWritten written) $
    DB.execute
      db
      [sql|
        UPDATE sx_badge_service_invoices SET status = ?, updated_at = ?
        WHERE status = ? AND invoice_id IN (SELECT invoice_id FROM sx_badge_service_name_invoices WHERE registration_id = ?)
      |]
      (invoiceStatusText ISExpired, truncateToSecond now, invoiceStatusText ISOpen, rid)
  pure written

isWritten :: VersionWrite -> Bool
isWritten = \case
  Written _ -> True
  _ -> False

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
    RPCode _ -> False
  PRegistered -> False
  PCodeIssued -> False

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
          issued_code_id = ?, registered_at = ?, failed_at = ?, error = ?, live = ?, moves = ?, links_refused_at = ?, updated_at = ?
        WHERE registration_id = ? AND version = ?
          AND (? = 0 OR (live = 1 AND NOT EXISTS (
            SELECT 1 FROM sx_badge_service_name_registrations o
            WHERE o.live = 1 AND o.registration_id <> ? AND (o.commitment = ? OR (o.owner = ? AND o.label_hash = ?))
          )))
      |]
      ( (Binary lh, nrLength, int64 nrDuration, Binary nrCommitment, nrSealed, nrPhase, nrPrice)
          :. (rvLabel <$> nrReveal, Binary . rvSecret <$> nrReveal, rvContactLink =<< nrReveal, rvChannelLink =<< nrReveal, int64 . rvNonce <$> nrReveal, int64 . rvDeadline <$> nrReveal, Binary . serializeSignature . rvSignature <$> nrReveal)
          :. (Binary <$> nrCommitTx, int64 <$> nrCommitBlock, Binary <$> nrRegisterTx, int64 <$> nrRegisterBlock, int64 <$> nrRevealAfter, int64 <$> nrRevealBefore, int64 <$> nrExpiresAt)
          :. (nrIssuedCodeId, truncateToSecond <$> nrRegisteredAt, truncateToSecond <$> nrFailedAt, nrError, BI live, nrMoves, truncateToSecond <$> nrLinksRefusedAt, truncateToSecond now)
          :. (rid, nrVersion, BI live, rid, Binary nrCommitment, Binary $ unAddress nrOwner, Binary lh)
      )
  if changed > 0 then pure $ Written (nrVersion + 1) else refusal
  where
    NameRegistration {nrId = RegistrationId rid, nrVersion, nrLabelHash = LabelHash lh, nrLength, nrDuration, nrOwner, nrCommitment, nrSealed, nrPhase, nrPrice, nrReveal} = r
    NameRegistration {nrCommitTx, nrCommitBlock, nrRegisterTx, nrRegisterBlock, nrRevealAfter, nrRevealBefore, nrExpiresAt} = r
    NameRegistration {nrIssuedCodeId, nrRegisteredAt, nrFailedAt, nrError, nrMoves, nrLinksRefusedAt} = r
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

-- | Newest first, at most findLimit.
findRegistrations :: DB.Connection -> Address -> IO [NameRegistration]
findRegistrations db owner =
  DB.query db (selectRegistrations <> " WHERE r.owner = ? ORDER BY r.created_at DESC, r.registration_id DESC LIMIT ?") (Binary $ unAddress owner, findLimit)
    >>= decodeAll toRegistration

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

-- | Deletes an unpaid registration and its invoice, unless a payment was recorded for it.
deleteUnpaidRegistration :: DB.Connection -> RegistrationId -> IO Bool
deleteUnpaidRegistration db (RegistrationId rid) = do
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
  when (deleted > 0) $
    mapM_ (DB.execute db "DELETE FROM sx_badge_service_invoices WHERE invoice_id = ?" . Only . fromOnly) (invoiceIds :: [Only Text])
  pure $ deleted > 0

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

insertPrepaidCode :: DB.Connection -> NewPrepaidCode -> UTCTime -> IO (Either CodeHashTaken Int64)
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
    (rid, editId, kind, Binary ntxRaw, Binary ntxHash, int64 ntxNonce, truncateToSecond now, TSPending)
  insertedRowId db

recordTransactionOutcome :: DB.Connection -> ByteString -> TxStatus -> Maybe Word64 -> IO Bool
recordTransactionOutcome db hash status block =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_name_transactions SET status = ?, mined_block = ? WHERE tx_hash = ?"
      (status, int64 <$> block, Binary hash)

transactionColumns :: Query
transactionColumns =
  [sql|
    SELECT name_transaction_id, kind, registration_id, name_record_edit_id, raw, tx_hash, registrar_nonce, sent_at, mined_block, status
    FROM sx_badge_service_name_transactions
  |]

type TransactionRow = (Int64, Text, Maybe Text, Maybe Int64, Binary ByteString, Binary ByteString, Int64, UTCTime, Maybe Int64, TxStatus)

toTransaction :: TransactionRow -> Either Text NameTransaction
toTransaction (ntId, kind, rid_, editId_, Binary ntRaw, Binary ntHash, nonce, ntSentAt, block, ntStatus) = do
  ntFor <- case (kind, rid_, editId_) of
    ("commit", Just rid, Nothing) -> Right $ TxCommit $ RegistrationId rid
    ("register", Just rid, Nothing) -> Right $ TxRegister $ RegistrationId rid
    ("edit", Nothing, Just editId) -> Right $ TxEdit editId
    _ -> Left $ "name_transactions.kind: " <> kind
  pure NameTransaction {ntId, ntFor, ntRaw, ntHash, ntNonce = word64 nonce, ntSentAt, ntMinedBlock = word64 <$> block, ntStatus}

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
      | state == minedEdit = Right $ ESMined $ fromBinary <$> minedTx
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
      (stateText, revert, Binary <$> minedTx, editId, pendingEdit)
  where
    (stateText, revert, minedTx) = case state of
      ESPending -> (pendingEdit, Nothing, Nothing)
      ESMined tx -> (minedEdit, Nothing, tx)
      ESReverted name -> (revertedEdit, name, Nothing)

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
