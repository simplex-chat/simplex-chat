{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Service
  ( welcomeGetOpts,
    badgeService,
    badgeServiceCLI,
    -- * Exposed for BadgeServiceTests: the catch-all's forcing behaviour is proved directly
    -- against a lazily-thunked pure exception, not just observed indirectly through the RPC
    -- round trip (which cannot yet construct one -- see B5's report).
    runHandler,
  )
where

import BadgeService.Catalog (catalogTotals, seedCatalog)
import BadgeService.Config (BadgeServiceEnv (..), checkFailureBuckets, newBadgeServiceEnv, readBadgeServiceConfig, takeCatalogBucket)
import BadgeService.Ledger (LedgerState (..), advance)
import BadgeService.Options
import BadgeService.Store
  ( BadgePurchaseRow (..),
    ServiceError (..),
    appendLedgerEntry,
    getActiveCatalog,
    getLastLedgerEntry,
    getLedgerSince,
    getPurchaseByKey,
    withServiceTransaction,
  )
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Logger.Simple
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Word (Word32)
import Simplex.Chat.Badges.Service
  ( BadgeCatalog (..),
    BadgeOffer (..),
    BadgePrice (..),
    BadgeServiceCommand (..),
    BadgeServiceErrorCode (..),
    BadgeServiceRequest (..),
    BadgeServiceResponse (..),
    BadgeStatement (..),
    StatementCreditType (..),
    StatementDebitType (..),
    StatementEntry (..),
    StatementEntryType (..),
    minSupportedBadgeVersion,
  )
import Simplex.Chat.Badges.Types
  ( BadgeLedgerEntry (..),
    BadgeOfferId (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
  )
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceEnv :: TMVar BadgeServiceEnv,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object)
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceEnv <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceEnv, serviceRequestQ}

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

badgeService :: BadgeServiceOpts -> ChatConfig -> IO ()
badgeService opts cfg = do
  env <- newServiceState
  let chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts env,
            postStartHook = Just $ badgePostStartHook opts env
          }
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc -> do
    -- preStartHook (badgePreStartHook) already ran and populated serviceEnv by the time this
    -- callback starts (Core.hs runs it before postStartHook, which runs before this), so a
    -- single read here is safe -- the value never changes again for the life of the process.
    bsEnv <- atomically $ readTMVar $ serviceEnv env
    forever $ do
      (_, event) <- atomically . readTBQueue $ outputQ cc
      case event of
        Right (CEvtServiceRequest u reqId sigKey_ reqData) -> handleServiceRequest bsEnv cc u reqId sigKey_ reqData
        _ -> pure ()

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts = do
  env <- newServiceState
  let eventHook _cc ev = do
        case ev of
          Right (CEvtServiceRequest u reqId sigKey_ reqData) ->
            atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey_, reqData)
          _ -> pure ()
        pure ev
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts env,
            postStartHook = Just $ badgePostStartHook opts env,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests env
    ]

processQueuedRequests :: ServiceState -> IO ()
processQueuedRequests env = do
  cc <- atomically $ readTMVar $ serviceCC env
  bsEnv <- atomically $ readTMVar $ serviceEnv env
  forever $ do
    (u, reqId, sigKey_, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest bsEnv cc u reqId sigKey_ reqData

-- Seeded here, after migrations and before badgePostStartHook starts the bot: every start
-- of the service must see the catalog before it can serve a request. B8's operator
-- subcommand (not yet implemented) will need to call seedCatalog the same way, so operator
-- tooling sees the same catalog.
--
-- The badge service env is built here too, once, after migrations and seedCatalog: it reads
-- and validates badge_service.ini, exits on a bad config (naming the file and the offending
-- key), and stores the built env for badgePostStartHook and the request handlers to reach.
badgePreStartHook :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePreStartHook opts@BadgeServiceOpts {configFile} ServiceState {serviceEnv} ChatController {config, chatStore} = do
  runBadgeServiceMigrations opts config chatStore
  seedCatalog chatStore
  readBadgeServiceConfig configFile >>= \case
    Left e -> putStrLn e >> exitFailure
    Right bsConfig -> do
      bsEnv <- newBadgeServiceEnv bsConfig chatStore
      atomically $ putTMVar serviceEnv bsEnv

badgePostStartHook :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} env cc = do
  -- SREQ delivery gates on this flag; Core starts serviceRequests=False, so the hook must set it.
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    -- DR required for service RPC; autoAccept off because badge service ignores contact events.
    Just _ -> do
      unless noAddress $ initializeBotAddress' (not testing) (Just True) False cc
      void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: BadgeServiceEnv -> ChatController -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest bsEnv cc User {userId} reqId signerKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  respObj <- runHandler reqIdT (dispatchRequest bsEnv signerKey reqData)
  sendChatCmd cc (APISendServiceResponse userId reqId respObj) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

-- | Runs 'action' and converts its result to a wire object, forcing that object fully
-- (encoding it and demanding the whole encoded length) before returning, all still inside the
-- 'catch' below. Laziness would otherwise let an exception escape uncaught: 'action' finishing
-- and returning a lazily-built 'BadgeServiceResponse' does not itself throw, even if a field
-- deep inside is an unevaluated 'error' thunk (as 'BadgeService.Catalog.chargeableMonths'
-- produces for a malformed offer, once B6\/B7 wire catalog totals into request handling) --
-- that thunk would only be forced later, by 'sendChatCmd''s own JSON encoding, OUTSIDE this
-- function, where nothing would catch it. Forcing the encoding here, inside the 'catch',
-- is what makes this a genuine catch-all rather than one that only covers IO exceptions.
--
-- 'processQueuedRequests' is a single-threaded 'forever' loop: an exception that got out of
-- here would kill the service for every user, not just fail the one request that caused it.
-- Every exception, IO or pure, is logged with the request id and turned into 'internal',
-- never repeating its message back to the client.
runHandler :: Text -> IO BadgeServiceResponse -> IO J.Object
runHandler reqIdT action =
  buildResponse `catch` \(e :: SomeException) -> do
    logError $ "badge service internal error for " <> reqIdT <> ": " <> tshow e
    pure $ responseObject BSPError {code = BSEInternal, message = Nothing, retryAfter = Nothing}
  where
    buildResponse = do
      resp <- action
      let obj = responseObject resp
      _ <- evaluate (LBS.length (J.encode (J.Object obj)))
      pure obj

-- | 'BadgeServiceResponse's four constructors are all records, so 'taggedObjectJSON' always
-- encodes them as a JSON object (never a bare string) -- the wildcard case cannot be hit by
-- any value of this type; it exists only so this function is total.
responseObject :: BadgeServiceResponse -> J.Object
responseObject resp = case J.toJSON resp of
  J.Object o -> o
  _ -> error "BadgeServiceResponse always encodes to a JSON object"

errorResponse :: BadgeServiceErrorCode -> Maybe Text -> Maybe Word32 -> BadgeServiceResponse
errorResponse code message retryAfter = BSPError {code, message, retryAfter}

notImplemented :: BadgeServiceResponse
notImplemented = errorResponse BSEInternal (Just "not implemented") Nothing

-- | Decode, version gate, and the signer\/record precondition (RPC doc "Identity"), then
-- dispatch. Order matches badges-rpc.md and the B5 brief: a decode failure is 'bad_request'
-- before anything else is even looked at; then the version gate; then the signer check
-- (badges-rpc.md: "the service rejects a purchaseKey that differs from [the verified signer]
-- with bad_request"); then the per-command signer\/record rule; only then dispatch.
dispatchRequest :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
dispatchRequest bsEnv signerKey reqData =
  case decodeRequest reqData of
    Left _ -> pure $ errorResponse BSEBadRequest Nothing Nothing
    Right BadgeServiceRequest {version, purchaseKey, request}
      | version < minSupportedBadgeVersion -> pure $ errorResponse BSEUnsupportedVersion Nothing Nothing
      | signerKey /= purchaseKey -> pure $ errorResponse BSEBadRequest Nothing Nothing
      | otherwise ->
          checkSignerRecord bsEnv request purchaseKey >>= \case
            Left err -> pure $ errorResponse err Nothing Nothing
            Right () -> dispatchCommand bsEnv purchaseKey request

decodeRequest :: J.Object -> Either String BadgeServiceRequest
decodeRequest = JT.parseEither J.parseJSON . J.Object

-- | The signer\/record precondition, applied to every command before dispatch:
--   * 'getBadgeCatalog' may be unsigned (no key at all); nothing further is required of it.
--   * 'purchaseBadge' requires a signature but NOT a pre-existing record -- an unknown key is
--     the normal first-purchase case, because B7 is what creates the purchase row. Getting
--     this inverted would make first purchases impossible.
--   * every other command, including a *signed* 'getBadgeCatalog', requires both a signature
--     and an existing purchase row: no key at all is 'bad_request' (nothing was signed), an
--     unknown key is 'unknown_purchase_key'.
checkSignerRecord :: BadgeServiceEnv -> BadgeServiceCommand -> Maybe C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode ())
checkSignerRecord _ BSCGetBadgeCatalog Nothing = pure $ Right ()
checkSignerRecord bsEnv BSCGetBadgeCatalog (Just key) = requirePurchaseRecord bsEnv key
checkSignerRecord _ (BSCPurchaseBadge {}) Nothing = pure $ Left BSEBadRequest
checkSignerRecord _ (BSCPurchaseBadge {}) (Just _) = pure $ Right ()
checkSignerRecord _ _ Nothing = pure $ Left BSEBadRequest
checkSignerRecord bsEnv _ (Just key) = requirePurchaseRecord bsEnv key

requirePurchaseRecord :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode ())
requirePurchaseRecord BadgeServiceEnv {store} key =
  withServiceTransaction store (\db -> getPurchaseByKey db key) >>= \case
    Right (Just _) -> pure $ Right ()
    Right Nothing -> pure $ Left BSEUnknownPurchaseKey
    Left _ -> pure $ Left BSEInternal

-- | Dispatch on the command, once the signer\/record precondition already passed.
-- 'getBadgeInvoice', 'upgradeBadgeSubscription' and 'pauseBadge' are out of scope (decision 5
-- \/ §6) and always 'bad_request'; 'getBadgeCatalog' and 'issueBadge' are B6\/B7's commands and
-- answer 'internal' \"not implemented\" until those steps land.
dispatchCommand :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
dispatchCommand _ _ (BSCGetBadgeInvoice {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand _ _ (BSCUpgradeBadgeSubscription {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand _ _ BSCPauseBadge = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand bsEnv purchaseKey BSCGetBadgeCatalog = handleGetBadgeCatalog bsEnv purchaseKey
dispatchCommand _ _ (BSCIssueBadge {}) = pure notImplemented
dispatchCommand bsEnv purchaseKey (BSCPurchaseBadge {payment}) = dispatchPurchase bsEnv purchaseKey payment

-- | 'checkSignerRecord' already requires a signature for every 'purchaseBadge', so
-- 'purchaseKey' is 'Just' here in every reachable case; the 'Nothing' clause only keeps this
-- function total.
--
-- Only 'SPCode' is implemented (B7); the others verify store evidence or transfer a receipt,
-- both out of scope (§6), so they are 'bad_request' permanently, not \"not implemented\".
--
-- The throttle (B5 decision 5) runs before 'SPCode' is processed: an empty per-signer or
-- global-failure bucket rejects the request with 'rate_limited' before it would otherwise
-- reach B7's (not yet implemented) code classifier. Neither bucket is debited here --
-- 'checkFailureBuckets' only peeks; only a classified failure debits, which is B7's job.
dispatchPurchase :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> ServicePayment -> IO BadgeServiceResponse
dispatchPurchase bsEnv (Just signerKey) (SPCode _code) =
  checkFailureBuckets bsEnv signerKey >>= \case
    Left retryAfter -> pure $ errorResponse BSERateLimited Nothing (Just retryAfter)
    Right () -> pure notImplemented
dispatchPurchase _ Nothing (SPCode _) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPApple {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPGoogle {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPInvoice {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPReceipt {}) = pure $ errorResponse BSEBadRequest Nothing Nothing

-- getBadgeCatalog (B6) --------------------------------------------------------

-- | Answers the catalog, and for a signed request the signer's statement as well.
--
-- Unsigned requests spend a token from the service-wide catalog bucket first (B5 decision 5):
-- there is no signer to key on and no failure to count, so the request itself is the only
-- thing that can be bounded. A signed request is not subject to it -- 'checkSignerRecord'
-- has already required an existing purchase row, which is the bound.
--
-- Both halves are read in ONE transaction, and it is a writing one: healing the ledger
-- (@advance now@) persists its @debit(lapse)@ row in the same transaction that then reads the
-- statement back, so the balance a client is told is the balance the database holds. This is
-- the only read command that writes (RPC "Statement and balance").
handleGetBadgeCatalog :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> IO BadgeServiceResponse
handleGetBadgeCatalog bsEnv@BadgeServiceEnv {store, now} signerKey = case signerKey of
  Nothing ->
    takeCatalogBucket bsEnv >>= \case
      Left retryAfter -> pure $ errorResponse BSERateLimited Nothing (Just retryAfter)
      Right () -> respond Nothing
  Just key -> respond (Just key)
  where
    respond key = do
      now' <- now
      withServiceTransaction store (catalogTxn now' key) >>= \case
        Left e -> do
          logError $ "getBadgeCatalog failed: " <> tshow e
          pure $ errorResponse BSEInternal Nothing Nothing
        Right (catalog, badgeStatement) -> do
          logUnpricedOffers catalog
          pure BSPBadgeCatalog {catalog, badgeStatement}
    catalogTxn now' key db = do
      -- catalogTotals is applied to what the DATABASE holds, never to Catalog.hs's defaults,
      -- so a price the operator deprecated or disabled is reflected without a rebuild
      -- (decision 8): the site, the RPC catalog and the charge all read this one result.
      catalog <- catalogTotals <$> getActiveCatalog db
      statement <- mapM (purchaseStatement now' db) key
      pure (catalog, statement)

-- | An offer that is pinned to a price the catalog also returned, yet still has no total,
-- is a malformed offer (@freeMonths >= months@): the client will render it as unavailable,
-- and nothing else in the system would ever say why. 'chargeableMonths' stopped saying so
-- with 'error' precisely so a request thread survives it (§9), so this is the only place it
-- gets named.
logUnpricedOffers :: BadgeCatalog -> IO ()
logUnpricedOffers BadgeCatalog {prices, offers} =
  forM_ offers $ \BadgeOffer {offerId = BadgeOfferId oid, priceId, total} ->
    case (priceId, total) of
      (Just pid, Nothing)
        | any (\BadgePrice {priceId = pid'} -> pid' == pid) prices ->
            logWarn $ "catalog offer " <> oid <> " has a pinned price but no chargeable total"
      _ -> pure ()

-- | Heals the purchase's ledger to @now@, then reads the whole of it back as a statement.
--
-- @advance@ is run against the last stored entry's state and, when it yields months, ONE
-- @debit(lapse)@ row is written for them (B2's calling convention: one row, whatever @k@ is).
-- A purchase with no ledger at all has nothing to heal and nothing to lapse -- there is no
-- balance to lapse from -- so it returns an empty statement rather than inventing an opening
-- entry.
--
-- 'previousEntryId' is 'Nothing': 'getBadgeCatalog' carries no cursor, so this is always the
-- full ledger, which is what that field's absence means.
purchaseStatement :: UTCTime -> DB.Connection -> C.PublicKeyEd25519 -> ExceptT ServiceError IO BadgeStatement
purchaseStatement now' db key = do
  purchase <- getPurchaseByKey db key
  case purchase of
    -- unreachable: checkSignerRecord already required the row for a signed request. Refused
    -- rather than answered with an empty statement, which would look like a real ledger.
    Nothing -> throwError $ SEDecodeError "getBadgeCatalog: signer has no purchase row"
    Just BadgePurchaseRow {badgePurchaseId} -> do
      healLedger now' db badgePurchaseId
      entries <- mapM (liftEither' . toStatementEntry) =<< getLedgerSince db badgePurchaseId Nothing
      pure BadgeStatement {entries, previousEntryId = Nothing}
  where
    liftEither' = either throwError pure

healLedger :: UTCTime -> DB.Connection -> Int64 -> ExceptT ServiceError IO ()
healLedger now' db badgePurchaseId =
  getLastLedgerEntry db badgePurchaseId >>= \case
    Nothing -> pure ()
    Just BadgeLedgerEntry {balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince} ->
      case advance now' LedgerState {balanceMonths, balanceStartTs, balanceBadgeType} of
        Nothing -> pure ()
        Just (k, LedgerState {balanceMonths = balanceMonths', balanceStartTs = balanceStartTs'}) -> do
          entryUuid <- liftIO (UUID.toText <$> UUID.nextRandom)
          void $
            appendLedgerEntry
              db
              BadgeLedgerEntry
                { entryId = 0, -- assigned by the database
                  entryUuid,
                  badgePurchaseId,
                  changeMonths = negate k,
                  balanceMonths = balanceMonths',
                  -- the entry's balance_start_ts is the state advance left, NOT the time the
                  -- row was created; created_at/service_created_at carry that (B2)
                  balanceStartTs = balanceStartTs',
                  balanceBadgeType,
                  wasPausedSince,
                  serviceCreatedAt = now',
                  createdAt = now',
                  entryType = LEDebit DTLapse
                }

-- | The stored ledger row as the client sees it. 'entryId' on the wire is the row's
-- @entry_uuid@, not its @entry_id@: the uuid is what the service authors and the client
-- copies verbatim (core §1), while @entry_id@ is a per-database IDENTITY that means nothing
-- outside this one service.
--
-- Four entry types are refused rather than converted, and none of them can be reached by
-- anything in this milestone:
--
--   * @CTPayment@ and @CTCharge@ carry 'Int64' ids against @TEXT@ columns -- the unresolved
--     mismatch §9 records as needing a decision before C1. 'BadgeService.Store' already
--     refuses to read or write them, so a row of either type cannot exist; inventing a
--     numeric-to-text coercion here is exactly what that refusal exists to prevent.
--   * @CTTransferIn@, @DTUpgrade@ and @DTTransferOut@ store a purchase *id* while the wire
--     types carry a purchase *key*. Converting needs an id-to-key lookup the store does not
--     expose, and transfers and upgrades are out of scope (§6), so nothing writes them.
--
-- A refusal fails the whole response with 'internal' rather than dropping the entry: a
-- statement that silently omits a ledger row is a wrong balance, which is worse than no
-- answer.
toStatementEntry :: BadgeLedgerEntry -> Either ServiceError StatementEntry
toStatementEntry BadgeLedgerEntry {entryUuid, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, entryType} = do
  entryType' <- statementEntryType entryType
  Right
    StatementEntry
      { entryId = entryUuid,
        changeMonths,
        balanceMonths,
        balanceStartTs,
        balanceBadgeType,
        wasPausedSince,
        createdAt,
        entryType = entryType'
      }

statementEntryType :: LedgerEntryType -> Either ServiceError StatementEntryType
statementEntryType = \case
  LECredit creditType -> SECredit <$> case creditType of
    CTSupport -> Right SCSupport
    CTOpening -> Right SCOpening
    CTUnknown {tag, json} -> Right SCUnknown {tag, json}
    CTPayment {} -> unresolved "credit(payment)" "invoiceId is Int64 against a TEXT column (§9, open before C1)"
    CTCharge {} -> unresolved "credit(charge)" "chargeId is Int64 against a TEXT column (§9, open before C1)"
    CTTransferIn {} -> unresolved "credit(transfer_in)" "stores a purchase id, the wire carries a purchase key; transfers are out of scope (§6)"
  LEDebit debitType -> SEDebit <$> case debitType of
    DTRefund -> Right SDRefund
    DTSupport -> Right SDSupport
    DTBadge -> Right SDBadge
    DTLapse -> Right SDLapse
    DTUnknown {tag, json} -> Right SDUnknown {tag, json}
    DTUpgrade {} -> unresolved "debit(upgrade)" "stores a purchase id, the wire carries a purchase key; upgrades are out of scope (§6)"
    DTTransferOut {} -> unresolved "debit(transfer_out)" "stores a purchase id, the wire carries a purchase key; transfers are out of scope (§6)"
  where
    unresolved what why = Left $ SEDecodeError ("cannot put " <> what <> " in a statement: " <> why)
