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
import BadgeService.Codes (RedeemOutcome (..), classifyRedemption, codeHash, normalizeCode)
import BadgeService.Config
  ( BadgeServiceConfig (issuer),
    BadgeServiceEnv (..),
    IssuerConfig (issuerKeyIdx),
    checkFailureBuckets,
    debitFailureBuckets,
    newBadgeServiceEnv,
    readBadgeServiceConfig,
    sweepSignerBucketsIO,
    takeCatalogBucket,
  )
import BadgeService.Credentials (issueSignedBadge)
import BadgeService.Ledger (LedgerState (..), advance, credit, initialLedgerState, issue)
import BadgeService.Options
import BadgeService.Store
  ( BadgePurchaseRow (..),
    NewIssuance (..),
    ServiceError (..),
    appendLedgerEntry,
    attachPurchasePayment,
    createCodePayment,
    createIssuance,
    createPurchase,
    getActiveCatalog,
    getCodeByHash,
    getIssuanceForPeriod,
    getIssuanceForRedeemedCode,
    getLastLedgerEntry,
    getLedgerEntryIdByUuid,
    getLedgerSince,
    getPurchaseByKey,
    markCodeRedeemed,
    withServiceTransaction,
  )
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Logger.Simple
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Word (Word32)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey, BadgeRequest (..), BadgeType)
import Simplex.Chat.Badges.Service
  ( BadgeBalance (..),
    BadgeCatalog (..),
    BadgeOffer (..),
    BadgePrice (..),
    BadgeServiceCommand (..),
    BadgeServiceErrorCode (..),
    BadgeServiceRequest (..),
    BadgeServiceResponse (..),
    BadgeStatement (..),
    BadgeUpgrade,
    StatementCreditType (..),
    StatementDebitType (..),
    StatementEntry (..),
    -- 'StatementEntryType' and 'LedgerEntryType' import their constructors only: their fields
    -- are both named 'credit'\/'debit' and would collide with each other and with
    -- 'BadgeService.Ledger.credit', which this module calls.
    StatementEntryType (SECredit, SEDebit),
    minSupportedBadgeVersion,
  )
import Simplex.Chat.Badges.Types
  ( BadgeIssuance (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (LECredit, LEDebit),
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
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc ->
    raceAny_ [processServiceEvents env cc, sweepSignerBucketsLoop env]

processServiceEvents :: ServiceState -> ChatController -> IO ()
processServiceEvents env cc = do
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
      processQueuedRequests env,
      sweepSignerBucketsLoop env
    ]

processQueuedRequests :: ServiceState -> IO ()
processQueuedRequests env = do
  cc <- atomically $ readTMVar $ serviceCC env
  bsEnv <- atomically $ readTMVar $ serviceEnv env
  forever $ do
    (u, reqId, sigKey_, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest bsEnv cc u reqId sigKey_ reqData

-- | How often the per-signer failure-bucket map is swept. Ten minutes is short against the
-- hour a default bucket takes to refill and long against how often a bucket is created (only a
-- classified redemption failure creates one), so the sweep is close to free while keeping the
-- map's steady-state size well under the growth cap 'debitFailureBuckets' already guarantees.
signerBucketSweepIntervalSeconds :: Int
signerBucketSweepIntervalSeconds = 600

-- | Runs the per-signer bucket sweep on a timer, as a third arm of the service's 'raceAny_'
-- alongside the bot and (in the CLI path) the terminal. B5 built 'sweepSignerBucketsIO' and
-- left it unscheduled because nothing there could create a map entry; B7 is the first step
-- whose redemptions can fail, so it is the first that needs the sweep to actually run (plan
-- \'9). The interval is real time -- 'threadDelay', not 'BadgeServiceEnv.now' -- because it
-- schedules the sweep rather than deciding anything; the eviction itself reads the injectable
-- clock through 'sweepSignerBucketsIO', so a test proves eviction by calling that directly
-- rather than waiting on this loop.
sweepSignerBucketsLoop :: ServiceState -> IO ()
sweepSignerBucketsLoop env = do
  bsEnv <- atomically $ readTMVar $ serviceEnv env
  forever $ do
    threadDelay $ signerBucketSweepIntervalSeconds * 1000000
    evicted <- sweepSignerBucketsIO bsEnv
    when (evicted > 0) $ logInfo $ "badge service swept " <> tshow evicted <> " recovered signer failure buckets"

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
-- deep inside is an unevaluated 'error' thunk -- that thunk would only be forced later, by
-- 'sendChatCmd''s own JSON encoding, OUTSIDE this function, where nothing would catch it.
-- Forcing the encoding here, inside the 'catch', is what makes this a genuine catch-all
-- rather than one that only covers IO exceptions. Nothing in the currently implemented
-- commands is known to build such a thunk -- B6 closed the one concrete hazard this used to
-- cite by name, 'BadgeService.Catalog.chargeableMonths', which now returns 'Maybe' instead of
-- calling 'error' -- so this guards against a partial function in some future response field,
-- not a specific one today; 'testBadgeServiceCatchAllContainsPureException' proves the
-- mechanism directly against a constructed thunk, since no real one currently exists to test
-- against.
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

badRequest :: BadgeServiceResponse
badRequest = errorResponse BSEBadRequest Nothing Nothing

-- | A store error is never repeated back to the client: it is logged with the command that
-- produced it and answered with 'internal', like every other unexpected failure.
storeFailed :: Text -> ServiceError -> IO BadgeServiceResponse
storeFailed what e = do
  logError $ what <> " failed: " <> tshow e
  pure $ errorResponse BSEInternal Nothing Nothing

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
            Right purchaseRow -> dispatchCommand bsEnv purchaseKey purchaseRow request

decodeRequest :: J.Object -> Either String BadgeServiceRequest
decodeRequest = JT.parseEither J.parseJSON . J.Object

-- | The signer\/record precondition, applied to every command before dispatch. Returns the
-- looked-up row on success ('Nothing' when none was required), so a handler that needs it
-- (B6's 'handleGetBadgeCatalog'; B7's future 'issueBadge') reads it once here rather than
-- looking it up again itself in a second transaction:
--   * 'getBadgeCatalog' may be unsigned (no key at all); nothing further is required of it,
--     and there is no row to return.
--   * 'purchaseBadge' requires a signature but NOT a pre-existing record -- an unknown key is
--     the normal first-purchase case, because B7 is what creates the purchase row. Getting
--     this inverted would make first purchases impossible. No row is looked up, so none is
--     returned even though the request is signed.
--   * every other command, including a *signed* 'getBadgeCatalog', requires both a signature
--     and an existing purchase row: no key at all is 'bad_request' (nothing was signed), an
--     unknown key is 'unknown_purchase_key'; found, the row is returned.
checkSignerRecord :: BadgeServiceEnv -> BadgeServiceCommand -> Maybe C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode (Maybe BadgePurchaseRow))
checkSignerRecord _ BSCGetBadgeCatalog Nothing = pure $ Right Nothing
checkSignerRecord bsEnv BSCGetBadgeCatalog (Just key) = requirePurchaseRecord bsEnv key
checkSignerRecord _ (BSCPurchaseBadge {}) Nothing = pure $ Left BSEBadRequest
checkSignerRecord _ (BSCPurchaseBadge {}) (Just _) = pure $ Right Nothing
checkSignerRecord _ _ Nothing = pure $ Left BSEBadRequest
checkSignerRecord bsEnv _ (Just key) = requirePurchaseRecord bsEnv key

requirePurchaseRecord :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode (Maybe BadgePurchaseRow))
requirePurchaseRecord BadgeServiceEnv {store} key =
  withServiceTransaction store (\db -> getPurchaseByKey db key) >>= \case
    Right (Just row) -> pure $ Right (Just row)
    Right Nothing -> pure $ Left BSEUnknownPurchaseKey
    Left _ -> pure $ Left BSEInternal

-- | Dispatch on the command, once the signer\/record precondition already passed.
-- 'getBadgeInvoice', 'upgradeBadgeSubscription' and 'pauseBadge' are out of scope (decision 5
-- \/ §6) and always 'bad_request'. 'getBadgeCatalog' (B6) and 'issueBadge' (B7) are the two
-- commands that use the 'Maybe' 'BadgePurchaseRow' 'checkSignerRecord' already looked up;
-- every other clause below ignores it.
dispatchCommand :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> Maybe BadgePurchaseRow -> BadgeServiceCommand -> IO BadgeServiceResponse
dispatchCommand _ _ _ (BSCGetBadgeInvoice {}) = pure badRequest
dispatchCommand _ _ _ (BSCUpgradeBadgeSubscription {}) = pure badRequest
dispatchCommand _ _ _ BSCPauseBadge = pure badRequest
dispatchCommand bsEnv _ purchaseRow BSCGetBadgeCatalog = handleGetBadgeCatalog bsEnv purchaseRow
dispatchCommand bsEnv _ purchaseRow (BSCIssueBadge {badgeRequest, balance}) = case purchaseRow of
  Just row -> handleIssueBadge bsEnv row badgeRequest balance
  -- unreachable: 'checkSignerRecord' answers 'unknown_purchase_key' for an 'issueBadge' whose
  -- key has no row, so the row is always 'Just' here; this clause only keeps the case total.
  Nothing -> pure $ errorResponse BSEUnknownPurchaseKey Nothing Nothing
dispatchCommand bsEnv purchaseKey _ (BSCPurchaseBadge {badgeRequest, payment, upgrade}) =
  dispatchPurchase bsEnv purchaseKey badgeRequest payment upgrade

-- | 'checkSignerRecord' already requires a signature for every 'purchaseBadge', so
-- 'purchaseKey' is 'Just' here in every reachable case; the 'Nothing' clause only keeps this
-- function total.
--
-- Only 'SPCode' is implemented (B7); the others verify store evidence or transfer a receipt,
-- both out of scope (§6), so they are 'bad_request' permanently. So is a purchase carrying an
-- @upgrade@: the store one-time upgrade it proves eligibility for needs store evidence, and
-- tier upgrades are out of scope too (§6), so it is refused before the payment is even looked
-- at rather than silently ignored while the code is consumed.
--
-- The throttle (B5 decision 5) runs before 'SPCode' is processed: an empty per-signer or
-- global-failure bucket rejects the request with 'rate_limited' before it reaches the code
-- classifier. Neither bucket is debited here -- 'checkFailureBuckets' only peeks; only a
-- classified failure debits, which 'handlePurchaseCode' does.
dispatchPurchase :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> BadgeRequest -> ServicePayment -> Maybe BadgeUpgrade -> IO BadgeServiceResponse
dispatchPurchase _ _ _ _ (Just _) = pure badRequest
dispatchPurchase bsEnv (Just signerKey) badgeRequest (SPCode code) Nothing =
  checkFailureBuckets bsEnv signerKey >>= \case
    Left retryAfter -> pure $ errorResponse BSERateLimited Nothing (Just retryAfter)
    Right () -> handlePurchaseCode bsEnv signerKey badgeRequest code
dispatchPurchase _ Nothing _ (SPCode _) Nothing = pure badRequest
dispatchPurchase _ _ _ (SPApple {}) Nothing = pure badRequest
dispatchPurchase _ _ _ (SPGoogle {}) Nothing = pure badRequest
dispatchPurchase _ _ _ (SPInvoice {}) Nothing = pure badRequest
dispatchPurchase _ _ _ (SPReceipt {}) Nothing = pure badRequest

-- getBadgeCatalog (B6) --------------------------------------------------------

-- | Answers the catalog, and for a signed request the signer's statement as well.
--
-- Takes the 'Maybe' 'BadgePurchaseRow' 'checkSignerRecord' already looked up ('Nothing' for
-- an unsigned request, 'Just' the row for a signed one -- an unknown signed key never reaches
-- here, 'checkSignerRecord' already answered 'unknown_purchase_key'), rather than a key: a
-- second lookup by key here would open a second transaction reading the same row.
--
-- Unsigned requests spend a token from the service-wide catalog bucket first (B5 decision 5):
-- there is no signer to key on and no failure to count, so the request itself is the only
-- thing that can be bounded. A signed request is not subject to it -- the row already in hand
-- is the bound.
--
-- Both halves are read in ONE transaction, and it is a writing one: healing the ledger
-- (@advance now@) persists its @debit(lapse)@ row in the same transaction that then reads the
-- statement back, so the balance a client is told is the balance the database holds. This is
-- the only read command that writes (RPC "Statement and balance").
handleGetBadgeCatalog :: BadgeServiceEnv -> Maybe BadgePurchaseRow -> IO BadgeServiceResponse
handleGetBadgeCatalog bsEnv@BadgeServiceEnv {store, now} purchaseRow = case purchaseRow of
  Nothing ->
    takeCatalogBucket bsEnv >>= \case
      Left retryAfter -> pure $ errorResponse BSERateLimited Nothing (Just retryAfter)
      Right () -> respond Nothing
  Just row -> respond (Just row)
  where
    respond row = do
      now' <- now
      withServiceTransaction store (catalogTxn now' row) >>= \case
        Left e -> do
          logError $ "getBadgeCatalog failed: " <> tshow e
          pure $ errorResponse BSEInternal Nothing Nothing
        Right (catalog, badgeStatement) -> do
          logUnpricedOffers catalog
          pure BSPBadgeCatalog {catalog, badgeStatement}
    catalogTxn now' row db = do
      -- catalogTotals is applied to what the DATABASE holds, never to Catalog.hs's defaults,
      -- so a price the operator deprecated or disabled is reflected without a rebuild
      -- (decision 8): the site, the RPC catalog and the charge all read this one result.
      catalog <- catalogTotals <$> getActiveCatalog db
      -- getBadgeCatalog carries no cursor, so this is always the full ledger (Nothing).
      statement <- mapM (\BadgePurchaseRow {badgePurchaseId} -> purchaseStatement now' db Nothing badgePurchaseId) row
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

-- | A resolved statement cursor: the wire @entryId@ the client asserted, and the local
-- @entry_id@ it resolved to. The two travel together so 'previousEntryId' always echoes the
-- value the client actually sent, and is never spelled independently of the id the query runs
-- on. A cursor exists only when the assertion resolved to an entry of this very purchase
-- ('getLedgerEntryIdByUuid'); an assertion naming nothing yields no cursor, and the RPC's
-- other permitted answer -- the complete history -- is what follows.
data StatementCursor = StatementCursor
  { cursorEntryId :: Int64,
    cursorEntryUuid :: Text
  }

-- | Heals the purchase's ledger to @now@, then reads it back as a statement -- the whole
-- ledger when there is no cursor, or only entries strictly after it otherwise (matches
-- 'getLedgerSince'\'s own semantics).
--
-- @advance@ is run against the last stored entry's state and, when it yields months, ONE
-- @debit(lapse)@ row is written for them (B2's calling convention: one row, whatever @k@ is).
-- A purchase with no ledger at all has nothing to heal and nothing to lapse -- there is no
-- balance to lapse from -- so it returns an empty statement rather than inventing an opening
-- entry.
--
-- Called at the end of the same transaction that wrote the command's entries, so the balance a
-- client is told is always the balance the database holds; a command with nothing to write
-- calls it in a transaction of its own that then writes nothing (the heal above is the only
-- write it could make, and it makes none when @advance@ yields nothing).
--
-- 'getBadgeCatalog' (B6) and 'purchaseBadge' (B7) pass no cursor -- neither command carries an
-- asserted entry -- for which 'previousEntryId' being 'Nothing' is exactly right (RPC: "absent
-- for the full ledger"). 'issueBadge' carries @balance.lastEntry@ and passes the cursor that
-- resolved from it.
purchaseStatement :: UTCTime -> DB.Connection -> Maybe StatementCursor -> Int64 -> ExceptT ServiceError IO BadgeStatement
purchaseStatement now' db cursor badgePurchaseId = do
  healLedger now' db badgePurchaseId
  entries <- mapM (liftEither' . toStatementEntry) =<< getLedgerSince db badgePurchaseId (cursorEntryId <$> cursor)
  pure BadgeStatement {entries, previousEntryId = cursorEntryUuid <$> cursor}
  where
    liftEither' = either throwError pure

-- | The lapse half of a plan on its own: @advance@ against the last stored entry, written
-- through the same 'writeLedgerPlan' every command uses, so the @debit(lapse)@ row a heal
-- appends is constructed in exactly one place. A purchase with no ledger has nothing to heal.
healLedger :: UTCTime -> DB.Connection -> Int64 -> ExceptT ServiceError IO ()
healLedger now' db badgePurchaseId =
  getLastLedgerEntry db badgePurchaseId >>= \case
    Nothing -> pure ()
    Just entry@BadgeLedgerEntry {balanceBadgeType, wasPausedSince} ->
      writeLedgerPlan
        db
        now'
        badgePurchaseId
        balanceBadgeType
        LedgerPlan
          { lpLapse = advance now' (ledgerStateOf entry),
            lpCredit = Nothing,
            lpIssue = IssuedNone,
            lpWasPausedSince = wasPausedSince
          }

-- | The stored ledger row as the client sees it. 'entryId' on the wire is the row's
-- @entry_uuid@, not its @entry_id@: the uuid is what the service authors and the client
-- copies verbatim (core §1), while @entry_id@ is a per-database IDENTITY that means nothing
-- outside this one service.
--
-- Four entry types are refused rather than converted, and none of them can be reached by
-- anything in this milestone:
--
--   * @CTCharge@ carries an 'Int64' id against a @TEXT@ column -- the unresolved mismatch §9
--     records. 'BadgeService.Store' already refuses to read or write it, so a row of that type
--     cannot exist; inventing a numeric-to-text coercion here is exactly what that refusal
--     exists to prevent. Subscriptions are out of scope (§6), so nothing writes one.
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
    -- The stored id is the PAYMENT's, and the wire field is the INVOICE's. Every payment this
    -- milestone writes is a code payment, which has no invoice at all (brief B7 step 4), so
    -- 'Nothing' is the right and only answer today. An invoice-funded payment (D-phase) reaches
    -- its invoice through @payments.invoice_id@, which is a join this pure function cannot do:
    -- whichever step first credits one must resolve the invoice id before building the entry.
    CTPayment {} -> Right SCPayment {invoiceId = Nothing}
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

-- purchaseBadge{code} and issueBadge (B7) -------------------------------------

-- | One retry of the whole redemption. A code claimed by a concurrent request between the
-- classification and the write ('SECodeConflict') is re-classified from the top, and that
-- second pass reaches a terminal answer -- a replay for the same key, @code_used@ for another
-- -- because the code is now redeemed and cannot become unredeemed by itself.
redemptionAttempts :: Int
redemptionAttempts = 1

-- | What @issue@ (B2) says should happen, computed in memory before anything is signed or
-- written.
data IssuePlan
  = -- | A period to issue: it is signed, then recorded as one @debit(badge)@ entry and one
    -- issuance row. Carries the state @issue@ left, the period start and the period end.
    IssuePeriod LedgerState UTCTime UTCTime
  | -- | The current month is already issued -- a positive balance whose @balanceStartTs@ a
    -- previous @issue@ moved past @now@. Its credential is fetched, not signed, and neither a
    -- @debit(badge)@ entry nor an issuance row is written: that month's pair already exists and
    -- B2's property 3 keeps them 1:1.
    IssueCached
  | -- | Nothing to issue: the balance is exhausted. Not an error -- the statement shows why.
    IssueExhausted

-- | An 'IssuePlan' with its credential resolved.
data IssueResult
  = IssuedPeriod LedgerState UTCTime UTCTime BadgeCredential
  | IssuedCached BadgeCredential
  | IssuedNone

issuedCredential :: IssueResult -> Maybe BadgeCredential
issuedCredential = \case
  IssuedPeriod _ _ _ cred -> Just cred
  IssuedCached cred -> Just cred
  IssuedNone -> Nothing

-- | Every ledger row a command will append, computed with B2's pure functions before any IO
-- happens. Parameterised over the issue step so one value carries first the plan
-- (@LedgerPlan IssuePlan@) and then, once the credential is resolved, the write set
-- (@LedgerPlan IssueResult@) -- the two can never drift apart into separate values.
data LedgerPlan a = LedgerPlan
  { -- | @advance@'s @debit(lapse)@: the months lapsed and the state it left. ONE row, whatever
    -- @k@ is (B2's calling convention). It belongs to the write set even when the command has
    -- nothing else to write.
    lpLapse :: Maybe (Int, LedgerState),
    -- | The @credit(payment)@ a funded command records: the months, the entry type naming the
    -- payment row it references, and the state after crediting. 'issueBadge' credits nothing.
    lpCredit :: Maybe (Int, LedgerCreditType, LedgerState),
    lpIssue :: a,
    -- | Carried forward from the last stored entry onto every row this plan appends: it marks
    -- the entry ending a pause, and pausing is out of scope (§6), so nothing here sets or
    -- clears it.
    lpWasPausedSince :: Maybe UTCTime
  }

-- | Step 4, entirely pure: @advance@, then the command's credit if it has one, then @issue@ --
-- in that order and against one timestamp, which is B2's calling convention. Nothing here
-- touches the database, so the whole prospective state is known before a signature is asked
-- for and before a transaction is opened.
planLedger :: UTCTime -> Maybe (Int, LedgerCreditType) -> Maybe UTCTime -> LedgerState -> LedgerPlan IssuePlan
planLedger now' creditWith wasPaused st0 =
  LedgerPlan {lpLapse = lapse, lpCredit = credited, lpIssue = issuePlan, lpWasPausedSince = wasPaused}
  where
    lapse = advance now' st0
    st1 = maybe st0 snd lapse
    -- 'credit' ignores the 'StatementCreditType' it takes (B2); it only names the transition,
    -- and 'SCPayment Nothing' is right for every credit this step records -- a code payment has
    -- no invoice (brief step 4).
    credited = (\(n, creditType) -> (n, creditType, credit now' n (SCPayment Nothing) st1)) <$> creditWith
    st2 = maybe st1 (\(_, _, st) -> st) credited
    issuePlan = case issue now' st2 of
      Just (st3, periodStart, periodEnd) -> IssuePeriod st3 periodStart periodEnd
      Nothing -> case st2 of
        LedgerState {balanceMonths = 0} -> IssueExhausted
        _ -> IssueCached

-- | Step 5: the only IO between the pure plan and the write, and the only place a credential is
-- produced. A fresh period is SIGNED (B4); an already-issued month has its credential FETCHED;
-- an exhausted balance has none. A signing failure returns its error code with nothing written
-- at all, so a redeemed-nothing code stays retryable (brief step 5).
--
-- @badgePurchaseId_@ is 'Nothing' only for a purchase that does not exist yet, which cannot be
-- in the 'IssueCached' state -- that state needs a stored ledger entry, which needs a purchase.
resolveIssue :: BadgeServiceEnv -> UTCTime -> Maybe Int64 -> BadgeRequest -> IssuePlan -> IO (Either BadgeServiceErrorCode IssueResult)
resolveIssue BadgeServiceEnv {config = bsConfig, store, issuerKey} now' badgePurchaseId_ badgeRequest = \case
  IssuePeriod st periodStart periodEnd ->
    issueSignedBadge (issuerKeyIdx (issuer bsConfig)) issuerKey badgeRequest periodEnd >>= \case
      Left code -> pure $ Left code
      Right cred -> pure $ Right $ IssuedPeriod st periodStart periodEnd cred
  IssueCached -> case badgePurchaseId_ of
    Nothing -> do
      logError "issue reported the current month as already issued for a purchase that does not exist"
      pure $ Left BSEInternal
    -- The already-issued month is the one containing @now@: the previous 'issue' set
    -- 'balanceStartTs' to that period's END (> now), and its START is at or before the instant
    -- that issue ran, which is at or before now. Probing at @now@ therefore names exactly that
    -- period. Stepping a month back from 'balanceStartTs' would not: 'addMonths' is not
    -- additive under clamping (31 Jan + 1 month = 28 Feb, and 28 Feb - 1 month = 28 Jan), so
    -- from a clamped boundary it can fall short of the very period it came from and pick up the
    -- issuance before it.
    Just pid ->
      withServiceTransaction store (\db -> getIssuanceForPeriod db pid now') >>= \case
        Right (Just issuance) -> pure $ Right $ IssuedCached (issuanceCredential issuance)
        Right Nothing -> do
          logError $ "no badge issuance covers the already-issued period of purchase " <> tshow pid
          pure $ Left BSEInternal
        Left e -> do
          logError $ "reading the cached badge issuance failed: " <> tshow e
          pure $ Left BSEInternal
  IssueExhausted -> pure $ Right IssuedNone

-- | Step 6's ledger writes, in the order the brief fixes: the @debit(lapse)@ @advance@ produced,
-- the @credit(payment)@, the @debit(badge)@, then the issuance carrying the signed credential
-- and pointing at that debit. A cached or exhausted issue writes neither of the last two.
--
-- Every entry this service appends is built here, including 'healLedger''s, so a column added
-- to 'BadgeLedgerEntry' cannot be filled correctly in one writer and forgotten in another.
writeLedgerPlan :: DB.Connection -> UTCTime -> Int64 -> BadgeType -> LedgerPlan IssueResult -> ExceptT ServiceError IO ()
writeLedgerPlan db now' pid badgeType LedgerPlan {lpLapse, lpCredit, lpIssue, lpWasPausedSince} = do
  forM_ lpLapse $ \(k, st) -> void $ appendEntry (negate k) st (LEDebit DTLapse)
  forM_ lpCredit $ \(n, creditType, st) -> void $ appendEntry n st (LECredit creditType)
  case lpIssue of
    IssuedPeriod st periodStart periodEnd cred -> do
      BadgeLedgerEntry {entryId} <- appendEntry (-1) st (LEDebit DTBadge)
      expiry <- either throwError pure (credentialExpiry cred)
      issuanceId <- liftIO (UUID.toText <$> UUID.nextRandom)
      void $
        createIssuance
          db
          NewIssuance
            { issuanceId,
              badgePurchaseId = pid,
              badgeType,
              periodStart,
              periodEnd,
              expiry,
              ledgerEntryId = Just entryId,
              credential = cred
            }
          now'
    IssuedCached _ -> pure ()
    IssuedNone -> pure ()
  where
    appendEntry changeMonths LedgerState {balanceMonths, balanceStartTs, balanceBadgeType} entryType = do
      entryUuid <- liftIO (UUID.toText <$> UUID.nextRandom)
      appendLedgerEntry
        db
        BadgeLedgerEntry
          { entryId = 0, -- assigned by the database
            entryUuid,
            badgePurchaseId = pid,
            changeMonths,
            balanceMonths,
            -- the entry's balance_start_ts is the state the transition left, NOT the time the
            -- row was created; created_at/service_created_at carry that (B2)
            balanceStartTs,
            balanceBadgeType,
            wasPausedSince = lpWasPausedSince,
            serviceCreatedAt = now',
            createdAt = now',
            entryType
          }

-- | The issuance row's @expiry@, read back from the credential rather than recomputed with
-- 'sundayAfter', so the stored expiry can never disagree with the one actually signed.
-- 'issueSignedBadge' always sets it (B4), so 'Nothing' is unreachable and is refused rather
-- than defaulted -- an issuance whose expiry does not match its credential is a wrong record.
credentialExpiry :: BadgeCredential -> Either ServiceError UTCTime
credentialExpiry BadgeCredential {badgeInfo = BadgeInfo {badgeExpiry}} =
  maybe (Left $ SEDecodeError "signed credential carries no badgeExpiry") Right badgeExpiry

ledgerStateOf :: BadgeLedgerEntry -> LedgerState
ledgerStateOf BadgeLedgerEntry {balanceMonths, balanceStartTs, balanceBadgeType} =
  LedgerState {balanceMonths, balanceStartTs, balanceBadgeType}

-- Accessors for fields whose names several records in scope share, so they are spelled once
-- here instead of as an ambiguous bare selector at every use.

entryWasPausedSince :: BadgeLedgerEntry -> Maybe UTCTime
entryWasPausedSince BadgeLedgerEntry {wasPausedSince} = wasPausedSince

rowPurchaseId :: BadgePurchaseRow -> Int64
rowPurchaseId BadgePurchaseRow {badgePurchaseId} = badgePurchaseId

rowPaymentId :: BadgePurchaseRow -> Maybe Text
rowPaymentId BadgePurchaseRow {paymentId} = paymentId

issuanceCredential :: BadgeIssuance -> BadgeCredential
issuanceCredential BadgeIssuance {credential} = credential

requestedBadgeType :: BadgeRequest -> BadgeType
requestedBadgeType BadgeRequest {badgeInfo = BadgeInfo {badgeType}} = badgeType

requestMasterKey :: BadgeRequest -> BadgeMasterKey
requestMasterKey BadgeRequest {masterKey} = masterKey

assertedEntryId :: BadgeBalance -> Text
assertedEntryId BadgeBalance {lastEntry = StatementEntry {entryId}} = entryId

-- | Resolves the client's asserted @balance.lastEntry.entryId@ against this purchase's ledger.
-- An assertion naming an entry the service holds is a prefix and becomes the cursor; one naming
-- anything else -- an unknown uuid, or an entry belonging to a different purchase -- yields no
-- cursor, and the complete history follows, which is the other answer the RPC permits
-- ("Statement and balance"). Its third answer, one @opening@ credit restating the balance,
-- needs opening entries, which nothing in this milestone writes.
resolveCursor :: DB.Connection -> Int64 -> Text -> ExceptT ServiceError IO (Maybe StatementCursor)
resolveCursor db pid entryUuid =
  fmap (\eid -> StatementCursor {cursorEntryId = eid, cursorEntryUuid = entryUuid}) <$> getLedgerEntryIdByUuid db pid entryUuid

-- | Redeems a code into a credential. The ordering is the contract, not a preference: the
-- classification reads, the plan is computed in memory, the credential is signed, and only then
-- is a transaction opened and written. Nothing is written before a signature succeeds or the
-- plan proves none is needed, so a failing signature leaves the code unredeemed and retryable.
--
-- Only a classified failure debits the throttle buckets (@code_invalid@, @code_used@,
-- @code_expired@, including a checksum rejection that never reached the database). A success
-- and the same-key replay debit nothing: they are not failures, and an honest client that
-- repeats a request after a timeout must not be throttled for it.
handlePurchaseCode :: BadgeServiceEnv -> C.PublicKeyEd25519 -> BadgeRequest -> Text -> IO BadgeServiceResponse
handlePurchaseCode bsEnv@BadgeServiceEnv {store, now} signerKey badgeRequest presentedCode = attempt redemptionAttempts
  where
    hash = codeHash (normalizeCode presentedCode)
    attempt attemptsLeft = do
      now' <- now
      runExceptT (classifyRedemption now' signerKey lookupCode presentedCode) >>= \case
        Left e -> storeFailed "purchaseBadge{code} classification" e
        Right outcome -> case outcome of
          -- a revoked code must read exactly like one that never existed (B3), so a guesser
          -- cannot learn that a code once existed
          RedeemInvalid -> failedRedemption BSECodeInvalid
          RedeemRevoked -> failedRedemption BSECodeInvalid
          RedeemUsedByOther -> failedRedemption BSECodeUsed
          RedeemExpired -> failedRedemption BSECodeExpired
          RedeemAlreadyRedeemedBySameKey pid -> replay now' pid
          RedeemOk badgeType months -> redeem attemptsLeft now' badgeType months
    -- Passed as an action so 'classifyRedemption' can reject a bad check character without ever
    -- forcing it: 31 of every 32 random guesses cost no database round trip, and this read
    -- transaction is not even opened for them (B3).
    lookupCode h = ExceptT $ withServiceTransaction store (\db -> getCodeByHash db h)
    failedRedemption code = do
      debitFailureBuckets bsEnv signerKey
      pure $ errorResponse code Nothing Nothing
    -- RPC "Idempotency": the same code presented again by the same key returns the credential it
    -- was already issued and records no second redemption. Healing the ledger is the only row
    -- this path can append, and only when months have genuinely lapsed since -- the RPC has the
    -- service heal its own ledger before answering any statement ("Statement and balance"), and
    -- a statement that showed a balance the database does not hold would be worse than the row.
    replay now' pid =
      withServiceTransaction store (replayTxn now' pid) >>= \case
        Left e -> storeFailed "purchaseBadge{code} replay" e
        Right (credential, statement) -> do
          when (isNothing credential) $
            logWarn $ "no badge issuance found for the redemption being replayed by purchase " <> tshow pid
          pure BSPBadgeCredential {credential, receipt = Nothing, statement}
    replayTxn now' pid db = do
      issuance <- getIssuanceForRedeemedCode db hash
      statement <- purchaseStatement now' db Nothing pid
      pure (issuanceCredential <$> issuance, statement)
    redeem attemptsLeft now' badgeType months
      -- The service signs exactly the content the client sent (RPC "Commands"), so a request
      -- naming a tier the code does not fund is refused rather than silently signed as the
      -- code's tier or, worse, as the tier asked for.
      | requestedBadgeType badgeRequest /= badgeType = pure badRequest
      | otherwise = do
          -- minted before the plan, so the credit entry can name the payment row it references,
          -- and before the transaction, so nothing but writes happens inside it
          paymentUuid <- UUID.toText <$> UUID.nextRandom
          withServiceTransaction store (planTxn now' badgeType months paymentUuid) >>= \case
            Left e -> storeFailed "purchaseBadge{code} planning" e
            Right (Left code) -> pure $ errorResponse code Nothing Nothing
            Right (Right (row_, plan)) ->
              resolveIssue bsEnv now' (rowPurchaseId <$> row_) badgeRequest (lpIssue plan) >>= \case
                Left code -> pure $ errorResponse code Nothing Nothing
                Right result ->
                  withServiceTransaction store (writeTxn now' badgeType paymentUuid row_ plan {lpIssue = result}) >>= \case
                    -- another request redeemed this code between the classification and this
                    -- write; nothing of ours committed, so re-classify and answer what the code
                    -- now is (a replay for this key, code_used for any other)
                    Left SECodeConflict | attemptsLeft > 0 -> attempt (attemptsLeft - 1)
                    Left e -> storeFailed "purchaseBadge{code} write" e
                    Right statement ->
                      pure BSPBadgeCredential {credential = issuedCredential result, receipt = Nothing, statement}
    planTxn now' badgeType months paymentUuid db =
      getPurchaseByKey db signerKey >>= \case
        -- the normal case: C4 mints a fresh key per redemption, so there is no purchase row and
        -- no ledger to read. Creating it is planned for the write transaction, not done here.
        Nothing -> pure $ Right (Nothing, planLedger now' creditWith Nothing (initialLedgerState now' badgeType))
        Just row@BadgePurchaseRow {badgePurchaseId, currentBadgeType}
          -- a repeated key is only produced by a non-standard client; a code of a different tier
          -- would have to convert the existing balance, and tier upgrades are out of scope (§6)
          | currentBadgeType /= badgeType -> pure $ Left BSEBadRequest
          | otherwise -> do
              lastEntry <- getLastLedgerEntry db badgePurchaseId
              let st0 = maybe (initialLedgerState now' badgeType) ledgerStateOf lastEntry
              pure $ Right (Just row, planLedger now' creditWith (lastEntry >>= entryWasPausedSince) st0)
      where
        creditWith = Just (months, CTPayment paymentUuid)
    writeTxn now' badgeType paymentUuid row_ plan db = do
      row <- maybe (createPurchase db signerKey (requestMasterKey badgeRequest) badgeType now') pure row_
      let pid = rowPurchaseId row
      createCodePayment db paymentUuid now'
      -- badge_purchases.payment_id is UNIQUE and holds at most one payment: a repeated key's
      -- second code still gets its own payments row, which the credit entry references, but
      -- leaves the purchase's pointer at the first one rather than repointing it
      when (isNothing (rowPaymentId row)) $ attachPurchasePayment db pid paymentUuid now'
      writeLedgerPlan db now' pid badgeType plan
      -- last, so a code claimed in between rolls back everything above with it
      markCodeRedeemed db hash pid now'
      purchaseStatement now' db Nothing pid

-- | Issues the next period from an existing balance: steps 4 to 6 with no code and no credit.
-- It is the only command that re-issues, and C3's worker is its only caller.
--
-- An exhausted balance is not an error: the response carries no credential and the statement
-- shows the zero balance. A repeat inside an already-issued month returns that month's cached
-- credential and writes nothing (RPC "Idempotency").
handleIssueBadge :: BadgeServiceEnv -> BadgePurchaseRow -> BadgeRequest -> BadgeBalance -> IO BadgeServiceResponse
handleIssueBadge bsEnv@BadgeServiceEnv {store, now} row badgeRequest badgeBalance
  -- as for purchaseBadge: the service signs the content it was sent, so a request naming a tier
  -- other than the purchase's own is refused rather than signed
  | requestedBadgeType badgeRequest /= currentType = pure badRequest
  | otherwise = do
      now' <- now
      withServiceTransaction store (planTxn now') >>= \case
        Left e -> storeFailed "issueBadge planning" e
        Right (cursor, plan) ->
          resolveIssue bsEnv now' (Just pid) badgeRequest (lpIssue plan) >>= \case
            Left code -> pure $ errorResponse code Nothing Nothing
            Right result ->
              -- One transaction either way: an issued period writes its rows and reads the
              -- statement back inside them, while a cached or exhausted issue writes nothing and
              -- the same call is a plain read. Neither can report a balance the database does
              -- not hold.
              withServiceTransaction store (writeTxn now' cursor plan {lpIssue = result}) >>= \case
                Left e -> storeFailed "issueBadge write" e
                Right statement ->
                  pure BSPBadgeCredential {credential = issuedCredential result, receipt = Nothing, statement}
  where
    BadgePurchaseRow {badgePurchaseId = pid, currentBadgeType = currentType} = row
    planTxn now' db = do
      cursor <- resolveCursor db pid (assertedEntryId badgeBalance)
      lastEntry <- getLastLedgerEntry db pid
      -- a purchase with no ledger at all has a zero balance, which 'planLedger' turns into
      -- 'IssueExhausted': no credential, and an empty statement rather than an invented entry
      let st0 = maybe (initialLedgerState now' currentType) ledgerStateOf lastEntry
      pure (cursor, planLedger now' Nothing (lastEntry >>= entryWasPausedSince) st0)
    writeTxn now' cursor plan db = do
      writeLedgerPlan db now' pid currentType plan
      purchaseStatement now' db cursor pid
