{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Operator tooling for redemption codes (decision 3): @simplex-badge-service codes
-- issue|revoke|status@. Runs standalone against the same database and ini as the service
-- ("BadgeService.Options" keeps the plain service run the DEFAULT when no subcommand is
-- given -- see its @optional@-wrapped subparser); this module owns the sub-subcommand
-- parser and the execution behind it. It asserts the schema is current and REFUSES to run a
-- pending migration itself -- 'openCurrentStore' uses 'MCError', never a confirming
-- 'MigrationConfirmation' -- because this may run against the database of a live service,
-- and silently migrating under a running process is how you corrupt one. It calls
-- 'seedCatalog' the same way the service does, does its work in one short transaction per
-- invocation, and exits: it never starts the bot.
--
-- @issue@ is the one place a redemption code's plaintext exists outside a user's clipboard:
-- 'runIssue' generates it in memory with 'BadgeService.Codes.generateBatchCode', hashes it
-- with 'BadgeService.Codes.codeHash' for the row 'insertCodes' writes, and only prints the
-- plaintext to stdout after that insert has committed. Nothing here ever writes a plaintext
-- code anywhere -- not to the database, not to a log, not to a temp file.
module BadgeService.Admin
  ( AdminOpts (..),
    AdminCmd (..),
    IssueOpts (..),
    adminCommandParser,
    runAdminCmd,
  )
where

import BadgeService.Catalog (seedCatalog)
import qualified BadgeService.Codes as Codes
import BadgeService.Config (BadgeServiceConfig (..), CodesConfig (..), readBadgeServiceConfig)
import BadgeService.Store
  ( BadgeCode (BadgeCode, badgeType, batch, createdAt, expiresAt, months, redeemedAt, redeemedPurchaseId, revokedAt, unredeemedAt),
    NewBadgeCode (NewBadgeCode),
    getCodeByHash,
    insertCodes,
    revokeBatch,
    withServiceTransaction,
  )
import Control.Exception (finally)
import Control.Monad (replicateM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Word (Word8)
import Options.Applicative
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Store (createChatStore)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..), MigrationError, migrationErrorDescription)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (textEncode)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations)
#else
import BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations)
#endif

-- | Everything the @codes@ subcommand needs on top of 'AdminCmd': the same database options
-- and the same ini path as a service run, so an operator points it at the live deployment
-- (docs/protocol/badges-web.md).
data AdminOpts = AdminOpts
  { adminCoreOptions :: CoreChatOpts,
    adminConfigFile :: FilePath,
    adminCmd :: AdminCmd
  }

data AdminCmd
  = CmdIssue IssueOpts
  | CmdRevoke Text
  | CmdStatus Text

-- | @--months@ is required and at least 1 (@codes.months@ carries @CHECK (months > 0)@, A3);
-- @--type@ accepts only 'BTSupporter'\/'BTLegend' (lifetime codes are out of scope, so
-- @investor@ is rejected); @--expires@ defaults to @[codes] default_expiry_days@ from the
-- ini when absent (filled in by 'runIssue') and is stored exactly as given otherwise, with no
-- validation beyond the date format -- a past date is accepted.
data IssueOpts = IssueOpts
  { issueType :: BadgeType,
    issueMonths :: Word8,
    issueCount :: Int,
    issueBatch :: Text,
    issueExpires :: Maybe Day
  }

-- Parsing -----------------------------------------------------------------------

-- | The @codes issue|revoke|status@ sub-subcommand parser, nested inside the @codes@ command
-- "BadgeService.Options" installs at the top level. Each leaf 'info' below is deliberately
-- bare (no explicit 'helper'): nested two levels inside
-- 'BadgeService.Options.badgeServiceCommand''s @hsubparser@, optparse-applicative already
-- installs @-h@\/@--help@ for every command an @hsubparser@ dispatches to, and adding a
-- second one here duplicated the line in @--help@ output.
adminCommandParser :: Parser AdminCmd
adminCommandParser =
  hsubparser $
    command "issue" (info issueP (progDesc "Mint new redemption codes, printing the plaintext once"))
      <> command "revoke" (info revokeP (progDesc "Revoke every unredeemed code in a batch"))
      <> command "status" (info statusP (progDesc "Report a redemption code's status"))
  where
    issueP =
      (\issueType issueMonths issueCount issueBatch issueExpires -> CmdIssue IssueOpts {issueType, issueMonths, issueCount, issueBatch, issueExpires})
        <$> typeOption
        <*> monthsOption
        <*> countOption
        <*> batchOption
        <*> expiresOption
    revokeP = CmdRevoke . T.pack <$> strOption (long "batch" <> metavar "BATCH" <> help "batch name to revoke")
    statusP = CmdStatus . T.pack <$> strOption (long "code" <> metavar "CODE" <> help "redemption code to look up")

typeOption :: Parser BadgeType
typeOption = option (eitherReader badgeTypeReader) (long "type" <> metavar "TYPE" <> help "supporter or legend")

-- | Deliberately narrower than 'Simplex.Chat.Badges.textDecode': that instance never fails
-- (falling back to 'BTUnknown'), but a code can only ever credit a balance the ledger can
-- represent, so @investor@ (a lifetime badge) and anything else are rejected here.
badgeTypeReader :: String -> Either String BadgeType
badgeTypeReader = \case
  "supporter" -> Right BTSupporter
  "legend" -> Right BTLegend
  s -> Left ("invalid --type '" <> s <> "': expected 'supporter' or 'legend'")

monthsOption :: Parser Word8
monthsOption = option (eitherReader monthsReader) (long "months" <> metavar "N" <> help "months credited per code (at least 1)")

monthsReader :: String -> Either String Word8
monthsReader s = case readMaybe s :: Maybe Int of
  Just n
    | n < 1 -> Left ("--months must be at least 1, got " <> show n)
    | n > fromIntegral (maxBound :: Word8) -> Left ("--months is too large, got " <> show n)
    | otherwise -> Right (fromIntegral n)
  Nothing -> Left ("--months must be an integer, got '" <> s <> "'")

countOption :: Parser Int
countOption = option (eitherReader countReader) (long "count" <> metavar "N" <> help "number of codes to issue")

countReader :: String -> Either String Int
countReader s = case readMaybe s :: Maybe Int of
  Just n | n >= 1 -> Right n
  Just n -> Left ("--count must be at least 1, got " <> show n)
  Nothing -> Left ("--count must be an integer, got '" <> s <> "'")

batchOption :: Parser Text
batchOption = T.pack <$> strOption (long "batch" <> metavar "BATCH" <> help "batch label stored on every issued code")

expiresOption :: Parser (Maybe Day)
expiresOption =
  optional $
    option
      (eitherReader dayReader)
      (long "expires" <> metavar "YYYY-MM-DD" <> help "expiry date (default: [codes] default_expiry_days from the ini)")

dayReader :: String -> Either String Day
dayReader s =
  maybe (Left ("--expires must be YYYY-MM-DD, got '" <> s <> "'")) Right $
    parseTimeM True defaultTimeLocale "%Y-%m-%d" s

-- Execution -----------------------------------------------------------------------

-- | Opens the store, asserts both the core and the badge service schema are current (failing
-- rather than migrating either), seeds the catalog, dispatches the one subcommand, then
-- closes the store. Never starts the bot.
runAdminCmd :: AdminOpts -> IO ()
runAdminCmd AdminOpts {adminCoreOptions, adminConfigFile, adminCmd} = do
  bsConfig <- readBadgeServiceConfig adminConfigFile >>= either dieConfig pure
  chatStore <- openCurrentStore adminCoreOptions
  (seedCatalog chatStore >> dispatch chatStore bsConfig) `finally` closeDBStore chatStore
  where
    dieConfig e = putStrLn e >> exitFailure
    dispatch chatStore bsConfig = case adminCmd of
      CmdIssue issueOpts -> runIssue chatStore bsConfig issueOpts
      CmdRevoke batchName -> runRevoke chatStore batchName
      CmdStatus code -> runStatus chatStore code

-- | Opens the shared chat\/badge-service database and confirms it needs no migration,
-- refusing to run one if it does ('MCError' on both the core chat schema and the badge
-- service schema): safe to point at the database of a live service.
openCurrentStore :: CoreChatOpts -> IO DBStore
openCurrentStore CoreChatOpts {dbOptions} = do
  chatStoreResult <- createChatStore (toDBOpts dbOptions chatSuffix False chatDBFunctions) noMigrateConfig
  chatStore <- either (dieMigration "chat") pure chatStoreResult
  badgeResult <-
    migrateDBSchema
      chatStore
      (toDBOpts dbOptions chatSuffix False [])
      (Just "sx_badge_service_migrations")
      badgeServiceSchemaMigrations
      noMigrateConfig
  case badgeResult of
    Right () -> pure chatStore
    Left e -> closeDBStore chatStore >> dieMigration "badge service" e
  where
    noMigrateConfig = MigrationConfig {confirm = MCError, backupPath = Nothing}

dieMigration :: String -> MigrationError -> IO a
dieMigration label e = do
  putStrLn $ "codes: " <> label <> " schema is not current (" <> migrationErrorDescription False e <> ")"
  putStrLn "codes: refusing to migrate automatically; start the service once (or migrate it) first"
  exitFailure

-- | Generates 'issueCount' fresh, unrelated codes ('BadgeService.Codes.generateBatchCode'),
-- hashes each for storage, and inserts every row in one transaction. Only after that
-- transaction commits are the plaintext codes printed to stdout, exactly once each, in the
-- same order they were generated -- the only place any of them is ever written down.
runIssue :: DBStore -> BadgeServiceConfig -> IssueOpts -> IO ()
runIssue chatStore BadgeServiceConfig {codes = CodesConfig {codesDefaultExpiryDays}} IssueOpts {issueType, issueMonths, issueCount, issueBatch, issueExpires} = do
  now <- getCurrentTime
  let expiresAt = maybe (defaultExpiry now) (\day -> UTCTime day 0) issueExpires
  drg <- C.newRandom
  plainCodes <- replicateM issueCount (Codes.generateBatchCode drg)
  -- Positional (not record) construction: 'NewBadgeCode' and 'BadgeCode' share field names
  -- ('badgeType', 'months', 'batch', 'expiresAt'), and both are in scope here (the latter for
  -- 'describeCode'), which DuplicateRecordFields cannot disambiguate in record syntax across
  -- two different constructors -- only 'NewBadgeCode' the bare constructor is imported.
  let newCode c = NewBadgeCode (Codes.codeHash (Codes.normalizeCode c)) issueType issueMonths issueBatch expiresAt
      newCodes = map newCode plainCodes
  withServiceTransaction chatStore (\db -> insertCodes db newCodes now) >>= \case
    Left err -> putStrLn ("codes issue: " <> show err) >> exitFailure
    Right () -> mapM_ (putStrLn . T.unpack) plainCodes
  where
    defaultExpiry now = addUTCTime (fromIntegral codesDefaultExpiryDays * nominalDay) now

-- | Sets @revoked_at@ on every unrevoked code in the batch, in one transaction; a batch name
-- matching nothing is not an error, just zero codes revoked ('BadgeService.Store.revokeBatch').
runRevoke :: DBStore -> Text -> IO ()
runRevoke chatStore batchName = do
  now <- getCurrentTime
  withServiceTransaction chatStore (\db -> revokeBatch db batchName now) >>= \case
    Left err -> putStrLn ("codes revoke: " <> show err) >> exitFailure
    Right n -> putStrLn (show n <> " code(s) revoked in batch " <> T.unpack batchName)

-- | Normalizes and hashes the presented code the same way redemption does, then reports what
-- 'getCodeByHash' finds: unredeemed, redeemed (by which purchase), revoked, or not found.
runStatus :: DBStore -> Text -> IO ()
runStatus chatStore presented =
  withServiceTransaction chatStore (\db -> getCodeByHash db (Codes.codeHash (Codes.normalizeCode presented))) >>= \case
    Left err -> putStrLn ("codes status: " <> show err) >> exitFailure
    Right Nothing -> putStrLn "not found"
    Right (Just (code, _redeemerKey)) -> putStrLn (describeCode code)

describeCode :: BadgeCode -> String
describeCode BadgeCode {badgeType, months, batch, expiresAt, redeemedAt, redeemedPurchaseId, revokedAt, unredeemedAt, createdAt} =
  unwords
    [ "type=" <> T.unpack (textEncode badgeType),
      "months=" <> show months,
      "batch=" <> T.unpack batch,
      "expires=" <> show expiresAt,
      "created=" <> show createdAt,
      "status=" <> redemptionLabel
    ]
  where
    redemptionLabel
      | Just _ <- revokedAt = "revoked"
      | Just pid <- redeemedPurchaseId, Just at <- redeemedAt = "redeemed (purchase=" <> show pid <> " at=" <> show at <> ")"
      | Just pid <- redeemedPurchaseId = "redeemed (purchase=" <> show pid <> ")"
      | Just at <- unredeemedAt = "unredeemed (previously redeemed, unredeemed at " <> show at <> ")"
      | otherwise = "unredeemed"
