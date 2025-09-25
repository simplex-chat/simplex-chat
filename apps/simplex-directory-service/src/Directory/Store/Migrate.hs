{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Store.Migrate where

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Time.Clock (getCurrentTime)
import Directory.Options
import Directory.Store
import Simplex.Chat (createChatDatabase)
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatDatabase (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Store.Groups (getGroupInfo, setGroupCustomData)
import Simplex.Chat.Store.Profiles (getUsers)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.Common
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..), MigrationError)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (maybeFirstRow, whenM)
import System.Directory (doesFileExist, renamePath)
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Directory.Store.Postgres.Migrations
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
import Directory.Store.SQLite.Migrations
#endif

runDirectoryMigrations :: DirectoryOpts -> ChatController -> IO (Either MigrationError ())
runDirectoryMigrations opts cc =
  migrateDBSchema
    chatStore
    (toDBOpts dbOptions chatSuffix False)
    (Just "sx_directory_migrations")
    directorySchemaMigrations
    MigrationConfig {confirm, backupPath = Nothing}
  where
    DirectoryOpts {coreOptions = CoreChatOpts {dbOptions, yesToUpMigrations}} = opts
    ChatController {chatStore, config = ChatConfig {confirmMigrations}} = cc
    confirm = if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations

versionWithLog :: Int
versionWithLog = 1

versionNoLog :: Int
versionNoLog = 2

checkCreateDirectoryInfo :: DirectoryOpts -> IO ()
checkCreateDirectoryInfo = (`withChatStore` (`withTransaction` checkCreate))
  where
    checkCreate db = do
      v_ <- getStoreVersion db
      getUsers db >>= \case
        [] -> case v_ of
          Just v -> when (v == versionWithLog) $ exit "unexpected store state: no active user and old store version"
          Nothing -> createDirectoryInfoTable db >> setStoreVersion db versionNoLog
        _ -> when (maybe True (versionWithLog ==) v_) $ exit "import directory store log using --migrate-directory-file=import"

importLogToDB :: DirectoryOpts -> IO ()
importLogToDB opts = withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
  v_ <- withTransaction st getStoreVersion
  forM_ v_ $ \v -> when (v >= versionNoLog) $ exit $ "directory version is already " ++ show v
  withActiveUser st $ \user -> withTransaction st $ \db -> do
    readDirectoryData logFile >>= mapM_ (importGroupRegistration db user)
    renamePath logFile (logFile ++ ".bak")
    setStoreVersion db versionNoLog
  where
    importGroupRegistration db user gr@GroupReg {dbGroupId} = do
      GroupInfo {customData, createdAt} <- either (exit . show) pure =<< runExceptT (getGroupInfo db supportedChatVRange user dbGroupId)
      let LegacyDirectoryGroupData {memberAcceptance} = legacyFromCustomData customData
          gr' = gr {memberAcceptance, createdAt}
      -- TODO verify that group owner contact and member IDs are correct
      insertGroupReg db gr'

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure

exportDBToLog :: DirectoryOpts -> IO ()
exportDBToLog opts =
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    whenM (doesFileExist logFile) $ exit $ "directory log file " ++ logFile ++ " already exists"
    withActiveUser st $ \user -> withTransaction st getStoreVersion >>= \case
      Just v | v >= versionNoLog -> withFile logFile WriteMode $ \h -> withTransaction st $ \db -> do
        gs <- getAllGroupRegs_ db user
        forM_ gs $ \(g, gr@GroupReg {memberAcceptance}) -> do
          B.hPutStrLn h $ strEncode $ GRCreate gr
          let gData = LegacyDirectoryGroupData {memberAcceptance}
          -- TODO verify that group owner contact and member IDs are correct
          setGroupCustomData db user g $ Just (legacyToCustomData gData)
      v_ -> exit $ "directory version is " ++ maybe "not set" show v_ ++  ", it cannot be exported to store log"

checkDBStoreLog :: DirectoryOpts -> IO ()
checkDBStoreLog _opts = pure ()

withDirectoryLog :: DirectoryOpts -> (FilePath -> IO ()) -> IO ()
withDirectoryLog DirectoryOpts {directoryLog} action =
  maybe (exit "directory log file not specified") action directoryLog

withChatStore :: DirectoryOpts -> (DBStore -> IO ()) -> IO ()
withChatStore DirectoryOpts {coreOptions = CoreChatOpts {dbOptions, yesToUpMigrations, migrationBackupPath}} action =
  createChatDatabase dbOptions migrationConfig >>= \case
    Left e -> exit $ show e
    Right ChatDatabase {chatStore, agentStore} -> do
      action chatStore
      closeDBStore chatStore
      closeDBStore agentStore
  where
    migrationConfig = MigrationConfig (if yesToUpMigrations then MCYesUp else MCConsole) migrationBackupPath

withActiveUser :: DBStore -> (User -> IO ()) -> IO ()
withActiveUser st action = withTransaction st getUsers >>= maybe (exit "no active user") action . find activeUser

createDirectoryInfoTable :: DB.Connection -> IO ()
createDirectoryInfoTable db =
  DB.execute_
    db
#if defined(dbPostgres)
    [sql|
      CREATE TABLE IF NOT EXISTS directory_store_info(
        version INTEGER NOT NULL,
        updated_at TEXT NOT NULL
      )
    |]
#else
    [sql|
      CREATE TABLE IF NOT EXISTS directory_store_info(
        version BIGINT NOT NULL,
        updated_at TEXT NOT NULL
      )
    |]
#endif

getStoreVersion :: DB.Connection -> IO (Maybe Int)
getStoreVersion db = do
  createDirectoryInfoTable db
  maybeFirstRow fromOnly $ DB.query_ db "SELECT MAX(version) FROM directory_store_info"

setStoreVersion :: DB.Connection -> Int -> IO ()
setStoreVersion db v = do
  ts <- getCurrentTime
  DB.execute db "UPDATE directory_store_info SET version = ?, updated_at = ?" (v, ts)
