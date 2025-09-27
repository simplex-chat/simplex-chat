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
import Directory.Options
import Directory.Store
import Simplex.Chat (createChatDatabase)
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatDatabase (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Store.Groups (getHostMember)
import Simplex.Chat.Store.Profiles (getUsers)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.Common
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..), MigrationError)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Util (whenM)
import System.Directory (doesFileExist, renamePath)
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)

#if defined(dbPostgres)
import Directory.Store.Postgres.Migrations
#else
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

checkDirectoryLog :: DirectoryOpts -> IO ()
checkDirectoryLog opts =
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    gs <- readDirectoryLogData logFile
    withActiveUser st $ \user -> withTransaction st $ \db -> do
      mapM_ (verifyGroupRegistration db user) gs

importDirectoryLogToDB :: DirectoryOpts -> IO ()
importDirectoryLogToDB opts = do
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    gs <- readDirectoryLogData logFile
    withActiveUser st $ \user -> withTransaction st $ \db -> do
      forM_ gs $ \gr -> do
        verifyGroupRegistration db user gr
        insertGroupReg db gr
      renamePath logFile (logFile ++ ".bak")

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure

exportDBToDirectoryLog :: DirectoryOpts -> IO ()
exportDBToDirectoryLog opts =
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    whenM (doesFileExist logFile) $ exit $ "directory log file " ++ logFile ++ " already exists"
    withActiveUser st $ \user -> withFile logFile WriteMode $ \h -> withTransaction st $ \db -> do
      gs <- getAllGroupRegs_ db user
      forM_ gs $ \(_, gr) -> do
        verifyGroupRegistration db user gr
        B.hPutStrLn h $ strEncode $ GRCreate gr
        deleteGroupReg db $ dbGroupId gr

verifyGroupRegistration :: DB.Connection -> User -> GroupReg -> IO ()
verifyGroupRegistration db user GroupReg {dbGroupId = gId, dbContactId = ctId, dbOwnerMemberId = mId} =
  runExceptT (getHostMember db supportedChatVRange user gId) >>= \case
    Left e -> exit $ "error loading group " <> show gId <> " host member: " <> show e
    Right GroupMember {groupMemberId = mId', memberContactId = ctId'} -> do
      unless (mId == Just mId') $ exit $ "bad group " <> show gId <> " host member ID: " <> show mId'
      unless (Just ctId == ctId') $ exit $ "bad group " <> show gId <> " contact ID: " <> show ctId'

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
