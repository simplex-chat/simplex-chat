{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Store.Migrate where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Directory.Listing
import Directory.Options
import Directory.Store
import Simplex.Chat (createChatDatabase)
import Simplex.Chat.Controller (ChatConfig (..), ChatDatabase (..))
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol (supportedChatVRange)
import Simplex.Chat.Store.Groups (getGroupInfo, getHostMember)
import Simplex.Chat.Store.Profiles (getUsers)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.Common
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, migrateDBSchema)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import Simplex.Messaging.Encoding.String
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (whenM)
import System.Directory (doesFileExist, renamePath)
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)

#if defined(dbPostgres)
import Directory.Store.Postgres.Migrations
#else
import Directory.Store.SQLite.Migrations
#endif

runDirectoryMigrations :: DirectoryOpts -> ChatConfig -> DBStore -> IO ()
runDirectoryMigrations opts ChatConfig {confirmMigrations} chatStore =
  migrateDBSchema
    chatStore
    (toDBOpts dbOptions chatSuffix False)
    (Just "sx_directory_migrations")
    directorySchemaMigrations
    MigrationConfig {confirm, backupPath = Nothing}
    >>= either (exit . ("directory migrations " <>) . show) pure
  where
    DirectoryOpts {coreOptions = CoreChatOpts {dbOptions, yesToUpMigrations}} = opts
    confirm = if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations

checkDirectoryLog :: DirectoryOpts -> ChatConfig -> IO ()
checkDirectoryLog opts cfg =
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    runDirectoryMigrations opts cfg st
    gs <- readDirectoryLogData logFile
    withActiveUser st $ \user -> withTransaction st $ \db -> do
      mapM_ (verifyGroupRegistration db user) gs
    putStrLn $ show (length gs) <> " group registrations OK"

importDirectoryLogToDB :: DirectoryOpts -> ChatConfig -> IO ()
importDirectoryLogToDB opts cfg = do
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    runDirectoryMigrations opts cfg st
    gs <- readDirectoryLogData logFile
    ctRegs <- TM.emptyIO
    withActiveUser st $ \user -> withTransaction st $ \db -> do
      forM_ gs $ \gr ->
        whenM (verifyGroupRegistration db user gr) $ do
          putStrLn $ "importing group " <> show (dbGroupId gr)
          insertGroupReg db =<< fixUserGroupRegId ctRegs gr
      renamePath logFile (logFile ++ ".bak")
    putStrLn $ show (length gs) <> " group registrations imported"
  where
    fixUserGroupRegId ctRegs gr@GroupReg {dbGroupId, dbContactId} = do
      ugIds <- fromMaybe [] <$> TM.lookupIO dbContactId ctRegs
      gr' <-
        if userGroupRegId gr `elem` ugIds
          then do
            let ugId = maximum ugIds + 1
            putStrLn $ "Warning: updating userGroupRegId for group " <> show dbGroupId <> ", contact " <> show dbContactId
            pure gr {userGroupRegId = ugId}
          else pure gr
      atomically $ TM.insert dbContactId (userGroupRegId gr' : ugIds) ctRegs
      pure gr'

exit :: String -> IO a
exit err = putStrLn ("Error: " <> err) >> exitFailure

exportDBToDirectoryLog :: DirectoryOpts -> ChatConfig -> IO ()
exportDBToDirectoryLog opts cfg =
  withDirectoryLog opts $ \logFile -> withChatStore opts $ \st -> do
    whenM (doesFileExist logFile) $ exit $ "directory log file " ++ logFile ++ " already exists"
    runDirectoryMigrations opts cfg st
    withActiveUser st $ \user -> do
      gs <- withFile logFile WriteMode $ \h -> withTransaction st $ \db -> do
        gs <- getAllGroupRegs_ db user
        forM_ gs $ \(_, gr) ->
          whenM (verifyGroupRegistration db user gr) $
            B.hPutStrLn h $ strEncode $ GRCreate gr
        pure gs
      putStrLn $ show (length gs) <> " group registrations exported"

saveGroupListingFiles :: DirectoryOpts -> ChatConfig -> IO ()
saveGroupListingFiles opts _cfg = case webFolder opts of
  Nothing -> exit "use --web-folder to generate listings"
  Just dir ->
    withChatStore opts $ \st -> withActiveUser st $ \user ->
      withTransaction st $ \db ->
        getAllListedGroups_ db supportedChatVRange user >>= generateListing dir

verifyGroupRegistration :: DB.Connection -> User -> GroupReg -> IO Bool
verifyGroupRegistration db user GroupReg {dbGroupId = gId, dbContactId = ctId, dbOwnerMemberId, groupRegStatus} =
  runExceptT (getGroupInfo db supportedChatVRange user gId) >>= \case
    Left e -> False <$ putStrLn ("Error: loading group " <> show gId <> " (skipping): " <> show e)
    Right GroupInfo {localDisplayName} -> do
      let groupRef = show gId <> " " <> T.unpack localDisplayName
      runExceptT (getHostMember db supportedChatVRange user gId) >>= \case
        Left e -> False <$ putStrLn ("Error: loading host member of group " <> groupRef <> " (skipping): " <> show e)
        Right GroupMember {groupMemberId = mId', memberContactId = ctId'} -> case dbOwnerMemberId of
          Nothing -> True <$ putStrLn ("Warning: group " <> groupRef <> " has no owner member ID, host member ID is " <> show mId' <> ", registration status: " <> B.unpack (strEncode groupRegStatus))
          Just mId
            | mId /= mId' -> False <$ putStrLn ("Error: different host member ID of " <> groupRef <> " (skipping): " <> show mId')
            | otherwise -> True <$ unless (Just ctId == ctId') (putStrLn $ "Warning: bad group " <> groupRef <> " contact ID: " <> show ctId')

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
