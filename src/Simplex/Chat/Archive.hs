{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Archive
  ( exportArchive,
    importArchive,
    deleteStorage,
    sqlCipherExport,
  )
where

import qualified Codec.Archive.Zip as Z
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Database.SQLite3 as SQL
import Simplex.Chat.Controller
import Simplex.Messaging.Agent.Client (agentClientStore)
import Simplex.Messaging.Agent.Store.SQLite
import Simplex.Messaging.Util
import System.FilePath
import UnliftIO.Directory
import UnliftIO.Exception (SomeException, bracket, catch)
import qualified UnliftIO.Exception as E
import UnliftIO.STM
import UnliftIO.Temporary

archiveAgentDbFile :: String
archiveAgentDbFile = "simplex_v1_agent.db"

archiveChatDbFile :: String
archiveChatDbFile = "simplex_v1_chat.db"

archiveFilesFolder :: String
archiveFilesFolder = "simplex_v1_files"

exportArchive :: ChatMonad m => ArchiveConfig -> m ()
exportArchive cfg@ArchiveConfig {archivePath, disableCompression} =
  handleErr $ withTempDir cfg "simplex-chat." $ \dir -> do
    fs@StorageFiles {chatStore, agentStore, filesPath} <- storageFiles
    setWALMode False `withStores` fs
    copyFile (dbFilePath chatStore) $ dir </> archiveChatDbFile
    copyFile (dbFilePath agentStore) $ dir </> archiveAgentDbFile
    setWALMode True `withStores` fs
    forM_ filesPath $ \fp ->
      copyDirectoryFiles fp $ dir </> archiveFilesFolder
    let method = if disableCompression == Just True then Z.Store else Z.Deflate
    Z.createArchive archivePath $ Z.packDirRecur method Z.mkEntrySelector dir
  where
    setWALMode walMode st = liftIO $ setSQLiteModeWAL st walMode

importArchive :: ChatMonad m => ArchiveConfig -> m [ArchiveError]
importArchive cfg@ArchiveConfig {archivePath} =
  handleErr $ withTempDir cfg "simplex-chat." $ \dir -> do
    Z.withArchive archivePath $ Z.unpackInto dir
    fs@StorageFiles {chatStore, agentStore, filesPath} <- storageFiles
    liftIO $ (closeSQLiteStore `withStores` fs) `catch` print @SomeException
    liftIO $ backupSQLiteStore `withStores` fs
    copyFile (dir </> archiveChatDbFile) $ dbFilePath chatStore
    copyFile (dir </> archiveAgentDbFile) $ dbFilePath agentStore
    copyFiles dir filesPath
      `E.catch` \(e :: E.SomeException) -> pure [AEImport . ChatError . CEException $ show e]
  where
    copyFiles dir filesPath = do
      let filesDir = dir </> archiveFilesFolder
      case filesPath of
        Just fp ->
          ifM
            (doesDirectoryExist filesDir)
            (copyDirectoryFiles filesDir fp)
            (pure [])
        _ -> pure []

withTempDir :: ChatMonad m => ArchiveConfig -> (String -> (FilePath -> m a) -> m a)
withTempDir cfg = case parentTempDirectory (cfg :: ArchiveConfig) of
  Just tmpDir -> withTempDirectory tmpDir
  _ -> withSystemTempDirectory

copyDirectoryFiles :: ChatMonad m => FilePath -> FilePath -> m [ArchiveError]
copyDirectoryFiles fromDir toDir = do
  createDirectoryIfMissing False toDir
  fs <- listDirectory fromDir
  foldM copyFileCatchError [] fs
  where
    copyFileCatchError fileErrs f =
      (copyDirectoryFile f $> fileErrs)
        `E.catch` \(e :: E.SomeException) -> pure (AEImportFile f (ChatError . CEException $ show e) : fileErrs)
    copyDirectoryFile f = do
      let fn = takeFileName f
          f' = fromDir </> fn
      whenM (doesFileExist f') $ copyFile f' $ toDir </> fn

deleteStorage :: ChatMonad m => m ()
deleteStorage = handleErr $ do
  fs <- storageFiles
  liftIO $ closeSQLiteStore `withStores` fs
  liftIO $ removeSQLiteStore `withStores` fs
  mapM_ removeDir $ filesPath fs
  mapM_ removeDir =<< chatReadVar tempDirectory
  where
    removeDir d = whenM (doesDirectoryExist d) $ removePathForcibly d

handleErr :: ChatMonad m => m a -> m a
handleErr = E.handle (throwError . mkChatError)

data StorageFiles = StorageFiles
  { chatStore :: SQLiteStore,
    agentStore :: SQLiteStore,
    filesPath :: Maybe FilePath
  }

storageFiles :: ChatMonad m => m StorageFiles
storageFiles = do
  ChatController {chatStore, filesFolder, smpAgent} <- ask
  let agentStore = agentClientStore smpAgent
  filesPath <- readTVarIO filesFolder
  pure StorageFiles {chatStore, agentStore, filesPath}

sqlCipherExport :: forall m. ChatMonad m => DBEncryptionConfig -> m ()
sqlCipherExport DBEncryptionConfig {currentKey = DBEncryptionKey key, newKey = DBEncryptionKey key'} =
  when (key /= key') $ do
    fs <- storageFiles
    checkFile `withDBs` fs
    liftIO $ backupSQLiteStore `withStores` fs
    checkEncryption `withStores` fs
    removeExported `withDBs` fs
    export `withDBs` fs
    -- closing after encryption prevents closing in case wrong encryption key was passed
    liftIO $ closeSQLiteStore `withStores` fs
    (moveExported `withStores` fs)
      `catchChatError` \e -> liftIO (restoreSQLiteStore `withStores` fs) >> throwError e
  where
    checkFile f = unlessM (doesFileExist f) $ throwDBError $ DBErrorNoFile f
    checkEncryption SQLiteStore {dbEncrypted} = do
      enc <- readTVarIO dbEncrypted
      when (enc && null key) $ throwDBError DBErrorEncrypted
      when (not enc && not (null key)) $ throwDBError DBErrorPlaintext
    exported = (<> ".exported")
    removeExported f = whenM (doesFileExist $ exported f) $ removeFile (exported f)
    moveExported SQLiteStore {dbFilePath = f, dbEncrypted} = do
      renameFile (exported f) f
      atomically $ writeTVar dbEncrypted $ not (null key')
    export f = do
      withDB f (`SQL.exec` exportSQL) DBErrorExport
      withDB (exported f) (`SQL.exec` testSQL) DBErrorOpen
      where
        withDB f' a err =
          liftIO (bracket (SQL.open $ T.pack f') SQL.close a $> Nothing)
            `catch` checkSQLError
            `catch` (\(e :: SomeException) -> sqliteError' e)
            >>= mapM_ (throwDBError . err)
          where
            checkSQLError e = case SQL.sqlError e of
              SQL.ErrorNotADatabase -> pure $ Just SQLiteErrorNotADatabase
              _ -> sqliteError' e
            sqliteError' :: Show e => e -> m (Maybe SQLiteError)
            sqliteError' = pure . Just . SQLiteError . show
        exportSQL =
          T.unlines $
            keySQL key
              <> [ "ATTACH DATABASE " <> sqlString (f <> ".exported") <> " AS exported KEY " <> sqlString key' <> ";",
                   "PRAGMA wal_checkpoint(TRUNCATE);",
                   "SELECT sqlcipher_export('exported');",
                   "DETACH DATABASE exported;"
                 ]
        testSQL =
          T.unlines $
            keySQL key'
              <> [ "PRAGMA foreign_keys = ON;",
                   "PRAGMA secure_delete = ON;",
                   "SELECT count(*) FROM sqlite_master;"
                 ]
        keySQL k = ["PRAGMA key = " <> sqlString k <> ";" | not (null k)]

withDBs :: Monad m => (FilePath -> m a) -> StorageFiles -> m a
action `withDBs` StorageFiles {chatStore, agentStore} = action (dbFilePath chatStore) >> action (dbFilePath agentStore)

withStores :: Monad m => (SQLiteStore -> m a) -> StorageFiles -> m a
action `withStores` StorageFiles {chatStore, agentStore} = action chatStore >> action agentStore
