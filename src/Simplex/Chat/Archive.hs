{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Archive
  ( exportArchive,
    importArchive,
    deleteStorage,
    sqlCipherExport,
    sqlCipherTestKey,
    archiveFilesFolder,
  )
where

import qualified Codec.Archive.Zip as Z
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteArray as BA
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite3 as SQL
import Simplex.Chat.Controller
import Simplex.Chat.Util ()
import Simplex.Messaging.Agent.Client (agentClientStore)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), closeSQLiteStore, keyString, sqlString, storeKey)
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

archiveAssetsFolder :: String
archiveAssetsFolder = "simplex_v1_assets"

wallpapersFolder :: String
wallpapersFolder = "wallpapers"

exportArchive :: ArchiveConfig -> CM' [ArchiveError]
exportArchive cfg@ArchiveConfig {archivePath, disableCompression} =
  withTempDir cfg "simplex-chat." $ \dir -> do
    StorageFiles {chatStore, agentStore, filesPath, assetsPath} <- storageFiles
    copyFile (dbFilePath chatStore) $ dir </> archiveChatDbFile
    copyFile (dbFilePath agentStore) $ dir </> archiveAgentDbFile
    errs <-
      forM filesPath $ \fp ->
        copyValidDirectoryFiles entrySelectorError fp $ dir </> archiveFilesFolder
    forM_ assetsPath $ \fp ->
      copyDirectoryFiles (fp </> wallpapersFolder) $ dir </> archiveAssetsFolder </> wallpapersFolder
    let method = if disableCompression == Just True then Z.Store else Z.Deflate
    Z.createArchive archivePath $ Z.packDirRecur method Z.mkEntrySelector dir
    pure $ fromMaybe [] errs
  where
    entrySelectorError f = (Z.mkEntrySelector f $> Nothing) `E.catchAny` (pure . Just . show)

importArchive :: ArchiveConfig -> CM' [ArchiveError]
importArchive cfg@ArchiveConfig {archivePath} =
  withTempDir cfg "simplex-chat." $ \dir -> do
    Z.withArchive archivePath $ Z.unpackInto dir
    fs@StorageFiles {chatStore, agentStore, filesPath, assetsPath} <- storageFiles
    liftIO $ closeSQLiteStore `withStores` fs
    backup `withDBs` fs
    copyFile (dir </> archiveChatDbFile) $ dbFilePath chatStore
    copyFile (dir </> archiveAgentDbFile) $ dbFilePath agentStore
    errs <- copyFiles (dir </> archiveFilesFolder) filesPath
    errs' <- copyFiles (dir </> archiveAssetsFolder </> wallpapersFolder) ((</> wallpapersFolder) <$> assetsPath)
    pure $ errs <> errs'
  where
    backup f = whenM (doesFileExist f) $ copyFile f $ f <> ".bak"
    copyFiles fromDir = \case
      Just fp ->
        ifM
          (doesDirectoryExist fromDir)
          (copyDirectoryFiles fromDir fp)
          (pure [])
          `E.catch` \(e :: E.SomeException) -> pure [AEImport $ show e]
      _ -> pure []

withTempDir :: ArchiveConfig -> (String -> (FilePath -> CM' a) -> CM' a)
withTempDir cfg = case parentTempDirectory (cfg :: ArchiveConfig) of
  Just tmpDir -> withTempDirectory tmpDir
  _ -> withSystemTempDirectory

copyDirectoryFiles :: FilePath -> FilePath -> CM' [ArchiveError]
copyDirectoryFiles fromDir toDir = copyValidDirectoryFiles (\_ -> pure Nothing) fromDir toDir

copyValidDirectoryFiles :: (FilePath -> IO (Maybe String)) -> FilePath -> FilePath -> CM' [ArchiveError]
copyValidDirectoryFiles isFileError fromDir toDir = do
  createDirectoryIfMissing True toDir
  fs <- listDirectory fromDir
  foldM copyFileCatchError [] fs
  where
    copyFileCatchError fileErrs f =
      liftIO (isFileError f) >>= \case
        Nothing ->
          (copyDirectoryFile f $> fileErrs)
            `E.catch` \(e :: E.SomeException) -> addErr $ show e
        Just e ->  addErr e
      where
        addErr e = pure $ AEFileError f e : fileErrs
    copyDirectoryFile f = do
      let fn = takeFileName f
          f' = fromDir </> fn
      whenM (doesFileExist f') $ copyFile f' $ toDir </> fn

deleteStorage :: CM ()
deleteStorage = do
  fs <- lift storageFiles
  liftIO $ closeSQLiteStore `withStores` fs
  remove `withDBs` fs
  mapM_ removeDir $ filesPath fs
  mapM_ removeDir $ assetsPath fs
  mapM_ removeDir =<< chatReadVar tempDirectory
  where
    remove f = whenM (doesFileExist f) $ removeFile f
    removeDir d = whenM (doesDirectoryExist d) $ removePathForcibly d

data StorageFiles = StorageFiles
  { chatStore :: SQLiteStore,
    agentStore :: SQLiteStore,
    filesPath :: Maybe FilePath,
    assetsPath :: Maybe FilePath
  }

storageFiles :: CM' StorageFiles
storageFiles = do
  ChatController {chatStore, filesFolder, assetsDirectory, smpAgent} <- ask
  let agentStore = agentClientStore smpAgent
  filesPath <- readTVarIO filesFolder
  assetsPath <- readTVarIO assetsDirectory
  pure StorageFiles {chatStore, agentStore, filesPath, assetsPath}

sqlCipherExport :: DBEncryptionConfig -> CM ()
sqlCipherExport DBEncryptionConfig {currentKey = DBEncryptionKey key, newKey = DBEncryptionKey key', keepKey} =
  when (key /= key') $ do
    fs <- lift storageFiles
    checkFile `withDBs` fs
    backup `withDBs` fs
    checkEncryption `withStores` fs
    removeExported `withDBs` fs
    export `withDBs` fs
    -- closing after encryption prevents closing in case wrong encryption key was passed
    liftIO $ closeSQLiteStore `withStores` fs
    (moveExported `withStores` fs)
      `catchChatError` \e -> (restore `withDBs` fs) >> throwError e
  where
    backup f = copyFile f (f <> ".bak")
    restore f = copyFile (f <> ".bak") f
    checkFile f = unlessM (doesFileExist f) $ throwDBError $ DBErrorNoFile f
    checkEncryption SQLiteStore {dbKey} = do
      enc <- maybe True (not . BA.null) <$> readTVarIO dbKey
      when (enc && BA.null key) $ throwDBError DBErrorEncrypted
      when (not enc && not (BA.null key)) $ throwDBError DBErrorPlaintext
    exported = (<> ".exported")
    removeExported f = whenM (doesFileExist $ exported f) $ removeFile (exported f)
    moveExported SQLiteStore {dbFilePath = f, dbKey} = do
      renameFile (exported f) f
      atomically $ writeTVar dbKey $ storeKey key' (fromMaybe False keepKey)
    export f = do
      withDB f (`SQL.exec` exportSQL) DBErrorExport
      withDB (exported f) (`SQL.exec` testSQL key') DBErrorOpen
      where
        exportSQL =
          T.unlines $
            keySQL key
              <> [ "ATTACH DATABASE " <> sqlString (T.pack f <> ".exported") <> " AS exported KEY " <> keyString key' <> ";",
                   "SELECT sqlcipher_export('exported');",
                   "DETACH DATABASE exported;"
                 ]

withDB :: FilePath -> (SQL.Database -> IO a) -> (SQLiteError -> DatabaseError) -> CM ()
withDB f' a err =
  liftIO (bracket (SQL.open $ T.pack f') SQL.close a $> Nothing)
    `catch` checkSQLError
    `catch` (\(e :: SomeException) -> sqliteError' e)
    >>= mapM_ (throwDBError . err)
  where
    checkSQLError e = case SQL.sqlError e of
      SQL.ErrorNotADatabase -> pure $ Just SQLiteErrorNotADatabase
      _ -> sqliteError' e
    sqliteError' :: Show e => e -> CM (Maybe SQLiteError)
    sqliteError' = pure . Just . SQLiteError . show

testSQL :: BA.ScrubbedBytes -> Text
testSQL k =
  T.unlines $
    keySQL k
      <> [ "PRAGMA foreign_keys = ON;",
           "PRAGMA secure_delete = ON;",
           "SELECT count(*) FROM sqlite_master;"
         ]

keySQL :: BA.ScrubbedBytes -> [Text]
keySQL k = ["PRAGMA key = " <> keyString k <> ";" | not (BA.null k)]

sqlCipherTestKey :: DBEncryptionKey -> CM ()
sqlCipherTestKey (DBEncryptionKey key) = do
  fs <- lift storageFiles
  testKey `withDBs` fs
  where
    testKey f = withDB f (`SQL.exec` testSQL key) DBErrorOpen

withDBs :: Monad m => (FilePath -> m b) -> StorageFiles -> m b
action `withDBs` StorageFiles {chatStore, agentStore} = action (dbFilePath chatStore) >> action (dbFilePath agentStore)

withStores :: Monad m => (SQLiteStore -> m b) -> StorageFiles -> m b
action `withStores` StorageFiles {chatStore, agentStore} = action chatStore >> action agentStore
