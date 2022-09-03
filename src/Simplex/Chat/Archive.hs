{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Archive
  ( exportArchive,
    importArchive,
    deleteStorage,
    encryptStorage,
    decryptStorage,
  )
where

import qualified Codec.Archive.Zip as Z
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Database.SQLite3 as SQL
import Simplex.Chat.Controller
import Simplex.Messaging.Agent.Client (agentStore)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..), sqlString)
import Simplex.Messaging.Util (unlessM, whenM)
import System.FilePath
import UnliftIO.Directory
import UnliftIO.Exception (SomeException, bracket, catch)
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
  withTempDir cfg "simplex-chat." $ \dir -> do
    StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
    copyFile chatDb $ dir </> archiveChatDbFile
    copyFile agentDb $ dir </> archiveAgentDbFile
    forM_ filesPath $ \fp ->
      copyDirectoryFiles fp $ dir </> archiveFilesFolder
    let method = if disableCompression == Just True then Z.Store else Z.Deflate
    Z.createArchive archivePath $ Z.packDirRecur method Z.mkEntrySelector dir

importArchive :: ChatMonad m => ArchiveConfig -> m ()
importArchive cfg@ArchiveConfig {archivePath} =
  withTempDir cfg "simplex-chat." $ \dir -> do
    Z.withArchive archivePath $ Z.unpackInto dir
    StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
    backup chatDb
    backup agentDb
    copyFile (dir </> archiveChatDbFile) chatDb
    copyFile (dir </> archiveAgentDbFile) agentDb
    let filesDir = dir </> archiveFilesFolder
    forM_ filesPath $ \fp ->
      whenM (doesDirectoryExist filesDir) $
        copyDirectoryFiles filesDir fp
  where
    backup f = whenM (doesFileExist f) $ copyFile f $ f <> ".bak"

withTempDir :: ChatMonad m => ArchiveConfig -> (String -> (FilePath -> m ()) -> m ())
withTempDir cfg = case parentTempDirectory cfg of
  Just tmpDir -> withTempDirectory tmpDir
  _ -> withSystemTempDirectory

copyDirectoryFiles :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectoryFiles fromDir toDir = do
  createDirectoryIfMissing False toDir
  fs <- listDirectory fromDir
  forM_ fs $ \f -> do
    let fn = takeFileName f
        f' = fromDir </> fn
    whenM (doesFileExist f') $ copyFile f' $ toDir </> fn

deleteStorage :: ChatMonad m => m ()
deleteStorage = do
  StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
  removeFile chatDb
  removeFile agentDb
  mapM_ removePathForcibly filesPath

data StorageFiles = StorageFiles
  { chatDb :: FilePath,
    chatKey :: String,
    agentDb :: FilePath,
    agentKey :: String,
    filesPath :: Maybe FilePath
  }

storageFiles :: ChatMonad m => m StorageFiles
storageFiles = do
  ChatController {chatStore, filesFolder, smpAgent} <- ask
  let SQLiteStore {dbFilePath = chatDb, dbKey = chatKey} = chatStore
      SQLiteStore {dbFilePath = agentDb, dbKey = agentKey} = agentStore smpAgent
  filesPath <- readTVarIO filesFolder
  pure StorageFiles {chatDb, chatKey, agentDb, agentKey, filesPath}

encryptStorage :: forall m. ChatMonad m => String -> m ()
encryptStorage key' = updateDatabase $ \f key -> export f key key'

decryptStorage :: forall m. ChatMonad m => m ()
decryptStorage = updateDatabase $ \f -> \case
  "" -> throwDBError DBENotEncrypted
  key -> export f key ""

updateDatabase :: ChatMonad m => (FilePath -> String -> m ()) -> m ()
updateDatabase update = do
  fs@StorageFiles {chatDb, chatKey, agentDb, agentKey} <- storageFiles
  checkFile `with` fs
  backup `with` fs
  (update chatDb chatKey >> update agentDb agentKey)
    `catchError` \e -> (restore `with` fs) >> throwError e
  where
    action `with` StorageFiles {chatDb, agentDb} = action chatDb >> action agentDb
    backup f = copyFile f (f <> ".bak")
    restore f = copyFile (f <> ".bak") f
    checkFile f = unlessM (doesFileExist f) $ throwDBError DBENoFile

export :: ChatMonad m => FilePath -> String -> String -> m ()
export f key key' = do
  withDB (`SQL.exec` exportSQL) DBEExportFailed
  renameFile (f <> ".exported") f
  withDB (`SQL.exec` testSQL) DBEOpenFailed
  where
    withDB a err =
      liftIO (bracket (SQL.open $ T.pack f) SQL.close a)
        `catch` \(e :: SomeException) -> liftIO (putStrLn $ "Database error: " <> show e) >> throwDBError (err $ show e)
    exportSQL =
      T.unlines $
        keySQL key
          <> [ "ATTACH DATABASE " <> sqlString (f <> ".exported") <> " AS exported KEY " <> sqlString key' <> ";",
               "SELECT sqlcipher_export('exported');",
               "DETACH DATABASE exported;"
             ]
    testSQL =
      T.unlines $
        keySQL key'
          <> [ "PRAGMA foreign_keys = ON;",
               "PRAGMA secure_delete = ON;",
               "PRAGMA auto_vacuum = FULL;",
               "SELECT count(*) FROM sqlite_master;"
             ]
    keySQL k = ["PRAGMA key = " <> sqlString k <> ";" | not (null k)]
