{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Archive where

import qualified Codec.Archive.Zip as Z
import Control.Monad.Reader
import Simplex.Chat.Controller
import Simplex.Messaging.Agent.Client (agentDbPath)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..))
import Simplex.Messaging.Util (whenM)
import System.FilePath
import UnliftIO.Directory
import UnliftIO.STM
import UnliftIO.Temporary

archiveAgentDbFile :: String
archiveAgentDbFile = "simplex_v1_agent.db"

archiveChatDbFile :: String
archiveChatDbFile = "simplex_v1_chat.db"

archiveFilesFolder :: String
archiveFilesFolder = "simplex_v1_files"

exportArchive :: ChatMonad m => ArchiveConfig -> m ()
exportArchive cfg@ArchiveConfig {archivePath, disableCompression} = do
  liftIO . print $ "exportArchive 1"
  withTempDir cfg "simplex-chat." $ \dir -> do
    liftIO . print $ "exportArchive 2, dir = " <> dir
    StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
    liftIO . print $ "exportArchive 3"
    copyFile chatDb $ dir </> archiveChatDbFile
    liftIO . print $ "exportArchive 4"
    copyFile agentDb $ dir </> archiveAgentDbFile
    liftIO . print $ "exportArchive 5"
    forM_ filesPath $ \fp -> do
      liftIO . print $ "exportArchive 6, fp = " <> fp
      copyDirectoryFiles fp $ dir </> archiveFilesFolder
    liftIO . print $ "exportArchive 7"
    let method = if disableCompression == Just True then Z.Store else Z.Deflate
    liftIO . print $ "exportArchive 8, method = " <> show method
    Z.createArchive archivePath $ Z.packDirRecur method Z.mkEntrySelector dir
    liftIO . print $ "exportArchive 9"

importArchive :: ChatMonad m => ArchiveConfig -> m ()
importArchive cfg@ArchiveConfig {archivePath} = do
  liftIO . print $ "importArchive 1"
  withTempDir cfg "simplex-chat." $ \dir -> do
    liftIO . print $ "importArchive 2, dir = " <> dir
    Z.withArchive archivePath $ Z.unpackInto dir
    liftIO . print $ "importArchive 3"
    StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
    liftIO . print $ "importArchive 4"
    backup chatDb
    liftIO . print $ "importArchive 5"
    backup agentDb
    liftIO . print $ "importArchive 6"
    copyFile (dir </> archiveChatDbFile) chatDb
    liftIO . print $ "importArchive 7"
    copyFile (dir </> archiveAgentDbFile) agentDb
    liftIO . print $ "importArchive 8"
    let filesDir = dir </> archiveFilesFolder
    liftIO . print $ "importArchive 9, filesDir = " <> filesDir
    forM_ filesPath $ \fp -> do
      liftIO . print $ "importArchive 10, fp = " <> fp
      whenM (doesDirectoryExist filesDir) $ do
        liftIO . print $ "importArchive 11"
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
    agentDb :: FilePath,
    filesPath :: Maybe FilePath
  }

storageFiles :: ChatMonad m => m StorageFiles
storageFiles = do
  ChatController {chatStore, filesFolder, smpAgent} <- ask
  let SQLiteStore {dbFilePath = chatDb} = chatStore
      agentDb = agentDbPath smpAgent
  filesPath <- readTVarIO filesFolder
  pure StorageFiles {chatDb, agentDb, filesPath}
