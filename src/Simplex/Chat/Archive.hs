{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Archive where

import qualified Codec.Archive.Zip as Z
import Control.Monad.Reader
import Simplex.Chat.Controller
import Simplex.Chat.Util (whenM)
import Simplex.Messaging.Agent.Client (agentDbPath)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore (..))
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
exportArchive ArchiveConfig {archivePath, disableCompression} =
  withSystemTempDirectory "simplex-chat." $ \dir -> do
    StorageFiles {chatDb, agentDb, filesPath} <- storageFiles
    copyFile chatDb $ dir </> archiveChatDbFile
    copyFile agentDb $ dir </> archiveAgentDbFile
    forM_ filesPath $ \fp ->
      copyDirectoryFiles fp $ dir </> archiveFilesFolder
    let method = if disableCompression == Just True then Z.Store else Z.Deflate
    Z.createArchive archivePath $ Z.packDirRecur method Z.mkEntrySelector dir

importArchive :: ChatMonad m => ArchiveConfig -> m ()
importArchive ArchiveConfig {archivePath} =
  withSystemTempDirectory "simplex-chat." $ \dir -> do
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

copyDirectoryFiles :: MonadIO m => FilePath -> FilePath -> m ()
copyDirectoryFiles fromDir toDir = do
  createDirectoryIfMissing False toDir
  fs <- listDirectory fromDir
  forM_ fs $ \f -> whenM (doesFileExist f) $ copyFile f $ toDir </> takeFileName f

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
