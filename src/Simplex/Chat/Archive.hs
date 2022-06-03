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

exportArchive :: ChatMonad m => ArchiveConfig -> m ()
exportArchive ArchiveConfig {archivePath} = do
  tmpDir <- getTemporaryDirectory
  withTempDirectory tmpDir "simplex-chat." $ \dir -> do
    createArchiveDir dir
    Z.createArchive archivePath (Z.packDirRecur Z.Deflate Z.mkEntrySelector dir)
  where
    createArchiveDir dir = do
      ChatController {chatStore, filesFolder, smpAgent} <- ask
      let SQLiteStore {dbFilePath = chatDb} = chatStore
          agentDb = agentDbPath smpAgent
      copyToArchive agentDb
      copyToArchive chatDb
      filesPath_ <- readTVarIO filesFolder
      mapM_ copyFilesFolder filesPath_
      where
        copyFilesFolder fp =
          listDirectory fp
            >>= mapM_ (\f -> whenM (doesFileExist f) $ copyToArchive f)
        copyToArchive f = copyFile f $ dir </> takeFileName f

importArchive :: ChatMonad m => ArchiveConfig -> m ()
importArchive ArchiveConfig {archivePath} = pure ()
