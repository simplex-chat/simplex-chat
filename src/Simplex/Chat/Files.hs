{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Simplex.Chat.Files where

import Simplex.Chat.Controller
import Simplex.Messaging.Util (ifM)
import System.FilePath (combine, makeValid, splitExtensions, takeFileName)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory (copyFile, doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory, removeFile, renameFile)
import UnliftIO.Exception (catchIO)

safeFileNameStr :: String -> String
safeFileNameStr = notDots . makeValid . takeFileName
  where
    notDots n = if n == "." || n == ".." then "_" else n

-- | The file name is sanitized, so the combined path cannot escape the folder.
uniqueCombine :: FilePath -> String -> IO FilePath
uniqueCombine fPath fName = tryCombine (0 :: Int)
  where
    tryCombine n =
      let (name, ext) = splitExtensions $ safeFileNameStr fName
          suffix = if n == 0 then "" else "_" <> show n
          f = fPath `combine` (name <> suffix <> ext)
       in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

moveFile :: MonadUnliftIO m => FilePath -> FilePath -> m ()
moveFile from to =
  renameFile from to `catchIO` \_ -> do
    copyFile from to
    removeFile from `catchIO` \_ -> pure ()

getChatTempDirectory :: CM' FilePath
getChatTempDirectory = chatReadVar' tempDirectory >>= maybe getTemporaryDirectory pure

getDefaultFilesFolder :: CM' FilePath
getDefaultFilesFolder = do
  dir <- (`combine` "Downloads") <$> getHomeDirectory
  ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory
