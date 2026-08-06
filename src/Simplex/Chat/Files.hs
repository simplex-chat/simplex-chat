{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Simplex.Chat.Files
  ( SafeFileName,
    safeFileName,
    uniqueCombine,
    getChatTempDirectory,
    getDefaultFilesFolder,
  )
where

import Simplex.Chat.Controller
import Simplex.Messaging.Util (ifM)
import System.FilePath (combine, makeValid, splitExtensions, takeFileName)
import UnliftIO.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory)

-- | A file name with directory components removed, so combining it with a folder cannot escape that folder.
newtype SafeFileName = SafeFileName String

safeFileName :: String -> SafeFileName
safeFileName = SafeFileName . makeValid . takeFileName

uniqueCombine :: FilePath -> SafeFileName -> IO FilePath
uniqueCombine fPath (SafeFileName fName) = tryCombine (0 :: Int)
  where
    tryCombine n =
      let (name, ext) = splitExtensions fName
          suffix = if n == 0 then "" else "_" <> show n
          f = fPath `combine` (name <> suffix <> ext)
       in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

getChatTempDirectory :: CM' FilePath
getChatTempDirectory = chatReadVar' tempDirectory >>= maybe getTemporaryDirectory pure

getDefaultFilesFolder :: CM' FilePath
getDefaultFilesFolder = do
  dir <- (`combine` "Downloads") <$> getHomeDirectory
  ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory
