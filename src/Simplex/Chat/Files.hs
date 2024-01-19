{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Simplex.Chat.Files where

import Control.Monad.IO.Class
import Simplex.Chat.Controller
import Simplex.Messaging.Util (ifM)
import System.FilePath (combine, splitExtensions)
import UnliftIO.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory)

uniqueCombine :: MonadIO m => FilePath -> String -> m FilePath
uniqueCombine fPath fName = tryCombine (0 :: Int)
  where
    tryCombine n =
      let (name, ext) = splitExtensions fName
          suffix = if n == 0 then "" else "_" <> show n
          f = fPath `combine` (name <> suffix <> ext)
       in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

getChatTempDirectory :: ChatMonad m => m FilePath
getChatTempDirectory = chatReadVar tempDirectory >>= maybe getTemporaryDirectory pure

getDefaultFilesFolder :: ChatMonad m => m FilePath
getDefaultFilesFolder = do
  dir <- (`combine` "Downloads") <$> getHomeDirectory
  ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory
