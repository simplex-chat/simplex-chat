{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Simplex.Chat.Files where

import Data.Char (isControl, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Simplex.Chat.Controller
import Simplex.Chat.Messages (CIFile (..))
import Simplex.Chat.Protocol (MsgContent (..))
import Simplex.Messaging.Util (ifM)
import System.FilePath (combine, makeValid, splitExtensions, takeExtension, takeFileName, (<.>))
import UnliftIO.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, getTemporaryDirectory)

safeFileNameStr :: String -> String
safeFileNameStr = notDots . map safeChar . makeValid . takeFileName
  where
    notDots n = if n == "." || n == ".." then "_" else n
    safeChar c
      | isControl c || (c >= '\x2028' && c <= '\x202E') || (c >= '\x2066' && c <= '\x2069') = '_'
      | otherwise = c

mediaFileName :: MsgContent -> String -> String
mediaFileName mc name = case mc of
  MCImage {} -> withExtension imageExtensions
  MCVideo {} -> withExtension videoExtensions
  MCVoice {} -> withExtension (".m4a" :| [])
  _ -> name
  where
    withExtension exts@(ext :| _)
      | map toLower (takeExtension name) `elem` exts = name
      | otherwise = name <.> ext

mediaFileNameMatches :: MsgContent -> Maybe (CIFile d) -> Bool
mediaFileNameMatches mc = all $ \CIFile {fileName = name} -> mediaFileName mc name == name

imageExtensions :: NonEmpty String
imageExtensions = ".jpg" :| [".jpeg", ".png", ".gif", ".webp", ".heic", ".heif", ".hif", ".avif"]

videoExtensions :: NonEmpty String
videoExtensions =
  ".mp4"
    :| [ ".3g2", ".3gp", ".3gp2", ".3gpp", ".3gpp2", ".asf", ".avi", ".axv", ".divx", ".dl", ".dv", ".f4v",
         ".fli", ".flv", ".gl", ".lsf", ".m1v", ".m2t", ".m2ts", ".m2v", ".m4v", ".mkv", ".mng", ".mov",
         ".movie", ".mp1v", ".mp2v", ".mp4v", ".mpe", ".mpeg", ".mpeg1", ".mpeg2", ".mpeg4", ".mpg", ".mpv",
         ".mpv1", ".mpv2", ".mts", ".ogv", ".qt", ".rm", ".rmvb", ".ts", ".vob", ".webm", ".wm", ".wmv", ".wrf", ".yt"
       ]

-- | The file name is sanitized, so the combined path cannot escape the folder.
uniqueCombine :: FilePath -> String -> IO FilePath
uniqueCombine fPath fName = tryCombine (0 :: Int)
  where
    tryCombine n =
      let (name, ext) = splitExtensions $ safeFileNameStr fName
          suffix = if n == 0 then "" else "_" <> show n
          f = fPath `combine` (name <> suffix <> ext)
       in ifM (doesFileExist f) (tryCombine $ n + 1) (pure f)

getChatTempDirectory :: CM' FilePath
getChatTempDirectory = chatReadVar' tempDirectory >>= maybe getTemporaryDirectory pure

getDefaultFilesFolder :: CM' FilePath
getDefaultFilesFolder = do
  dir <- (`combine` "Downloads") <$> getHomeDirectory
  ifM (doesDirectoryExist dir) (pure dir) getChatTempDirectory
