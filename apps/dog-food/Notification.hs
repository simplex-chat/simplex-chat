{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Notification where

import ChatTerminal.Core (safeDecodeUtf8)
import Control.Monad (void)
import DBus.Notify (connectSession, summary, body, appName, Body(Text), notify, blankNote)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Info (os)
import System.Process (readCreateProcess, shell)

data Notification = Notification {title :: ByteString, text :: ByteString}

initializeNotifications :: IO (Notification -> IO ())
initializeNotifications = case os of
  "darwin" -> pure macNotify
  "mingw32" -> initWinNotify
  "linux" ->
    doesFileExist "/proc/sys/kernel/osrelease" >>= \case
      False -> pure linuxNotify
      True -> do
        v <- readFile "/proc/sys/kernel/osrelease"
        if "wsl" `isInfixOf` map toLower v
          then initWinNotify
          else pure linuxNotify
  _ -> pure . const $ pure ()

macNotify :: Notification -> IO ()
macNotify Notification {title, text} =
  void $ readCreateProcess (shell . T.unpack $ script) ""
  where
    script :: Text
    script = "osascript -e 'display notification \"" <> safeDecodeUtf8 text <> "\" with title \"" <> safeDecodeUtf8 title <> "\"'"

initWinNotify :: IO (Notification -> IO ())
initWinNotify = winNotify <$> savePowershellScript

savePowershellScript :: IO FilePath
savePowershellScript = pure ""

winNotify :: FilePath -> Notification -> IO ()
winNotify path Notification {title, text} =
  void $ readCreateProcess (shell "powershell.exe \"... -Text 'Hi'\"") ""

linuxNotify :: Notification -> IO ()
linuxNotify Notification {title, text} =
  do
    client <- connectSession
    let linuxNtf =
          linuxNote
            { summary = T.unpack $ safeDecodeUtf8 title,
              body = Just $ Text (T.unpack $ safeDecodeUtf8 text)
            }
    void $ notify client linuxNtf
  where
    linuxNote = blankNote {appName = "simplex-chat"}
