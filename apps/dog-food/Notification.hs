{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Notification where

import ChatTerminal.Core (safeDecodeUtf8)
import Control.Monad (void)
-- import DBus.Notify (Body (Text), appName, blankNote, body, connectSession, notify, summary)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getAppUserDataDirectory)
import System.FilePath (combine)
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
  
linuxNotify :: Notification -> IO ()
linuxNotify Notification {title, text} = 
  void $ readCreateProcess (shell . T.unpack $ script) ""
  where
    script :: Text
    script = "notify-send \"" <> safeDecodeUtf8 title <> " " <> safeDecodeUtf8 text <> "\"'"
  {- do -}
  -- client <- connectSession
  -- let linuxNtf =
  --       linuxNote
  --         { summary = T.unpack $ safeDecodeUtf8 title,
  --           body = Just $ Text (T.unpack $ safeDecodeUtf8 text)
  --         }
  -- void $ notify client linuxNtf
  -- where
  --   linuxNote = blankNote {appName = "simplex-chat"}

macNotify :: Notification -> IO ()
macNotify Notification {title, text} =
  void $ readCreateProcess (shell . T.unpack $ script) ""
  where
    script :: Text
    script = "osascript -e 'display notification \"" <> safeDecodeUtf8 text <> "\" with title \"" <> safeDecodeUtf8 title <> "\"'"

initWinNotify :: IO (Notification -> IO ())
initWinNotify = winNotify <$> savePowershellScript

savePowershellScript :: IO FilePath
savePowershellScript = do
  appDir <- getAppUserDataDirectory "simplex"
  let psScript = combine appDir "win-toast-notify.ps1"
  writeFile psScript
    "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] > $null\n\
    \$Template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02)\n\
    \$RawXml = [xml] $Template.GetXml()\n\
    \($RawXml.toast.visual.binding.text|where {$_.id -eq \"1\"}).AppendChild($RawXml.CreateTextNode($args[0])) > $null\n\
    \($RawXml.toast.visual.binding.text|where {$_.id -eq \"2\"}).AppendChild($RawXml.CreateTextNode($args[1])) > $null\n\
    \$SerializedXml = New-Object Windows.Data.Xml.Dom.XmlDocument\n\
    \$SerializedXml.LoadXml($RawXml.OuterXml)\n\
    \$Toast = [Windows.UI.Notifications.ToastNotification]::new($SerializedXml)\n\
    \$Toast.Tag = \"simplex-chat\"\n\
    \$Toast.Group = \"simplex-chat\"\n\
    \$Toast.ExpirationTime = [DateTimeOffset]::Now.AddMinutes(1)\n\
    \$Notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier(\"PowerShell\")\n\
    \$Notifier.Show($Toast);\n"
  return psScript

winNotify :: FilePath -> Notification -> IO ()
winNotify path Notification {title, text} =
  void $ readCreateProcess (shell . T.unpack $ script) ""
  where
    script :: Text
    script = "powershell.exe \"" <> T.pack path <> " " <> safeDecodeUtf8 title <> " " <> safeDecodeUtf8 text <> "\""
