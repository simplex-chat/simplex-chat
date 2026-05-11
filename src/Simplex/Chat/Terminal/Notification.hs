{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Terminal.Notification (Notification (..), initializeNotifications) where

import Data.List (isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Simplex.Messaging.Util (catchAll_)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getAppUserDataDirectory)
import System.FilePath (combine)
import System.Info (os)
import System.Process (callProcess)

data Notification = Notification {title :: T.Text, text :: T.Text}

initializeNotifications :: IO (Notification -> IO ())
initializeNotifications =
  hideException <$> case os of
    "darwin" -> pure macNotify
    "mingw32" -> initWinNotify
    "linux" ->
      doesFileExist "/proc/sys/kernel/osrelease" >>= \case
        False -> initLinuxNotify
        True -> do
          v <- readFile "/proc/sys/kernel/osrelease"
          if "Microsoft" `isInfixOf` v || "WSL" `isInfixOf` v
            then initWslNotify
            else initLinuxNotify
    _ -> pure noNotifications

noNotifications :: Notification -> IO ()
noNotifications _ = pure ()

hideException :: (a -> IO ()) -> (a -> IO ())
hideException f a = f a `catchAll_` pure ()

initLinuxNotify :: IO (Notification -> IO ())
initLinuxNotify = do
  found <- isJust <$> findExecutable "notify-send"
  pure $ if found then linuxNotify else noNotifications

linuxNotify :: Notification -> IO ()
linuxNotify Notification {title, text} =
  callProcess "notify-send" [T.unpack title, T.unpack text]

macNotify :: Notification -> IO ()
macNotify Notification {title, text} =
  callProcess "osascript" ["-e", "display notification \"" <> macEscape text <> "\" with title \"" <> macEscape title <> "\""]

macEscape :: T.Text -> String
macEscape = concatMap esc . T.unpack
  where
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc c = [c]

initWslNotify :: IO (Notification -> IO ())
initWslNotify = wslNotify <$> savePowershellScript

wslNotify :: FilePath -> Notification -> IO ()
wslNotify path Notification {title, text} =
  callProcess "powershell.exe" ["-File", path, T.unpack title, T.unpack text]

initWinNotify :: IO (Notification -> IO ())
initWinNotify = winNotify <$> savePowershellScript

winNotify :: FilePath -> Notification -> IO ()
winNotify path Notification {title, text} =
  callProcess "powershell.exe" ["-File", path, T.unpack title, T.unpack text]

savePowershellScript :: IO FilePath
savePowershellScript = do
  appDir <- getAppUserDataDirectory "simplex"
  createDirectoryIfMissing False appDir
  let psScript = combine appDir "win-toast-notify.ps1"
  writeFile
    psScript
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
