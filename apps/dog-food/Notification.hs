{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Notification (Notification (..), initializeNotifications) where

import ChatTerminal.Core (safeDecodeUtf8)
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory)
import System.FilePath (combine)
import System.Info (os)
import System.Process (readCreateProcess, shell)

data Notification = Notification {title :: ByteString, text :: ByteString}

initializeNotifications :: IO (Notification -> IO ())
initializeNotifications = case os of
  "darwin" -> pure $ notify macScript
  "mingw32" -> initWinNotify
  "linux" ->
    doesFileExist "/proc/sys/kernel/osrelease" >>= \case
      False -> pure $ notify linuxScript
      True -> do
        v <- readFile "/proc/sys/kernel/osrelease"
        if "Microsoft" `isInfixOf` v || "WSL" `isInfixOf` v
          then initWslNotify
          else pure $ notify linuxScript
  _ -> pure . const $ pure ()

notify :: (Notification -> Text) -> Notification -> IO ()
notify script notification =
  void $ readCreateProcess (shell . T.unpack $ script notification) ""

linuxScript :: Notification -> Text
linuxScript Notification {title, text} = "notify-send '" <> linuxEscape (safeDecodeUtf8 title) <> "' '" <> linuxEscape (safeDecodeUtf8 text) <> "'"

linuxEscape :: Text -> Text
linuxEscape text = do
  T.concatMap
    ( \c ->
        case c of
          '\'' -> "'\\''"
          _ -> T.singleton c
    )
    text

macScript :: Notification -> Text
macScript Notification {title, text} = "osascript -e 'display notification \"" <> safeDecodeUtf8 text <> "\" with title \"" <> safeDecodeUtf8 title <> "\"'"

initWslNotify :: IO (Notification -> IO ())
initWslNotify = notify . wslScript <$> savePowershellScript

wslScript :: FilePath -> Notification -> Text
wslScript path Notification {title, text} = "powershell.exe \"" <> T.pack path <> " \\\"" <> wslEscape (safeDecodeUtf8 title) <> "\\\" \\\"" <> wslEscape (safeDecodeUtf8 text) <> "\\\"\""

wslEscape :: Text -> Text
wslEscape text = do
  T.concatMap
    ( \c ->
        case c of
          '`' -> "\\`\\`"
          '\\' -> "\\\\"
          '"' -> "\\`\\\""
          _ -> T.singleton c
    )
    text

initWinNotify :: IO (Notification -> IO ())
initWinNotify = notify . winScript <$> savePowershellScript

winScript :: FilePath -> Notification -> Text
winScript path Notification {title, text} = "powershell.exe \"" <> T.pack path <> " '" <> winRemoveQuotes (safeDecodeUtf8 title) <> "' '" <> winRemoveQuotes (safeDecodeUtf8 text) <> "'\""

winRemoveQuotes :: Text -> Text
winRemoveQuotes text = do
  T.concatMap
    ( \c ->
        case c of
          '`' -> ""
          '\'' -> ""
          '"' -> ""
          _ -> T.singleton c
    )
    text

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
