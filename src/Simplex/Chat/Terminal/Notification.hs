{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Terminal.Notification (Notification (..), initializeNotifications) where

import Control.Monad (void)
import Data.List (isInfixOf)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Util (catchAll_)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, getAppUserDataDirectory)
import System.FilePath (combine)
import System.Info (os)
import System.Process (readCreateProcess, shell)

data Notification = Notification {title :: Text, text :: Text}

initializeNotifications :: IO (Notification -> IO ())
initializeNotifications =
  hideException <$> case os of
    "darwin" -> pure $ notify macScript
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
  pure $ if found then notify linuxScript else noNotifications

notify :: (Notification -> Text) -> Notification -> IO ()
notify script notification =
  void $ readCreateProcess (shell . T.unpack $ script notification) ""

linuxScript :: Notification -> Text
linuxScript Notification {title, text} = "notify-send '" <> linuxEscape title <> "' '" <> linuxEscape text <> "'"

linuxEscape :: Text -> Text
linuxEscape = replaceAll $ fromList [('\'', "'\\''")]

macScript :: Notification -> Text
macScript Notification {title, text} = "osascript -e 'display notification \"" <> macEscape text <> "\" with title \"" <> macEscape title <> "\"'"

macEscape :: Text -> Text
macEscape = replaceAll $ fromList [('"', "\\\""), ('\'', "")]

initWslNotify :: IO (Notification -> IO ())
initWslNotify = notify . wslScript <$> savePowershellScript

wslScript :: FilePath -> Notification -> Text
wslScript path Notification {title, text} = "powershell.exe \"" <> T.pack path <> " \\\"" <> wslEscape title <> "\\\" \\\"" <> wslEscape text <> "\\\"\""

wslEscape :: Text -> Text
wslEscape = replaceAll $ fromList [('`', "\\`\\`"), ('\\', "\\\\"), ('"', "\\`\\\"")]

initWinNotify :: IO (Notification -> IO ())
initWinNotify = notify . winScript <$> savePowershellScript

winScript :: FilePath -> Notification -> Text
winScript path Notification {title, text} = "powershell.exe \"" <> T.pack path <> " '" <> winRemoveQuotes title <> "' '" <> winRemoveQuotes text <> "'\""

winRemoveQuotes :: Text -> Text
winRemoveQuotes = replaceAll $ fromList [('`', ""), ('\'', ""), ('"', "")]

replaceAll :: Map Char Text -> Text -> Text
replaceAll rules = T.concatMap $ \c -> T.singleton c `fromMaybe` M.lookup c rules

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
