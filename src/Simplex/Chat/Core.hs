{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Core
  ( simplexChatCore,
    runSimplexChat,
    sendChatCmdStr,
    sendChatCmd,
    printResponseEvent,
  )
where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Reader
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Library.Commands
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..), CreateBotOpts (..))
import Simplex.Chat.Remote.Types (RemoteHostId)
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences (FeatureAllowed (..), FilesPreference (..), Preferences (..), emptyChatPrefs)
import Simplex.Chat.View (ChatResponseEvent, serializeChatError, serializeChatResponse)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {confirmMigrations, testView, chatHooks} opts@ChatOpts {coreOptions = CoreChatOpts {dbOptions, logAgent, yesToUpMigrations, migrationBackupPath}, createBot, maintenance} chat =
  case logAgent of
    Just level -> do
      setLogLevel level
      withGlobalLogging logCfg initRun
    _ -> initRun
  where
    initRun = createChatDatabase dbOptions migrationConfig >>= either exit run
    migrationConfig = MigrationConfig (if confirmMigrations == MCConsole && yesToUpMigrations then MCYesUp else confirmMigrations) migrationBackupPath
    exit e = do
      putStrLn $ "Error opening database: " <> show e
      exitFailure
    run db@ChatDatabase {chatStore} = do
      u_ <- getSelectActiveUser chatStore
      let backgroundMode = not maintenance
      newChatController db u_ cfg opts backgroundMode >>= \case
        Left e -> do
          putStrLn $ "Error starting chat: " <> show e
          exitFailure
        Right cc -> do
          u <- maybe (createActiveUser cc createBot) pure u_
          unless testView $ putStrLn $ "Current user: " <> userStr u
          unless maintenance $ forM_ (preStartHook chatHooks) ($ cc)
          runSimplexChat opts u cc chat

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc@ChatController {config = ChatConfig {chatHooks}} chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True) cc
      forM_ (postStartHook chatHooks) ($ cc)
      a2 <- async $ chat u cc
      waitEither_ a1 a2

sendChatCmdStr :: ChatController -> String -> IO (Either ChatError ChatResponse)
sendChatCmdStr cc s = runReaderT (execChatCommand Nothing (encodeUtf8 $ T.pack s) 0) cc

sendChatCmd :: ChatController -> ChatCommand -> IO (Either ChatError ChatResponse)
sendChatCmd cc cmd = runReaderT (execChatCommand' cmd 0) cc

getSelectActiveUser :: DBStore -> IO (Maybe User)
getSelectActiveUser st = do
  users <- withTransaction st getUsers
  case find activeUser users of
    Just u -> pure $ Just u
    Nothing -> selectUser users
  where
    selectUser :: [User] -> IO (Maybe User)
    selectUser = \case
      [] -> pure Nothing
      [user] -> Just <$> withTransaction st (`setActiveUser` user)
      users -> do
        putStrLn "Select user profile:"
        forM_ (zip [1 :: Int ..] users) $ \(n, user) -> putStrLn $ show n <> ": " <> userStr user
        loop
        where
          loop = do
            nStr <- getWithPrompt $ "user number (1 .. " <> show (length users) <> ")"
            case readMaybe nStr :: Maybe Int of
              Nothing -> putStrLn "not a number" >> loop
              Just n
                | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
                | otherwise ->
                    let user = users !! (n - 1)
                     in Just <$> withTransaction st (`setActiveUser` user)

createActiveUser :: ChatController -> Maybe CreateBotOpts -> IO User
createActiveUser cc = \case
  Just CreateBotOpts {botDisplayName, allowFiles, clientService} -> do
    let preferences = if allowFiles then Nothing else Just emptyChatPrefs {files = Just FilesPreference {allow = FANo}}
    createUser exitFailure clientService $ (mkProfile botDisplayName) {peerType = Just CPTBot, preferences}
  Nothing -> do
    putStrLn
      "No user profiles found, it will be created now.\n\
      \Please choose your display name.\n\
      \It will be sent to your contacts when you connect.\n\
      \It is only stored on your device and you can change it later."
    loop
  where
    loop = do
      displayName <- T.pack <$> getWithPrompt "display name"
      createUser loop False $ mkProfile displayName
    mkProfile displayName = Profile {displayName, fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, peerType = Nothing, preferences = Nothing}
    createUser onError clientService p =
      execChatCommand' (CreateActiveUser NewUser {profile = Just p, pastTimestamp = False, clientService = BoolDef clientService}) 0 `runReaderT` cc >>= \case
        Right (CRActiveUser user) -> pure user
        r -> printResponseEvent (Nothing, Nothing) (config cc) r >> onError

printResponseEvent :: ChatResponseEvent r => (Maybe RemoteHostId, Maybe User) -> ChatConfig -> Either ChatError r -> IO ()
printResponseEvent hu cfg = \case
  Right r -> do
    ts <- getCurrentTime
    tz <- getCurrentTimeZone
    putStrLn $ serializeChatResponse hu cfg ts tz (fst hu) r
  Left e -> do
    putStrLn $ serializeChatError True cfg e

getWithPrompt :: String -> IO String
getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

userStr :: User -> String
userStr User {localDisplayName, profile = LocalProfile {fullName}} =
  T.unpack $ localDisplayName <> if T.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"
