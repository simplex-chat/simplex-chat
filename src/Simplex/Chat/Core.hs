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
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
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
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types
import Simplex.Chat.Types.Preferences (FeatureAllowed (..), FilesPreference (..), Preferences (..), emptyChatPrefs)
import Simplex.Chat.View (ChatResponseEvent, serializeChatError, serializeChatResponse, simplexChatContact)
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {confirmMigrations, testView, chatHooks} opts@ChatOpts {coreOptions = coreOptions@CoreChatOpts {dbOptions, logAgent, yesToUpMigrations, migrationBackupPath}, createBot, maintenance} chat =
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
      users <- withTransaction chatStore getUsers
      u_ <- selectActiveUser coreOptions chatStore users
      let backgroundMode = not maintenance
      cc <- newChatController db u_ cfg opts backgroundMode
      u <- maybe (createActiveUser cc coreOptions createBot) pure u_
      unless testView $ putStrLn $ "Current user: " <> userStr u
      unless maintenance $ forM_ (preStartHook chatHooks) ($ cc)
      runSimplexChat cfg opts u cc chat

runSimplexChat :: ChatConfig -> ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatConfig {testView} ChatOpts {coreOptions = CoreChatOpts {chatRelay}, maintenance} u cc@ChatController {config = ChatConfig {chatHooks}} chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True) cc
      when (chatRelay && not testView) $ askCreateRelayAddress cc u
      forM_ (postStartHook chatHooks) ($ cc)
      a2 <- async $ chat u cc
      waitEither_ a1 a2

sendChatCmdStr :: ChatController -> String -> IO (Either ChatError ChatResponse)
sendChatCmdStr cc s = runReaderT (execChatCommand Nothing (encodeUtf8 $ T.pack s) 0) cc

sendChatCmd :: ChatController -> ChatCommand -> IO (Either ChatError ChatResponse)
sendChatCmd cc cmd = runReaderT (execChatCommand' cmd 0) cc

selectActiveUser :: CoreChatOpts -> DBStore -> [User] -> IO (Maybe User)
selectActiveUser CoreChatOpts {chatRelay} st users
  | chatRelay =
      case find (\User {userChatRelay} -> isTrue userChatRelay) users of
        Just u
          | activeUser u -> pure $ Just u
          | otherwise -> Just <$> withTransaction st (`setActiveUser` u)
        Nothing -> pure Nothing
  | otherwise =
      case find activeUser users of
        Just u -> pure $ Just u
        Nothing -> selectUser
  where
    selectUser :: IO (Maybe User)
    selectUser = case users of
      [] -> pure Nothing
      [user] -> Just <$> withTransaction st (`setActiveUser` user)
      _users -> do
        putStrLn "Select user profile:"
        forM_ (zip [1 :: Int ..] users) $ \(n, user) -> putStrLn $ show n <> ": " <> userStr user
        loop
        where
          loop = do
            nStr <- withPrompt ("user number (1 .. " <> show (length users) <> "): ") getLine
            case readMaybe nStr :: Maybe Int of
              Nothing -> putStrLn "not a number" >> loop
              Just n
                | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
                | otherwise ->
                    let user = users !! (n - 1)
                     in Just <$> withTransaction st (`setActiveUser` user)

createActiveUser :: ChatController -> CoreChatOpts -> Maybe CreateBotOpts -> IO User
createActiveUser cc CoreChatOpts {chatRelay} = \case
  Just CreateBotOpts {botDisplayName, allowFiles} -> do
    let preferences = if allowFiles then Nothing else Just emptyChatPrefs {files = Just FilesPreference {allow = FANo}}
    createUser exitFailure $ (mkProfile botDisplayName) {peerType = Just CPTBot, preferences}
  Nothing
    | chatRelay -> do
        putStrLn
          "No chat relay user profile found, it will be created now.\n\
          \Please choose chat relay display name."
        loop
    | otherwise -> do
        putStrLn
          "No user profiles found, it will be created now.\n\
          \Please choose your display name.\n\
          \It will be sent to your contacts when you connect.\n\
          \It is only stored on your device and you can change it later."
        loop
  where
    loop = do
      displayName <- T.pack <$> withPrompt "display name: " getLine
      createUser loop $ mkProfile displayName
    mkProfile displayName = Profile {displayName, fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, peerType = Nothing, preferences = Nothing}
    createUser onError p =
      execChatCommand' (CreateActiveUser NewUser {profile = Just p, pastTimestamp = False, userChatRelay = chatRelay}) 0 `runReaderT` cc >>= \case
        Right (CRActiveUser user) -> pure user
        r -> printResponseEvent (Nothing, Nothing) (config cc) r >> onError

askCreateRelayAddress :: ChatController -> User -> IO ()
askCreateRelayAddress cc@ChatController {chatStore} user =
  withTransaction chatStore (\db -> runExceptT $ getUserAddress db user) >>= \case
    Right _ -> pure ()
    Left SEUserContactLinkNotFound -> promptCreate
    Left e -> printChatError (config cc) $ ChatErrorStore e
  where
    promptCreate :: IO ()
    promptCreate = do
      ok <- onOffPrompt "Create relay address" True
      when ok $
        execChatCommand' CreateMyAddress 0 `runReaderT` cc >>= \case
          Right (CRUserContactLinkCreated _ address) -> do
            putStrLn "Chat relay address is created:"
            putStrLn $ addressStr address
          r -> printResponseEvent (Nothing, Nothing) (config cc) r
    addressStr :: CreatedLinkContact -> String
    addressStr (CCLink cReq shortLink) = B.unpack $ maybe cReqStr strEncode shortLink
      where
        cReqStr = strEncode $ simplexChatContact cReq

printResponseEvent :: ChatResponseEvent r => (Maybe RemoteHostId, Maybe User) -> ChatConfig -> Either ChatError r -> IO ()
printResponseEvent hu cfg = \case
  Right r -> do
    ts <- getCurrentTime
    tz <- getCurrentTimeZone
    putStrLn $ serializeChatResponse hu cfg ts tz (fst hu) r
  Left e -> printChatError cfg e

printChatError :: ChatConfig -> ChatError -> IO ()
printChatError cfg e = putStrLn $ serializeChatError True cfg e

withPrompt :: String -> IO a -> IO a
withPrompt s a = putStr s >> hFlush stdout >> a

onOffPrompt :: String -> Bool -> IO Bool
onOffPrompt prompt def =
  withPrompt (prompt <> if def then " (Yn): " else " (yN): ") $
    getLine >>= \case
      "" -> pure def
      "y" -> pure True
      "Y" -> pure True
      "n" -> pure False
      "N" -> pure False
      _ -> putStrLn "Invalid input, please enter 'y' or 'n'" >> onOffPrompt prompt def

userStr :: User -> String
userStr User {localDisplayName, profile = LocalProfile {fullName}} =
  T.unpack $ localDisplayName <> if T.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"
