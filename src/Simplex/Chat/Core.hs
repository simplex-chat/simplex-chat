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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import System.FilePath (takeExtension)
import UnliftIO.STM
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
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import Simplex.Messaging.Encoding.String
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {confirmMigrations, testView, chatHooks} opts@ChatOpts {coreOptions = coreOptions@CoreChatOpts {dbOptions, logAgent, yesToUpMigrations, migrationBackupPath, maintenance}, createBot, userDisplayName, userImageFile} chat =
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
      let backgroundMode = maintenance
      newChatController db u_ cfg opts backgroundMode >>= \case
        Left e -> do
          putStrLn $ "Error starting chat: " <> show e
          exitFailure
        Right cc -> do
          forM_ (preStartHook chatHooks) ($ cc)
          u0 <- case u_ of
            Nothing -> noMaintenance >> createActiveUser cc coreOptions createBot userDisplayName
            Just u@User {localDisplayName} -> do
              forM_ userDisplayName $ \name ->
                when (localDisplayName /= name) $ do
                  putStrLn $ "Active user display name " <> show localDisplayName <> " does not match --user-display-name " <> show name
                  exitFailure
              pure u
          u <- maybe (pure u0) (applyUserImage cc chatStore u0) userImageFile
          unless testView $ putStrLn $ "Current user: " <> userStr u
          runSimplexChat cfg opts u cc chat
    noMaintenance = when maintenance $ do
      putStrLn "exiting: no active user in maintenance mode"
      exitFailure

runSimplexChat :: ChatConfig -> ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatConfig {testView} ChatOpts {coreOptions = CoreChatOpts {chatRelay, chatRelayServer, maintenance}} u cc@ChatController {config = ChatConfig {chatHooks}} chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True) cc
      when (chatRelay && not testView) $ askCreateRelayAddress cc u chatRelayServer
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

createActiveUser :: ChatController -> CoreChatOpts -> Maybe CreateBotOpts -> Maybe Text -> IO User
createActiveUser cc CoreChatOpts {chatRelay} createBot_ userDisplayName_ = case createBot_ of
  Just CreateBotOpts {botDisplayName, allowFiles, clientService} -> do
    let preferences = if allowFiles then Nothing else Just emptyChatPrefs {files = Just FilesPreference {allow = FANo}}
    createUser exitFailure clientService $ (mkProfile botDisplayName) {peerType = Just CPTBot, preferences}
  Nothing -> case userDisplayName_ of
    Just displayName -> createUser exitFailure False $ mkProfile displayName
    Nothing -> putStrLn prompt >> loop
      where
        prompt
          | chatRelay =
              "No chat relay user profile found, it will be created now.\n\
              \Please choose chat relay display name."
          | otherwise =
              "No user profiles found, it will be created now.\n\
              \Please choose your display name.\n\
              \It will be sent to your contacts when you connect.\n\
              \It is only stored on your device and you can change it later."
  where
    loop = do
      displayName <- T.pack <$> withPrompt "display name" getLine
      createUser loop False $ mkProfile displayName
    mkProfile displayName = Profile {displayName, fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, peerType = Nothing, preferences = Nothing, badge = Nothing, contactDomain = Nothing}
    createUser onError clientService p =
      execChatCommand' (CreateActiveUser NewUser {profile = Just p, pastTimestamp = False, userChatRelay = BoolDef chatRelay, clientService = BoolDef clientService}) 0 `runReaderT` cc >>= \case
        Right (CRActiveUser user) -> pure user
        r -> printResponseEvent (Nothing, Nothing) (config cc) r >> onError

askCreateRelayAddress :: ChatController -> User -> Maybe SMPServerWithAuth -> IO ()
askCreateRelayAddress cc@ChatController {chatStore} user@User {userId} server_ =
  withTransaction chatStore (\db -> runExceptT $ getUserAddress db user) >>= \case
    Right _ -> pure ()
    Left SEUserContactLinkNotFound -> promptCreate
    Left e -> printChatError (config cc) $ ChatErrorStore e
  where
    promptCreate :: IO ()
    promptCreate = do
      ok <- onOffPrompt "Create relay address" True
      when ok $
        execChatCommand' (APICreateMyAddress userId server_) 0 `runReaderT` cc >>= \case
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

applyUserImage :: ChatController -> DBStore -> User -> FilePath -> IO User
applyUserImage cc store u@User {profile = p@LocalProfile {image = currentImg}} path = do
  newImg <- loadImageFile path >>= either failExit pure
  if currentImg == Just newImg
    then pure u
    else do
      let p' = (fromLocalProfile p) {image = Just newImg} :: Profile
      withTransaction store (\db -> runExceptT $ updateUserProfile db u p') >>= \case
        Left e -> failExit $ "Failed to update user profile: " <> show e
        Right u' -> u' <$ atomically (writeTVar (currentUser cc) (Just u'))
  where
    failExit msg = putStrLn msg >> exitFailure

loadImageFile :: FilePath -> IO (Either String ImageData)
loadImageFile path = case map toLower (takeExtension path) of
  ".png" -> readAs "image/png"
  ".jpg" -> readAs "image/jpg"
  ".jpeg" -> readAs "image/jpg"
  ext -> pure $ Left $ "--user-image-file: unsupported image extension " <> show ext <> " (only .png, .jpg, .jpeg)"
  where
    -- matches the cap mobile/desktop UIs pass to resizeImageToStrSize for profile images
    maxProfileImageSize = 12500
    readAs mime = do
      bs <- BS.readFile path
      let url = "data:" <> mime <> ";base64," <> decodeUtf8 (B64.encode bs)
      pure $ if T.length url > maxProfileImageSize
        then Left $ "--user-image-file: encoded image size " <> show (T.length url) <> " bytes exceeds max " <> show maxProfileImageSize <> " bytes"
        else Right $ ImageData url

userStr :: User -> String
userStr User {localDisplayName, profile = LocalProfile {fullName}} =
  T.unpack $ localDisplayName <> if T.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"
