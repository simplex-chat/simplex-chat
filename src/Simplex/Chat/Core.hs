{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Core
  ( simplexChatCore,
    runSimplexChat,
    sendChatCmdStr,
    sendChatCmd,
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
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types
import Simplex.Chat.View (serializeChatResponse)
import Simplex.Messaging.Agent.Store.SQLite (SQLiteStore, withTransaction)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Async

simplexChatCore :: ChatConfig -> ChatOpts -> (User -> ChatController -> IO ()) -> IO ()
simplexChatCore cfg@ChatConfig {confirmMigrations, testView} opts@ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, dbKey, logAgent}} chat =
  case logAgent of
    Just level -> do
      setLogLevel level
      withGlobalLogging logCfg initRun
    _ -> initRun
  where
    initRun = createChatDatabase dbFilePrefix dbKey False confirmMigrations >>= either exit run
    exit e = do
      putStrLn $ "Error opening database: " <> show e
      exitFailure
    run db@ChatDatabase {chatStore} = do
      u_ <- getSelectActiveUser chatStore
      cc <- newChatController db u_ cfg opts False
      u <- maybe (createActiveUser cc) pure u_
      unless testView $ putStrLn $ "Current user: " <> userStr u
      runSimplexChat opts u cc chat

runSimplexChat :: ChatOpts -> User -> ChatController -> (User -> ChatController -> IO ()) -> IO ()
runSimplexChat ChatOpts {maintenance} u cc chat
  | maintenance = wait =<< async (chat u cc)
  | otherwise = do
      a1 <- runReaderT (startChatController True True) cc
      a2 <- async $ chat u cc
      waitEither_ a1 a2

sendChatCmdStr :: ChatController -> String -> IO ChatResponse
sendChatCmdStr cc s = runReaderT (execChatCommand Nothing . encodeUtf8 $ T.pack s) cc

sendChatCmd :: ChatController -> ChatCommand -> IO ChatResponse
sendChatCmd cc cmd = runReaderT (execChatCommand' cmd) cc

getSelectActiveUser :: SQLiteStore -> IO (Maybe User)
getSelectActiveUser st = do
  users <- withTransaction st getUsers
  case find activeUser users of
    Just u -> pure $ Just u
    Nothing -> selectUser users
  where
    selectUser :: [User] -> IO (Maybe User)
    selectUser = \case
      [] -> pure Nothing
      [user@User {userId}] -> do
        withTransaction st (`setActiveUser` userId)
        pure $ Just user
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
                | otherwise -> do
                    let user@User {userId} = users !! (n - 1)
                    withTransaction st (`setActiveUser` userId)
                    pure $ Just user

createActiveUser :: ChatController -> IO User
createActiveUser cc = do
  putStrLn
    "No user profiles found, it will be created now.\n\
    \Please choose your display name.\n\
    \It will be sent to your contacts when you connect.\n\
    \It is only stored on your device and you can change it later."
  loop
  where
    loop = do
      displayName <- T.pack <$> getWithPrompt "display name"
      let profile = Just Profile {displayName, fullName = "", image = Nothing, contactLink = Nothing, preferences = Nothing}
      execChatCommand' (CreateActiveUser NewUser {profile, pastTimestamp = False}) `runReaderT` cc >>= \case
        CRActiveUser user -> pure user
        r -> do
          ts <- getCurrentTime
          tz <- getCurrentTimeZone
          putStrLn $ serializeChatResponse (Nothing, Nothing) ts tz Nothing r
          loop

getWithPrompt :: String -> IO String
getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

userStr :: User -> String
userStr User {localDisplayName, profile = LocalProfile {fullName}} =
  T.unpack $ localDisplayName <> if T.null fullName || localDisplayName == fullName then "" else " (" <> fullName <> ")"
