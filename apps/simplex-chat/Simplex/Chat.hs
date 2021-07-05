{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.Aeson as J
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Controller
import Simplex.Chat.Protocol
import Simplex.Chat.Styled (plain)
import Simplex.Chat.Types
import Simplex.Help
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Protocol
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (bshow, raceAny_)
import Simplex.Notification
import Simplex.Store
import Simplex.Terminal
import Simplex.View
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import qualified UnliftIO.Exception as E
import UnliftIO.STM

data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddContact
  | Connect SMPQueueInfo
  | DeleteContact ContactRef
  | SendMessage ContactRef ByteString
  deriving (Show)

runChatController :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
runChatController =
  raceAny_
    [ inputSubscriber,
      agentSubscriber,
      chatSubscriber,
      notificationSubscriber
    ]

inputSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
inputSubscriber = do
  q <- asks inputQ
  forever $
    atomically (readTBQueue q) >>= \case
      InputControl _ -> pure ()
      InputCommand s ->
        case parseAll chatCommandP . encodeUtf8 $ T.pack s of
          Left e -> printToView [plain s, "invalid input: " <> plain e]
          Right cmd -> do
            case cmd of
              SendMessage c msg -> showSentMessage c msg
              _ -> printToView [plain s]
            user <- asks currentUser
            runExceptT (processChatCommand user cmd) >>= \case
              Left e -> showChatError e
              _ -> pure ()

processChatCommand :: ChatMonad m => User -> ChatCommand -> m ()
processChatCommand User {userId, profile} = \case
  ChatHelp -> printToView chatHelpInfo
  MarkdownHelp -> printToView markdownInfo
  AddContact -> do
    (connId, qInfo) <- withAgent createConnection
    withStore $ \st -> createDirectConnection st userId connId
    showInvitation qInfo
  Connect qInfo -> do
    connId <- withAgent $ \agent -> joinConnection agent qInfo $ LB.toStrict (J.encode profile)
    withStore $ \st -> createDirectConnection st userId connId
  DeleteContact cRef -> do
    conns <- withStore $ \st -> getContactConnections st userId cRef
    withAgent $ \smp -> forM_ conns $ \Connection {agentConnId} ->
      deleteConnection smp agentConnId `catchError` \(_ :: AgentErrorType) -> pure ()
    withStore $ \st -> deleteContact st userId cRef
    unsetActive $ ActiveC cRef
    when (null conns) . throwError . ChatErrorContact $ CENotFound cRef
    showContactDeleted cRef
  SendMessage cRef msg -> do
    Connection {agentConnId} <- withStore $ \st -> getContactConnection st userId cRef
    let body = MsgBodyContent {contentType = SimplexContentType XCText, contentHash = Nothing, contentData = MBFull $ MsgData msg}
        rawMsg = rawChatMessage ChatMessage {chatMsgId = Nothing, chatMsgEvent = XMsgNew MTText, chatMsgBody = [body], chatDAGIdx = Nothing}
    void . withAgent $ \smp -> sendMessage smp agentConnId $ serializeRawChatMessage rawMsg
    setActive $ ActiveC cRef

agentSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
agentSubscriber = do
  aQ <- asks $ subQ . smpAgent
  cQ <- asks chatQ
  forever $ do
    (_, agentConnId, resp) <- atomically (readTBQueue aQ)
    User {userId} <- asks currentUser
    runExceptT (withStore $ \st -> getConnectionChatDirection st userId agentConnId) >>= \case
      -- TODO handle errors
      Left e -> liftIO $ print e
      Right chatDirection -> do
        case resp of
          MSG agentMsgMeta msgBody -> do
            atomically . writeTBQueue cQ $
              case first B.pack (parseAll rawChatMessageP msgBody) >>= toChatMessage of
                Right chatMessage -> ChatTransmission {agentMsgMeta, chatDirection, chatMessage}
                Left msgError -> ChatTransmissionError {agentMsgMeta, chatDirection, msgBody, msgError}
          agentMessage ->
            atomically $ writeTBQueue cQ AgentTransmission {agentConnId, chatDirection, agentMessage}

chatSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
chatSubscriber = do
  cQ <- asks chatQ
  forever $ do
    User {userId, profile} <- asks currentUser
    atomically (readTBQueue cQ) >>= \case
      ChatTransmission
        { agentMsgMeta = meta,
          chatDirection = ReceivedDirectMessage Contact {localContactRef = c},
          chatMessage = ChatMessage {chatMsgEvent, chatMsgBody}
        } ->
          case chatMsgEvent of
            XMsgNew MTText -> do
              case find (isSimplexContentType XCText) chatMsgBody of
                Just MsgBodyContent {contentData = MBFull (MsgData bs)} -> do
                  let text = safeDecodeUtf8 bs
                  showReceivedMessage c (snd $ broker meta) text (integrity meta)
                  showToast ("@" <> c) text
                  setActive $ ActiveC c
                _ -> pure ()
            _ -> pure ()
      AgentTransmission {agentConnId, chatDirection = ReceivedDirectMessage NewContact {activeConn}, agentMessage} ->
        void . runExceptT $ case agentMessage of
          CONF confId connInfo -> do
            -- TODO update connection status
            saveContact userId activeConn connInfo
            withAgent $ \a -> allowConnection a agentConnId confId $ LB.toStrict (J.encode profile)
          INFO connInfo ->
            saveContact userId activeConn connInfo
          _ -> pure ()
      AgentTransmission {chatDirection = ReceivedDirectMessage Contact {localContactRef = c}, agentMessage} ->
        case agentMessage of
          CON -> do
            -- TODO update connection status
            showContactConnected c
            showToast ("@" <> c) "connected"
            setActive $ ActiveC c
          END -> do
            showContactDisconnected c
            showToast ("@" <> c) "disconnected"
            unsetActive $ ActiveC c
          _ -> pure ()
      _ -> pure ()
  where
    saveContact userId activeConn connInfo = do
      p <- liftEither . first (ChatErrorContact . CEProfile) $ J.eitherDecodeStrict' connInfo
      withStore $ \st -> createDirectContact st userId activeConn p

getCreateActiveUser :: SQLiteStore -> IO User
getCreateActiveUser st = do
  user <-
    getUsers st >>= \case
      [] -> newUser
      users -> maybe (selectUser users) pure (find activeUser users)
  putStrLn $ "Current user: " <> userStr user
  pure user
  where
    newUser :: IO User
    newUser = do
      putStrLn
        "No user profiles found, it will be created now.\n\
        \Please choose your alias and your profile name.\n\
        \They will be sent to your contacts when you connect.\n\
        \They are only stored on your device and you can change them later."
      loop
      where
        loop = do
          contactRef <- getContactRef
          displayName <- T.pack <$> getWithPrompt "profile name (optional)"
          liftIO (runExceptT $ createUser st Profile {contactRef, displayName} True) >>= \case
            Left SEDuplicateContactRef -> do
              putStrLn "chosen alias already used by another profile on this device, choose another one"
              loop
            Left e -> putStrLn ("database error " <> show e) >> exitFailure
            Right user -> pure user
    selectUser :: [User] -> IO User
    selectUser [user] = do
      liftIO $ setActiveUser st (userId user)
      pure user
    selectUser users = do
      putStrLn "Select user profile: "
      forM_ (zip [1 ..] users) $ \(n :: Int, user) -> putStrLn $ show n <> " - " <> userStr user
      loop
      where
        loop = do
          nStr <- getWithPrompt $ "user profile number (1 .. " <> show (length users) <> ")"
          case readMaybe nStr :: Maybe Int of
            Nothing -> putStrLn "invalid user number" >> loop
            Just n
              | n <= 0 || n > length users -> putStrLn "invalid user number" >> loop
              | otherwise -> do
                let user = users !! (n - 1)
                liftIO $ setActiveUser st (userId user)
                pure user
    userStr :: User -> String
    userStr User {localContactRef, profile = Profile {displayName}} =
      T.unpack $ localContactRef <> if T.null displayName then "" else " (" <> displayName <> ")"
    getContactRef :: IO ContactRef
    getContactRef = do
      contactRef <- getWithPrompt "alias (no spaces)"
      if null contactRef || isJust (find (== ' ') contactRef)
        then putStrLn "alias has space(s), choose another one" >> getContactRef
        else pure $ T.pack contactRef
    getWithPrompt :: String -> IO String
    getWithPrompt s = putStr (s <> ": ") >> hFlush stdout >> getLine

showToast :: (MonadUnliftIO m, MonadReader ChatController m) => Text -> Text -> m ()
showToast title text = atomically . (`writeTBQueue` Notification {title, text}) =<< asks notifyQ

notificationSubscriber :: (MonadUnliftIO m, MonadReader ChatController m) => m ()
notificationSubscriber = do
  ChatController {notifyQ, sendNotification} <- ask
  forever $ atomically (readTBQueue notifyQ) >>= liftIO . sendNotification

withAgent :: ChatMonad m => (AgentClient -> ExceptT AgentErrorType m a) -> m a
withAgent action =
  asks smpAgent
    >>= runExceptT . action
    >>= liftEither . first ChatErrorAgent

withStore ::
  ChatMonad m =>
  (forall m'. (MonadUnliftIO m', MonadError StoreError m') => SQLiteStore -> m' a) ->
  m a
withStore action = do
  st <- asks chatStore
  runExceptT (action st `E.catch` handleInternal) >>= \case
    Right c -> pure c
    Left e -> throwError $ storeError e
  where
    -- TODO when parsing exception happens in store, the agent hangs;
    -- changing SQLError to SomeException does not help
    handleInternal :: (MonadError StoreError m') => E.SomeException -> m' a
    handleInternal e = throwError . SEInternal $ bshow e
    storeError :: StoreError -> ChatError
    storeError = \case
      SEContactNotFound c -> ChatErrorContact $ CENotFound c
      e -> ChatErrorStore e

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/add" <|> "/a") $> AddContact
    <|> ("/connect " <|> "/c ") *> (Connect <$> smpQueueInfoP)
    <|> ("/delete " <|> "/d ") *> (DeleteContact <$> contactRef)
    <|> A.char '@' *> (SendMessage <$> contactRef <*> (A.space *> A.takeByteString))
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
  where
    contactRef = safeDecodeUtf8 <$> A.takeTill (== ' ')
