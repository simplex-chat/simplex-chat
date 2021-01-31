{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ChatOptions
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding
import Numeric.Natural
import Simplex.Messaging.Agent (getSMPAgentClient, runSMPAgentClient)
import Simplex.Messaging.Agent.Client (AgentClient (..))
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Transmission
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Transport (getLn, putLn)
import Simplex.Messaging.Util (bshow, raceAny_)
import qualified System.Console.ANSI as C
import System.IO

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- TODO maybe take it out of config
      tbqSize = 16,
      connIdBytes = 12,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

data ChatClient = ChatClient
  { inQ :: TBQueue ChatCommand,
    outQ :: TBQueue ChatResponse,
    smpServer :: SMPServer,
    activeContact :: TVar (Maybe Contact),
    username :: TVar (Maybe Contact)
  }

newtype Contact = Contact {toBs :: ByteString}

-- | GroupMessage ChatGroup ByteString
-- | AddToGroup Contact
data ChatCommand
  = ChatHelp
  | AddContact Contact
  | AcceptContact Contact SMPQueueInfo
  | ChatWith Contact
  | SetName Contact
  | SendMessage Contact ByteString

chatCommandP :: Parser ChatCommand
chatCommandP =
  "/help" $> ChatHelp
    <|> "/add " *> (AddContact <$> contact)
    <|> "/accept " *> acceptContact
    <|> "/chat " *> chatWith
    <|> "/name " *> setName
    <|> "@" *> sendMessage
  where
    acceptContact = AcceptContact <$> contact <* A.space <*> smpQueueInfoP
    chatWith = ChatWith <$> contact
    setName = SetName <$> contact
    sendMessage = SendMessage <$> contact <* A.space <*> A.takeByteString
    contact = Contact <$> A.takeTill (== ' ')

data ChatResponse
  = ChatHelpInfo
  | Invitation SMPQueueInfo
  | Connected Contact
  | ReceivedMessage Contact ByteString
  | Disconnected Contact
  | YesYes
  | ErrorInput ByteString
  | ChatError AgentErrorType
  | NoChatResponse

serializeChatResponse :: Maybe Contact -> ChatResponse -> ByteString
serializeChatResponse name = \case
  ChatHelpInfo -> chatHelpInfo
  Invitation qInfo -> "ask your contact to enter: /accept " <> showName name <> " " <> serializeSmpQueueInfo qInfo
  Connected c -> ttyContact c <> " connected"
  ReceivedMessage c t -> ttyContact c <> ": " <> t
  Disconnected c -> "disconnected from " <> ttyContact c <> " - try \"/chat " <> toBs c <> "\""
  YesYes -> "you got it!"
  ErrorInput t -> "invalid input: " <> t
  ChatError e -> "chat error: " <> bshow e
  NoChatResponse -> ""
  where
    showName Nothing = "<your name>"
    showName (Just (Contact a)) = a

chatHelpInfo :: ByteString
chatHelpInfo =
  "Using chat:\n\
  \/add <name>       - create invitation to send out-of-band\n\
  \                    to your contact <name>\n\
  \                    (any unique string without spaces)\n\
  \/accept <name> <invitation> - accept <invitation>\n\
  \                    (a string that starts from \"smp::\")\n\
  \                    from your contact <name>\n\
  \/chat <name>      - resume chat with <name>\n\
  \/name <name>      - set <name> to use in invitations\n\
  \@<name> <message> - send <message> (any string) to contact <name>\n\
  \                    @<name> can be omitted to send to previous"

main :: IO ()
main = do
  ChatOpts {dbFileName, smpServer, name} <- getChatOpts
  putStrLn "simpleX chat prototype (no encryption), \"/help\" for usage information"
  t <- getChatClient smpServer (Contact <$> name)
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $
  env <- newSMPAgentEnv cfg {dbFile = dbFileName}
  dogFoodChat t env

dogFoodChat :: ChatClient -> Env -> IO ()
dogFoodChat t env = do
  c <- runReaderT getSMPAgentClient env
  raceAny_
    [ runReaderT (runSMPAgentClient c) env,
      sendToAgent t c,
      sendToTTY t,
      receiveFromAgent t c,
      receiveFromTTY t
    ]

getChatClient :: SMPServer -> Maybe Contact -> IO ChatClient
getChatClient srv name = atomically $ newChatClient (tbqSize cfg) srv name

newChatClient :: Natural -> SMPServer -> Maybe Contact -> STM ChatClient
newChatClient qSize smpServer name = do
  inQ <- newTBQueue qSize
  outQ <- newTBQueue qSize
  activeContact <- newTVar Nothing
  username <- newTVar name
  return ChatClient {inQ, outQ, smpServer, activeContact, username}

receiveFromTTY :: ChatClient -> IO ()
receiveFromTTY t =
  forever $ getChatLn t >>= processOrError . A.parseOnly (chatCommandP <* A.endOfInput)
  where
    processOrError = \case
      Left err -> atomically . writeTBQueue (outQ t) . ErrorInput $ B.pack err
      Right ChatHelp -> atomically . writeTBQueue (outQ t) $ ChatHelpInfo
      Right (SetName a) -> atomically $ do
        writeTVar (username t) $ Just a
        writeTBQueue (outQ t) YesYes
      Right cmd -> atomically $ writeTBQueue (inQ t) cmd

sendToTTY :: ChatClient -> IO ()
sendToTTY ChatClient {outQ, username} = forever $ do
  atomically (readTBQueue outQ) >>= \case
    NoChatResponse -> return ()
    resp -> do
      name <- readTVarIO username
      putLn stdout $ serializeChatResponse name resp

sendToAgent :: ChatClient -> AgentClient -> IO ()
sendToAgent ChatClient {inQ, smpServer, activeContact} AgentClient {rcvQ} =
  forever . atomically $ do
    cmd <- readTBQueue inQ
    writeTBQueue rcvQ `mapM_` agentTransmission cmd
    setActiveContact cmd
  where
    setActiveContact :: ChatCommand -> STM ()
    setActiveContact cmd =
      writeTVar activeContact $ case cmd of
        ChatWith a -> Just a
        SendMessage a _ -> Just a
        _ -> Nothing
    agentTransmission :: ChatCommand -> Maybe (ATransmission 'Client)
    agentTransmission = \case
      AddContact a -> transmission a $ NEW smpServer
      AcceptContact a qInfo -> transmission a $ JOIN qInfo $ ReplyVia smpServer
      ChatWith a -> transmission a SUB
      SendMessage a msg -> transmission a $ SEND msg
      ChatHelp -> Nothing
      SetName _ -> Nothing
    transmission :: Contact -> ACommand 'Client -> Maybe (ATransmission 'Client)
    transmission (Contact a) cmd = Just ("1", a, cmd)

receiveFromAgent :: ChatClient -> AgentClient -> IO ()
receiveFromAgent t c = forever . atomically $ do
  resp <- chatResponse <$> readTBQueue (sndQ c)
  writeTBQueue (outQ t) resp
  setActiveContact resp
  where
    chatResponse :: ATransmission 'Agent -> ChatResponse
    chatResponse (_, a, resp) = case resp of
      INV qInfo -> Invitation qInfo
      CON -> Connected $ Contact a
      END -> Disconnected $ Contact a
      MSG {m_body} -> ReceivedMessage (Contact a) m_body
      SENT _ -> NoChatResponse
      OK -> YesYes
      ERR e -> ChatError e
    setActiveContact :: ChatResponse -> STM ()
    setActiveContact = \case
      Connected a -> set $ Just a
      ReceivedMessage a _ -> set $ Just a
      Disconnected _ -> set Nothing
      _ -> return ()
      where
        set a = writeTVar (activeContact t) a

getChatLn :: ChatClient -> IO ByteString
getChatLn t = do
  setTTY NoBuffering
  getChar >>= \case
    '/' -> getRest "/"
    '@' -> getRest "@"
    ch -> do
      let s = encodeUtf8 $ T.singleton ch
      readTVarIO (activeContact t) >>= \case
        Nothing -> getRest s
        Just a -> getWithContact a s
  where
    getWithContact :: Contact -> ByteString -> IO ByteString
    getWithContact a s = do
      C.cursorBackward 1
      B.hPut stdout $ "    " <> ttyContact a <> " " <> s
      getRest $ "@" <> toBs a <> " " <> s
    getRest :: ByteString -> IO ByteString
    getRest s = do
      setTTY LineBuffering
      (s <>) <$> getLn stdin

setTTY :: BufferMode -> IO ()
setTTY mode = do
  hSetBuffering stdin mode
  hSetBuffering stdout mode

ttyContact :: Contact -> ByteString
ttyContact (Contact a) = withSGR contactSGR $ "@" <> a

contactSGR :: [C.SGR]
contactSGR = [C.SetColor C.Foreground C.Vivid C.Cyan]

withSGR :: [C.SGR] -> ByteString -> ByteString
withSGR sgr s = B.pack (C.setSGRCode sgr) <> s <> B.pack (C.setSGRCode [C.Reset])
