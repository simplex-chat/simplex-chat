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
import ChatTerminal
import ChatTerminal.Core
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding
import Numeric.Natural
import Simplex.Markdown
import Simplex.Messaging.Agent (getSMPAgentClient, runSMPAgentClient)
import Simplex.Messaging.Agent.Client (AgentClient (..))
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Transmission
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (raceAny_)
import Styled
import System.Console.ANSI.Types
import System.Directory (getAppUserDataDirectory)
import Types

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- TODO maybe take it out of config
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

data ChatClient = ChatClient
  { inQ :: TBQueue ChatCommand,
    outQ :: TBQueue ChatResponse,
    smpServer :: SMPServer
  }

-- | GroupMessage ChatGroup ByteString
-- | AddToGroup Contact
data ChatCommand
  = ChatHelp
  | MarkdownHelp
  | AddConnection Contact
  | Connect Contact SMPQueueInfo
  | DeleteConnection Contact
  | SendMessage Contact ByteString

chatCommandP :: Parser ChatCommand
chatCommandP =
  ("/help" <|> "/h") $> ChatHelp
    <|> ("/markdown" <|> "/m") $> MarkdownHelp
    <|> ("/add " <|> "/a ") *> (AddConnection <$> contact)
    <|> ("/connect " <> "/c ") *> connect
    <|> ("/delete " <> "/d ") *> (DeleteConnection <$> contact)
    <|> "@" *> sendMessage
  where
    connect = Connect <$> contact <* A.space <*> smpQueueInfoP
    sendMessage = SendMessage <$> contact <* A.space <*> A.takeByteString
    contact = Contact <$> A.takeTill (== ' ')

data ChatResponse
  = ChatHelpInfo
  | MarkdownInfo
  | Invitation SMPQueueInfo
  | Connected Contact
  | Confirmation Contact
  | ReceivedMessage Contact ByteString MsgIntegrity
  | Disconnected Contact
  | YesYes
  | ContactError ConnectionErrorType Contact
  | ErrorInput ByteString
  | ChatError AgentErrorType
  | NoChatResponse

serializeChatResponse :: ChatOpts -> ChatResponse -> [StyledString]
serializeChatResponse ChatOpts {msgIntegrity} = \case
  ChatHelpInfo -> chatHelpInfo
  MarkdownInfo -> markdownInfo
  Invitation qInfo ->
    [ "pass this invitation to your contact (via any channel): ",
      "",
      (bPlain . serializeSmpQueueInfo) qInfo,
      "",
      "and ask them to connect: /c <name_for_you> <invitation_above>"
    ]
  Connected c -> [ttyContact c <> " connected"]
  Confirmation c -> [ttyContact c <> " ok"]
  ReceivedMessage c t mi ->
    prependFirst (ttyFromContact c) (msgPlain t)
      ++ showIntegrity mi
  Disconnected c -> ["disconnected from " <> ttyContact c <> " - restart chat"]
  YesYes -> ["you got it!"]
  ContactError e c -> case e of
    UNKNOWN -> ["no contact " <> ttyContact c]
    DUPLICATE -> ["contact " <> ttyContact c <> " already exists"]
    SIMPLEX -> ["contact " <> ttyContact c <> " did not accept invitation yet"]
  ErrorInput t -> ["invalid input: " <> bPlain t]
  ChatError e -> ["chat error: " <> plain (show e)]
  NoChatResponse -> [""]
  where
    prependFirst :: StyledString -> [StyledString] -> [StyledString]
    prependFirst s [] = [s]
    prependFirst s (s' : ss) = (s <> s') : ss
    msgPlain :: ByteString -> [StyledString]
    msgPlain = map styleMarkdownText . T.lines . safeDecodeUtf8
    showIntegrity :: MsgIntegrity -> [StyledString]
    showIntegrity MsgOk = []
    showIntegrity (MsgError err)
      | msgIntegrity = msgError $ case err of
        MsgSkipped fromId toId -> unwords ["skipped message IDs from", show fromId, "to", show toId]
        MsgBadId msgId -> "unexpected message ID " <> show msgId
        MsgBadHash -> "incorrect message hash"
        MsgDuplicate -> "duplicate message ID"
      | otherwise = []
    msgError :: String -> [StyledString]
    msgError s = [styled (Colored Red) s]

chatHelpInfo :: [StyledString]
chatHelpInfo =
  map
    styleMarkdown
    [ Markdown (Colored Cyan) "Using Simplex chat prototype.",
      "Follow these steps to set up a connection:",
      "",
      Markdown (Colored Green) "Step 1: " <> Markdown (Colored Cyan) "/add bob" <> " -- Alice adds her contact, Bob (she can use any name).",
      indent <> "Alice should send the invitation printed by the /add command",
      indent <> "to her contact, Bob, out-of-band, via any trusted channel.",
      "",
      Markdown (Colored Green) "Step 2: " <> Markdown (Colored Cyan) "/connect alice <invitation>" <> " -- Bob accepts the invitation.",
      indent <> "Bob also can use any name for his contact, Alice,",
      indent <> "followed by the invitation he received out-of-band.",
      "",
      Markdown (Colored Green) "Step 3: " <> "Bob and Alice are notified that the connection is set up,",
      indent <> "both can now send messages:",
      indent <> Markdown (Colored Cyan) "@bob Hello, Bob!" <> " -- Alice messages Bob.",
      indent <> Markdown (Colored Cyan) "@alice Hey, Alice!" <> " -- Bob replies to Alice.",
      "",
      Markdown (Colored Green) "Other commands:",
      indent <> Markdown (Colored Cyan) "/delete" <> " -- deletes contact and all messages with them.",
      indent <> Markdown (Colored Cyan) "/markdown" <> " -- prints the supported markdown syntax.",
      "",
      "The commands may be abbreviated to a single letter: " <> listCommands ["/a", "/c", "/d", "/m"]
    ]
  where
    listCommands = mconcat . intersperse ", " . map highlight
    highlight = Markdown (Colored Cyan)
    indent = "        "

markdownInfo :: [StyledString]
markdownInfo =
  map
    styleMarkdown
    [ "Markdown:",
      "  *bold*          - " <> Markdown Bold "bold text",
      "  _italic_        - " <> Markdown Italic "italic text" <> " (shown as underlined)",
      "  +underlined+    - " <> Markdown Underline "underlined text",
      "  ~strikethrough~ - " <> Markdown StrikeThrough "strikethrough text" <> " (shown as inverse)",
      "  `code snippet`  - " <> Markdown Snippet "a + b // no *markdown* here",
      "  !1 text!        - " <> red "red text" <> " (1-6: red, green, blue, yellow, cyan, magenta)",
      "  #secret#        - " <> Markdown Secret "secret text" <> " (can be copy-pasted)"
    ]
  where
    red = Markdown (Colored Red)

main :: IO ()
main = do
  opts@ChatOpts {dbFileName, smpServer, termMode} <- welcomeGetOpts
  t <- getChatClient smpServer
  ct <- newChatTerminal (tbqSize cfg) termMode
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  env <- newSMPAgentEnv cfg {dbFile = dbFileName}
  dogFoodChat t ct env opts

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFileName} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype"
  putStrLn $ "db: " <> dbFileName
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

dogFoodChat :: ChatClient -> ChatTerminal -> Env -> ChatOpts -> IO ()
dogFoodChat t ct env opts = do
  c <- runReaderT getSMPAgentClient env
  raceAny_
    [ runReaderT (runSMPAgentClient c) env,
      sendToAgent t ct c,
      sendToChatTerm t ct opts,
      receiveFromAgent t ct c,
      receiveFromChatTerm t ct,
      chatTerminal ct
    ]

getChatClient :: SMPServer -> IO ChatClient
getChatClient srv = atomically $ newChatClient (tbqSize cfg) srv

newChatClient :: Natural -> SMPServer -> STM ChatClient
newChatClient qSize smpServer = do
  inQ <- newTBQueue qSize
  outQ <- newTBQueue qSize
  return ChatClient {inQ, outQ, smpServer}

receiveFromChatTerm :: ChatClient -> ChatTerminal -> IO ()
receiveFromChatTerm t ct = forever $ do
  atomically (readTBQueue $ inputQ ct)
    >>= processOrError . parseAll chatCommandP . encodeUtf8 . T.pack
  where
    processOrError = \case
      Left err -> writeOutQ . ErrorInput $ B.pack err
      Right ChatHelp -> writeOutQ ChatHelpInfo
      Right MarkdownHelp -> writeOutQ MarkdownInfo
      Right cmd -> atomically $ writeTBQueue (inQ t) cmd
    writeOutQ = atomically . writeTBQueue (outQ t)

sendToChatTerm :: ChatClient -> ChatTerminal -> ChatOpts -> IO ()
sendToChatTerm ChatClient {outQ} ChatTerminal {outputQ} opts = forever $ do
  atomically (readTBQueue outQ) >>= \case
    NoChatResponse -> return ()
    resp -> atomically . writeTBQueue outputQ $ serializeChatResponse opts resp

sendToAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
sendToAgent ChatClient {inQ, smpServer} ct AgentClient {rcvQ} = do
  atomically $ writeTBQueue rcvQ ("1", "", SUBALL) -- hack for subscribing to all
  forever . atomically $ do
    cmd <- readTBQueue inQ
    writeTBQueue rcvQ `mapM_` agentTransmission cmd
    setActiveContact cmd
  where
    setActiveContact :: ChatCommand -> STM ()
    setActiveContact = \case
      SendMessage a _ -> setActive ct a
      DeleteConnection a -> unsetActive ct a
      _ -> pure ()
    agentTransmission :: ChatCommand -> Maybe (ATransmission 'Client)
    agentTransmission = \case
      AddConnection a -> transmission a $ NEW smpServer
      Connect a qInfo -> transmission a $ JOIN qInfo $ ReplyVia smpServer
      DeleteConnection a -> transmission a DEL
      SendMessage a msg -> transmission a $ SEND msg
      ChatHelp -> Nothing
      MarkdownHelp -> Nothing
    transmission :: Contact -> ACommand 'Client -> Maybe (ATransmission 'Client)
    transmission (Contact a) cmd = Just ("1", a, cmd)

receiveFromAgent :: ChatClient -> ChatTerminal -> AgentClient -> IO ()
receiveFromAgent t ct c = forever . atomically $ do
  resp <- chatResponse <$> readTBQueue (sndQ c)
  writeTBQueue (outQ t) resp
  setActiveContact resp
  where
    chatResponse :: ATransmission 'Agent -> ChatResponse
    chatResponse (_, a, resp) = case resp of
      INV qInfo -> Invitation qInfo
      CON -> Connected contact
      END -> Disconnected contact
      MSG {msgBody, msgIntegrity} -> ReceivedMessage contact msgBody msgIntegrity
      SENT _ -> NoChatResponse
      OK -> Confirmation contact
      ERR (CONN e) -> ContactError e contact
      ERR e -> ChatError e
      where
        contact = Contact a
    setActiveContact :: ChatResponse -> STM ()
    setActiveContact = \case
      Connected a -> setActive ct a
      ReceivedMessage a _ _ -> setActive ct a
      Disconnected a -> unsetActive ct a
      _ -> pure ()

setActive :: ChatTerminal -> Contact -> STM ()
setActive ct = writeTVar (activeContact ct) . Just

unsetActive :: ChatTerminal -> Contact -> STM ()
unsetActive ct a = modifyTVar (activeContact ct) unset
  where
    unset a' = if Just a == a' then Nothing else a'
