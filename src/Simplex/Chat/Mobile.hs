{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Mobile where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (find)
import Data.Maybe (fromMaybe)
import Foreign.C.String
import Foreign.C.Types (CInt (..))
import Foreign.StablePtr
import GHC.Generics (Generic)
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Markdown (ParsedMarkdown (..), parseMaybeMarkdownList)
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Chat.Util (safeDecodeUtf8)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (yesToMigrations))
import Simplex.Messaging.Protocol (CorrId (..))
import System.Timeout (timeout)

foreign export ccall "chat_init" cChatInit :: CString -> IO (StablePtr ChatController)

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CJSONString

foreign export ccall "chat_recv_msg_wait" cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString

foreign export ccall "chat_parse_markdown" cChatParseMarkdown :: CString -> IO CJSONString

-- | initialize chat controller
-- The active user has to be created and the chat has to be started before most commands can be used.
cChatInit :: CString -> IO (StablePtr ChatController)
cChatInit fp = peekCAString fp >>= chatInit >>= newStablePtr

-- | send command to chat (same syntax as in terminal for now)
cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString
cChatSendCmd cPtr cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- peekCAString cCmd
  newCAString =<< chatSendCmd c cmd

-- | receive message from chat (blocking)
cChatRecvMsg :: StablePtr ChatController -> IO CJSONString
cChatRecvMsg cc = deRefStablePtr cc >>= chatRecvMsg >>= newCAString

-- |  receive message from chat (blocking up to `t` microseconds (1/10^6 sec), returns empty string if times out)
cChatRecvMsgWait :: StablePtr ChatController -> CInt -> IO CJSONString
cChatRecvMsgWait cc t = deRefStablePtr cc >>= (`chatRecvMsgWait` fromIntegral t) >>= newCAString

-- | parse markdown - returns ParsedMarkdown type JSON
cChatParseMarkdown :: CString -> IO CJSONString
cChatParseMarkdown s = newCAString . chatParseMarkdown =<< peekCAString s

mobileChatOpts :: ChatOpts
mobileChatOpts =
  ChatOpts
    { dbFilePrefix = undefined,
      smpServers = [],
      logConnections = False,
      logAgent = False,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      maintenance = True
    }

defaultMobileConfig :: ChatConfig
defaultMobileConfig =
  defaultChatConfig
    { yesToMigrations = True,
      agentConfig = (agentConfig defaultChatConfig) {yesToMigrations = True}
    }

type CJSONString = CString

getActiveUser_ :: SQLiteStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> getUsers st

chatInit :: String -> IO ChatController
chatInit dbFilePrefix = do
  let f = chatStoreFile dbFilePrefix
  chatStore <- createStore f (yesToMigrations (defaultMobileConfig :: ChatConfig))
  user_ <- getActiveUser_ chatStore
  newChatController chatStore user_ defaultMobileConfig mobileChatOpts {dbFilePrefix} Nothing

chatSendCmd :: ChatController -> String -> IO JSONString
chatSendCmd cc s = LB.unpack . J.encode . APIResponse Nothing <$> runReaderT (execChatCommand $ B.pack s) cc

chatRecvMsg :: ChatController -> IO JSONString
chatRecvMsg ChatController {outputQ} = json <$> atomically (readTBQueue outputQ)
  where
    json (corr, resp) = LB.unpack $ J.encode APIResponse {corr, resp}

chatRecvMsgWait :: ChatController -> Int -> IO JSONString
chatRecvMsgWait cc time = fromMaybe "" <$> timeout time (chatRecvMsg cc)

chatParseMarkdown :: String -> JSONString
chatParseMarkdown = LB.unpack . J.encode . ParsedMarkdown . parseMaybeMarkdownList . safeDecodeUtf8 . B.pack

data APIResponse = APIResponse {corr :: Maybe CorrId, resp :: ChatResponse}
  deriving (Generic)

instance ToJSON APIResponse where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
