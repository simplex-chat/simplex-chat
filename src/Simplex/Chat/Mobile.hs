{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Mobile where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (find)
import Foreign.C.String
import Foreign.StablePtr
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Types
import Simplex.Chat.View

foreign export ccall "chat_init_store" cChatInitStore :: CString -> IO (StablePtr ChatStore)

foreign export ccall "chat_get_user" cChatGetUser :: StablePtr ChatStore -> IO CJSONString

foreign export ccall "chat_create_user" cChatCreateUser :: StablePtr ChatStore -> CJSONString -> IO CJSONString

foreign export ccall "chat_start" cChatStart :: StablePtr ChatStore -> IO (StablePtr ChatController)

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CString

-- | creates or connects to chat store
cChatInitStore :: CString -> IO (StablePtr ChatStore)
cChatInitStore fp = peekCString fp >>= chatInitStore >>= newStablePtr

-- | returns JSON in the form `{"user": <user object>}` or `{}` in case there is no active user (to show dialog to enter displayName/fullName)
cChatGetUser :: StablePtr ChatStore -> IO CJSONString
cChatGetUser cc = deRefStablePtr cc >>= chatGetUser >>= newCString

-- | accepts Profile JSON, returns JSON `{"user": <user object>}` or `{"error": "<error>"}`
cChatCreateUser :: StablePtr ChatStore -> CJSONString -> IO CJSONString
cChatCreateUser cPtr profileCJson = do
  c <- deRefStablePtr cPtr
  p <- peekCString profileCJson
  newCString =<< chatCreateUser c p

-- | this function starts chat - it cannot be started during initialization right now, as it cannot work without user (to be fixed later)
cChatStart :: StablePtr ChatStore -> IO (StablePtr ChatController)
cChatStart st = deRefStablePtr st >>= chatStart >>= newStablePtr

-- | send command to chat (same syntax as in terminal for now)
cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString
cChatSendCmd cPtr cCmd = do
  c <- deRefStablePtr cPtr
  cmd <- peekCString cCmd
  newCString =<< chatSendCmd c cmd

-- | receive message from chat (blocking)
cChatRecvMsg :: StablePtr ChatController -> IO CString
cChatRecvMsg cc = deRefStablePtr cc >>= chatRecvMsg >>= newCString

mobileChatOpts :: ChatOpts
mobileChatOpts =
  ChatOpts
    { dbFilePrefix = "simplex_v1", -- two database files will be created: simplex_v1_chat.db and simplex_v1_agent.db
      smpServers = defaultSMPServers,
      logging = False
    }

type CJSONString = CString

data ChatStore = ChatStore
  { dbFilePrefix :: FilePath,
    chatStore :: SQLiteStore
  }

chatInitStore :: String -> IO ChatStore
chatInitStore dbFilePrefix = do
  let f = chatStoreFile dbFilePrefix
  chatStore <- createStore f $ dbPoolSize defaultChatConfig
  pure ChatStore {dbFilePrefix, chatStore}

getActiveUser_ :: SQLiteStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> getUsers st

-- | returns JSON in the form `{"user": <user object>}` or `{}`
chatGetUser :: ChatStore -> IO JSONString
chatGetUser ChatStore {chatStore} =
  maybe "{}" (jsonObject . ("user" .=)) <$> getActiveUser_ chatStore

-- | returns JSON in the form `{"user": <user object>}` or `{"error": "<error>"}`
chatCreateUser :: ChatStore -> JSONString -> IO JSONString
chatCreateUser ChatStore {chatStore} profileJson =
  case J.eitherDecodeStrict' $ B.pack profileJson of
    Left e -> err e
    Right p ->
      runExceptT (createUser chatStore p True) >>= \case
        Right user -> pure . jsonObject $ "user" .= user
        Left e -> err e
  where
    err e = pure . jsonObject $ "error" .= show e

chatStart :: ChatStore -> IO ChatController
chatStart ChatStore {dbFilePrefix, chatStore} = do
  Just user <- getActiveUser_ chatStore
  cc <- newChatController chatStore user defaultChatConfig mobileChatOpts {dbFilePrefix} . const $ pure ()
  void . forkIO $ runReaderT runChatController cc
  pure cc

chatSendCmd :: ChatController -> String -> IO JSONString
chatSendCmd cc s = crToJSON <$> runReaderT (execChatCommand s) cc

chatRecvMsg :: ChatController -> IO String
chatRecvMsg ChatController {outputQ} = serializeChatResponse . snd <$> atomically (readTBQueue outputQ)

jsonObject :: J.Series -> JSONString
jsonObject = LB.unpack . JE.encodingToLazyByteString . J.pairs

crToJSON :: ChatResponse -> JSONString
crToJSON = \case
  CRUserProfile p -> o "profile" $ J.object ["profile" .= p]
  r -> o "terminal" $ J.object ["response" .= serializeChatResponse r]
  where
    o :: String -> J.Value -> JSONString
    o tp params = jsonObject ("type" .= tp <> "params" .= params)
