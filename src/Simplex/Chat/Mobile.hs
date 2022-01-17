{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Mobile where

import Control.Concurrent.STM
import Control.Monad.Except
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
import Simplex.Chat.Styled
import Simplex.Chat.Types

-- | prefix to database file path
mobileDBPrefix :: FilePath
mobileDBPrefix = "simplex_v1"

foreign export ccall "chat_init" cChatInit :: IO (StablePtr ChatController)

foreign export ccall "chat_get_user" cChatGetUser :: StablePtr ChatController -> IO CJSONString

foreign export ccall "chat_create_user" cChatCreateUser :: StablePtr ChatController -> CJSONString -> IO CJSONString

foreign export ccall "chat_send_cmd" cChatSendCmd :: StablePtr ChatController -> CString -> IO CJSONString

foreign export ccall "chat_recv_msg" cChatRecvMsg :: StablePtr ChatController -> IO CString

-- | this function returns opaque pointer that needs to be sent to other functions
cChatInit :: IO (StablePtr ChatController)
cChatInit = chatInit >>= newStablePtr

-- | returns JSON in the form `{"user": <user object>}` or `{}` in case there is no active user (to show dialog to enter displayName/fullName)
cChatGetUser :: StablePtr ChatController -> IO CJSONString
cChatGetUser cc = deRefStablePtr cc >>= chatGetUser >>= newCString

-- | accepts Profile JSON, returns JSON `{"user": <user object>}` or `{"error": "<error>"}`
cChatCreateUser :: StablePtr ChatController -> CJSONString -> IO CJSONString
cChatCreateUser cPtr profileCJson = do
  c <- deRefStablePtr cPtr
  p <- peekCString profileCJson
  newCString =<< chatCreateUser c p

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
      smpServers = defaultSMPServers
    }

type CJSONString = CString

type JSONString = String

chatInit :: IO ChatController
chatInit = do
  let f = chatStoreFile mobileDBPrefix
  st <- createStore f $ dbPoolSize defaultChatConfig
  user <- getActiveUser_ st
  newChatController st user defaultChatConfig mobileChatOpts . const $ pure ()

getActiveUser_ :: SQLiteStore -> IO (Maybe User)
getActiveUser_ st = find activeUser <$> getUsers st

-- | returns JSON in the form `{"user": <user object>}` or `{}`
chatGetUser :: ChatController -> IO JSONString
chatGetUser ChatController {currentUser} =
  maybe "{}" (jsonObject . ("user" .=)) <$> readTVarIO currentUser

-- | returns JSON in the form `{"user": <user object>}` or `{"error": "<error>"}`
chatCreateUser :: ChatController -> JSONString -> IO JSONString
chatCreateUser ChatController {chatStore, currentUser} profileJson = do
  case J.eitherDecodeStrict' $ B.pack profileJson of
    Left e -> err e
    Right p ->
      runExceptT (createUser chatStore p True) >>= \case
        Right user -> do
          atomically (writeTVar currentUser $ Just user)
          pure . jsonObject $ "user" .= user
        Left e -> err e
  where
    err e = pure . jsonObject $ "error" .= show e

chatSendCmd :: ChatController -> String -> IO JSONString
chatSendCmd ChatController {inputQ} s = atomically (writeTBQueue inputQ $ InputCommand s) >> pure "{}"

chatRecvMsg :: ChatController -> IO String
chatRecvMsg ChatController {outputQ} = unlines . map unStyle <$> atomically (readTBQueue outputQ)

jsonObject :: J.Series -> JSONString
jsonObject = LB.unpack . JE.encodingToLazyByteString . J.pairs
