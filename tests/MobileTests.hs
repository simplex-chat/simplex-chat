{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module MobileTests where

import ChatClient
import ChatTests
import Control.Monad.Except
import Simplex.Chat.Mobile
import Simplex.Chat.Store
import Test.Hspec

mobileTests :: Spec
mobileTests = do
  describe "mobile API" $ do
    it "start new chat without user" testChatApiNoUser
    it "start new chat with existing user" testChatApi

noActiveUser :: String
#if defined(darwin_HOST_OS)
noActiveUser = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"noActiveUser\":{}}}}}}}"
#else
noActiveUser = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}}"
#endif

activeUserExists :: String
#if defined(darwin_HOST_OS)
activeUserExists = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"activeUserExists\":{}}}}}}}"
#else
activeUserExists = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"activeUserExists\"}}}}"
#endif

activeUser :: String
#if defined(darwin_HOST_OS)
activeUser = "{\"resp\":{\"activeUser\":{\"user\":{\"userId\":1,\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"displayName\":\"alice\",\"fullName\":\"Alice\"},\"activeUser\":true}}}}"
#else
activeUser = "{\"resp\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"displayName\":\"alice\",\"fullName\":\"Alice\"},\"activeUser\":true}}}"
#endif

chatStarted :: String
#if defined(darwin_HOST_OS)
chatStarted = "{\"resp\":{\"chatStarted\":{}}}"
#else
chatStarted = "{\"resp\":{\"type\":\"chatStarted\"}}"
#endif

testChatApiNoUser :: IO ()
testChatApiNoUser = withTmpFiles $ do
  cc <- chatInit testDBPrefix
  chatSendCmd cc "/u" `shouldReturn` noActiveUser
  print activeUser
  chatSendCmd cc "/_start" `shouldReturn` noActiveUser
  chatSendCmd cc "/u alice Alice" `shouldReturn` activeUser
  chatSendCmd cc "/_start" `shouldReturn` chatStarted

testChatApi :: IO ()
testChatApi = withTmpFiles $ do
  let f = chatStoreFile testDBPrefix
  st <- createStore f 1 True
  Right _ <- runExceptT $ createUser st aliceProfile True
  cc <- chatInit testDBPrefix
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/u alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` chatStarted
