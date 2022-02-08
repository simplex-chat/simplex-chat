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
noActiveUser = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"noActiveUser\":{}}}}}}}"

activeUserExists :: String
activeUserExists = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"activeUserExists\":{}}}}}}}"

activeUser :: String
activeUser = "{\"resp\":{\"activeUser\":{\"user\":{\"userId\":1,\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"displayName\":\"alice\",\"fullName\":\"Alice\"},\"activeUser\":true}}}}"

testChatApiNoUser :: IO ()
testChatApiNoUser = withTmpFiles $ do
  cc <- chatInit testDBPrefix
  chatSendCmd cc "/u" `shouldReturn` noActiveUser
  chatSendCmd cc "/_start" `shouldReturn` noActiveUser
  chatSendCmd cc "/u alice Alice" `shouldReturn` activeUser
  chatSendCmd cc "/_start" `shouldReturn` "{\"resp\":{\"chatStarted\":{}}}"

testChatApi :: IO ()
testChatApi = withTmpFiles $ do
  let f = chatStoreFile testDBPrefix
  st <- createStore f 1 True
  Right _ <- runExceptT $ createUser st aliceProfile True
  cc <- chatInit testDBPrefix
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/u alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` "{\"resp\":{\"chatStarted\":{}}}"
