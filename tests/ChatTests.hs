module ChatTests where

import ChatTests.ChatList
import ChatTests.Direct
import ChatTests.Files
import ChatTests.Forward
import ChatTests.Groups
import ChatTests.Local
import ChatTests.Profiles
import Test.Hspec hiding (it)

chatTests :: SpecWith FilePath
chatTests = do
  describe "direct tests" chatDirectTests
  describe "forward tests" chatForwardTests
  describe "group tests" chatGroupTests
  describe "local chats tests" chatLocalChatsTests
  describe "file tests" chatFileTests
  describe "profile tests" chatProfileTests
  describe "chat list pagination tests" chatListTests
