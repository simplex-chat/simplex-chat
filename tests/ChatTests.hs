module ChatTests where

import ChatTests.ChatList
import ChatTests.Direct
import ChatTests.Files
import ChatTests.Groups
import ChatTests.Profiles
import Test.Hspec

chatTests :: SpecWith FilePath
chatTests = do
  describe "direct tests" chatDirectTests
  describe "group tests" chatGroupTests
  describe "file tests" chatFileTests
  describe "profile tests" chatProfileTests
  describe "chat list pagination tests" chatListTests
