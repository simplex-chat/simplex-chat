module ChatTests where

import ChatTests.Direct
import ChatTests.Files
import ChatTests.Groups
import ChatTests.Profiles
import Test.Hspec

chatTests :: SpecWith FilePath
chatTests = do
  chatDirectTests
  chatGroupTests
  chatFileTests
  chatProfileTests
