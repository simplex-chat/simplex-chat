{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesCodesTests (namesCodesTests) where

import Control.Monad (forM_)
import Simplex.Chat.Badges.Code (NameCode (..), NamePrepaidCode, formatNameCode, randomNamePrepaidCode)
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import qualified Simplex.Messaging.Crypto as C
import StoreService.Group.Command (CmdAction (..), GroupCmd (..), groupCmdAction)
import Test.Hspec

namesCodesTests :: Spec
namesCodesTests = describe "StoreService names: prepaid codes" $ do
  describe "group commands" $ do
    it "parses /issue name and /bulk name with the length, years and count" testGroupNameCommands
    it "answers the usage to a bad length, years or count, or a badge option" testGroupNameCommandsRefused
    it "parses /revoke of a prepaid code, telling a moderator it was not revoked" testGroupRevokeNameCommand

newCode :: Int -> IO NamePrepaidCode
newCode len = C.newRandom >>= (`randomNamePrepaidCode` len)

testGroupNameCommands :: IO ()
testGroupNameCommands = do
  let p = groupCmdAction GRModerator
  p "/issue name 8" `shouldBe` RunCmd (GCIssueName 8 2)
  p "/issue name 6 years 5" `shouldBe` RunCmd (GCIssueName 6 5)
  p "/issue name 63 years 10" `shouldBe` RunCmd (GCIssueName 63 10)
  p "/bulk name 7 count 3" `shouldBe` RunCmd (GCBulkName 7 2 3)
  p "/bulk name 12 years 3 count 100" `shouldBe` RunCmd (GCBulkName 12 3 100)
  p "/bulk name 6 count 100" `shouldBe` RunCmd (GCBulkName 6 2 100)
  groupCmdAction GRMember "/issue name 8" `shouldBe` IgnoreMsg

testGroupRevokeNameCommand :: IO ()
testGroupRevokeNameCommand = do
  code <- newCode 8
  let cmd = "/revoke " <> formatNameCode (NCPrepaid code)
  groupCmdAction GRAdmin cmd `shouldBe` RunCmd (GCRevokeName code)
  groupCmdAction GRModerator cmd `shouldBe` ReplyText "Only admins can revoke codes. This code is now visible to all members."
  groupCmdAction GRAdmin (cmd <> " junk") `shouldBe` ReplyText "Usage: /revoke <code>"

testGroupNameCommandsRefused :: IO ()
testGroupNameCommandsRefused = do
  let p = groupCmdAction GRModerator
      issueUsage = ReplyText "Usage: /issue <kind> [<length>] [years <Y> | months <M>] [uses <N>]\nname <length>: length 6–63, years 2–10\nsupporter, legend, investor: months 1–255, uses 1–1000\ne.g. /issue name 6 years 5 · /issue supporter months 12"
      bulkUsage = ReplyText "Usage: /bulk <kind> [<length>] [years <Y> | months <M>] count <N>\nname <length>: length 6–63, years 2–10, count 1–100\nsupporter, legend, investor: months 1–255, count 1–100\ne.g. /bulk name 6 count 100 · /bulk legend months 3 count 10"
  forM_ ["name", "name 5", "name 64", "name 06", "name 8 years 1", "name 8 years 0", "name 8 years 11", "name 8 uses 2", "name 8 months 2", "name 8 paid", "name 8 years 18446744073709551618", "name 8 count 2", "name 8 count 1"] $ \args ->
    (args, p ("/issue " <> args)) `shouldBe` (args, issueUsage)
  forM_ ["name 8", "name 8 count 0", "name 8 count 101", "name 8 years 1 count 2", "name 5 count 2", "name 08 count 2", "name 8 months 2 count 2", "name 8 count 2 uses 2", "name 8 uses 2 count 2", "name 8 count 2 years 5", "name count 2"] $ \args ->
    (args, p ("/bulk " <> args)) `shouldBe` (args, bulkUsage)
