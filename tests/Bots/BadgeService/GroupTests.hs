{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.GroupTests where

import BadgeService.Config (GroupConfig (..))
import BadgeService.Group (GroupAction (..), GroupEvent (..), TrackerAction (..), coalesceTrackerRefreshes, inertGroupConfig, noOwnerHint, orphanHint, trackerDecision)
import BadgeService.Group.Command
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeText, randomBadgeCode)
import Simplex.Chat.Controller (ChatCommand (DeleteGroup, MemberRole))
import Simplex.Chat.Library.Commands (parseChatCommand)
import Simplex.Chat.Types (GroupProfile (..))
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Util (tshow)
import Test.Hspec

badgeGroupTests :: Spec
badgeGroupTests = describe "badge group" $ do
  let p = groupCmdAction GROwner
      issueUsage = ReplyText "use: /issue <type> [months <M>] [uses <N>]"
      bulkUsage = ReplyText "use: /bulk <type> [months <M>] count <B>"
      revokeUsage = ReplyText "use: /revoke <code>"
  it "issue defaults" $ p "/issue supporter" `shouldBe` RunCmd (GCIssue BTSupporter 1 1)
  it "issue with months and uses" $ p "/issue legend months 6 uses 50" `shouldBe` RunCmd (GCIssue BTLegend 6 50)
  it "bulk with count" $ p "/bulk supporter count 20" `shouldBe` RunCmd (GCBulk BTSupporter 1 20)
  it "rejects values past the upper bounds" $ do
    p "/issue supporter uses 1001" `shouldBe` issueUsage
    p "/bulk supporter count 101" `shouldBe` bulkUsage
    p "/issue supporter months 256" `shouldBe` issueUsage
  it "rejects zero values" $ do
    p "/bulk supporter count 0" `shouldBe` bulkUsage
    p "/issue supporter months 0" `shouldBe` issueUsage
  it "rejects values past the machine word" $ do
    let past64 = tshow (2 ^ (64 :: Int) + 1 :: Integer)
    p ("/issue supporter uses " <> past64) `shouldBe` issueUsage
    p ("/bulk supporter count " <> past64) `shouldBe` bulkUsage
    p ("/issue supporter months " <> past64) `shouldBe` issueUsage
  it "accepts the upper bounds" $ do
    p "/issue supporter uses 1000" `shouldBe` RunCmd (GCIssue BTSupporter 1 1000)
    p "/bulk supporter count 100" `shouldBe` RunCmd (GCBulk BTSupporter 1 100)
    p "/issue supporter months 255" `shouldBe` RunCmd (GCIssue BTSupporter 255 1)
  it "revoke" $ do
    code <- newCode
    p ("/revoke " <> badgeCodeText code) `shouldBe` RunCmd (GCRevoke code)
  it "authorization, per role, as (issue, bulk, revoke)" $ do
    someCode <- newCode
    let runs role t = case groupCmdAction role t of
          RunCmd _ -> True
          _ -> False
        allowed role =
          ( runs role "/issue supporter",
            runs role "/bulk supporter count 1",
            runs role ("/revoke " <> badgeCodeText someCode)
          )
        roles = [GRUnknown "future", GRRelay, GRObserver, GRAuthor, GRMember, GRModerator, GRAdmin, GROwner]
    map (\role -> (role, allowed role)) roles
      `shouldBe` [ (GRUnknown "future", (False, False, False)),
                   (GRRelay, (False, False, False)),
                   (GRObserver, (False, False, False)),
                   (GRAuthor, (False, False, False)),
                   (GRMember, (False, False, False)),
                   (GRModerator, (True, True, False)),
                   (GRAdmin, (True, True, True)),
                   (GROwner, (True, True, True))
                 ]
  describe "log hints give commands --run-cli parses for a group name with a space" $ do
    let hintCommand = fst . T.breakOn " in --run-cli" . snd . T.breakOnEnd " with "
        parsedHint hint = let cmd = hintCommand hint in (cmd, parseChatCommand (encodeUtf8 cmd))
    it "owner" $
      case parsedHint (T.replace "<member>" "alice" $ noOwnerHint "SimpleX Badges_1") of
        (_, Right (MemberRole g m GROwner)) -> (g, m) `shouldBe` ("SimpleX Badges_1", "alice")
        (cmd, r) -> expectationFailure $ "owner command " <> show cmd <> " parsed as " <> show r
    it "orphan group" $
      case parsedHint (orphanHint "SimpleX Badges_1") of
        (_, Right (DeleteGroup g)) -> g `shouldBe` "SimpleX Badges_1"
        (cmd, r) -> expectationFailure $ "delete command " <> show cmd <> " parsed as " <> show r
  describe "classifying a group message" $ do
    it "answers an advertised command that does not parse" $ do
      map p
        [ "/issue",
          "/issue supporter uses 0",
          "/issue supporter 3",
          "/issue suporter",
          "/issue supporter  uses 5",
          "/bulk supporter",
          "/revoke",
          "/revoke not-a-badge-code"
        ]
        `shouldBe` replicate 5 issueUsage <> [bulkUsage, revokeUsage, revokeUsage]
    it "answers a mistyped command only to a sender who may run it" $ do
      map (`groupCmdAction` "/issue supporter uses 0") [GRMember, GRModerator, GRAdmin]
        `shouldBe` [IgnoreMsg, issueUsage, issueUsage]
      map (`groupCmdAction` "/bulk supporter") [GRMember, GRModerator, GRAdmin]
        `shouldBe` [IgnoreMsg, bulkUsage, bulkUsage]
      map (`groupCmdAction` "/revoke not-a-badge-code") [GRMember, GRModerator, GRAdmin]
        `shouldBe` [IgnoreMsg, IgnoreMsg, revokeUsage]
    it "accepts a command padded with whitespace" $ do
      p " /issue supporter " `shouldBe` RunCmd (GCIssue BTSupporter 1 1)
      p "\t/bulk supporter count 1" `shouldBe` RunCmd (GCBulk BTSupporter 1 1)
    it "says nothing to anything that is not an advertised command" $
      map p ["", "hello", "issue supporter", "/issued supporter", "/help", "see /issue above"]
        `shouldBe` replicate 6 IgnoreMsg
    it "says nothing to a command the sender may not run" $ do
      map (`groupCmdAction` "/issue supporter") [GRMember, GRAuthor] `shouldBe` [IgnoreMsg, IgnoreMsg]
      groupCmdAction GRMember "/bulk supporter count 1" `shouldBe` IgnoreMsg
    it "tells a sender below admin that their revoke did not happen" $ do
      code <- newCode
      map (`groupCmdAction` ("/revoke " <> badgeCodeText code)) [GRMember, GRModerator]
        `shouldBe` replicate 2 (ReplyText "only admins can revoke codes, and this code is now visible to the group - ask an admin to revoke it")
  describe "configured group name and description" $ do
    let cfg name descr = GroupConfig {gDisplayName = name, gDescription = descr}
        profile name descr =
          GroupProfile
            { displayName = name,
              fullName = "",
              shortDescr = Nothing,
              description = descr,
              image = Nothing,
              publicGroup = Nothing,
              groupPreferences = Nothing,
              memberAdmission = Nothing
            }
    it "says nothing when the config matches the group profile" $ do
      inertGroupConfig (cfg "SimpleX Badges" (Just "badge ops desk")) (profile "SimpleX Badges" (Just "badge ops desk"))
        `shouldBe` Nothing
      inertGroupConfig (cfg "SimpleX Badges" Nothing) (profile "SimpleX Badges" Nothing) `shouldBe` Nothing
    it "says nothing when an omitted description meets an empty one" $
      inertGroupConfig (cfg "SimpleX Badges" Nothing) (profile "SimpleX Badges" (Just "")) `shouldBe` Nothing
    it "reports a name the config would change" $
      inertGroupConfig (cfg "SimpleX Badges 2026" (Just "badge ops desk")) (profile "SimpleX Badges" (Just "badge ops desk"))
        `shouldBe` Just "badge group config is not applied to an existing group: display_name \"SimpleX Badges 2026\", group has \"SimpleX Badges\""
    it "shows a non-ASCII name as written" $
      inertGroupConfig (cfg "Значки" Nothing) (profile "SimpleX Badges" Nothing)
        `shouldBe` Just "badge group config is not applied to an existing group: display_name \"Значки\", group has \"SimpleX Badges\""
    it "reports a description the config would change or remove" $ do
      inertGroupConfig (cfg "SimpleX Badges" (Just "new desk")) (profile "SimpleX Badges" (Just "badge ops desk"))
        `shouldBe` Just "badge group config is not applied to an existing group: description \"new desk\", group has \"badge ops desk\""
      inertGroupConfig (cfg "SimpleX Badges" Nothing) (profile "SimpleX Badges" (Just "badge ops desk"))
        `shouldBe` Just "badge group config is not applied to an existing group: description \"\", group has \"badge ops desk\""
    it "reports both fields when both would change" $
      inertGroupConfig (cfg "SimpleX Badges 2026" (Just "new desk")) (profile "SimpleX Badges" Nothing)
        `shouldBe` Just
          "badge group config is not applied to an existing group: \
          \display_name \"SimpleX Badges 2026\", group has \"SimpleX Badges\"; \
          \description \"new desk\", group has \"\""
  describe "tracker" $
    it "edits within 24h, reposts after" $ do
      let t0 = UTCTime (fromGregorian 2026 1 1) 0
          within = addUTCTime (23 * 3600) t0
          past = addUTCTime (25 * 3600) t0
      trackerDecision within t0 `shouldBe` Edit
      trackerDecision past t0 `shouldBe` Repost
  describe "coalescing tracker refreshes" $ do
    it "keeps one refresh per code, carrying the highest claim" $ do
      code <- newCode
      coalesceTrackerRefreshes [GETracker 1 code 1, GETracker 1 code 3, GETracker 2 code 1]
        `shouldBe` [GETracker 1 code 3, GETracker 2 code 1]
    it "keeps the highest claim whichever order it queued in" $ do
      code <- newCode
      coalesceTrackerRefreshes [GETracker 1 code 3, GETracker 1 code 1] `shouldBe` [GETracker 1 code 3]
    it "keeps every other event, in order" $ do
      code <- newCode
      coalesceTrackerRefreshes
        [ GEInGroup 7 (GACommand GRAdmin "a"),
          GETracker 1 code 1,
          GEInGroup 7 GAJoined,
          GETracker 1 code 2
        ]
        `shouldBe` [GEInGroup 7 (GACommand GRAdmin "a"), GEInGroup 7 GAJoined, GETracker 1 code 2]

newCode :: IO BadgeCode
newCode = C.newRandom >>= randomBadgeCode
