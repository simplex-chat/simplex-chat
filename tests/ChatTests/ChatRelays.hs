{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, memberJoinChannel', prepareChannel, prepareChannel', prepareChannel1Relay, setupRelay)
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import ProtocolTests (testGroupProfile)
import Simplex.Chat.Protocol (LinkOwnerSig, MsgChatLink (..), MsgContent (..))
import Simplex.Chat.Types (GroupProfile (..))
import Simplex.Chat.Controller (CorsOrigin (..))
import Simplex.Chat.Web (WebChannelPreview (..), WebMessage (..), extractOrigin, removeStaleFiles, writeCorsConfig)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Util (decodeJSON)
import qualified Data.Set as S
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec hiding (it)

chatRelayTests :: SpecWith TestParams
chatRelayTests = do
  describe "configure chat relays" $ do
    it "get and set chat relays" testGetSetChatRelays
    it "re-add soft-deleted relay by same address" testReAddRelaySameAddress
    it "re-add soft-deleted relay by same name" testReAddRelaySameName
    it "test chat relay" testChatRelayTest
    it "relay profile updated in address" testRelayProfileUpdateInAddress
  describe "relay capabilities" $ do
    it "relay sends webDomain in capabilities" testRelayWebCapabilities
  describe "web preview" $ do
    it "render messages and members" testWebPreviewRender
    it "incremental render adds new messages" testWebPreviewIncremental
    it "edited and deleted messages" testWebPreviewEditedDeleted
    it "reactions in rendered messages" testWebPreviewReactions
    it "non-public group produces no file" testWebPreviewNonPublic
    it "multiple channels produce multiple files" testWebPreviewMultipleChannels
    it "channel deletion removes preview file" testWebPreviewChannelDeleted
    it "removeStaleFiles preserves non-base64url files" testWebPreviewStaleCleanup
    it "generate CORS config" testWebPreviewCors
    it "extractOrigin strips path from URL" testExtractOrigin
  describe "share channel card" $ do
    it "share channel card in direct chat" testShareChannelDirect
    it "share channel card in group" testShareChannelGroup
    it "share channel card in channel" testShareChannelChannel

testGetSetChatRelays :: HasCallStack => TestParams -> IO ()
testGetSetChatRelays ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True

        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True

        alice ##> ("/relays name=bob_relay " <> bobSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    bob_relay: " <> bobSLink)

        alice ##> ("/relays name=cath_relay " <> cathSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    cath_relay: " <> cathSLink)

        alice ##> ("/relays name=bob_relay " <> bobSLink <> " name=cath_relay " <> cathSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice
          <### [ ConsoleString $ "    bob_relay: " <> bobSLink,
                 ConsoleString $ "    cath_relay: " <> cathSLink
               ]

-- Relay used by a channel is soft-deleted (referenced in group_relays).
-- Re-adding with same address should un-delete it.
testReAddRelaySameAddress :: HasCallStack => TestParams -> IO ()
testReAddRelaySameAddress ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True

        -- Configure bob as relay and create channel (creates group_relays reference)
        alice ##> ("/relays name=bob_relay " <> bobSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob

        -- Replace bob_relay with cath_relay (bob_relay is soft-deleted, referenced in group_relays)
        alice ##> ("/relays name=cath_relay " <> cathSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    cath_relay: " <> cathSLink)

        -- Re-add with same address but different name - should succeed (un-deletes soft-deleted row by address)
        alice ##> ("/relays name=bob_relay2 " <> bobSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    bob_relay2: " <> bobSLink)

-- Relay used by a channel is soft-deleted (referenced in group_relays).
-- Re-adding with same name and same address should un-delete it.
testReAddRelaySameName :: HasCallStack => TestParams -> IO ()
testReAddRelaySameName ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True

        -- Configure bob as relay named "my_relay" and create channel
        alice ##> ("/relays name=my_relay " <> bobSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob

        -- Replace with cath_relay (my_relay is soft-deleted)
        alice ##> ("/relays name=cath_relay " <> cathSLink)
        alice <## "ok"

        -- Re-add with same name and same address - should succeed (un-deletes by address match)
        alice ##> ("/relays name=my_relay " <> bobSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    my_relay: " <> bobSLink)

testChatRelayTest :: HasCallStack => TestParams -> IO ()
testChatRelayTest ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        -- Setup: bob (relay) creates address
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True

        -- Setup: cath (normal user) creates address
        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True

        -- Scenario 1: Happy path - test relay address succeeds
        alice ##> ("/relay test " <> bobSLink)
        alice <## "relay test passed, profile: bob (Bob)"

        -- Scenario 2: Non-relay address - cath is not a relay user,
        -- her address has ContactShortLinkData, not RelayAddressLinkData
        alice ##> ("/relay test " <> cathSLink)
        alice <##. "relay test failed at RTSDecodeLink, error: "

        -- Scenario 3: Deleted address - bob deletes his address
        bob ##> "/da"
        bob <## "Your chat address is deleted - accepted contacts will remain connected."
        bob <## "To create a new chat address use /ad"
        alice ##> ("/relay test " <> bobSLink)
        alice <##. "relay test failed at RTSGetLink, error: "

testRelayProfileUpdateInAddress :: HasCallStack => TestParams -> IO ()
testRelayProfileUpdateInAddress ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      bob ##> "/ad"
      (bobSLink, _cLink) <- getContactLinks bob True

      alice ##> ("/relay test " <> bobSLink)
      alice <## "relay test passed, profile: bob (Bob)"

      bob ##> "/p bob2 Bob relay"
      bob <## "user profile is changed to bob2 (Bob relay) (your 0 contacts are notified)"

      threadDelay 100000

      alice ##> ("/relay test " <> bobSLink)
      alice <## "relay test passed, profile: bob2 (Bob relay)"

testShareChannelDirect :: HasCallStack => TestParams -> IO ()
testShareChannelDirect ps =
  testChat3 aliceProfile bobProfile cathProfile test ps
  where
    test alice bob cath = withRelay ps $ \relay -> do
      (shortLink, fullLink) <- prepareChannel1Relay "news" alice relay
      connectUsers alice bob
      -- alice gets ownerSig from share content API (for validation later)
      alice ##> "/_share chat content #1 @2"
      alice <## "link to join channel #news (signed):"
      (_, apiOwnerSig) <- getTermLine2 alice
      -- alice sends the card to bob
      alice ##> "/share chat #news @bob"
      alice <# "@bob link to join channel #news (signed):"
      _ <- getTermLine2 alice -- alice's testView ownerSig
      bob <# "alice> link to join channel #news (signed):"
      -- bob captures the received ownerSig from message view (testView)
      (sLink, cSig) <- getTermLine2 bob
      sLink `shouldBe` shortLink
      cSig `shouldBe` apiOwnerSig
      -- bob verifies owner signature via connect plan
      bob ##> ("/_connect plan 1 " <> shortLink <> " sig=" <> cSig)
      bob <## "group link: ok to connect via relays"
      bob <## "owner signature: verified"
      _ <- getTermLine bob -- group link data
      -- bob joins
      memberJoinChannel' "news" 1 0 1 0 [relay] [alice] shortLink fullLink bob
      connectUsers bob cath
      -- bob (subscriber) shares unsigned - not owner
      bob ##> "/share chat #news @cath"
      bob <# "@cath link to join channel #news:"
      _ <- getTermLine bob
      cath <# "bob> link to join channel #news:"
      _ <- getTermLine cath
      -- bob tries to replay alice's signed card to cath - binding mismatch, sig stripped at receive
      let sig = fromMaybe (error "bad sig") (decodeJSON (T.pack cSig) :: Maybe LinkOwnerSig)
          cLink = either error id $ strDecode (B.pack sLink)
          mc = MCChat (T.pack sLink) (MCLGroup cLink (testGroupProfile {displayName = "news"} :: GroupProfile)) (Just sig)
          cm = "{\"msgContent\":" <> LB.unpack (J.encode mc) <> "}"
      bob ##> ("/_send @3 json [" <> cm <> "]")
      bob <# "@cath link to join group #news (signed):"
      _ <- getTermLine2 bob -- bob's testView ownerSig (his sent has the sig data)
      -- cath sees it without signature - binding was for alice->bob, not bob->cath, sig stripped
      cath <# "bob> link to join group #news:"
      _ <- getTermLine cath
      -- cath joins anyway
      memberJoinChannel "news" [relay] [alice] shortLink fullLink cath
      alice #> "#news hello"
      relay <# "#news> hello"
      [bob, cath] *<# "#news> hello [>>]"

testShareChannelGroup :: HasCallStack => TestParams -> IO ()
testShareChannelGroup ps =
  testChat3 aliceProfile bobProfile cathProfile test ps
  where
    test alice bob cath = withRelay ps $ \relay -> do
      (shortLink, fullLink) <- prepareChannel1Relay "news" alice relay
      createGroup2 "team" alice bob
      alice ##> "/share chat #news #team"
      alice <# "#team link to join channel #news:"
      _ <- getTermLine alice
      bob <# "#team alice> link to join channel #news:"
      sLink <- getTermLine bob
      sLink `shouldBe` shortLink
      memberJoinChannel' "news" 2 0 1 0 [relay] [alice] sLink fullLink bob
      createGroup2 "work" bob cath
      bob ##> "/share chat #news #work"
      bob <# "#work link to join channel #news:"
      _ <- getTermLine bob
      cath <# "#work bob> link to join channel #news:"
      _ <- getTermLine cath
      memberJoinChannel' "news" 2 0 0 0 [relay] [alice] shortLink fullLink cath
      alice #> "#news hello"
      relay <# "#news> hello"
      [bob, cath] *<# "#news> hello [>>]"

testShareChannelChannel :: HasCallStack => TestParams -> IO ()
testShareChannelChannel ps =
  testChat3 aliceProfile bobProfile cathProfile test ps
  where
    test alice bob cath = withRelay ps $ \relay -> do
      relaySLink <- setupRelay alice relay
      (sLink1, fLink1) <- prepareChannel "news" alice relay
      (sLink2, fLink2) <- prepareChannel' 2 "updates" alice relay
      -- bob joins "updates" first (relay doesn't know bob yet, no suffix)
      memberJoinChannel "updates" [relay] [alice] sLink2 fLink2 bob
      -- alice (owner) shares "news" to "updates" - signed
      alice ##> "/_share chat content #1 #2(as_group=on)"
      alice <## "link to join channel #news (signed):"
      (apiLink, apiOwnerSig) <- getTermLine2 alice
      apiLink `shouldBe` sLink1
      alice ##> "/share chat #news #updates"
      alice <# "#updates link to join channel #news (signed):"
      _ <- getTermLine2 alice -- link, ownerSig
      relay <# "#updates> link to join channel #news (signed):"
      _ <- getTermLine2 relay -- link, ownerSig
      bob <# "#updates> link to join channel #news (signed): [>>]"
      (cLink, cSig) <- getTermLine2 bob
      cLink `shouldBe` (sLink1 <> " [>>]")
      cSig `shouldBe` apiOwnerSig
      -- bob verifies alice's signature via connect plan
      bob ##> ("/_connect plan 1 " <> sLink1 <> " sig=" <> cSig)
      bob <## "group link: ok to connect via relays"
      bob <## "owner signature: verified"
      _ <- getTermLine bob -- group link data
      -- bob joins "news" (group #2 for bob, relay knows bob from "updates" so sfx=1)
      memberJoinChannel' "news" 2 1 1 1 [relay] [alice] sLink1 fLink1 bob
      -- bob creates channel "bob_ch" for delivery to cath
      bob ##> ("/relays name=relay " <> relaySLink)
      bob <## "ok"
      (sLink3, fLink3) <- prepareChannel "bob_ch" bob relay
      memberJoinChannel "bob_ch" [relay] [bob] sLink3 fLink3 cath
      -- bob (subscriber) shares "news" to "bob_ch" - unsigned (not owner)
      bob ##> "/share chat #news #bob_ch"
      bob <# "#bob_ch link to join channel #news:"
      _ <- getTermLine bob
      relay <# "#bob_ch> link to join channel #news:"
      _ <- getTermLine relay
      cath <# "#bob_ch> link to join channel #news: [>>]"
      _ <- getTermLine cath
      -- bob tries to replay alice's signed card to bob_ch - binding mismatch, sig stripped at receive
      let sig = fromMaybe (error "bad sig") (decodeJSON (T.pack cSig) :: Maybe LinkOwnerSig)
          cLink' = either error id $ strDecode (B.pack sLink1)
          mc = MCChat (T.pack sLink1) (MCLGroup cLink' (testGroupProfile {displayName = "news"} :: GroupProfile)) (Just sig)
          cm = "{\"msgContent\":" <> LB.unpack (J.encode mc) <> "}"
      bob ##> ("/_send #3 json [" <> cm <> "]")
      bob <# "#bob_ch link to join group #news (signed):"
      _ <- getTermLine2 bob -- bob's testView ownerSig (his sent has the sig data)
      relay <# "#bob_ch bob_2> link to join group #news:"
      _ <- getTermLine relay
      cath <# "#bob_ch bob> link to join group #news: [>>]"
      _ <- getTermLine cath
      -- cath joins "news" (group #2 for cath since "bob_ch" is #1)
      memberJoinChannel' "news" 2 1 0 1 [relay] [alice] sLink1 fLink1 cath
      -- alice sends message, both receive
      alice #> "#news hello"
      relay <# "#news> hello"
      [bob, cath] *<# "#news> hello [>>]"

getTermLine2 :: TestCC -> IO (String, String)
getTermLine2 c = (,) <$> getTermLine c <*> getTermLine c

testRelayWebCapabilities :: HasCallStack => TestParams -> IO ()
testRelayWebCapabilities ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps (relayWebTestOpts "relay.example.com" (tmpPath ps </> "web_cap") Nothing) "bob" bobProfile $ \relay -> do
      rName <- userName relay
      relay ##> "/ad"
      (relaySLink, _cLink) <- getContactLinks relay True
      alice ##> ("/relays name=" <> rName <> " " <> relaySLink)
      alice <## "ok"
      alice ##> "/public group relays=1 #news"
      alice <## "group #news is created"
      alice <## "wait for selected relay(s) to join, then you can invite members via group link"
      concurrentlyN_
        [ do
            alice <## "#news: group link relays updated, current relays:"
            alice <### [EndsWith ": active, web: relay.example.com"]
            alice <## "group link:"
            _ <- getTermLine alice
            pure (),
          relay <## "#news: you joined the group as relay"
        ]

-- Helper: set up relay with web config + channel
withWebChannel :: TestParams -> String -> (TestCC -> TestCC -> FilePath -> IO ()) -> IO ()
withWebChannel ps gName test = do
  let webDir = tmpPath ps </> "web_" <> gName
      corsFile = tmpPath ps </> "cors_" <> gName <> ".conf"
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps (relayWebTestOpts "relay.example.com" webDir (Just corsFile)) "bob" bobProfile $ \relay -> do
      _ <- setupRelay alice relay
      createChannelWithRelayWeb gName alice relay
      test alice relay webDir

createChannelWithRelayWeb :: HasCallStack => String -> TestCC -> TestCC -> IO ()
createChannelWithRelayWeb gName owner relay = do
  owner ##> ("/public group relays=1 #" <> gName)
  owner <## ("group #" <> gName <> " is created")
  owner <## "wait for selected relay(s) to join, then you can invite members via group link"
  concurrentlyN_
    [ do
        owner <## ("#" <> gName <> ": group link relays updated, current relays:")
        owner <### [EndsWith ": active, web: relay.example.com"]
        owner <## "group link:"
        _ <- getTermLine owner
        pure (),
      relay <## ("#" <> gName <> ": you joined the group as relay")
    ]

-- Poll for a JSON preview file written by the worker that satisfies predicate, with timeout
waitPreviewWith :: HasCallStack => FilePath -> (WebChannelPreview -> Bool) -> IO WebChannelPreview
waitPreviewWith webDir check = go 50
  where
    go :: Int -> IO WebChannelPreview
    go 0 = error "waitPreview: timed out waiting for matching JSON file"
    go n = do
      files <- filter (\f -> takeExtension f == ".json") <$> listDirectory webDir
      case files of
        [f] -> do
          jsonBytes <- LB.readFile (webDir </> f)
          case J.eitherDecode jsonBytes of
            Right p | check p -> pure p
            _ -> threadDelay 100000 >> go (n - 1)
        _ -> threadDelay 100000 >> go (n - 1)

waitPreview :: HasCallStack => FilePath -> IO WebChannelPreview
waitPreview webDir = waitPreviewWith webDir (const True)

testWebPreviewRender :: HasCallStack => TestParams -> IO ()
testWebPreviewRender ps =
  withWebChannel ps "news" $ \alice relay webDir -> do
    alice #> "#news hello from the channel"
    relay <# "#news> hello from the channel"
    alice #> "#news second message"
    relay <# "#news> second message"
    wPreview <- waitPreviewWith webDir (\p -> length (messages p) >= 2)
    let GroupProfile {displayName = chName} = channel wPreview
    chName `shouldBe` "news"
    length (messages wPreview) `shouldBe` 2
    content (messages wPreview !! 0) `shouldBe` MCText "hello from the channel"
    content (messages wPreview !! 1) `shouldBe` MCText "second message"
    length (members wPreview) `shouldSatisfy` (>= 1)
    all (\m -> ts m > read "2020-01-01 00:00:00 UTC") (messages wPreview) `shouldBe` True
    jsonFiles <- filter (\f -> takeExtension f == ".json") <$> listDirectory webDir
    length jsonFiles `shouldBe` 1

testWebPreviewIncremental :: HasCallStack => TestParams -> IO ()
testWebPreviewIncremental ps =
  withWebChannel ps "inc" $ \alice relay webDir -> do
    alice #> "#inc first"
    relay <# "#inc> first"
    p1 <- waitPreviewWith webDir (\p -> length (messages p) >= 1)
    length (messages p1) `shouldBe` 1
    content (messages p1 !! 0) `shouldBe` MCText "first"
    alice #> "#inc second"
    relay <# "#inc> second"
    alice #> "#inc third"
    relay <# "#inc> third"
    p2 <- waitPreviewWith webDir (\p -> length (messages p) >= 3)
    length (messages p2) `shouldBe` 3
    content (messages p2 !! 0) `shouldBe` MCText "first"
    content (messages p2 !! 1) `shouldBe` MCText "second"
    content (messages p2 !! 2) `shouldBe` MCText "third"

testWebPreviewEditedDeleted :: HasCallStack => TestParams -> IO ()
testWebPreviewEditedDeleted ps =
  withWebChannel ps "ed" $ \alice relay webDir -> do
    alice #> "#ed msg one"
    relay <# "#ed> msg one"
    alice #> "#ed msg two"
    relay <# "#ed> msg two"
    msgId2 <- lastItemId alice
    alice #> "#ed msg three"
    relay <# "#ed> msg three"
    msgId3 <- lastItemId alice
    alice ##> ("/_update item #1 " <> msgId2 <> " text msg two edited")
    alice <# "#ed [edited] msg two edited"
    relay <# "#ed> [edited] msg two edited"
    alice #$> ("/_delete item #1 " <> msgId3 <> " broadcast", id, "message marked deleted")
    relay <# "#ed> [marked deleted] msg three"
    p <- waitPreviewWith webDir (\p -> length (messages p) == 2 && any edited (messages p))
    length (messages p) `shouldBe` 2
    content (messages p !! 0) `shouldBe` MCText "msg one"
    content (messages p !! 1) `shouldBe` MCText "msg two edited"
    edited (messages p !! 0) `shouldBe` False
    edited (messages p !! 1) `shouldBe` True

testWebPreviewReactions :: HasCallStack => TestParams -> IO ()
testWebPreviewReactions ps =
  withWebChannel ps "react" $ \alice relay webDir -> do
    alice #> "#react hello"
    relay <# "#react> hello"
    alice ##> "+1 #react hello"
    alice <## "added 👍"
    relay <# "#react alice> > hello"
    relay <## "    + 👍"
    p <- waitPreviewWith webDir (\p -> not (null (messages p)) && not (null (reactions (head (messages p)))))
    length (messages p) `shouldBe` 1
    length (reactions (messages p !! 0)) `shouldSatisfy` (>= 1)

testWebPreviewNonPublic :: HasCallStack => TestParams -> IO ()
testWebPreviewNonPublic ps = do
  let webDir = tmpPath ps </> "web_nonpub"
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps (relayWebTestOpts "relay.example.com" webDir Nothing) "bob" bobProfile $ \relay -> do
      _ <- setupRelay alice relay
      alice ##> "/g private"
      alice <## "group #private is created"
      alice <## "to add members use /a private <name> or /create link #private"
      alice #> "#private hello"
      threadDelay 2000000
      files <- filter (\f -> takeExtension f == ".json") <$> listDirectory webDir
      length files `shouldBe` 0

testWebPreviewMultipleChannels :: HasCallStack => TestParams -> IO ()
testWebPreviewMultipleChannels ps = do
  let webDir = tmpPath ps </> "web_multi"
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps (relayWebTestOpts "relay.example.com" webDir Nothing) "bob" bobProfile $ \relay -> do
      _ <- setupRelay alice relay
      createChannelWithRelayWeb "ch1" alice relay
      createChannelWithRelayWeb "ch2" alice relay
      alice #> "#ch1 msg in ch1"
      relay <# "#ch1> msg in ch1"
      alice #> "#ch2 msg in ch2"
      relay <# "#ch2> msg in ch2"
      threadDelay 2000000
      files <- filter (\f -> takeExtension f == ".json") <$> listDirectory webDir
      length files `shouldBe` 2

testWebPreviewChannelDeleted :: HasCallStack => TestParams -> IO ()
testWebPreviewChannelDeleted ps =
  withWebChannel ps "del" $ \alice relay webDir -> do
    alice #> "#del hello"
    relay <# "#del> hello"
    _ <- waitPreviewWith webDir (\p -> not (null (messages p)))
    jsonFiles <- filter (\f -> takeExtension f == ".json") <$> listDirectory webDir
    length jsonFiles `shouldBe` 1
    let previewFile = webDir </> head jsonFiles
    alice ##> "/d #del"
    alice <## "#del: you deleted the group (signed)"
    relay <## "#del: alice deleted the group (signed)"
    relay <## "use /d #del to delete the local copy of the group"
    waitFileDeleted previewFile 50

testWebPreviewStaleCleanup :: HasCallStack => TestParams -> IO ()
testWebPreviewStaleCleanup ps = do
  let webDir = tmpPath ps </> "web_stale_unit"
      activeFile = "abc123.json"
      staleFile = "AAAA_stale.json"
      safeFile = "my.config.json"
  createDirectoryIfMissing True webDir
  writeFile (webDir </> activeFile) "{}"
  writeFile (webDir </> staleFile) "{}"
  writeFile (webDir </> safeFile) "{}"
  removeStaleFiles webDir (S.singleton activeFile)
  doesFileExist (webDir </> staleFile) `shouldReturn` False
  doesFileExist (webDir </> safeFile) `shouldReturn` True
  doesFileExist (webDir </> activeFile) `shouldReturn` True

waitFileDeleted :: HasCallStack => FilePath -> Int -> IO ()
waitFileDeleted _ 0 = error "waitFileDeleted: timed out"
waitFileDeleted path n =
  doesFileExist path >>= \case
    False -> pure ()
    True -> threadDelay 100000 >> waitFileDeleted path (n - 1)

testWebPreviewCors :: HasCallStack => TestParams -> IO ()
testWebPreviewCors ps = do
  let corsFile = tmpPath ps </> "simplex-cors.conf"
      entries =
        [ ("abc123.json", CorsAny),
          ("def456.json", CorsOrigins ["https://owner-site.com"]),
          ("ghi789.json", CorsOrigins [])
        ]
  writeCorsConfig entries corsFile
  corsContent <- readFile corsFile
  corsContent `shouldContain` "/channel/abc123.json \"*\""
  corsContent `shouldContain` "/channel/def456.json \"https://owner-site.com\""
  corsContent `shouldContain` "# ghi789.json (no origin configured)"
  corsContent `shouldContain` "Access-Control-Allow-Origin"
  corsContent `shouldContain` "Access-Control-Allow-Methods"

testExtractOrigin :: HasCallStack => TestParams -> IO ()
testExtractOrigin _ps = do
  extractOrigin "https://owner.example.com/channel.html" `shouldBe` Just "https://owner.example.com"
  extractOrigin "https://owner.example.com/path/to/page?q=1#frag" `shouldBe` Just "https://owner.example.com"
  extractOrigin "https://owner.example.com:8443/page" `shouldBe` Just "https://owner.example.com:8443"
  extractOrigin "https://owner.example.com" `shouldBe` Just "https://owner.example.com"
  extractOrigin "http://localhost:3000/preview" `shouldBe` Just "http://localhost:3000"
  extractOrigin "ftp://example.com/file" `shouldBe` Nothing
  extractOrigin "not-a-url" `shouldBe` Nothing

-- Create a public group with relay=1, wait for relay to join
createChannelWithRelay :: HasCallStack => String -> TestCC -> TestCC -> IO ()
createChannelWithRelay gName owner relay = do
  owner ##> ("/public group relays=1 #" <> gName)
  owner <## ("group #" <> gName <> " is created")
  owner <## "wait for selected relay(s) to join, then you can invite members via group link"
  concurrentlyN_
    [ do
        owner <## ("#" <> gName <> ": group link relays updated, current relays:")
        owner <## "  - relay id 1: active"
        owner <## "group link:"
        _ <- getTermLine owner
        pure (),
      relay <## ("#" <> gName <> ": you joined the group as relay")
    ]
