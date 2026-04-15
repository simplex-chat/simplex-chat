module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, memberJoinChannel', prepareChannel, prepareChannel', prepareChannel1Relay, setupRelay)
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Test.Hspec hiding (it)

chatRelayTests :: SpecWith TestParams
chatRelayTests = do
  describe "configure chat relays" $ do
    it "get and set chat relays" testGetSetChatRelays
    it "re-add soft-deleted relay by same address" testReAddRelaySameAddress
    it "re-add soft-deleted relay by same name" testReAddRelaySameName
    it "test chat relay" testChatRelayTest
    it "relay profile updated in address" testRelayProfileUpdateInAddress
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
      alice ##> "/share chat #news @bob"
      alice <# "@bob channel #news (signed)"
      bob <# "alice> channel #news (signed)"
      memberJoinChannel' "news" 1 0 1 0 [relay] [alice] shortLink fullLink bob
      connectUsers bob cath
      bob ##> "/share chat #news @cath"
      bob <# "@cath channel #news (signed)"
      cath <# "bob> channel #news (signed)"
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
      alice <# "#team channel #news"
      bob <# "#team alice> channel #news"
      memberJoinChannel' "news" 2 0 1 0 [relay] [alice] shortLink fullLink bob
      createGroup2 "work" bob cath
      bob ##> "/share chat #news #work"
      bob <# "#work channel #news"
      cath <# "#work bob> channel #news"
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
      alice ##> "/share chat #news #updates"
      alice <# "#updates channel #news (signed)"
      relay <# "#updates alice_1> channel #news (signed)"
      bob <# "#updates alice> channel #news (signed) [>>]"
      -- bob joins "news" (group #2 for bob, relay knows bob from "updates" so sfx=1)
      memberJoinChannel' "news" 2 1 1 1 [relay] [alice] sLink1 fLink1 bob
      -- bob creates channel "bob_ch" for delivery to cath
      bob ##> ("/relays name=relay " <> relaySLink)
      bob <## "ok"
      (sLink3, fLink3) <- prepareChannel "bob_ch" bob relay
      memberJoinChannel "bob_ch" [relay] [bob] sLink3 fLink3 cath
      -- bob (subscriber) shares "news" to "bob_ch" - signed (not verifiable as owner)
      bob ##> "/share chat #news #bob_ch"
      bob <# "#bob_ch channel #news (signed)"
      relay <# "#bob_ch bob_2> channel #news (signed)"
      cath <# "#bob_ch bob> channel #news (signed) [>>]"
      -- cath joins "news" (group #2 for cath since "bob_ch" is #1)
      memberJoinChannel' "news" 2 1 0 1 [relay] [alice] sLink1 fLink1 cath
      -- alice sends message, both receive
      alice #> "#news hello"
      relay <# "#news> hello"
      [bob, cath] *<# "#news> hello [>>]"

withRelay :: HasCallStack => TestParams -> (TestCC -> IO ()) -> IO ()
withRelay ps = withNewTestChatOpts ps relayTestOpts "relay" relayProfile

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
