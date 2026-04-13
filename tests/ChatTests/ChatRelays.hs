{-# LANGUAGE OverloadedStrings #-}

module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
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
  describe "public group invitations" $ do
    it "share public group in direct chat" testSharePublicGroupDirect
    it "share public group in group chat" testSharePublicGroupInGroup
    it "share public group twice reuses hidden group" testSharePublicGroupDedup
    it "share public group in channel" testSharePublicGroupInChannel
    it "cannot share non-public group" testShareNonPublicGroupFails

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

testSharePublicGroupDirect :: HasCallStack => TestParams -> IO ()
testSharePublicGroupDirect ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        alice ##> ("/relays name=bob_relay " <> bobSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob
        connectUsers alice cath
        alice ##> "/share #team @cath"
        alice <### [WithTime "@cath invitation to join team (owner verified)", "shared public group #team"]
        cath <# "alice> invitation to join team (verifying...)"
        cath <## "alice (Alice) shared public group #team"
        -- wait for async verification to complete
        threadDelay 2000000
        -- verify owner sig status updated to verified in chat history
        cath ##> "/tail @alice 1"
        cath <# "alice> invitation to join team (owner verified)"

testSharePublicGroupInGroup :: HasCallStack => TestParams -> IO ()
testSharePublicGroupInGroup ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        alice ##> ("/relays name=bob_relay " <> bobSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob
        -- create a regular group with alice and cath
        createGroup2 "friends" alice cath
        alice ##> "/share #team #friends"
        alice <### [WithTime "#friends invitation to join team", "shared public group #team"]
        cath <# "#friends alice> invitation to join team"
        cath <## "#friends: alice shared public group #team"

testSharePublicGroupDedup :: HasCallStack => TestParams -> IO ()
testSharePublicGroupDedup ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        alice ##> ("/relays name=bob_relay " <> bobSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob
        connectUsers alice cath
        -- share twice, second reuses hidden group
        alice ##> "/share #team @cath"
        alice <### [WithTime "@cath invitation to join team (owner verified)", "shared public group #team"]
        cath <# "alice> invitation to join team (verifying...)"
        cath <## "alice (Alice) shared public group #team"
        alice ##> "/share #team @cath"
        alice <### [WithTime "@cath invitation to join team (owner verified)", "shared public group #team"]
        cath <# "alice> invitation to join team (verifying...)"
        cath <## "alice (Alice) shared public group #team"
        threadDelay 1000000

testSharePublicGroupInChannel :: HasCallStack => TestParams -> IO ()
testSharePublicGroupInChannel ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob ->
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True
        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True
        -- create two channels: "team" to share, "news" to share in
        alice ##> ("/relays name=bob_relay " <> bobSLink <> " name=cath_relay " <> cathSLink)
        alice <## "ok"
        createChannelWithRelay "team" alice bob
        -- second channel with cath as relay (relay id 2)
        alice ##> "/public group relays=2 #news"
        alice <## "group #news is created"
        alice <## "wait for selected relay(s) to join, then you can invite members via group link"
        concurrentlyN_
          [ do
              alice <## "#news: group link relays updated, current relays:"
              alice <## "  - relay id 2: active"
              alice <## "group link:"
              _ <- getTermLine alice
              pure (),
            cath <## "#news: you joined the group as relay"
          ]
        -- share team in news channel
        alice ##> "/share #team #news"
        alice <### [WithTime "#news invitation to join team", "shared public group #team"]
        -- cath is relay, forwards but doesn't create hidden group

testShareNonPublicGroupFails :: HasCallStack => TestParams -> IO ()
testShareNonPublicGroupFails =
  testChat2 aliceProfile bobProfile $ \alice bob -> do
    connectUsers alice bob
    alice ##> "/g team"
    alice <## "group #team is created"
    alice <## "to add members use /a team <name> or /create link #team"
    alice ##> "/share #team @bob"
    alice <## "bad chat command: not a public group"

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
