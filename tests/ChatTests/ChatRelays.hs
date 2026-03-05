module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Test.Hspec hiding (it)

chatRelayTests :: SpecWith TestParams
chatRelayTests = do
  describe "configure chat relays" $ do
    it "get and set chat relays" testGetSetChatRelays
    it "re-add soft-deleted relay by same address" testReAddRelaySameAddress
    it "re-add soft-deleted relay by same name" testReAddRelaySameName

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
