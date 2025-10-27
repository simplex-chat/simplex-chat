module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Test.Hspec hiding (it)

chatRelayTests :: SpecWith TestParams
chatRelayTests = do
  describe "configure chat relays" $ do
    it "get and set chat relays" testGetSetChatRelays

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
