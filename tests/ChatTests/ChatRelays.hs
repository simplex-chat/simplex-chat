module ChatTests.ChatRelays where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Test.Hspec hiding (it)

chatRelayTests :: SpecWith TestParams
chatRelayTests = do
  describe "configure chat relays" $ do
    fit "get and set chat relays" testGetSetChatRelays

testGetSetChatRelays :: HasCallStack => TestParams -> IO ()
testGetSetChatRelays ps =
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "bob" bobProfile $ \bob -> do
      withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath -> do
        bob ##> "/ad"
        (bobSLink, _cLink) <- getContactLinks bob True

        cath ##> "/ad"
        (cathSLink, _cLink) <- getContactLinks cath True

        -- alice ##> "/_relays 1"

        alice ##> ("/relays " <> bobSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    (" <> bobSLink <> ")")

        alice ##> ("/relays " <> cathSLink)
        alice <## "ok"

        alice ##> "/relays"
        alice <## "Your servers"
        alice <## "  Chat relays"
        alice <## ("    (" <> cathSLink <> ")")

        -- alice ##> ("/relays " <> bobSLink <> " " <> cathSLink)
        -- alice <## "ok"

        -- alice ##> "/relays"
        -- alice <## "Your servers"
        -- alice <## "  Chat relays"
        -- alice <## ("    (" <> bobSLink <> ")")
        -- alice <## ("    (" <> cathSLink <> ")")