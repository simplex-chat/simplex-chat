{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, prepareChannel1Relay)
import ChatTests.Utils
import Control.Concurrent.Async (concurrently_)
import Data.Text (Text)
import qualified Data.Text as T
import NameResolver
import Simplex.Messaging.Names.Record (NameReservedReason (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Test.Hspec hiding (it)

chatNamesTests :: SpecWith TestParams
chatNamesTests = do
  it "connect by resolved name" testConnectByName
  it "connect by name not claimed in link profile is rejected" testConnectByNameNotClaimed
  it "connect by name to a known contact not claimed in profile is rejected" testConnectByNameKnownContactNotClaimed
  it "connect by unregistered name reports the registration" testConnectByNameNotFound
  it "set name not resolving to own address is rejected" testSetNameNotOwnAddress
  it "channel name is not verified just by joining via link" testChannelDomainLinkJoinUnverified
  it "verify channel name, fail on re-point, retain status on refresh" testChannelDomainVerify
  it "connect by channel name" testConnectByChannelName
  it "connect by name resolving to channel (primary) and direct contact" testConnectByNameChannelAndContact
  it "connect by name resolving to direct contact (primary) and channel" testConnectByNameContactAndChannel
  it "connect by name resolving to business (primary) and channel" testConnectByNameBusinessAndChannel
  describe "connection plan: the name lookup answers" $ do
    it "2b. expired, no local chat" testPlanNameExpired
    it "2c. available, no local chat" testPlanNameAvailable
    it "2d. reserved for community" testPlanNameReservedCommunity
    it "2e. reserved for another reason" testPlanNameReservedOther
    it "2f. registered with no usable link" testPlanNameNoValidLink
    it "3a. known chat, nothing actionable" testPlanKnownNameLive
    it "3b. known chat, name expired" testPlanKnownNameExpired
    it "3c. known chat, name moved to a new address" testPlanKnownNameAddressChanged
    it "3d. known chat, name now available" testPlanKnownNameAvailable
    it "4a. own name, live" testPlanOwnNameLive
    it "4c. own name, expired" testPlanOwnNameExpired
    it "4d. own name, now available" testPlanOwnNameAvailable
    it "2h. the request failed" testPlanNameResolverFailed
    it "resolve=never: local hit and miss" testPlanNameResolveNever

testConnectByName :: HasCallStack => TestParams -> IO ()
testConnectByName ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = aliceSimplexName
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceName (contactNameRecord "alice.simplex" (T.pack shortLink))
      alice ##> "/_set domain 1 alice.simplex"
      alice <## "new contact address set"
      bob ##> "/c @alice.simplex"
      bob <## "alice: connection started"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      bob ##> "/i alice"
      bob <## "contact ID: 2"
      bob <## "receiving messages via: localhost"
      bob <## "sending messages via: localhost"
      _ <- getTermLine bob
      bob <## "SimpleX name: @alice.simplex (verified)"
      bob <## "you've shared main profile with this contact"
      bob <## "connection not verified, use /code command to see security code"
      bob <## "quantum resistant end-to-end encryption"
      _ <- getTermLine bob
      pure ()

testConnectByNameNotClaimed :: HasCallStack => TestParams -> IO ()
testConnectByNameNotClaimed ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceName (contactNameRecord "alice.simplex" (T.pack shortLink))
      bob ##> "/c @alice.simplex"
      bob <## "SimpleX name alice.simplex is not included in the connection link's profile"

testConnectByNameKnownContactNotClaimed :: HasCallStack => TestParams -> IO ()
testConnectByNameKnownContactNotClaimed ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      bob ##> ("/c " <> shortLink)
      bob <## "connection request sent!"
      alice <## "bob (Bob) wants to connect to you!"
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      registerName reg aliceName (contactNameRecord "alice.simplex" (T.pack shortLink))
      bob ##> "/c @alice.simplex"
      bob <## "SimpleX name alice.simplex is not included in the connection link's profile"

testConnectByNameNotFound :: HasCallStack => TestParams -> IO ()
testConnectByNameNotFound ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test _alice bob = do
      enableNamesRole bob
      bob ##> "/c @nobody.simplex"
      bob <## "SimpleX name nobody.simplex: nothing to connect to"
      bob <##. "available:"

testSetNameNotOwnAddress :: HasCallStack => TestParams -> IO ()
testSetNameNotOwnAddress ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      bob ##> "/ad"
      (bobShortLink, _) <- getContactLinks bob True
      registerName reg aliceName (contactNameRecord "alice.simplex" (T.pack bobShortLink))
      alice ##> "/ad"
      _ <- getContactLinks alice True
      alice ##> "/_set domain 1 alice.simplex"
      alice <## "SimpleX name alice.simplex has no valid connection link"

-- a self-claimed name is never auto-verified from link data: the claim is not proof of ownership
testChannelDomainLinkJoinUnverified :: HasCallStack => TestParams -> IO ()
testChannelDomainLinkJoinUnverified ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice cath
        registerName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        memberJoinChannel "team" [cath] [alice] shortLink fullLink bob
        -- a link-data refresh must not mark the self-claimed name verified
        bob ##> ("/_connect plan 1 " <> shortLink <> " resolve=allGroups")
        bob <## "group link: known group #team"
        bob <## "use #team <message> to send messages" -- no "SimpleX name" line: status stays unknown
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

testChannelDomainVerify :: HasCallStack => TestParams -> IO ()
testChannelDomainVerify ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (shortLink, fullLink) <- prepareChannel1Relay "team" alice cath
        registerName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        -- setting the name resolved it, so the owner's channel is verified
        alice ##> "/_verify domain #1"
        alice <## "SimpleX name #team verified"
        memberJoinChannel "team" [cath] [alice] shortLink fullLink bob
        bob ##> "/_verify domain #1"
        bob <## "SimpleX name #team verified"
        -- the name is re-pointed to a different link: verification fails
        registerName reg teamName (channelNameRecord "team.simplex" "https://simplex.chat/other")
        bob ##> "/_verify domain #1"
        bob <## "SimpleX name #team not verified: the name does not resolve to the link in the group profile"
        -- a link-data refresh keeps the failed status, not overwritten with verified
        bob ##> ("/_connect plan 1 " <> shortLink <> " resolve=allGroups")
        bob <## "group link: known group #team"
        bob <## "SimpleX name: #team (verification failed)"
        bob <## "use #team <message> to send messages"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

testConnectByChannelName :: HasCallStack => TestParams -> IO ()
testConnectByChannelName ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (shortLink, _) <- prepareChannel1Relay "team" alice cath
        registerName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        bob ##> "/c #team.simplex"
        bob <## "#team: connection started"
        concurrentlyN_
          [ bob
              <### [ "#team: joining the group (connecting to relay cath)...",
                     "#team: you joined the group (connected to relay cath)"
                   ]
          , do
              cath <## "bob (Bob): accepting request to join group #team..."
              cath <## "#team: bob joined the group"
          , alice <### [EndsWith "introduced bob (Bob) in the channel"]
          ]
        bob ##> ("/_connect plan 1 " <> shortLink)
        bob <## "group link: known group #team"
        bob <## "SimpleX name: #team (verified)"
        bob <## "use #team <message> to send messages"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

-- The bare name "team.simplex" resolves to both a channel and a direct contact. The channel is tried
-- first and succeeds (bob has joined #team), so it is the primary (planSimplexName); otherSimplexName
-- is the direct contact @team.simplex, shown as "You can also connect to @team.simplex in direct chat".
testConnectByNameChannelAndContact :: HasCallStack => TestParams -> IO ()
testConnectByNameChannelAndContact ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (channelLink, _) <- prepareChannel1Relay "team" alice cath
        alice ##> "/ad"
        (contactLink, _) <- getContactLinks alice True
        registerName reg teamName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink) (T.pack channelLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        bob ##> "/c #team.simplex"
        bob <## "#team: connection started"
        concurrentlyN_
          [ bob
              <### [ "#team: joining the group (connecting to relay cath)...",
                     "#team: you joined the group (connected to relay cath)"
                   ]
          , do
              cath <## "bob (Bob): accepting request to join group #team..."
              cath <## "#team: bob joined the group"
          , alice <### [EndsWith "introduced bob (Bob) in the channel"]
          ]
        bob ##> "/_connect plan 1 team.simplex"
        bob <## "group link: known group #team"
        bob <## "SimpleX name: #team (verified)"
        bob <## "use #team <message> to send messages"
        bob <## "You can also connect to @team.simplex in direct chat"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

-- The bare name "acme.simplex" resolves to both a channel and a direct contact. The channel is tried
-- first but its group profile does not claim the domain, so the channel side of the plan fails; the
-- plan falls back to the direct contact as primary (planSimplexName) while otherSimplexName is the
-- channel #acme, shown as "You can also join channel #acme". The channel link is a real, fetchable
-- #acme channel, so the failure is the faithful "channel does not claim this domain" case, not a broken link.
testConnectByNameContactAndChannel :: HasCallStack => TestParams -> IO ()
testConnectByNameContactAndChannel ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (channelLink, _) <- prepareChannel1Relay "acme" alice cath
        alice ##> "/ad"
        (contactLink, _) <- getContactLinks alice True
        registerName reg acmeName (contactAndChannelNameRecord "acme.simplex" (T.pack contactLink) (T.pack channelLink))
        alice ##> "/_set domain 1 acme.simplex"
        alice <## "new contact address set"
        bob ##> "/_connect plan 1 acme.simplex"
        bob <## "contact address: ok to connect"
        _ <- getTermLine bob -- contact short link data (JSON, printed in test view)
        bob <## "You can also join channel #acme"
  where
    acmeName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "acme" [])

testConnectByNameBusinessAndChannel :: HasCallStack => TestParams -> IO ()
testConnectByNameBusinessAndChannel ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (channelLink, _) <- prepareChannel1Relay "biz" alice cath
        alice ##> "/ad"
        (contactLink, fullLink) <- getContactLinks alice True
        registerName reg bizName (contactAndChannelNameRecord "biz.simplex" (T.pack contactLink) (T.pack channelLink))
        alice ##> "/auto_accept on business"
        alice <## "auto_accept on, business"
        alice ##> "/_set domain 1 biz.simplex"
        alice <## "new contact address set"
        bob ##> "/_connect plan 1 biz.simplex"
        bob <## "business address: ok to connect"
        contactSLinkData <- getTermLine bob -- contact short link data (JSON, printed in test view)
        bob <## "You can also join channel #biz"
        -- preparing the business by name saves its domain on the group, so it is then found by local name search
        bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> contactLink <> " domain=biz.simplex " <> contactSLinkData)
        bob <## "#alice: group is prepared"
        -- host changes its profile so the handshake's group-profile write fires; it must not wipe the saved domain
        alice ##> "/p alice Alice Biz"
        alice <## "user bio changed to Alice Biz (your 0 contacts are notified)"
        bob ##> "/_connect plan 1 @biz.simplex resolve=never"
        bob <## "business address: known prepared business #alice"
        bob ##> "/_connect group #1"
        bob <## "#alice: connection started"
        alice <## "#bob (Bob): accepting business address request..."
        bob <## "#alice: joining the group..."
        alice <## "#bob: bob_1 joined the group"
        bob <## "#alice: you joined the group"
        -- after fully connecting, the business must still be found by local name search
        bob ##> "/_connect plan 1 @biz.simplex resolve=never"
        bob <## "business address: known business #alice"
        bob <## "use #alice <message> to send messages"
        -- the business's verified domain survives the handshake and is shown in group info
        bob ##> "/i #alice"
        bob <## "group ID: 1"
        bob <## "current members: 2"
        bob <## "SimpleX name: @biz.simplex (verified)"
  where
    bizName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "biz" [])

-- The states the name-lookup canvas draws, one test per row. Each sets up the registry answer and
-- asserts the plan the CLI renders for it; the row numbers are the canvas's.
aliceSimplexName :: SimplexNameInfo
aliceSimplexName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])

-- alice publishes alice.simplex on her address; the registration is left for the caller to change.
withAliceName :: HasCallStack => (NameRegistry -> Text -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
withAliceName test ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (setup reg) ps
  where
    setup reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack shortLink))
      alice ##> "/_set domain 1 alice.simplex"
      alice <## "new contact address set"
      test reg (T.pack shortLink) alice bob

-- bob connects to alice by name, so his contact is found by local name search afterwards.
connectBobByName :: HasCallStack => TestCC -> TestCC -> IO ()
connectBobByName alice bob = do
  bob ##> "/c @alice.simplex"
  bob <## "alice: connection started"
  alice <## "bob (Bob) wants to connect to you!"
  alice <## "to accept: /ac bob"
  alice <## "to reject: /rc bob (the sender will NOT be notified)"
  alice ##> "/ac bob"
  alice <## "bob (Bob): accepting contact request, you can send messages to contact"
  concurrently_
    (bob <## "alice (Alice): contact is connected")
    (alice <## "bob (Bob): contact is connected")

testPlanNameExpired :: HasCallStack => TestParams -> IO ()
testPlanNameExpired = withAliceName $ \reg shortLink _alice bob -> do
  registerExpiredName reg aliceSimplexName (contactNameRecord "alice.simplex" shortLink)
  bob ##> "/_connect plan 1 @alice.simplex"
  bob <## "SimpleX name alice.simplex: nothing to connect to"
  bob <##. "registered, expires "

testPlanNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanNameAvailable = withAliceName $ \reg _l _alice bob -> do
  registerAvailableName reg sunflower 3
  bob ##> "/_connect plan 1 @sunflower.simplex"
  bob <## "SimpleX name sunflower.simplex: nothing to connect to"
  bob <## "available: 1000 cents/year, min length 3"
  where
    sunflower = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "sunflower" [])

testPlanNameReservedCommunity :: HasCallStack => TestParams -> IO ()
testPlanNameReservedCommunity = withAliceName $ \reg _l _alice bob -> do
  registerReservedName reg privacy NRRCommunity
  bob ##> "/_connect plan 1 @privacy.simplex"
  bob <## "SimpleX name privacy.simplex: nothing to connect to"
  bob <## "reserved: community"
  where
    privacy = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "privacy" [])

testPlanNameReservedOther :: HasCallStack => TestParams -> IO ()
testPlanNameReservedOther = withAliceName $ \reg _l _alice bob -> do
  registerReservedName reg acme NRRTrademark
  bob ##> "/_connect plan 1 @acme.simplex"
  bob <## "SimpleX name acme.simplex: nothing to connect to"
  bob <## "reserved: trademark"
  where
    acme = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "acme" [])

testPlanNameNoValidLink :: HasCallStack => TestParams -> IO ()
testPlanNameNoValidLink = withAliceName $ \reg _l _alice bob -> do
  registerName reg boogaloo (emptyNameRecord "boogaloo.simplex")
  bob ##> "/_connect plan 1 @boogaloo.simplex"
  bob <## "SimpleX name boogaloo.simplex: nothing to connect to"
  bob <## "registered"
  where
    boogaloo = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "boogaloo" [])

testPlanKnownNameLive :: HasCallStack => TestParams -> IO ()
testPlanKnownNameLive = withAliceName $ \_reg _l alice bob -> do
  connectBobByName alice bob
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @alice.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  bob <## "registered"

testPlanKnownNameExpired :: HasCallStack => TestParams -> IO ()
testPlanKnownNameExpired = withAliceName $ \reg shortLink alice bob -> do
  connectBobByName alice bob
  registerExpiredName reg aliceSimplexName (contactNameRecord "alice.simplex" shortLink)
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @alice.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  bob <##. "registered, expires "

testPlanKnownNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanKnownNameAvailable = withAliceName $ \reg _l alice bob -> do
  connectBobByName alice bob
  unregisterName reg aliceSimplexName
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @alice.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  bob <## "available: 1000 cents/year, min length 1"

testPlanOwnNameLive :: HasCallStack => TestParams -> IO ()
testPlanOwnNameLive = withAliceName $ \_reg _l alice _bob -> do
  alice ##> "/_connect plan 1 @alice.simplex resolve=all"
  alice <## "contact address: own address"
  alice <## "registered"

testPlanOwnNameExpired :: HasCallStack => TestParams -> IO ()
testPlanOwnNameExpired = withAliceName $ \reg shortLink alice _bob -> do
  registerExpiredName reg aliceSimplexName (contactNameRecord "alice.simplex" shortLink)
  alice ##> "/_connect plan 1 @alice.simplex resolve=all"
  alice <## "contact address: own address"
  alice <##. "registered, expires "

testPlanOwnNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanOwnNameAvailable = withAliceName $ \reg _l alice _bob -> do
  unregisterName reg aliceSimplexName
  alice ##> "/_connect plan 1 @alice.simplex resolve=all"
  alice <## "contact address: own address"
  alice <## "available: 1000 cents/year, min length 1"

testPlanNameResolveNever :: HasCallStack => TestParams -> IO ()
testPlanNameResolveNever = withAliceName $ \_reg _l alice bob -> do
  connectBobByName alice bob
  -- a hit answers from the store, with no registration attached
  bob ##> "/_connect plan 1 @alice.simplex resolve=never"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @alice.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  -- a miss is not resolved online, and is reported as such
  bob ##> "/_connect plan 1 @nobody.simplex resolve=never"
  bob <## "no matching chat found, name resolution is disabled"

-- 3c: bob has a chat found by alice.simplex, then the name is re-pointed at cath's address, which
-- claims it in turn. Only resolve=all re-resolves a name whose chat is known, and the plan that
-- comes back is the new address, marked as changed; bob's existing chat is what resolve=never returns.
testPlanKnownNameAddressChanged :: HasCallStack => TestParams -> IO ()
testPlanKnownNameAddressChanged ps = withSmpServerAndNames $ \reg ->
  testChat3 aliceProfile bobProfile cathProfile (test reg) ps
  where
    test reg alice bob cath = do
      mapM_ enableNamesRole [alice, bob, cath]
      alice ##> "/ad"
      (aliceLink, _) <- getContactLinks alice True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack aliceLink))
      alice ##> "/_set domain 1 alice.simplex"
      alice <## "new contact address set"
      connectBobByName alice bob
      -- the name now leads to cath, who claims it
      cath ##> "/ad"
      (cathLink, _) <- getContactLinks cath True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack cathLink))
      cath ##> "/_set domain 1 alice.simplex"
      cath <## "new contact address set"
      -- resolve=unknown keeps answering from the store, so the move is not noticed
      bob ##> "/_connect plan 1 @alice.simplex"
      bob <## "contact address: known contact alice"
      bob <## "SimpleX name: @alice.simplex (verified)"
      bob <## "use @alice <message> to send messages"
      bob <## "registered"
      -- resolve=all re-resolves it and reports the new address
      bob ##> "/_connect plan 1 @alice.simplex resolve=all"
      bob <## "contact address: ok to connect, address changed"
      -- the chat bob already has is still what a local-only lookup returns
      bob ##> "/_connect plan 1 @alice.simplex resolve=never"
      bob <## "contact address: known contact alice"

-- 2h: the registry could not be asked. As today, this stays an error rather than a plan.
testPlanNameResolverFailed :: HasCallStack => TestParams -> IO ()
testPlanNameResolverFailed = withAliceName $ \reg _l _alice bob -> do
  failNameResolution reg broken
  bob ##> "/_connect plan 1 @broken.simplex"
  bob .<## "smpErr = NAME {nameErr = RESOLVER {resolverErr = \"HTTP 500\"}}}"
  where
    broken = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "broken" [])
