{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, prepareChannel1Relay)
import ChatTests.Utils
import Control.Concurrent.Async (concurrently_)
import qualified Data.Text as T
import NameResolver
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Test.Hspec hiding (it)

chatNamesTests :: SpecWith TestParams
chatNamesTests = do
  it "connect by resolved name" testConnectByName
  it "connect by name not claimed in link profile is rejected" testConnectByNameNotClaimed
  it "connect by name to a known contact not claimed in profile is rejected" testConnectByNameKnownContactNotClaimed
  it "connect by unregistered name fails to resolve" testConnectByNameNotFound
  it "set name not resolving to own address is rejected" testSetNameNotOwnAddress
  it "channel name is not verified just by joining via link" testChannelDomainLinkJoinUnverified
  it "verify channel name, fail on re-point, retain status on refresh" testChannelDomainVerify
  it "connect by channel name" testConnectByChannelName
  it "connect by name resolving to channel (primary) and direct contact" testConnectByNameChannelAndContact
  it "connect by name resolving to direct contact (primary) and channel" testConnectByNameContactAndChannel
  it "connect by name resolving to business (primary) and channel" testConnectByNameBusinessAndChannel
  it "gift a name to a contact by name, using their published meta-address" testGiftByContactName
  it "gift to a raw address is found by scanning, with no message" testGiftByMetaAddress
  it "accept a gifted name, and it appears in your names" testAcceptGiftedName
  it "decline a gifted name, and it is gone" testDeclineGiftedName
  it "import a recovery key on a fresh profile and find the bought name" testImportRecoveryKeyFindsNames
  it "buying a name already taken fails cleanly" testBuyTakenNameFails

testConnectByName :: HasCallStack => TestParams -> IO ()
testConnectByName ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceName (contactNameRecord "alice" (T.pack shortLink))
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
      registerName reg aliceName (contactNameRecord "alice" (T.pack shortLink))
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
      registerName reg aliceName (contactNameRecord "alice" (T.pack shortLink))
      bob ##> "/c @alice.simplex"
      bob <## "SimpleX name alice.simplex is not included in the connection link's profile"

testConnectByNameNotFound :: HasCallStack => TestParams -> IO ()
testConnectByNameNotFound ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test _alice bob = do
      enableNamesRole bob
      bob ##> "/c @nobody.simplex"
      bob .<## "smpErr = NAME {nameErr = NOT_FOUND}}"

testSetNameNotOwnAddress :: HasCallStack => TestParams -> IO ()
testSetNameNotOwnAddress ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      bob ##> "/ad"
      (bobShortLink, _) <- getContactLinks bob True
      registerName reg aliceName (contactNameRecord "alice" (T.pack bobShortLink))
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
        registerName reg teamName (channelNameRecord "team" (T.pack shortLink))
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
        registerName reg teamName (channelNameRecord "team" (T.pack shortLink))
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
        registerName reg teamName (channelNameRecord "team" "https://simplex.chat/other")
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
        registerName reg teamName (channelNameRecord "team" (T.pack shortLink))
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
        registerName reg teamName (contactAndChannelNameRecord "team" (T.pack contactLink) (T.pack channelLink))
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
        registerName reg acmeName (contactAndChannelNameRecord "acme" (T.pack contactLink) (T.pack channelLink))
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
        registerName reg bizName (contactAndChannelNameRecord "biz" (T.pack contactLink) (T.pack channelLink))
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

-- | The point of putting the meta-address in the profile: the sender pastes
-- nothing and no handshake happens beyond the connection they already have.
--
-- Bob publishes his meta-address, it travels to Alice with his profile, and she
-- gifts him a name by contact name alone. He then finds it by scanning the
-- announcement, having received no message about it at all.
testGiftByContactName :: HasCallStack => TestParams -> IO ()
testGiftByContactName ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice bob = do
      -- publishing is what /names address does; it updates the profile
      -- publishing puts the meta-address in bob's profile, which then travels
      -- to alice with it when they connect - nothing extra is exchanged
      bob ##> "/names address"
      bob <##. "name address"
      bob <##. "meta-address ("
      connectUsers alice bob
      alice ##> "/names buy alicechat"
      alice <## "registered alicechat.simplex"
      alice <##. "tx "
      alice ##> "/names gift alicechat @bob"
      alice <## "gift alicechat.simplex done"
      alice <##. "tx "
      -- bob is told, and his client records the destination from the message
      bob <# "alice> You were given the SimpleX name alicechat.simplex"
      -- The transfer message carries the ephemeral key, so bob's client records
      -- the destination on arrival. No rescan.
      bob ##> "/names incoming"
      bob <## "names sent to you, not yet accepted:"
      bob <##. "  alicechat.simplex at 0x"
      bob <## ""
      bob <## "accepting links this profile to the name on chain; declining leaves no trace"

-- | The other half of discovery: a gift to a raw address sends no message, so
-- the recipient finds it only by scanning the chain announcement. This is the
-- path a restored device takes, where there is no message to read.
testGiftByMetaAddress :: HasCallStack => TestParams -> IO ()
testGiftByMetaAddress ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice bob = do
      bob ##> "/names address"
      bob <##. "name address"
      metaLine <- getTermLine bob
      let meta = reverse . takeWhile (/= ' ') . reverse $ metaLine
      alice ##> "/names buy scanchat"
      alice <## "registered scanchat.simplex"
      alice <##. "tx "
      alice ##> ("/names gift scanchat " <> meta)
      alice <## "gift scanchat.simplex done"
      alice <##. "tx "
      -- No contact, so no message: bob has been told nothing at all.
      bob ##> "/names incoming"
      bob <## "no names have been sent to you"
      bob ##> "/names rescan"
      bob <## "found 1 name(s) sent to you - see /names incoming"
      bob ##> "/names incoming"
      bob <## "names sent to you, not yet accepted:"
      bob <##. "  scanchat.simplex at 0x"
      bob <## ""
      bob <## "accepting links this profile to the name on chain; declining leaves no trace"

-- | Accepting through the client: bob is gifted a name, accepts it, and it then
-- shows up under his own names. The gift e2e tests stop at discovery; this
-- covers the other half.
testAcceptGiftedName :: HasCallStack => TestParams -> IO ()
testAcceptGiftedName ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice bob = do
      bob ##> "/names address"
      bob <##. "name address"
      bob <##. "meta-address ("
      connectUsers alice bob
      alice ##> "/names buy acceptme"
      alice <## "registered acceptme.simplex"
      alice <##. "tx "
      alice ##> "/names gift acceptme @bob"
      alice <## "gift acceptme.simplex done"
      alice <##. "tx "
      bob <# "alice> You were given the SimpleX name acceptme.simplex"
      bob ##> "/names incoming"
      bob <## "names sent to you, not yet accepted:"
      addrLine <- getTermLine bob
      bob <## ""
      bob <## "accepting links this profile to the name on chain; declining leaves no trace"
      let addr = reverse . takeWhile (/= ' ') . reverse $ addrLine
      bob ##> ("/names accept " <> addr)
      bob <##. "accepted acceptme.simplex at 0x"
      -- now it is one of bob's names
      bob ##> "/names list"
      bob <## "your names:"
      bob <##. "  acceptme.simplex (expires "

-- | Declining through the client: the name leaves the incoming list and never
-- becomes one of bob's names. Declining writes nothing on chain.
testDeclineGiftedName :: HasCallStack => TestParams -> IO ()
testDeclineGiftedName ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice bob = do
      bob ##> "/names address"
      bob <##. "name address"
      bob <##. "meta-address ("
      connectUsers alice bob
      alice ##> "/names buy declineme"
      alice <## "registered declineme.simplex"
      alice <##. "tx "
      alice ##> "/names gift declineme @bob"
      alice <## "gift declineme.simplex done"
      alice <##. "tx "
      bob <# "alice> You were given the SimpleX name declineme.simplex"
      bob ##> "/names incoming"
      bob <## "names sent to you, not yet accepted:"
      addrLine <- getTermLine bob
      bob <## ""
      bob <## "accepting links this profile to the name on chain; declining leaves no trace"
      let addr = reverse . takeWhile (/= ' ') . reverse $ addrLine
      bob ##> ("/names decline " <> addr)
      bob <##. "declined the name at 0x"
      bob ##> "/names incoming"
      bob <## "no names have been sent to you"
      bob ##> "/names list"
      bob <## "you own no names"

-- | The survives-a-dead-device property, through the client: a fresh profile
-- imports the recovery key of a profile that bought a name, and finds the name
-- - buying nothing, asking no one.
testImportRecoveryKeyFindsNames :: HasCallStack => TestParams -> IO ()
testImportRecoveryKeyFindsNames ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice bob = do
      alice ##> "/names buy keepsake"
      alice <## "registered keepsake.simplex"
      alice <##. "tx "
      alice ##> "/names key"
      alice <## "name recovery key:"
      alice <## ""
      phrase <- getTermLine alice
      alice <## ""
      alice <## "write this down and keep it offline - it is the only way to recover your names"
      alice <## "then run /names key saved"
      -- bob is a fresh profile; importing alice's key makes him alice on a new
      -- device, so her bought name is now his to find.
      bob ##> ("/names key import " <> phrase)
      bob <## "wallet ready, recovery key NOT saved"
      bob ##> "/names list"
      bob <## "your names:"
      bob <##. "  keepsake.simplex (expires "

-- | Buying a name someone already holds must fail cleanly rather than charge
-- and silently do nothing - the race a name loses between quote and buy.
testBuyTakenNameFails :: HasCallStack => TestParams -> IO ()
testBuyTakenNameFails ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test alice _bob = do
      alice ##> "/names buy takentwice"
      alice <## "registered takentwice.simplex"
      alice <##. "tx "
      alice ##> "/names buy takentwice"
      alice <## "bad chat command: that name is already taken"
