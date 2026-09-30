{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (memberJoinChannel, memberJoinChannel', prepareChannel', prepareChannel1Relay)
import ChatTests.Utils
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import NameResolver
import Simplex.Chat.Controller (ChatResponse (..), ConnectionPlan (..), ContactAddressPlan (..), GroupLinkPlan (..), NamePrice (..), NameWarning (..))
import Simplex.Chat.Library.Commands (execChatCommand', nameLinkOrWarning, parseChatCommand, setNameWarning)
import Simplex.Chat.Messages (AChatInfo (..), ChatInfo (..))
import Simplex.Chat.Types (Contact (..), GroupInfo (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Names.Record (NamePricing (..), NameRecord, NameRegistration (..), NameReservedReason (..), USDCents (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), roundedToUTCTime)
import Test.Hspec hiding (it)

chatNamesTests :: SpecWith TestParams
chatNamesTests = do
  it "connect by resolved name" testConnectByName
  it "connect by name not claimed in link profile is rejected" testConnectByNameNotClaimed
  it "prepare with a name not claimed in link profile is not verified" testPrepareNameNotClaimed
  it "connect by name to a known contact not claimed in profile is rejected" testConnectByNameKnownContactNotClaimed
  it "connect by unregistered name reports it is available" testConnectByNameNotFound
  it "set name not resolving to own address is rejected" testSetNameNotOwnAddress
  it "channel name is not verified just by joining via link" testChannelDomainLinkJoinUnverified
  it "verify channel name, fail on re-point, retain status on refresh" testChannelDomainVerify
  it "connect by channel name" testConnectByChannelName
  it "connect by name resolving to channel (primary) and direct contact" testConnectByNameChannelAndContact
  it "connect by name resolving to direct contact (primary) and channel" testConnectByNameContactAndChannel
  it "connect by name resolving to business (primary) and channel" testConnectByNameBusinessAndChannel
  describe "connection plan: the name lookup answers" $ do
    it "reserved for another reason" testPlanNameReservedOther
    it "registered with no usable link" testPlanNameNoValidLink
    it "known chat and own name, name moved to a new address" testPlanKnownNameAddressChanged
    it "known chat, name moved, new chat opened" testPlanKnownNameNewChatOpened
    it "known chat, name now available" testPlanKnownNameAvailable
    it "known chat and own name, name without link or reserved, stored as resolved" testPlanKnownNameReserved
    it "known chat and own name, the request failed" testPlanKnownNameResolverFailed
    it "known chat, the name's new link cannot be fetched" testPlanKnownNameLinkFailed
    it "own channel expired, joined channel moved to a new channel" testPlanChannelNameMoved
    it "known chat, resolved over a day ago or past expiry" testPlanKnownNameStale
    it "no local chat, resolved on every call" testPlanNameResolvedEveryCall
    it "own name, expired" testPlanOwnNameExpired
    it "own name, now available" testPlanOwnNameAvailable
    it "the request failed" testPlanNameResolverFailed
    it "resolve=never: local hit and miss" testPlanNameResolveNever
  describe "name warnings" $ do
    it "link or warning for a name with no local chat" $ \_ -> testNameLinkOrWarning
    it "warning for own name" $ \_ -> testOwnNameWarning

testConnectByName :: HasCallStack => TestParams -> IO ()
testConnectByName ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])
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
      bob <## "SimpleX name nobody.simplex is available: $20 for 2 years"

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
        alice ##> "/_set domain 1 team.simplex"
        alice <## "new contact address set"
        alice ##> "/_connect plan 1 team.simplex"
        alice <## "group link: own link for group #team"
        bob ##> "/_connect plan 1 team.simplex"
        bob <## "group link: ok to connect via relays"
        _ <- getTermLine bob
        bob <## "You can also connect to @team.simplex in direct chat"
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
        setGroupNamesStale bob
        bob ##> "/_connect plan 1 team.simplex"
        knownTeamPlan bob
        bob <## "You can also connect to @team.simplex in direct chat"
        bob ##> "/_connect plan 1 #team.simplex resolve=all"
        knownTeamPlan bob
        bob ##> "/_connect plan 1 team.simplex"
        knownTeamPlan bob
        bob ##> "/_connect plan 1 team.simplex resolve=all"
        knownTeamPlan bob
        bob <## "You can also connect to @team.simplex in direct chat"
        registerName reg teamName (contactNameRecord "team.simplex" (T.pack contactLink))
        setGroupNamesStale bob
        bob ##> "/_connect plan 1 team.simplex"
        knownTeamPlan bob
        bob <## "You can also connect to @team.simplex in direct chat"
        bob ##> "/_connect plan 1 team.simplex"
        knownTeamPlan bob
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])
    knownTeamPlan :: HasCallStack => TestCC -> IO ()
    knownTeamPlan cc = do
      cc <## "group link: known group #team"
      cc <## "SimpleX name: #team (verified)"
      cc <## "use #team <message> to send messages"

-- The bare name "acme.simplex" resolves to both a channel and a direct contact. The channel is tried
-- first but its group profile does not claim the domain, so the channel side of the plan fails; the
-- plan falls back to the direct contact as primary (planSimplexName); only the owner's plan offers the
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
        alice ##> "/_connect plan 1 acme.simplex"
        alice <## "contact address: own address"
        alice <## "You can also join channel #acme"
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
        registerExpiredName reg bizName (contactAndChannelNameRecord "biz.simplex" (T.pack contactLink) (T.pack channelLink))
        bob ##> "/_connect plan 1 @biz.simplex"
        bob <## "business address: known business #alice"
        bob <## "use #alice <message> to send messages"
        setGroupNamesStale bob
        bob ##> "/_connect plan 1 @biz.simplex"
        bob <## "business address: known business #alice"
        bob <## "use #alice <message> to send messages"
        bob <##. "SimpleX name biz.simplex expired on "
        -- the business's verified domain survives the handshake and is shown in group info
        bob ##> "/i #alice"
        bob <## "group ID: 1"
        bob <## "current members: 2"
        bob <## "SimpleX name: @biz.simplex (verified)"
  where
    bizName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "biz" [])

aliceSimplexName :: SimplexNameInfo
aliceSimplexName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "alice" [])

withAliceName :: HasCallStack => (NameRegistry -> NameRecord -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
withAliceName test ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (setup reg) ps
  where
    setup reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      aliceRecord <- setAliceName reg alice
      test reg aliceRecord alice bob

setAliceName :: HasCallStack => NameRegistry -> TestCC -> IO NameRecord
setAliceName reg alice = do
  alice ##> "/ad"
  (shortLink, _) <- getContactLinks alice True
  let aliceRecord = contactNameRecord "alice.simplex" (T.pack shortLink)
  registerName reg aliceSimplexName aliceRecord
  alice ##> "/_set domain 1 alice.simplex"
  alice <## "new contact address set"
  pure aliceRecord

knownAlicePlan :: HasCallStack => TestCC -> IO ()
knownAlicePlan bob = do
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @alice.simplex (verified)"
  bob <## "use @alice <message> to send messages"

planExistingChat :: TestCC -> ByteString -> IO (Maybe String)
planExistingChat TestCC {chatController = cc} cmd = do
  cmd' <- either fail pure $ parseChatCommand cmd
  r <- execChatCommand' cmd' 0 `runReaderT` cc
  case r of
    Right CRConnectionPlan {connectionPlan = CPContactAddress CAPOk {existingChat_} _} -> pure $ chatName =<< existingChat_
    Right CRConnectionPlan {connectionPlan = CPGroupLink GLPOk {existingChat_} _} -> pure $ chatName =<< existingChat_
    _ -> fail $ "unexpected response: " <> show r
  where
    chatName :: AChatInfo -> Maybe String
    chatName (AChatInfo _ (DirectChat Contact {localDisplayName})) = Just $ T.unpack localDisplayName
    chatName (AChatInfo _ (GroupChat GroupInfo {localDisplayName} _)) = Just $ T.unpack localDisplayName
    chatName _ = Nothing

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

setContactNamesStale :: TestCC -> IO ()
setContactNamesStale cc = withCCTransaction cc $ \db -> DB.execute_ db "UPDATE contact_profiles SET contact_domain_resolved_at = datetime('now', '-2 days')"

setGroupNamesStale :: TestCC -> IO ()
setGroupNamesStale cc = withCCTransaction cc $ \db -> DB.execute_ db "UPDATE groups SET group_domain_resolved_at = datetime('now', '-2 days')"

testPrepareNameNotClaimed :: HasCallStack => TestParams -> IO ()
testPrepareNameNotClaimed ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      alice ##> "/ad"
      (shortLink, fullLink) <- getContactLinks alice True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack shortLink))
      alice ##> "/_set domain 1 alice.simplex"
      alice <## "new contact address set"
      bob ##> ("/_connect plan 1 " <> shortLink)
      bob <## "contact address: ok to connect"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> shortLink <> " domain=bob.simplex " <> contactSLinkData)
      bob <## "alice: contact is prepared"
      bob ##> "/_connect plan 1 @alice.simplex resolve=never"
      bob <## "no matching chat found, name resolution is disabled"

testPlanNameReservedOther :: HasCallStack => TestParams -> IO ()
testPlanNameReservedOther = withAliceName $ \reg _r _alice bob -> do
  registerReservedName reg acmeName NRRTrademark
  bob ##> "/_connect plan 1 acme.simplex"
  bob <## "SimpleX name acme.simplex: nothing to connect to"
  bob <## "SimpleX name acme.simplex is not registered"
  where
    acmeName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "acme" [])

testPlanNameNoValidLink :: HasCallStack => TestParams -> IO ()
testPlanNameNoValidLink = withAliceName $ \reg _r _alice bob -> do
  registerName reg boogalooName (emptyRecord "boogaloo.simplex")
  bob ##> "/_connect plan 1 @boogaloo.simplex"
  bob <## "SimpleX name boogaloo.simplex: nothing to connect to"
  bob <## "SimpleX name boogaloo.simplex has no valid link"
  where
    boogalooName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "boogaloo" [])

testPlanKnownNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanKnownNameAvailable = withAliceName $ \reg _r alice bob -> do
  connectBobByName alice bob
  unregisterName reg aliceSimplexName
  setContactNamesStale bob
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  bob <## "SimpleX name alice.simplex is no longer registered, available: $20 for 2 years"
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  bob <## "SimpleX name alice.simplex is no longer registered, available: $20 for 2 years"

testPlanKnownNameReserved :: HasCallStack => TestParams -> IO ()
testPlanKnownNameReserved = withAliceName $ \reg _r alice bob -> do
  connectBobByName alice bob
  registerName reg aliceSimplexName (emptyRecord "alice.simplex")
  setContactNamesStale bob
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  alice ##> "/_connect plan 1 @alice.simplex"
  alice <## "contact address: own address"
  registerExpiredName reg aliceSimplexName (emptyRecord "alice.simplex")
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  registerReservedName reg aliceSimplexName NRRTrademark
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  alice ##> "/_connect plan 1 @alice.simplex"
  alice <## "contact address: own address"
  registerReservedName reg aliceSimplexName NRRCommunity
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  bob <## "SimpleX name alice.simplex is reserved for community"
  alice ##> "/_connect plan 1 @alice.simplex"
  alice <## "contact address: own address"
  alice <## "SimpleX name alice.simplex is reserved for community"

testPlanKnownNameResolverFailed :: HasCallStack => TestParams -> IO ()
testPlanKnownNameResolverFailed = withAliceName $ \reg _r alice bob -> do
  connectBobByName alice bob
  failNameResolution reg aliceSimplexName
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  bob ##> "/_connect plan 1 alice.simplex resolve=all"
  knownAlicePlan bob
  alice ##> "/_connect plan 1 @alice.simplex"
  alice .<## "smpErr = NAME {nameErr = RESOLVER {resolverErr = \"HTTP 500\"}}}"

testPlanKnownNameStale :: HasCallStack => TestParams -> IO ()
testPlanKnownNameStale = withAliceName $ \reg aliceRecord alice bob -> do
  connectBobByName alice bob
  registerExpiredName reg aliceSimplexName aliceRecord
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  setContactNamesStale bob
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  bob <##. "SimpleX name alice.simplex expired on "
  withCCTransaction bob $ \db -> DB.execute_ db "UPDATE contact_profiles SET contact_domain_resolved_at = datetime('now'), contact_domain_expires_at = datetime('now', '-1 hours')"
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  bob <##. "SimpleX name alice.simplex expired on "
  registerName reg aliceSimplexName aliceRecord
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  registerExpiredName reg aliceSimplexName aliceRecord
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob

testPlanNameResolvedEveryCall :: HasCallStack => TestParams -> IO ()
testPlanNameResolvedEveryCall = withAliceName $ \reg aliceRecord _alice bob -> do
  bob ##> "/_connect plan 1 @alice.simplex"
  bob <## "contact address: ok to connect"
  _ <- getTermLine bob
  bob ##> "/_connect plan 1 alice.simplex"
  bob <## "contact address: ok to connect"
  _ <- getTermLine bob
  registerExpiredName reg aliceSimplexName aliceRecord
  bob ##> "/_connect plan 1 @alice.simplex"
  bob <## "SimpleX name alice.simplex: nothing to connect to"
  bob <##. "SimpleX name alice.simplex expired on "

testPlanOwnNameExpired :: HasCallStack => TestParams -> IO ()
testPlanOwnNameExpired = withAliceName $ \reg aliceRecord alice _bob -> do
  registerExpiredName reg aliceSimplexName aliceRecord
  alice ##> "/c @alice.simplex"
  alice <## "contact address: own address"
  alice <##. "your SimpleX name alice.simplex expired on "

testPlanOwnNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanOwnNameAvailable = withAliceName $ \reg _r alice _bob -> do
  unregisterName reg aliceSimplexName
  alice ##> "/_connect plan 1 @alice.simplex resolve=all"
  alice <## "contact address: own address"
  alice <## "your SimpleX name alice.simplex is no longer registered, available: $20 for 2 years"

testPlanNameResolveNever :: HasCallStack => TestParams -> IO ()
testPlanNameResolveNever = withAliceName $ \reg _r alice bob -> do
  connectBobByName alice bob
  unregisterName reg aliceSimplexName
  setContactNamesStale bob
  bob ##> "/_connect plan 1 alice.simplex resolve=never"
  knownAlicePlan bob
  alice ##> "/_connect plan 1 @alice.simplex resolve=never"
  alice <## "contact address: own address"
  bob ##> "/_connect plan 1 @nobody.simplex resolve=never"
  bob <## "no matching chat found, name resolution is disabled"
  bob ##> "/_connect plan 1 nobody.simplex resolve=never"
  bob <## "no matching chat found, name resolution is disabled"

testPlanKnownNameAddressChanged :: HasCallStack => TestParams -> IO ()
testPlanKnownNameAddressChanged ps = withSmpServerAndNames $ \reg ->
  testChat3 aliceProfile bobProfile cathProfile (test reg) ps
  where
    test reg alice bob cath = do
      mapM_ enableNamesRole [alice, bob, cath]
      _ <- setAliceName reg alice
      connectBobByName alice bob
      cath ##> "/ad"
      (cathLink, _) <- getContactLinks cath True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack cathLink))
      bob ##> "/_connect plan 1 @alice.simplex resolve=all"
      knownAlicePlan bob
      alice ##> "/_connect plan 1 @alice.simplex"
      alice <## "contact address: own address"
      cath ##> "/_set domain 1 alice.simplex"
      cath <## "new contact address set"
      alice ##> "/_connect plan 1 @alice.simplex"
      alice <## "contact address: ok to connect, address changed"
      _ <- getTermLine alice
      planExistingChat alice "/_connect plan 1 @alice.simplex" `shouldReturn` Nothing
      setContactNamesStale bob
      bob ##> "/_connect plan 1 @alice.simplex"
      bob <## "contact address: ok to connect, address changed"
      _ <- getTermLine bob
      bob ##> "/c @alice.simplex"
      bob <## "contact address: ok to connect, address changed"
      _ <- getTermLine bob
      planExistingChat bob "/_connect plan 1 @alice.simplex" `shouldReturn` Just "alice"
      bob ##> "/_connect plan 1 @alice.simplex resolve=never"
      knownAlicePlan bob

testPlanKnownNameNewChatOpened :: HasCallStack => TestParams -> IO ()
testPlanKnownNameNewChatOpened ps = withSmpServerAndNames $ \reg ->
  testChat3 aliceProfile bobProfile cathProfile (test reg) ps
  where
    test reg alice bob cath = do
      mapM_ enableNamesRole [alice, bob, cath]
      _ <- setAliceName reg alice
      connectBobByName alice bob
      cath ##> "/ad"
      (cathLink, cathFullLink) <- getContactLinks cath True
      registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack cathLink))
      cath ##> "/_set domain 1 alice.simplex"
      cath <## "new contact address set"
      bob ##> "/_connect plan 1 @alice.simplex resolve=all"
      bob <## "contact address: ok to connect, address changed"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> cathFullLink <> " " <> cathLink <> " domain=alice.simplex " <> contactSLinkData)
      bob <## "cath: contact is prepared"
      failNameResolution reg aliceSimplexName
      bob ##> "/_connect plan 1 @alice.simplex"
      bob <## "contact address: known prepared contact cath"
      bob <## "SimpleX name: @alice.simplex (verified)"

testPlanNameResolverFailed :: HasCallStack => TestParams -> IO ()
testPlanNameResolverFailed = withAliceName $ \reg _r _alice bob -> do
  failNameResolution reg brokenName
  bob ##> "/_connect plan 1 broken.simplex"
  bob .<## "smpErr = NAME {nameErr = RESOLVER {resolverErr = \"HTTP 500\"}}}"
  where
    brokenName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "broken" [])

testPlanKnownNameLinkFailed :: HasCallStack => TestParams -> IO ()
testPlanKnownNameLinkFailed ps = withSmpServerAndNames $ \reg ->
  testChat3 aliceProfile bobProfile cathProfile (test reg) ps
  where
    test reg alice bob cath = do
      mapM_ enableNamesRole [alice, bob, cath]
      _ <- setAliceName reg alice
      connectBobByName alice bob
      _ <- setAliceName reg cath
      cath ##> "/da"
      cath <## "Your chat address is deleted - accepted contacts will remain connected."
      cath <## "To create a new chat address use /ad"
      bob ##> "/_connect plan 1 @alice.simplex resolve=all"
      knownAlicePlan bob

testPlanChannelNameMoved :: HasCallStack => TestParams -> IO ()
testPlanChannelNameMoved ps = withSmpServerAndNames $ \reg ->
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
        bob ##> "/_verify domain #1"
        bob <## "SimpleX name #team verified"
        registerExpiredName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink))
        alice ##> "/c #team.simplex"
        alice <## "group link: own link for group #team"
        alice <##. "your SimpleX name team.simplex expired on "
        (shortLink2, fullLink2) <- prepareChannel' 2 "team2" alice cath
        registerName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink2))
        alice ##> "/public group access #team2 domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice_1 updated group #team2: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        bob ##> "/_connect plan 1 #team.simplex resolve=all"
        bob <## "group link: ok to connect via relays, address changed"
        _ <- getTermLine bob
        planExistingChat bob "/_connect plan 1 #team.simplex resolve=all" `shouldReturn` Just "team"
        memberJoinChannel' "team2" 2 1 1 1 [cath] [alice] shortLink2 fullLink2 bob
        bob ##> "/_verify domain #2"
        bob <## "SimpleX name #team verified"
        bob ##> "/_connect plan 1 #team.simplex resolve=never"
        bob <## "group link: known group #team2"
        bob <## "SimpleX name: #team (verified)"
        bob <## "use #team2 <message> to send messages"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

testNameLinkOrWarning :: IO ()
testNameLinkOrWarning = do
  linkOrWarning NTContact (registered Nothing Nothing contactRecord) `shouldBe` Right contactLink
  linkOrWarning NTContact (registered (Just 1100) Nothing contactRecord) `shouldBe` Right contactLink
  linkOrWarning NTPublicGroup (registered Nothing Nothing channelRecord) `shouldBe` Right channelLink
  linkOrWarning NTPublicGroup (registered Nothing Nothing contactRecord) `shouldBe` Left NWNoValidLink
  linkOrWarning NTContact (NRRegistered Nothing Nothing (Just NRRCommunity) contactRecord) `shouldBe` Right contactLink
  linkOrWarning NTContact (registered (Just 900) (Just 2000) contactRecord) `shouldBe` Left (NWExpired (utc 900) (Just $ utc 2000))
  linkOrWarning NTContact (registered (Just 900) Nothing contactRecord) `shouldBe` Left (NWExpired (utc 900) Nothing)
  linkOrWarning NTContact (NRAvailable $ pricing 5 M.empty) `shouldBe` Left (NWAvailable $ NamePrice (USDCents 2000) 2)
  linkOrWarning NTContact (NRAvailable $ pricing 3 (M.fromList [(5, USDCents 5000)])) `shouldBe` Left (NWAvailable $ NamePrice (USDCents 10000) 2)
  linkOrWarning NTContact (NRAvailable $ pricing 6 M.empty) `shouldBe` Left NWNotRegistered
  linkOrWarning NTContact (NRReserved NRRCommunity) `shouldBe` Left NWReservedForCommunity
  linkOrWarning NTContact (NRReserved NRRTrademark) `shouldBe` Left NWNotRegistered
  where
    linkOrWarning nameType = nameLinkOrWarning (RoundedSystemTime 1000) (SimplexNameInfo nameType (SimplexDomain TLDSimplex "alice" []))
    registered expires graceUntil = NRRegistered (RoundedSystemTime <$> expires) (RoundedSystemTime <$> graceUntil) Nothing
    pricing minLabelLength registrationPrices = NamePricing {registrationPrices, basePrice = USDCents 1000, minLabelLength}
    contactRecord = contactNameRecord "alice.simplex" contactLinkStr
    channelRecord = channelNameRecord "alice.simplex" channelLinkStr
    contactLink = either error id $ strDecode $ encodeUtf8 contactLinkStr
    channelLink = either error id $ strDecode $ encodeUtf8 channelLinkStr
    contactLinkStr = "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"
    channelLinkStr = "simplex:/c#AQIDBAUGBwgBAgMEBQYHCAECAwQFBgcIAQIDBAUGBwg?h=smp.simplex.im&p=5223&c=1234-w"

testOwnNameWarning :: IO ()
testOwnNameWarning = do
  ownWarning (NWExpired (utc 900) Nothing) `shouldBe` Just (NWOwnExpired (utc 900) Nothing)
  ownWarning (NWAvailable price) `shouldBe` Just (NWOwnAvailable price)
  ownWarning NWReservedForCommunity `shouldBe` Just NWReservedForCommunity
  ownWarning NWNotRegistered `shouldBe` Nothing
  ownWarning NWNoValidLink `shouldBe` Nothing
  where
    ownWarning w = nameWarning_ $ setNameWarning w $ CPContactAddress CAPOwnLink Nothing
    price = NamePrice (USDCents 2000) 2

utc :: Int64 -> UTCTime
utc = roundedToUTCTime . RoundedSystemTime
