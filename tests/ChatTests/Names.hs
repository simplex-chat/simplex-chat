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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import NameResolver
import Simplex.Chat.Controller (ChatConfig (..), ChatResponse (..), ConnectionPlan (..), ContactAddressPlan (..), GroupLinkPlan (..), NamePrice (..), NameWarning (..))
import Simplex.Chat.Library.Commands (execChatCommand', nameRecordOrWarning, parseChatCommand)
import Simplex.Chat.Messages (AChatInfo (..), ChatInfo (..))
import Simplex.Chat.Types (Contact (..), GroupInfo (..))
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
    it "known chat and own name, name moved to another known chat" testPlanKnownNameMovedToKnownChat
    it "known chat, name now available" testPlanKnownNameAvailable
    it "known chat and own name, name without link or reserved, stored as resolved" testPlanKnownNameReserved
    it "known chat and own name, the request failed" testPlanKnownNameResolverFailed
    it "known chat and own name, the name's new link cannot be fetched" testPlanKnownNameLinkFailed
    it "own channel expired, joined channel moved to a new channel, new channel joined" testPlanChannelNameMoved
    it "joined channel moved to a channel with no relays" testPlanChannelNameMovedNoRelays
    it "no local chat, the channel has no relays" testPlanNameChannelNoRelays
    it "known chats, the name's link differs only in its key hash" testPlanNameLinkKeyHash
    it "local chats, and whether a lookup can show more" testPlanLocalChats
    it "bare name, the other kind's chat moved, offered" testPlanNameOtherKindMoved
    it "bare name, the other kind is a business chat" testPlanNameOtherKindBusiness
    it "known chat, resolved on every call" testPlanKnownNameStale
    it "no local chat, resolved on every call" testPlanNameResolvedEveryCall
    it "own name, expired" testPlanOwnNameExpired
    it "own name, now available" testPlanOwnNameAvailable
    it "the request failed" testPlanNameResolverFailed
    it "name resolved less than a minute ago" testPlanNameCached
    it "resolve=never: local hit and miss" testPlanNameResolveNever
  describe "name warnings" $ do
    it "registration record or warning" $ \_ -> testNameRecordOrWarning

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
        bob ##> "/_connect plan 1 #team.simplex"
        knownTeamPlan bob
        planLocal bob "/_connect plan 1 #team.simplex resolve=never" `shouldReturn` ["team"]
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
        alice <## "You can also connect to @team.simplex in direct chat"
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
        bob <## "You can also connect to @team.simplex in direct chat"
        bob ##> "/_connect plan 1 #team.simplex resolve=all"
        knownTeamPlan bob
        bob ##> "/_connect plan 1 team.simplex resolve=all"
        knownTeamPlan bob
        bob <## "You can also connect to @team.simplex in direct chat"
        registerName reg teamName (contactNameRecord "team.simplex" (T.pack contactLink))
        bob ##> "/_connect plan 1 team.simplex"
        bob <## "contact address: ok to connect"
        _ <- getTermLine bob
        bob ##> "/_connect plan 1 #team.simplex resolve=never"
        knownTeamPlan bob
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
        registerName reg acmeName (contactAndChannelNameRecord "acme.simplex" (T.pack $ otherLink contactLink) (T.pack channelLink))
        bob ##> "/_connect plan 1 acme.simplex"
        bob <## "SimpleX name acme.simplex is not included in the connection link's profile"
        registerName reg acmeName (contactAndChannelNameRecord "acme.simplex" (T.pack contactLink) (T.pack channelLink))
        alice ##> "/_set domain 1 acme.simplex"
        alice <## "new contact address set"
        bob ##> "/_connect plan 1 acme.simplex"
        bob <## "contact address: ok to connect"
        _ <- getTermLine bob -- contact short link data (JSON, printed in test view)
        bob <## "You can also join channel #acme"
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
        registerExpiredName reg bizName (contactAndChannelNameRecord "biz.simplex" (T.pack contactLink) (T.pack channelLink))
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
      (shortLink, _) <- setAliceName reg alice
      test reg (contactNameRecord "alice.simplex" (T.pack shortLink)) alice bob

setAliceName :: HasCallStack => NameRegistry -> TestCC -> IO (String, String)
setAliceName reg alice = do
  alice ##> "/ad"
  links@(shortLink, _) <- getContactLinks alice True
  registerName reg aliceSimplexName (contactNameRecord "alice.simplex" (T.pack shortLink))
  alice ##> "/_set domain 1 alice.simplex"
  alice <## "new contact address set"
  pure links

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
    Right CRConnectionPlan {connectionPlan = CPContactAddress CAPOk {} _ existingChat_} -> pure $ chatName =<< existingChat_
    Right CRConnectionPlan {connectionPlan = CPGroupLink GLPOk {} _ existingChat_} -> pure $ chatName =<< existingChat_
    _ -> fail $ "unexpected response: " <> show r

planLocal :: TestCC -> ByteString -> IO [String]
planLocal TestCC {chatController = cc} cmd = do
  cmd' <- either fail pure $ parseChatCommand cmd
  r <- execChatCommand' cmd' 0 `runReaderT` cc
  case r of
    Right CRConnectionPlan {connectionPlan = CPContactAddress cap _ existingChat_} -> pure $ maybeToList (case cap of CAPKnown Contact {localDisplayName} -> Just (T.unpack localDisplayName); _ -> Nothing) <> maybeToList (chatName =<< existingChat_)
    Right CRConnectionPlan {connectionPlan = CPGroupLink glp _ existingChat_} -> pure $ maybeToList (groupName glp) <> maybeToList (chatName =<< existingChat_)
    _ -> fail $ "unexpected response: " <> show r
  where
    groupName GLPKnown {groupInfo = GroupInfo {localDisplayName}} = Just $ T.unpack localDisplayName
    groupName GLPOwnLink {groupInfo = GroupInfo {localDisplayName}} = Just $ T.unpack localDisplayName
    groupName _ = Nothing

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

testPrepareNameNotClaimed :: HasCallStack => TestParams -> IO ()
testPrepareNameNotClaimed ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      (shortLink, fullLink) <- setAliceName reg alice
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
  registerName reg boogalooName (channelNameRecord "boogaloo.simplex" "simplex:/c#AQIDBAUGBwgBAgMEBQYHCAECAwQFBgcIAQIDBAUGBwg?h=smp.simplex.im&p=5223&c=1234-w")
  bob ##> "/_connect plan 1 @boogaloo.simplex"
  bob <## "SimpleX name boogaloo.simplex has no valid connection link"
  where
    boogalooName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "boogaloo" [])

testPlanKnownNameAvailable :: HasCallStack => TestParams -> IO ()
testPlanKnownNameAvailable = withAliceName $ \reg _r alice bob -> do
  connectBobByName alice bob
  unregisterName reg aliceSimplexName
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
  bob ##> "/_connect plan 1 @alice.simplex resolve=all"
  knownAlicePlan bob
  alice ##> "/_connect plan 1 @alice.simplex"
  alice <## "contact address: own address"
  registerExpiredName reg aliceSimplexName (emptyRecord "alice.simplex")
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  bob <##. "SimpleX name alice.simplex expired on "
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
  bob <## "SimpleX name from cache"
  bob ##> "/_connect plan 1 alice.simplex resolve=all"
  knownAlicePlan bob
  bob <## "SimpleX name from cache"
  alice ##> "/_connect plan 1 @alice.simplex"
  alice <## "contact address: own address"

testPlanKnownNameStale :: HasCallStack => TestParams -> IO ()
testPlanKnownNameStale = withAliceName $ \reg aliceRecord alice bob -> do
  connectBobByName alice bob
  registerExpiredName reg aliceSimplexName aliceRecord
  bob ##> "/_connect plan 1 @alice.simplex"
  knownAlicePlan bob
  bob <##. "SimpleX name alice.simplex expired on "
  registerName reg aliceSimplexName aliceRecord
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
      (cathLink, cathFullLink) <- setAliceName reg cath
      bob ##> "/_connect plan 1 @alice.simplex resolve=all"
      bob <## "contact address: ok to connect, address changed"
      contactSLinkData <- getTermLine bob
      bob ##> ("/_prepare contact 1 " <> cathFullLink <> " " <> cathLink <> " domain=alice.simplex " <> contactSLinkData)
      bob <## "cath: contact is prepared"
      bob ##> "/_connect plan 1 @alice.simplex resolve=never"
      bob <## "contact address: known prepared contact cath"
      bob <## "SimpleX name: @alice.simplex (verified)"

testPlanKnownNameMovedToKnownChat :: HasCallStack => TestParams -> IO ()
testPlanKnownNameMovedToKnownChat ps = withSmpServerAndNames $ \reg ->
  testChat3 aliceProfile bobProfile cathProfile (test reg) ps
  where
    test reg alice bob cath = do
      mapM_ enableNamesRole [alice, bob, cath]
      _ <- setAliceName reg alice
      connectBobByName alice bob
      (cathLink, _) <- setAliceName reg cath
      bob ##> ("/c " <> cathLink)
      bob <## "connection request sent!"
      cath <## "bob (Bob) wants to connect to you!"
      cath <## "to accept: /ac bob"
      cath <## "to reject: /rc bob (the sender will NOT be notified)"
      cath ##> "/ac bob"
      cath <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "cath (Catherine): contact is connected")
        (cath <## "bob (Bob): contact is connected")
      bob ##> "/_connect plan 1 @alice.simplex"
      bob <## "contact address: known contact cath"
      bob <## "SimpleX name: @alice.simplex (verified)"
      bob <## "use @cath <message> to send messages"
      alice ##> ("/c " <> cathLink)
      alice <## "connection request sent!"
      cath <## "alice (Alice) wants to connect to you!"
      cath <## "to accept: /ac alice"
      cath <## "to reject: /rc alice (the sender will NOT be notified)"
      cath ##> "/ac alice"
      cath <## "alice (Alice): accepting contact request, you can send messages to contact"
      concurrently_
        (alice <## "cath (Catherine): contact is connected")
        (cath <## "alice (Alice): contact is connected")
      alice ##> "/_connect plan 1 @alice.simplex"
      alice <## "contact address: known contact cath"
      alice <## "SimpleX name: @alice.simplex (verified)"
      alice <## "use @cath <message> to send messages"

testPlanNameResolverFailed :: HasCallStack => TestParams -> IO ()
testPlanNameResolverFailed = withAliceName $ \reg _r _alice bob -> do
  failNameResolution reg brokenName
  bob ##> "/_connect plan 1 broken.simplex"
  bob .<## "smpErr = NAME {nameErr = RESOLVER {resolverErr = \"HTTP 500\"}}}"
  bob ##> "/_connect plan 1 @alice.simplex"
  bob <## "contact address: ok to connect"
  _ <- getTermLine bob
  failNameResolution reg aliceSimplexName
  bob ##> "/_connect plan 1 @alice.simplex"
  bob <## "contact address: ok to connect"
  _ <- getTermLine bob
  bob <## "SimpleX name from cache"
  where
    brokenName = SimplexNameInfo NTContact (SimplexDomain TLDSimplex "broken" [])

testPlanNameCached :: HasCallStack => TestParams -> IO ()
testPlanNameCached ps = withSmpServerAndNames $ \reg ->
  testChatCfg2 testCfg {nameCacheTTL = 60} aliceProfile bobProfile (test reg) ps
  where
    test reg alice bob = do
      mapM_ enableNamesRole [alice, bob]
      _ <- setAliceName reg alice
      bob ##> "/_connect plan 1 @alice.simplex"
      bob <## "contact address: ok to connect"
      _ <- getTermLine bob
      unregisterName reg aliceSimplexName
      bob ##> "/_connect plan 1 alice.simplex"
      bob <## "contact address: ok to connect"
      _ <- getTermLine bob
      bob <## "SimpleX name from cache"

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
      alice ##> "/_connect plan 1 @alice.simplex"
      alice <## "contact address: own address"
      cath ##> "/_connect plan 1 @alice.simplex"
      cath <##. "error: "

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
        planLocal bob "/_connect plan 1 #team.simplex resolve=never" `shouldReturn` ["team"]
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

testPlanChannelNameMovedNoRelays :: HasCallStack => TestParams -> IO ()
testPlanChannelNameMovedNoRelays ps = withSmpServerAndNames $ \reg ->
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
        (shortLink2, _) <- prepareChannel' 2 "team2" alice cath
        registerName reg teamName (channelNameRecord "team.simplex" (T.pack shortLink2))
        alice ##> "/public group access #team2 domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice_1 updated group #team2: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        cath ##> "/leave #team2"
        cath <## "#team2: you left the group (future invitations will be rejected)"
        cath <## "use /group allow #team2 to allow future invitations"
        cath <## "use /d #team2 to delete the group (also clears the rejection)"
        alice <## "#team2: cath_1 left the group (signed)"
        threadDelay 100000
        bob ##> ("/_connect plan 1 " <> shortLink2)
        bob <## "group link: channel has no active relays, please try to join later"
        bob ##> "/_connect plan 1 #team.simplex resolve=all"
        bob <## "group link: channel has no active relays, please try to join later"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

testPlanNameChannelNoRelays :: HasCallStack => TestParams -> IO ()
testPlanNameChannelNoRelays ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (channelLink, _) <- prepareChannel1Relay "team" alice cath
        alice ##> "/ad"
        (contactLink, _) <- getContactLinks alice True
        registerName reg teamName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink) (T.pack channelLink))
        cath ##> "/leave #team"
        cath <## "#team: you left the group (future invitations will be rejected)"
        cath <## "use /group allow #team to allow future invitations"
        cath <## "use /d #team to delete the group (also clears the rejection)"
        alice <## "#team: cath left the group (signed)"
        threadDelay 100000
        bob ##> "/_connect plan 1 #team.simplex"
        bob <## "group link: channel has no active relays, please try to join later"
        bob ##> "/_connect plan 1 team.simplex"
        bob <## "group link: channel has no active relays, please try to join later"
        bob <## "You can also connect to @team.simplex in direct chat"
        alice ##> "/_set domain 1 team.simplex"
        alice <## "new contact address set"
        bob ##> "/_connect plan 1 @team.simplex"
        bob <## "contact address: ok to connect"
        _ <- getTermLine bob
        bob ##> "/_connect plan 1 @nobody.simplex resolve=never"
        bob <## "no matching chat found, name resolution is disabled"
  where
    teamName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

testPlanNameLinkKeyHash :: HasCallStack => TestParams -> IO ()
testPlanNameLinkKeyHash = withTeamChats $ \reg contactLink channelLink _alice bob -> do
  registerName reg teamSimplexName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink <> "?c=LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI") (T.pack channelLink))
  bob ##> "/_connect plan 1 team.simplex"
  knownTeamPlan bob
  bob <## "You can also connect to @team.simplex in direct chat"
  bob ##> "/_connect plan 1 #team.simplex resolve=never"
  knownTeamPlan bob

testPlanLocalChats :: HasCallStack => TestParams -> IO ()
testPlanLocalChats = withTeamChats $ \reg contactLink channelLink alice bob -> do
  planLocal bob "/_connect plan 1 team.simplex resolve=never" `shouldReturn` ["team"]
  planLocal bob "/_connect plan 1 #team.simplex resolve=never" `shouldReturn` ["team"]
  planLocal bob "/_connect plan 1 @team.simplex resolve=never" `shouldReturn` ["alice"]
  bob ##> "/_connect plan 1 team.simplex"
  knownTeamPlan bob
  bob <## "You can also connect to @team.simplex in direct chat"
  bob ##> "/_connect plan 1 @team.simplex"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @team.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  registerExpiredName reg teamSimplexName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink) (T.pack channelLink))
  bob ##> "/_connect plan 1 team.simplex"
  knownTeamPlan bob
  bob <##. "SimpleX name team.simplex expired on "
  planLocal bob "/_connect plan 1 team.simplex resolve=never" `shouldReturn` ["team"]
  planLocal alice "/_connect plan 1 team.simplex resolve=never" `shouldReturn` ["team"]
  planLocal alice "/_connect plan 1 @team.simplex resolve=never" `shouldReturn` []

otherLink :: String -> String
otherLink l = case break (== '#') l of
  (pre, '#' : c : rest) -> pre <> "#" <> [if c == 'A' then 'B' else 'A'] <> rest
  _ -> error "no link key"

teamSimplexName :: SimplexNameInfo
teamSimplexName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "team" [])

knownTeamPlan :: HasCallStack => TestCC -> IO ()
knownTeamPlan cc = do
  cc <## "group link: known group #team"
  cc <## "SimpleX name: #team (verified)"
  cc <## "use #team <message> to send messages"

testPlanNameOtherKindMoved :: HasCallStack => TestParams -> IO ()
testPlanNameOtherKindMoved = withTeamChats $ \reg _contactLink channelLink alice bob -> do
  bob ##> "/_connect plan 1 @team.simplex resolve=all"
  bob <## "contact address: known contact alice"
  bob <## "SimpleX name: @team.simplex (verified)"
  bob <## "use @alice <message> to send messages"
  alice ##> "/da"
  alice <## "Your chat address is deleted - accepted contacts will remain connected."
  alice <## "To create a new chat address use /ad"
  bob <## "alice removed contact address"
  alice ##> "/ad"
  (contactLink2, _) <- getContactLinks alice True
  registerName reg teamSimplexName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink2) (T.pack channelLink))
  bob ##> "/_connect plan 1 team.simplex"
  knownTeamPlan bob
  bob <## "You can also connect to @team.simplex in direct chat"
  bob ##> "/_connect plan 1 @team.simplex"
  bob <## "contact address: ok to connect, address changed"
  _ <- getTermLine bob
  pure ()

testPlanNameOtherKindBusiness :: HasCallStack => TestParams -> IO ()
testPlanNameOtherKindBusiness ps = withSmpServerAndNames $ \reg ->
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
        alice ##> "/public group access #biz domain=biz.simplex"
        alice <## "updated public group access: domain=biz.simplex"
        cath <## "alice updated group #biz: (signed)"
        cath <## "updated public group access: domain=biz.simplex"
        alice ##> "/_set domain 1 biz.simplex"
        alice <## "new contact address set"
        bob ##> "/_connect plan 1 @biz.simplex"
        bob <## "business address: ok to connect"
        contactSLinkData <- getTermLine bob
        bob ##> ("/_prepare contact 1 " <> fullLink <> " " <> contactLink <> " domain=biz.simplex " <> contactSLinkData)
        bob <## "#alice: group is prepared"
        bob ##> "/_connect group #1"
        bob <## "#alice: connection started"
        alice <## "#bob (Bob): accepting business address request..."
        bob <## "#alice: joining the group..."
        alice <## "#bob: bob_1 joined the group"
        bob <## "#alice: you joined the group"
        bob ##> "/c #biz.simplex"
        bob <## "#biz: connection started"
        concurrentlyN_
          [ bob
              <### [ "#biz: joining the group (connecting to relay cath)...",
                     "#biz: you joined the group (connected to relay cath)"
                   ]
          , do
              cath <## "bob (Bob): accepting request to join group #biz..."
              cath <## "#biz: bob joined the group"
          , alice <### [EndsWith "(Bob) in the channel"]
          ]
        planLocal bob "/_connect plan 1 biz.simplex resolve=never" `shouldReturn` ["biz"]
        bob ##> "/_connect plan 1 biz.simplex"
        knownBizPlan bob
        bob <## "You can also connect to @biz.simplex in direct chat"
  where
    bizName = SimplexNameInfo NTPublicGroup (SimplexDomain TLDSimplex "biz" [])
    knownBizPlan cc = do
      cc <## "group link: known group #biz"
      cc <## "SimpleX name: #biz (verified)"
      cc <## "use #biz <message> to send messages"

withTeamChats :: HasCallStack => (NameRegistry -> String -> String -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
withTeamChats test ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        mapM_ enableNamesRole [alice, cath, bob]
        (channelLink, _) <- prepareChannel1Relay "team" alice cath
        alice ##> "/ad"
        (contactLink, _) <- getContactLinks alice True
        registerName reg teamSimplexName (contactAndChannelNameRecord "team.simplex" (T.pack contactLink) (T.pack channelLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=team.simplex"
        alice ##> "/_set domain 1 team.simplex"
        alice <## "new contact address set"
        bob ##> "/c @team.simplex"
        bob <## "alice: connection started"
        alice <## "bob (Bob) wants to connect to you!"
        alice <## "to accept: /ac bob"
        alice <## "to reject: /rc bob (the sender will NOT be notified)"
        alice ##> "/ac bob"
        alice <## "bob (Bob): accepting contact request, you can send messages to contact"
        concurrently_
          (bob <## "alice (Alice): contact is connected")
          (alice <## "bob (Bob): contact is connected")
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
          , alice <### [EndsWith "(Bob) in the channel"]
          ]
        test reg contactLink channelLink alice bob

testNameRecordOrWarning :: IO ()
testNameRecordOrWarning = do
  recordOrWarning (registered Nothing Nothing record) `shouldBe` Right record
  recordOrWarning (registered (Just 1100) Nothing record) `shouldBe` Right record
  recordOrWarning (NRRegistered Nothing Nothing (Just NRRCommunity) record) `shouldBe` Right record
  recordOrWarning (registered (Just 900) (Just 2000) record) `shouldBe` Left (NWExpired (utc 900) (Just $ utc 2000))
  recordOrWarning (registered (Just 900) Nothing record) `shouldBe` Left (NWExpired (utc 900) Nothing)
  recordOrWarning (registered (Just 800) (Just 900) record) `shouldBe` Left (NWExpired (utc 800) Nothing)
  recordOrWarning (NRAvailable $ pricing 5 M.empty) `shouldBe` Left (NWAvailable $ NamePrice (USDCents 2000) 2)
  recordOrWarning (NRAvailable $ pricing 3 (M.fromList [(5, USDCents 5000)])) `shouldBe` Left (NWAvailable $ NamePrice (USDCents 10000) 2)
  recordOrWarning (NRAvailable $ pricing 6 M.empty) `shouldBe` Left NWNotRegistered
  recordOrWarning (NRReserved NRRCommunity) `shouldBe` Left NWReservedForCommunity
  recordOrWarning (NRReserved NRRTrademark) `shouldBe` Left NWNotRegistered
  where
    recordOrWarning = nameRecordOrWarning (RoundedSystemTime 1000) (SimplexDomain TLDSimplex "alice" [])
    registered expires graceUntil = NRRegistered (RoundedSystemTime <$> expires) (RoundedSystemTime <$> graceUntil) Nothing
    pricing minLabelLength registrationPrices = NamePricing {registrationPrices, basePrice = USDCents 1000, minLabelLength}
    record = contactNameRecord "alice.simplex" "https://smp4.simplex.im/a#lXUjJW5vHYQzoLYgmi8GbxkGP41_kjefFvBrdwg-0Ok"

utc :: Int64 -> UTCTime
utc = roundedToUTCTime . RoundedSystemTime
