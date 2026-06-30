{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Groups (prepareChannel1Relay)
import ChatTests.Utils
import Control.Concurrent.Async (concurrently_)
import qualified Data.Text as T
import NameResolver
import Simplex.Messaging.SimplexName (SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Test.Hspec hiding (it)

chatNamesTests :: SpecWith TestParams
chatNamesTests = do
  it "connect by resolved name" testConnectByName
  it "connect by name not claimed in link profile is rejected" testConnectByNameNotClaimed
  it "connect by name to a known contact not claimed in profile is rejected" testConnectByNameKnownContactNotClaimed
  it "connect by unregistered name fails to resolve" testConnectByNameNotFound
  it "set name not resolving to own address is rejected" testSetNameNotOwnAddress
  it "connect by channel name" testConnectByChannelName

testConnectByName :: HasCallStack => TestParams -> IO ()
testConnectByName ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexNameDomain TLDSimplex "alice" [])
    test reg alice bob = do
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceName (contactNameRecord "alice" (T.pack shortLink))
      alice ##> "/_set_name 1 @alice.simplex"
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
    aliceName = SimplexNameInfo NTContact (SimplexNameDomain TLDSimplex "alice" [])
    test reg alice bob = do
      alice ##> "/ad"
      (shortLink, _) <- getContactLinks alice True
      registerName reg aliceName (contactNameRecord "alice" (T.pack shortLink))
      bob ##> "/c @alice.simplex"
      bob <## "SimpleX name @alice.simplex is not included in the connection link's profile"

testConnectByNameKnownContactNotClaimed :: HasCallStack => TestParams -> IO ()
testConnectByNameKnownContactNotClaimed ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexNameDomain TLDSimplex "alice" [])
    test reg alice bob = do
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
      bob <## "SimpleX name @alice.simplex is not included in the connection link's profile"

testConnectByNameNotFound :: HasCallStack => TestParams -> IO ()
testConnectByNameNotFound ps = withSmpServerAndNames $ \_reg ->
  testChat2 aliceProfile bobProfile test ps
  where
    test _alice bob = do
      bob ##> "/c @nobody.simplex"
      bob .<## "smpErr = NAME {nameErr = NOT_FOUND}}"

testSetNameNotOwnAddress :: HasCallStack => TestParams -> IO ()
testSetNameNotOwnAddress ps = withSmpServerAndNames $ \reg ->
  testChat2 aliceProfile bobProfile (test reg) ps
  where
    aliceName = SimplexNameInfo NTContact (SimplexNameDomain TLDSimplex "alice" [])
    test reg alice bob = do
      bob ##> "/ad"
      (bobShortLink, _) <- getContactLinks bob True
      registerName reg aliceName (contactNameRecord "alice" (T.pack bobShortLink))
      alice ##> "/ad"
      _ <- getContactLinks alice True
      alice ##> "/_set_name 1 @alice.simplex"
      alice <## "bad chat command: name does not point to your address"

testConnectByChannelName :: HasCallStack => TestParams -> IO ()
testConnectByChannelName ps = withSmpServerAndNames $ \reg ->
  withNewTestChat ps "alice" aliceProfile $ \alice ->
    withNewTestChatOpts ps relayTestOpts "cath" cathProfile $ \cath ->
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        (shortLink, _) <- prepareChannel1Relay "team" alice cath
        registerName reg teamName (channelNameRecord "team" (T.pack shortLink))
        alice ##> "/public group access #team domain=team.simplex"
        alice <## "updated public group access: domain=#team.simplex"
        cath <## "alice updated group #team: (signed)"
        cath <## "updated public group access: domain=#team.simplex"
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
    teamName = SimplexNameInfo NTPublicGroup (SimplexNameDomain TLDSimplex "team" [])
