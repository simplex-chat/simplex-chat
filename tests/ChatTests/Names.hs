{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
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
  it "connect by unregistered name fails to resolve" testConnectByNameNotFound
  it "set name not resolving to own address is rejected" testSetNameNotOwnAddress

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
      bob <## "contact address: known prepared contact alice"
      bob <## "simplex name: @alice.simplex (verified)"
      bob ##> "/_connect contact @2 text hello"
      bob
        <### [ "alice: connection started",
               WithTime "@alice hello"
             ]
      alice
        <### [ "bob (Bob) wants to connect to you!",
               WithTime "bob> hello"
             ]
      alice <## "to accept: /ac bob"
      alice <## "to reject: /rc bob (the sender will NOT be notified)"
      alice ##> "/ac bob"
      alice <## "bob (Bob): accepting contact request, you can send messages to contact"
      concurrently_
        (bob <## "alice (Alice): contact is connected")
        (alice <## "bob (Bob): contact is connected")
      alice <##> bob
      -- the name is bound to the link profile (verified on connect) but the contact address
      -- carries no proof, so on-demand proof verification is inconclusive
      bob ##> "/_verify name @2"
      bob <## "simplex name @alice.simplex not verified: no name proof to verify"

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
      bob <## "simplex name @alice.simplex is not included in the connection link's profile"

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
