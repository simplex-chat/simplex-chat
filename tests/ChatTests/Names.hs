{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module ChatTests.Names where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import qualified Data.Text as T
import NameResolver
import Simplex.Messaging.SimplexName (SimplexNameDomain (..), SimplexNameInfo (..), SimplexNameType (..), SimplexTLD (..))
import Test.Hspec hiding (it)

chatNamesTests :: SpecWith TestParams
chatNamesTests = do
  it "connect by resolved name" testConnectByName
  it "connect by name not claimed in link profile is rejected" testConnectByNameNotClaimed
  it "connect by unregistered name fails to resolve" testConnectByNameNotFound

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
