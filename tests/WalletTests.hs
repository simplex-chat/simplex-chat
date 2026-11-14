{-# LANGUAGE OverloadedStrings #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import qualified Data.ByteString.Char8 as B
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Simplex.Chat.Wallet (WalletAddress (..), deriveAccount, masterMnemonic)
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import Simplex.Messaging.Crypto.BIP44 (mkAccountIndex)
import Simplex.Messaging.Encoding.String (strEncode)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

testPhrase12 :: Text
testPhrase12 = T.unwords $ replicate 11 "abandon" <> ["about"]

testPhrase24 :: Text
testPhrase24 = T.unwords $ replicate 23 "abandon" <> ["art"]

walletEntropy :: Text -> B39.WalletEntropy
walletEntropy = either error id . B39.parsePhrase

walletAddress :: Text -> Word32 -> IO WalletAddress
walletAddress phrase n = do
  g <- C.newRandom
  either error id <$> deriveAccount g (walletEntropy phrase) (either error id $ mkAccountIndex n)

accountCreated :: HasCallStack => TestCC -> String -> Expectation
accountCreated cc idx = (take 1 . words <$> getTermLine cc) `shouldReturn` [idx]

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "derives the addresses another wallet derives for the same phrase" $ do
    let addrOf n = strEncode . address <$> walletAddress testPhrase12 n
    addrOf 0 `shouldReturn` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 1 `shouldReturn` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "every account has its own address" $ do
    addrs <- mapM (fmap address . walletAddress testPhrase12) [0 .. 9]
    length (nub addrs) `shouldBe` 10
  Hspec.it "renders the path an account is derived at" $ do
    (keyPath <$> walletAddress testPhrase12 0) `shouldReturn` "m/44'/60'/0'/0/0"
    (keyPath <$> walletAddress testPhrase12 7) `shouldReturn` "m/44'/60'/7'/0/0"
  Hspec.it "round-trips the phrase it was parsed from" $
    masterMnemonic (walletEntropy testPhrase24) `shouldBe` testPhrase24

walletTests :: SpecWith TestParams
walletTests = do
  it "creates a wallet only on the create command, and at most one" testWalletCreate
  it "creates each account at the next index" testWalletCreateAccount
  it "keeps each profile's accounts apart" testWalletAccountsPerProfile
  it "does not reuse a deleted profile's account" testWalletDeletedProfileAccount
  it "derives a created account from the exported phrase" testWalletExport
  it "the wallet, the accounts and the counter persist across a restart" testWalletPersists
  it "deletes the wallet, and a new one can be created" testWalletDelete

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet export master"
  alice <## "no wallet on this device"
  alice ##> "/_wallet create account 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet delete"
  alice <## "no wallet on this device"
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create new"
  alice <## "this device already has a wallet"

testWalletCreateAccount :: HasCallStack => TestParams -> IO ()
testWalletCreateAccount ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "1"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "2"
  alice ##> "/_wallet 1"
  alice <## "wallet, next account 3, accounts: 1, 2"

testWalletAccountsPerProfile :: HasCallStack => TestParams -> IO ()
testWalletAccountsPerProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "1"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet 2"
  alice <## "wallet, next account 2, no accounts for this profile"
  alice ##> "/_wallet create account 2"
  alice `accountCreated` "2"
  alice ##> "/_wallet 2"
  alice <## "wallet, next account 3, accounts: 2"

testWalletDeletedProfileAccount :: HasCallStack => TestParams -> IO ()
testWalletDeletedProfileAccount ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "1"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/delete user alice"
  alice <### ["ok", "completed deleting user"]
  alice ##> "/_wallet 2"
  alice <## "wallet, next account 2, no accounts for this profile"
  alice ##> "/_wallet create account 2"
  alice `accountCreated` "2"

testWalletExport :: HasCallStack => TestParams -> IO ()
testWalletExport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet export master"
  phrase <- getTermLine alice
  length (words phrase) `shouldBe` 24
  addr <- B.unpack . strEncode . address <$> walletAddress (T.pack phrase) 1
  alice ##> "/_wallet create account 1"
  (words <$> getTermLine alice) `shouldReturn` ["1", "m/44'/60'/1'/0/0", addr]

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  phrase <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create new"
    alice <## "wallet, next account 1, no accounts for this profile"
    alice ##> "/_wallet create account 1"
    alice `accountCreated` "1"
    alice ##> "/_wallet export master"
    getTermLine alice
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet 1"
    alice <## "wallet, next account 2, accounts: 1"
    alice ##> "/_wallet export master"
    alice <## phrase
    alice ##> "/_wallet create account 1"
    alice `accountCreated` "2"

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "1"
  alice ##> "/_wallet delete"
  alice <## "ok"
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet create new"
  alice <## "wallet, next account 1, no accounts for this profile"
  alice ##> "/_wallet create account 1"
  alice `accountCreated` "1"
