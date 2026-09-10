{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.List (intersect, nub)
import Simplex.Chat.Wallet (SeedId (..), WalletSeed (..), accountAddress, accountSecret, deriveNameKey, importRecoveryKey, recoveryKeyPhrase, renderNameKeyPath)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The standard BIP-39 test vector, so the addresses can be checked elsewhere.
testPhrase :: ByteString
testPhrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

testSeed :: WalletSeed
testSeed = WalletSeed {wsId = SeedId 1, wsEntropy = either error id $ importRecoveryKey testPhrase}

walletDerivationTests :: Spec
walletDerivationTests = do
  -- profile 0's names are MetaMask's account list, and each profile's first
  -- name is the matching Ledger Live account
  Hspec.it "name keys line up with other wallets' derivation" $ do
    let addrOf i k = either error (show . accountAddress) (deriveNameKey testSeed i k)
    -- MetaMask accounts 1 and 2 for this phrase
    addrOf 0 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 0 1 `shouldBe` "0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
    -- Ledger Live account 2 for this phrase
    addrOf 1 0 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "derives the same secret as other wallets" $
    -- MetaMask account 1 for this phrase, as exported by "Show private key"
    either error (show . accountSecret) (deriveNameKey testSeed 0 0)
      `shouldBe` "\"0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727\""
  Hspec.it "renders the path a name key sits at" $ do
    renderNameKeyPath 0 0 `shouldBe` "m/44'/60'/0'/0/0"
    renderNameKeyPath 2 7 `shouldBe` "m/44'/60'/2'/0/7"
  Hspec.it "round-trips the phrase it was imported from" $
    recoveryKeyPhrase testSeed `shouldBe` Right testPhrase
  Hspec.it "refuses a phrase with a bad checksum" $
    importRecoveryKey (B.unwords $ replicate 12 "abandon") `shouldSatisfy` isLeft

walletTests :: SpecWith TestParams
walletTests = do
  it "creates no key until asked, then derives addresses" testWalletCreate
  it "the key and the addresses come back after a restart" testWalletPersists
  it "a second profile gets its own account, on the same key" testWalletSecondProfile
  it "imports a phrase, exports it, and refuses a second import" testWalletImport
  it "exports the secret of any name key" testWalletExportDerivedSecret
  it "deletes the key, and a key can be imported again" testWalletDelete
  it "binds a profile to the account it had" testWalletBind

nameRows :: HasCallStack => TestCC -> IO [(String, String)]
nameRows cc = mapM (\_ -> nameRow <$> getTermLine cc) [0 .. 1 :: Int]
  where
    nameRow l = case words l of
      ["name", _, path, addr] -> (path, addr)
      _ -> error $ "unexpected wallet row: " <> l

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet"
  alice <## "no wallet key"
  -- asking creates nothing
  alice ##> "/_wallet"
  alice <## "no wallet key"
  alice ##> "/_wallet export"
  alice <## "bad chat command: no wallet key on this device"
  alice ##> "/_wallet create"
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/0'/0/0", "m/44'/60'/0'/0/1"]
  length (nub $ map snd rows) `shouldBe` 2

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  rows <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create"
    nameRows alice
  -- same database, new session: a name bought at that address must stay reachable
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet"
    rows' <- nameRows alice
    rows' `shouldBe` rows

testWalletSecondProfile :: HasCallStack => TestParams -> IO ()
testWalletSecondProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create"
  rows <- nameRows alice
  alice ##> "/_wallet export"
  phrase <- getTermLine alice
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- other profiles are named, never numbered
  alice ##> "/_wallet"
  alice <## "this profile has no wallet key"
  alice <## "also on this key: alice"
  -- the key belongs to the device, so a profile without an account exports it too
  alice ##> "/_wallet export"
  alice <## phrase
  alice ##> "/_wallet create"
  rows' <- nameRows alice
  alice <## "also on this key: alice"
  map fst rows' `shouldBe` ["m/44'/60'/1'/0/0", "m/44'/60'/1'/0/1"]
  null (map snd rows `intersect` map snd rows') `shouldBe` True

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "name 0  m/44'/60'/0'/0/0  0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
  alice <## "name 1  m/44'/60'/0'/0/1  0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
  alice ##> "/_wallet export"
  alice <## B.unpack testPhrase
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "bad chat command: this device already has a wallet key"
  -- a mistyped phrase says nothing about which word was wrong
  alice ##> ("/_wallet import " <> B.unpack (B.unwords $ replicate 12 "abandon"))
  alice <## "bad chat command: bad recovery phrase"

testWalletExportDerivedSecret :: HasCallStack => TestParams -> IO ()
testWalletExportDerivedSecret ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  alice ##> "/_wallet export 0 0"
  alice <## "m/44'/60'/0'/0/0  0x9858EfFD232B4033E47d90003D41EC34EcaEda94  0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
  -- an index BIP-32 cannot harden is rejected, not wrapped into another account
  alice ##> "/_wallet export 4294967296 0"
  alice <## "bad chat command: Failed reading: empty"
  -- any path derives, whether or not a profile holds that account
  alice ##> "/_wallet export 3 7"
  l <- getTermLine alice
  case words l of
    [path, addr, secret] -> do
      path `shouldBe` "m/44'/60'/3'/0/7"
      length addr `shouldBe` 42
      length secret `shouldBe` 66
    _ -> error $ "unexpected export row: " <> l

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  alice ##> "/_wallet delete"
  alice <## "no wallet key"
  -- deleting unbinds the profile, so a key can be imported again
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  pure ()

-- | Restoring a chat database older than the key rebinds profiles in the order
-- they ask, so the account a profile had is set by hand.
testWalletBind :: HasCallStack => TestParams -> IO ()
testWalletBind ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  alice ##> "/_wallet bind 3"
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/3'/0/0", "m/44'/60'/3'/0/1"]
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet bind 3"
  alice <## "bad chat command: another profile uses this account"
  -- the counter moved past the account bound by hand
  alice ##> "/_wallet create"
  rows' <- nameRows alice
  alice <## "also on this key: alice"
  map fst rows' `shouldBe` ["m/44'/60'/4'/0/0", "m/44'/60'/4'/0/1"]
