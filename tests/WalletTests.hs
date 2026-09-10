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
import Simplex.Chat.Wallet (AccountIndex, NameIndex, WalletSeed (..), deriveNameKey, importRecoveryKey, nameKeySecret, recoveryKeyPhrase, renderNameKeyPath, seedMaster)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The standard BIP-39 test vector, so the addresses can be checked elsewhere.
testPhrase :: ByteString
testPhrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

testSeed :: WalletSeed
testSeed = WalletSeed {wsId = 1, wsEntropy = either error id $ importRecoveryKey testPhrase}

nameKey :: AccountIndex -> NameIndex -> Either String S.PrivateKey
nameKey acc nm = seedMaster testSeed >>= \m -> deriveNameKey m acc nm

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "name keys line up with other wallets' derivation" $ do
    let addrOf i k = either error (show . addressFromPrivateKey) (nameKey i k)
    -- MetaMask accounts 1 and 2 for this phrase
    addrOf 0 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 0 1 `shouldBe` "0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
    -- Ledger Live account 2 for this phrase
    addrOf 1 0 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "derives the same secret as other wallets" $
    -- MetaMask account 1 for this phrase, as exported by "Show private key"
    either error nameKeySecret (nameKey 0 0)
      `shouldBe` "0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
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
  it "binds a profile once, and only to an account BIP-32 can harden" testWalletBindLimits
  it "needs no import when the database was backed up after the key" testWalletBackupAfterKey
  it "rebinds by index when the database was backed up before the key" testWalletBackupBeforeKey
  it "discards a key imported before the database is restored" testWalletImportThenRestore

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
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/0'/0/0", "m/44'/60'/0'/0/1"]
  length (nub $ map snd rows) `shouldBe` 2

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  rows <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create"
    alice <## "no account for this profile"
    alice ##> "/_wallet bind"
    nameRows alice
  -- same database, new session: a name bought at that address must stay reachable
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet"
    rows' <- nameRows alice
    rows' `shouldBe` rows

testWalletSecondProfile :: HasCallStack => TestParams -> IO ()
testWalletSecondProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create"
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
  rows <- nameRows alice
  alice ##> "/_wallet export"
  phrase <- getTermLine alice
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- other profiles are named, never numbered
  alice ##> "/_wallet"
  alice <## "no account for this profile"
  alice <## "also on same seed: alice"
  -- the key belongs to the device, so a profile without an account exports it too
  alice ##> "/_wallet export"
  alice <## phrase
  alice ##> "/_wallet create"
  alice <## "bad chat command: this device already has a wallet key"
  alice ##> "/_wallet bind"
  rows' <- nameRows alice
  alice <## "also on same seed: alice"
  map fst rows' `shouldBe` ["m/44'/60'/1'/0/0", "m/44'/60'/1'/0/1"]
  null (map snd rows `intersect` map snd rows') `shouldBe` True

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  -- import binds nothing: which account a profile had is what it is recovering
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
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
  -- the secret of a name key needs no profile bound to that account
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet export 0 0"
  alice <## "m/44'/60'/0'/0/0  0x9858EfFD232B4033E47d90003D41EC34EcaEda94  0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
  -- an index BIP-32 cannot harden is rejected, not wrapped into another account
  alice ##> "/_wallet export 4294967296 0"
  alice <## "bad chat command: Failed reading: empty"
  -- any path derives, whether or not a profile holds that account
  alice ##> "/_wallet export 3 7"
  alice <## "m/44'/60'/3'/0/7  0xb8cb8628d242fF621adb05E75b7bF16c9b496740  0x5fa3f03c127d150c82f54291f9989c955c3857a54df7abbb50e28199a0bbaac1"
  -- a secret whose first byte is zero keeps its 64 hex digits
  alice ##> "/_wallet export 0 15"
  alice <## "m/44'/60'/0'/0/15  0xa25d37554EB084969C85362f7E6B1A6108e51d0e  0x009a1ccd9c667416d9db6246a35d022b1799517c0cd8547bb07ce280c119ae3c"

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
  _ <- nameRows alice
  alice ##> "/_wallet delete"
  alice <## "no wallet key"
  -- deleting unbinds the profile, so a key can be imported again
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"

-- | A profile says which account was its, as nothing else knows.
testWalletBind :: HasCallStack => TestParams -> IO ()
testWalletBind ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind 3"
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/3'/0/0", "m/44'/60'/3'/0/1"]
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet bind 3"
  alice <## "bad chat command: another profile uses this account"
  -- the counter moved past the account bound by hand
  alice ##> "/_wallet bind"
  rows' <- nameRows alice
  alice <## "also on same seed: alice"
  map fst rows' `shouldBe` ["m/44'/60'/4'/0/0", "m/44'/60'/4'/0/1"]

-- | The state a chat database backed up before the key restores to.
forgetKey :: HasCallStack => TestCC -> IO ()
forgetKey cc = do
  cc ##> "/sql chat UPDATE users SET wallet_seed_id = NULL, wallet_account_index = NULL"
  cc ##> "/sql chat DELETE FROM wallet_seeds"

testWalletBackupAfterKey :: HasCallStack => TestParams -> IO ()
testWalletBackupAfterKey ps = do
  rows <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create"
    alice <## "no account for this profile"
    alice ##> "/_wallet bind"
    nameRows alice
  -- the key and the binding are both in the database, so the restore is all of it
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet"
    rows' <- nameRows alice
    rows' `shouldBe` rows
    alice ##> ("/_wallet import " <> B.unpack testPhrase)
    alice <## "bad chat command: this device already has a wallet key"

testWalletBackupBeforeKey :: HasCallStack => TestParams -> IO ()
testWalletBackupBeforeKey ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind 1"
  aliceRows <- nameRows alice
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet bind 0"
  alisaRows <- nameRows alice
  alice <## "also on same seed: alice"
  forgetKey alice
  alice ##> "/_wallet"
  alice <## "no wallet key"
  -- the phrase alone puts every account back, and each profile says which was its
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind 0"
  alisaRows' <- nameRows alice
  alisaRows' `shouldBe` alisaRows
  alice ##> "/user alice"
  showActiveUser alice "alice (Alice)"
  alice ##> "/_wallet bind 1"
  aliceRows' <- nameRows alice
  alice <## "also on same seed: alisa"
  aliceRows' `shouldBe` aliceRows

testWalletImportThenRestore :: HasCallStack => TestParams -> IO ()
testWalletImportThenRestore ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  -- restoring the database replaces the key with what the backup held, which is nothing
  forgetKey alice
  alice ##> "/_wallet"
  alice <## "no wallet key"
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/0'/0/0", "m/44'/60'/0'/0/1"]

testWalletBindLimits :: HasCallStack => TestParams -> IO ()
testWalletBindLimits ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "no account for this profile"
  alice ##> "/_wallet bind"
  _ <- nameRows alice
  -- a profile that has an account asks for another one by number
  alice ##> "/_wallet bind"
  alice <## "bad chat command: this profile already has an account"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet bind 2147483647"
  rows <- nameRows alice
  alice <## "also on same seed: alice"
  map fst rows `shouldBe` ["m/44'/60'/2147483647'/0/0", "m/44'/60'/2147483647'/0/1"]
  -- the counter is past what BIP-32 can harden, where it would repeat account 0
  alice ##> "/create user carol"
  showActiveUser alice "carol"
  alice ##> "/_wallet bind"
  alice <## "bad chat command: no free account on this key"
