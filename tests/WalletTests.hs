{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.List (nub)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Chat.Wallet (NameIndex, WalletSeed (..), deriveNameKey, importRecoveryKey, nameKeySecret, recoveryKeyPhrase, renderNameKeyPath, seedMaster)
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The standard BIP-39 test vector, so the addresses can be checked elsewhere.
testPhrase :: ByteString
testPhrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

testSeed :: WalletSeed
testSeed = WalletSeed {wsId = 1, wsEntropy = either error id $ importRecoveryKey testPhrase}

nameKey :: NameIndex -> Either String S.PrivateKey
nameKey nm = seedMaster testSeed >>= \m -> deriveNameKey m nm

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "name keys line up with other wallets' derivation" $ do
    let addrOf k = either error (show . addressFromPrivateKey) (nameKey k)
    -- MetaMask accounts 2 and 3 for this phrase; account 1 is the unused m/44'/60'/0'/0/0
    addrOf 1 `shouldBe` "0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
    addrOf 2 `shouldBe` "0xb6716976A3ebe8D39aCEB04372f22Ff8e6802D7A"
  Hspec.it "derives the same secret as other wallets" $
    -- MetaMask account 2 for this phrase, as exported by "Show private key"
    either error nameKeySecret (nameKey 1)
      `shouldBe` "0x9a983cb3d832fbde5ab49d692b7a8bf5b5d232479c99333d0fc8e1d21f1b55b6"
  Hspec.it "renders the path a name key sits at" $ do
    renderNameKeyPath 1 `shouldBe` "m/44'/60'/0'/0/1"
    renderNameKeyPath 7 `shouldBe` "m/44'/60'/0'/0/7"
  Hspec.it "round-trips the phrase it was imported from" $
    recoveryKeyPhrase testSeed `shouldBe` Right testPhrase
  Hspec.it "refuses a phrase with a bad checksum" $
    importRecoveryKey (B.unwords $ replicate 12 "abandon") `shouldSatisfy` isLeft

walletTests :: SpecWith TestParams
walletTests = do
  it "creates no seed until asked, then derives addresses" testWalletCreate
  it "the seed and the addresses come back after a restart" testWalletPersists
  it "every profile sees the same names, as the seed is the device's" testWalletSharedByProfiles
  it "imports a phrase, exports it, and refuses a second import" testWalletImport
  it "exports the secret of any name key" testWalletExportDerivedSecret
  it "deletes the seed, and a seed can be imported again" testWalletDelete
  it "discards a seed imported before the database is restored" testWalletImportThenRestore

-- | The state a chat database backed up before the seed restores to.
forgetSeed :: HasCallStack => TestCC -> IO ()
forgetSeed cc = cc ##> "/sql chat DELETE FROM wallet_seeds"

nameRows :: HasCallStack => TestCC -> IO [(String, String)]
nameRows cc = mapM (\_ -> nameRow <$> getTermLine cc) [0 .. 1 :: Int]
  where
    nameRow l = case words l of
      [path, addr] -> (path, addr)
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
  map fst rows `shouldBe` ["m/44'/60'/0'/0/1", "m/44'/60'/0'/0/2"]
  length (nub $ map snd rows) `shouldBe` 2
  -- create is for the seed, and this device has one
  alice ##> "/_wallet create"
  alice <## "bad chat command: this device already has a wallet key"

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

testWalletSharedByProfiles :: HasCallStack => TestParams -> IO ()
testWalletSharedByProfiles ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  rows <- nameRows alice
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- the seed belongs to the device, so a name is not a profile's to see or not
  alice ##> "/_wallet"
  rows' <- nameRows alice
  rows' `shouldBe` rows
  alice ##> "/_wallet export"
  alice <## B.unpack testPhrase

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  alice <## "m/44'/60'/0'/0/1  0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
  alice <## "m/44'/60'/0'/0/2  0xb6716976A3ebe8D39aCEB04372f22Ff8e6802D7A"
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
  alice ##> "/_wallet export 1"
  alice <## "m/44'/60'/0'/0/1  0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0  0x9a983cb3d832fbde5ab49d692b7a8bf5b5d232479c99333d0fc8e1d21f1b55b6"
  -- a secret whose first byte is zero keeps its 64 hex digits
  alice ##> "/_wallet export 15"
  alice <## "m/44'/60'/0'/0/15  0xa25d37554EB084969C85362f7E6B1A6108e51d0e  0x009a1ccd9c667416d9db6246a35d022b1799517c0cd8547bb07ce280c119ae3c"
  -- an index BIP-32 cannot reach is rejected, not wrapped into another key
  alice ##> "/_wallet export 4294967296"
  alice <## "bad chat command: Failed reading: empty"

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  alice ##> "/_wallet delete"
  alice <## "no wallet key"
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  pure ()

testWalletImportThenRestore :: HasCallStack => TestParams -> IO ()
testWalletImportThenRestore ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  _ <- nameRows alice
  -- restoring the database replaces the seed with what the backup held, which is nothing
  forgetSeed alice
  alice ##> "/_wallet"
  alice <## "no wallet key"
  alice ##> ("/_wallet import " <> B.unpack testPhrase)
  rows <- nameRows alice
  map fst rows `shouldBe` ["m/44'/60'/0'/0/1", "m/44'/60'/0'/0/2"]
