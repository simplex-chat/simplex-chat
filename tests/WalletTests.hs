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
import Simplex.Chat.Wallet (SeedId (..), WalletSeed (..), accountAddress, deriveNameKey, importRecoveryKey, recoveryKeyPhrase, renderNameKeyPath)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The BIP-39 test vector every wallet ships with, so the addresses below can
-- be checked against MetaMask and Ledger Live.
testPhrase :: ByteString
testPhrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

testSeed :: WalletSeed
testSeed = WalletSeed {wsId = SeedId 1, wsEntropy = either error id $ importRecoveryKey testPhrase}

-- | Derivation is the part other wallets have to agree with, so it is pinned
-- here rather than left to the end-to-end tests.
walletDerivationTests :: Spec
walletDerivationTests = do
  -- Name keys are plain BIP-44: profile 0's names are exactly MetaMask's
  -- account list (m/44'/60'/0'/0/k), and each profile's first name is the
  -- matching Ledger Live account (m/44'/60'/i'/0/0). That is what lets an owner
  -- move a single name into another wallet.
  Hspec.it "name keys line up with other wallets' derivation" $ do
    let addrOf i k = either error (show . accountAddress) (deriveNameKey testSeed i k)
    -- MetaMask accounts 1 and 2 for this phrase
    addrOf 0 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 0 1 `shouldBe` "0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
    -- Ledger Live account 2 for this phrase
    addrOf 1 0 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "renders the path a name key sits at" $ do
    renderNameKeyPath 0 0 `shouldBe` "m/44'/60'/0'/0/0"
    renderNameKeyPath 2 7 `shouldBe` "m/44'/60'/2'/0/7"
  Hspec.it "round-trips the phrase it was imported from" $
    recoveryKeyPhrase testSeed `shouldBe` Right testPhrase
  -- Without the checksum a mistyped word imports a key that owns nothing.
  Hspec.it "refuses a phrase with a bad checksum" $
    importRecoveryKey (B.unwords $ replicate 12 "abandon") `shouldSatisfy` isLeft

walletTests :: SpecWith TestParams
walletTests = do
  it "creates no key until asked, then shows the derived addresses" testWalletCreate
  it "the key and the addresses come back after a restart" testWalletPersists
  it "a second profile gets its own account, on the same key" testWalletSecondProfile
  it "imports a phrase, exports it, and refuses a second import" testWalletImport
  it "deletes the key only with the last word of the phrase" testWalletDelete

-- | The derivation path and address of each name shown for a profile's account.
accountRows :: HasCallStack => TestCC -> String -> Int -> IO [(String, String)]
accountRows cc profile acct = do
  cc <## ("  account " <> show acct <> " (" <> profile <> ")")
  mapM (\_ -> nameRow <$> getTermLine cc) [0 .. 1 :: Int]
  where
    nameRow l = case words l of
      ["name", _, path, addr] -> (path, addr)
      _ -> error $ "unexpected wallet row: " <> l

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/wallet"
  alice <## "no wallet key on this device - create one with /wallet create"
  -- asked again: still nothing, and nothing was created by asking
  alice ##> "/wallet"
  alice <## "no wallet key on this device - create one with /wallet create"
  alice ##> "/wallet export"
  alice <## "bad chat command: no wallet key for this profile - create one with /wallet create"
  alice ##> "/wallet create"
  alice <## "key 1"
  rows <- accountRows alice "alice, active" 0
  -- one key per name: the two addresses differ and sit at consecutive indices
  map fst rows `shouldBe` ["m/44'/60'/0'/0/0", "m/44'/60'/0'/0/1"]
  length (nub $ map snd rows) `shouldBe` 2

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  rows <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/wallet create"
    alice <## "key 1"
    accountRows alice "alice, active" 0
  -- same database, new session: the seed has to come back from the DB, or the
  -- name bought at that address is unreachable
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/wallet"
    alice <## "key 1"
    rows' <- accountRows alice "alice, active" 0
    rows' `shouldBe` rows

testWalletSecondProfile :: HasCallStack => TestParams -> IO ()
testWalletSecondProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/wallet create"
  alice <## "key 1"
  rows <- accountRows alice "alice, active" 0
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/wallet"
  alice <## "key 1"
  _ <- accountRows alice "alice" 0
  alice <## "this profile has no key yet - add one with /wallet create"
  -- the same key, a different account, so the two profiles do not share names
  alice ##> "/wallet create"
  alice <## "key 1"
  _ <- accountRows alice "alice" 0
  rows' <- accountRows alice "alisa, active" 1
  null (map snd rows `intersect` map snd rows') `shouldBe` True

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "key 1"
  alice <## "  account 0 (alice, active)"
  alice <## "    name 0  m/44'/60'/0'/0/0  0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
  alice <## "    name 1  m/44'/60'/0'/0/1  0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
  alice ##> "/wallet export"
  alice <## "write this down - anyone who knows these words controls the names this key owns:"
  alice <## ("  " <> B.unpack testPhrase)
  -- one key per device: a second would make the addresses shown depend on which
  -- key was picked
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "bad chat command: this device already has a wallet key"
  -- a mistyped phrase says nothing about which word was wrong
  alice ##> ("/wallet import " <> B.unpack (B.unwords $ replicate 12 "abandon"))
  alice <## "bad chat command: bad recovery phrase"

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "key 1"
  _ <- accountRows alice "alice, active" 0
  alice ##> "/wallet delete abandon"
  alice <## "bad chat command: to confirm, pass the last word of the recovery phrase"
  alice ##> "/wallet delete about"
  alice <## "no wallet key on this device - create one with /wallet create"
  -- deleting unbinds the profile, so a real phrase can now be imported
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "key 1"
  _ <- accountRows alice "alice, active" 0
  pure ()
