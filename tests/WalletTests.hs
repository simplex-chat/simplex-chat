{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Monad (replicateM_)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Simplex.Chat.Help (walletHelpInfo)
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
  it "creates no key until asked, then shows the next name's address" testWalletCreate
  it "the key and the address come back after a restart" testWalletPersists
  it "a second profile gets its own account" testWalletSecondProfile
  it "imports a phrase, exports it, and refuses a second import" testWalletImport

-- | The address a name would be bought at, and the path to reach it from the
-- phrase. Returned so tests can compare addresses without pinning a random one.
nextNameAddress :: HasCallStack => TestCC -> Int -> IO String
nextNameAddress cc acct = do
  cc <## ("wallet account " <> show acct)
  addr <- getTermLine cc
  cc <## ("  at m/44'/60'/" <> show acct <> "'/0/0")
  pure addr

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/wallet"
  alice <## "no wallet key on this device - create one with /wallet create"
  -- asked again: still nothing, and nothing was created by asking
  alice ##> "/wallet"
  alice <## "no wallet key on this device - create one with /wallet create"
  alice ##> "/wallet export"
  alice <## "bad chat command: no wallet key on this device"
  alice ##> "/wallet create"
  _ <- nextNameAddress alice 0
  alice ##> "/help wallet"
  alice <## "Your wallet key:"
  replicateM_ (length walletHelpInfo - 1) (getTermLine alice)

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  addr <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/wallet create"
    nextNameAddress alice 0
  -- same database, new session: the seed has to come back from the DB, or the
  -- name bought at that address is unreachable
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/wallet"
    addr' <- nextNameAddress alice 0
    addr' `shouldBe` addr

testWalletSecondProfile :: HasCallStack => TestParams -> IO ()
testWalletSecondProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/wallet create"
  addr <- nextNameAddress alice 0
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/wallet"
  alice <## "wallet key on this device, but this profile has no account - add one with /wallet create"
  -- the same key, a different account, so the two profiles do not share names
  alice ##> "/wallet create"
  addr' <- nextNameAddress alice 1
  addr' `shouldNotBe` addr

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "wallet account 0"
  alice <## "next name will be owned by 0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
  alice <## "  at m/44'/60'/0'/0/0"
  alice ##> "/wallet export"
  alice <## "write this down - anyone who knows these words controls the names this key owns:"
  alice <## ("  " <> B.unpack testPhrase)
  -- one key per device: a second would make the address shown depend on which
  -- key was picked
  alice ##> ("/wallet import " <> B.unpack testPhrase)
  alice <## "bad chat command: this device already has a wallet key"
