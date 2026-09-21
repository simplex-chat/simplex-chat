{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (isRight)
import Data.List (nub)
import qualified Data.Text as T
import Simplex.Chat.Wallet (AccountIndex, WalletError (..), WalletSeed (..), accountSecret, deriveAccountKey, entropyFromMnemonic, renderAccountPath, seedMaster, seedMnemonic)
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Simplex.Messaging.Util (safeDecodeUtf8)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The standard BIP-39 test vector, 12 words, so the addresses below can be
-- checked against any other wallet. Importing takes 24 words, so this phrase is
-- only used for derivation, never through a command.
testPhrase12 :: ByteString
testPhrase12 = B.unwords $ replicate 11 "abandon" <> ["about"]

-- | The 24 word all-zero-entropy vector, the length the commands take. It is a
-- different seed from the 12 word one, so it reaches different addresses.
testPhrase24 :: ByteString
testPhrase24 = B.unwords $ replicate 23 "abandon" <> ["art"]

seedFromPhrase :: ByteString -> WalletSeed
seedFromPhrase phrase =
  WalletSeed {wsId = 1, wsEntropy = BA.convert . B39.mnemonicToEntropy . either error id $ B39.parseMnemonic phrase}

accountKey :: WalletSeed -> AccountIndex -> S.PrivateKey
accountKey seed n = either (error . show) id $ seedMaster seed >>= \m -> deriveAccountKey m n

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "accounts are the accounts another wallet derives for the same phrase" $ do
    let addrOf = show . addressFromPrivateKey . accountKey (seedFromPhrase testPhrase12)
    -- Ledger Live accounts 1 and 2 for this phrase, the published values for it
    addrOf 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 1 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "the exported secret is the one another wallet shows for that account" $ do
    let k = accountKey (seedFromPhrase testPhrase12) 0
    -- as a wallet shows it for m/44'/60'/0'/0/0 of this phrase
    accountSecret k `shouldBe` "0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
  Hspec.it "every account has its own address" $ do
    let seed = seedFromPhrase testPhrase12
        addrs = map (show . addressFromPrivateKey . accountKey seed) [0 .. 9]
    length (nub addrs) `shouldBe` 10
  Hspec.it "a secret whose first byte is zero keeps its 64 hex digits" $ do
    let k = either error id . S.mkPrivateKey $ B.pack ('\0' : replicate 31 '\1')
        secret = T.unpack $ accountSecret k
    take 4 secret `shouldBe` "0x00"
    length secret `shouldBe` 66
  Hspec.it "renders the path an account sits at" $ do
    renderAccountPath 0 `shouldBe` "m/44'/60'/0'/0/0"
    renderAccountPath 7 `shouldBe` "m/44'/60'/7'/0/0"
  Hspec.it "round-trips the phrase it was imported from" $
    seedMnemonic (seedFromPhrase testPhrase24) `shouldBe` Right (safeDecodeUtf8 testPhrase24)
  Hspec.it "takes 24 words only, with a valid checksum" $ do
    entropyFromMnemonic testPhrase24 `shouldSatisfy` isRight
    entropyFromMnemonic testPhrase12 `shouldBe` Left WEBadMnemonic
    entropyFromMnemonic (B.unwords $ replicate 24 "abandon") `shouldBe` Left WEBadMnemonic

testWalletHiddenProfile :: HasCallStack => TestParams -> IO ()
testWalletHiddenProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/hide user my_password"
  alice <## "current user alisa:"
  alice <## "messages are hidden (use /tail to view)"
  alice <## "profile is hidden"
  alice ##> "/_wallet bind"
  alice <## "wallet: a hidden profile cannot own an account"

testWalletExportNotMine :: HasCallStack => TestParams -> IO ()
testWalletExportNotMine ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind"
  alice <## "accounts: 0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- account 0 is the other profile's, and its key is not this profile's to take
  alice ##> "/_wallet export account 0"
  alice <## "wallet: another profile holds this account"
  -- an account nobody holds is still derivable, which is what a scan needs
  alice ##> "/_wallet export account 7"
  row <- getTermLine alice
  words row !! 1 `shouldBe` "m/44'/60'/7'/0/0"

testWalletIndexTooLarge :: HasCallStack => TestParams -> IO ()
testWalletIndexTooLarge ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  -- 2^31 is already a hardened component, so it would derive account 0's key
  alice ##> "/_wallet address account=2147483648"
  alice <## "wallet: account index is too large to harden"
  alice ##> "/_wallet bind account=2147483648"
  alice <## "wallet: account index is too large to harden"
  alice ##> "/_wallet export account 2147483648"
  alice <## "wallet: account index is too large to harden"
  -- the largest index that can be hardened is usable, and the counter follows it
  alice ##> "/_wallet bind account=2147483647"
  alice <## "accounts: 2147483647"
  alice ##> "/_wallet bind"
  alice <## "wallet: account index is too large to harden"

-- | The address a wallet reaches when the secret is imported as a private key.
addressFromSecret :: String -> String
addressFromSecret secret =
  show . addressFromPrivateKey . either error id . S.mkPrivateKey . either error id $
    BAE.convertFromBase BAE.Base16 (B.drop 2 $ B.pack secret)

walletTests :: SpecWith TestParams
walletTests = do
  it "creates no wallet until asked, and only one" testWalletCreate
  it "binds the next free account, and re-binding one it holds changes nothing" testWalletBind
  it "keeps each profile's accounts apart" testWalletAccountsPerProfile
  it "taking the next account skips one already bound by index" testWalletBindByIndexThenNext
  it "derives an address without taking it" testWalletAddress
  it "exports the master phrase and one account's secret" testWalletExport
  it "will not take a new account on an imported phrase" testWalletImport
  it "the wallet and the accounts come back after a restart" testWalletPersists
  it "deletes the wallet, and one can be made again" testWalletDelete
  it "a hidden profile is bound no account" testWalletHiddenProfile
  it "will not export an account another profile holds" testWalletExportNotMine
  it "refuses an index BIP-32 cannot harden, on every command" testWalletIndexTooLarge

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet"
  alice <## "no wallet on this device"
  -- reading creates nothing
  alice ##> "/_wallet"
  alice <## "no wallet on this device"
  alice ##> "/_wallet export master"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet create new"
  alice <## "wallet: this device already has a wallet"
  alice ##> "/_wallet delete"
  alice <## "ok"
  -- a mistyped phrase says nothing about which word was wrong
  alice ##> ("/_wallet create mnemonic=" <> B.unpack (B.unwords $ replicate 24 "abandon"))
  alice <## "wallet: not a valid 24 word recovery phrase"

testWalletBind :: HasCallStack => TestParams -> IO ()
testWalletBind ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind"
  alice <## "accounts: 0"
  -- a profile owns as many accounts as it owns names
  alice ##> "/_wallet bind"
  alice <## "accounts: 0, 1"
  -- binding one it already holds changes nothing
  alice ##> "/_wallet bind account=0"
  alice <## "accounts: 0, 1"

testWalletAccountsPerProfile :: HasCallStack => TestParams -> IO ()
testWalletAccountsPerProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind"
  alice <## "accounts: 0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- the wallet is the device's, the accounts are the profile's
  alice ##> "/_wallet"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind"
  alice <## "accounts: 1"
  alice ##> "/_wallet bind account=0"
  alice <## "wallet: another profile holds this account"

testWalletBindByIndexThenNext :: HasCallStack => TestParams -> IO ()
testWalletBindByIndexThenNext ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  -- an account a scan found, bound by index, is still taken
  alice ##> "/_wallet bind account=2"
  alice <## "accounts: 2"
  alice ##> "/_wallet bind"
  alice <## "accounts: 2, 3"
  -- accounts are listed by index, not by the order they were bound
  alice ##> "/_wallet bind account=1"
  alice <## "accounts: 1, 2, 3"
  -- and binding a low index never moves the counter back onto an account held
  alice ##> "/_wallet bind"
  alice <## "accounts: 1, 2, 3, 4"

testWalletAddress :: HasCallStack => TestParams -> IO ()
testWalletAddress ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  -- reading the next free account does not take it
  alice ##> "/_wallet address"
  addr <- getTermLine alice
  alice ##> "/_wallet address"
  addr' <- getTermLine alice
  addr' `shouldBe` addr
  words addr !! 1 `shouldBe` "m/44'/60'/0'/0/0"
  alice ##> "/_wallet address account=3"
  at3 <- getTermLine alice
  words at3 !! 1 `shouldBe` "m/44'/60'/3'/0/0"
  -- a malformed index is a parse error, never a silent bind of the next account
  alice ##> "/_wallet bind account=abc"
  alice <## "bad chat command: Failed reading: empty"
  alice ##> "/_wallet"
  alice <## "wallet, no accounts for this profile"
  -- an index BIP-32 cannot harden is refused rather than folded onto a low one
  alice ##> "/_wallet address account=2147483648"
  alice <## "wallet: account index is too large to harden"

testWalletExport :: HasCallStack => TestParams -> IO ()
testWalletExport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet export master"
  alice <## B.unpack testPhrase24
  alice ##> "/_wallet export account 0"
  row <- getTermLine alice
  case words row of
    [idx, path, address, secret] -> do
      idx `shouldBe` "0"
      path `shouldBe` "m/44'/60'/0'/0/0"
      -- m/44'/60'/0'/0/0 of the 24 word vector, pinned outside this
      -- implementation, so a change of path fails here rather than shipping
      address `shouldBe` "0xF278cF59F82eDcf871d630F28EcC8056f25C1cdb"
      addressFromSecret secret `shouldBe` address
    _ -> expectationFailure $ "unexpected export row: " <> row

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  -- the phrase does not say how many accounts it has been used for
  alice ##> "/_wallet bind"
  alice <## "wallet: unknown how many accounts this phrase has used, a scan of the chain has to run first"
  alice ##> "/_wallet address"
  alice <## "wallet: unknown how many accounts this phrase has used, a scan of the chain has to run first"
  -- binding an account a scan found is what a restored device does
  alice ##> "/_wallet bind account=4"
  alice <## "accounts: 4"
  -- and the counter stays unknown, because the phrase still does not say
  alice ##> "/_wallet bind"
  alice <## "wallet: unknown how many accounts this phrase has used, a scan of the chain has to run first"

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  phrase <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create new"
    alice <## "wallet, no accounts for this profile"
    alice ##> "/_wallet bind account=2"
    alice <## "accounts: 2"
    alice ##> "/_wallet export master"
    getTermLine alice
  -- same database, new session: an account holding a name must stay reachable
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet"
    alice <## "accounts: 2"
    alice ##> "/_wallet export master"
    alice <## phrase

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind account=1"
  alice <## "accounts: 1"
  alice ##> "/_wallet delete"
  alice <## "ok"
  alice ##> "/_wallet"
  alice <## "no wallet on this device"
  -- the accounts went with the entropy they were counted against
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
