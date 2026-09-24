{-# LANGUAGE OverloadedStrings #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Monad (void)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Wallet (AccountIndex, AccountKey, WalletAddress (..), WalletError (..), accountSecret, deriveAccount, entropyFromMnemonic, seedMnemonic)
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

testPhrase12 :: Text
testPhrase12 = T.unwords $ replicate 11 "abandon" <> ["about"]

testPhrase24 :: Text
testPhrase24 = T.unwords $ replicate 23 "abandon" <> ["art"]

seedEntropy :: Text -> BA.ScrubbedBytes
seedEntropy phrase = B39.mnemonicToEntropy . either error id $ B39.parseMnemonic phrase

walletAccount :: BA.ScrubbedBytes -> AccountIndex -> IO (AccountKey, WalletAddress)
walletAccount entropy n = either (error . show) id <$> deriveAccount entropy n

addressFromSecret :: String -> IO String
addressFromSecret secret = do
  k <- either error id <$> S.mkPrivateKey (either error id $ BAE.convertFromBase BAE.Base16 (B.drop 2 $ B.pack secret))
  B.unpack . strEncode <$> addressFromPrivateKey k

exportRow :: HasCallStack => String -> (String, String, String, String)
exportRow row = case words row of
  [idx, path, addr, secret] -> (idx, path, addr, secret)
  _ -> error $ "unexpected export row: " <> row

accountBound :: HasCallStack => TestCC -> String -> Expectation
accountBound cc idx = (take 1 . words <$> getTermLine cc) `shouldReturn` [idx]

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "derives the addresses another wallet derives for the same phrase" $ do
    let addrOf n = address . snd <$> walletAccount (seedEntropy testPhrase12) n
    addrOf 0 `shouldReturn` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 1 `shouldReturn` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "the exported secret is the one another wallet shows for that account" $ do
    (k, _) <- walletAccount (seedEntropy testPhrase12) 0
    accountSecret k `shouldBe` "0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
  Hspec.it "every account has its own address" $ do
    addrs <- mapM (fmap (address . snd) . walletAccount (seedEntropy testPhrase12)) [0 .. 9]
    length (nub addrs) `shouldBe` 10
  Hspec.it "renders a secret whose first byte is zero with 64 hex digits" $ do
    k <- either error id <$> S.mkPrivateKey (BA.convert $ B.pack ('\0' : replicate 31 '\1'))
    let secret = T.unpack $ accountSecret k
    take 4 secret `shouldBe` "0x00"
    length secret `shouldBe` 66
  Hspec.it "renders the path an account is derived at" $ do
    (keyPath . snd <$> walletAccount (seedEntropy testPhrase12) 0) `shouldReturn` "m/44'/60'/0'/0/0"
    (keyPath . snd <$> walletAccount (seedEntropy testPhrase12) 7) `shouldReturn` "m/44'/60'/7'/0/0"
  Hspec.it "rejects an account index at or above 2^31" $
    (void <$> deriveAccount (seedEntropy testPhrase12) 2147483648) `shouldReturn` Left WEIndexTooLarge
  Hspec.it "round-trips the phrase it was imported from" $
    seedMnemonic (seedEntropy testPhrase24) `shouldBe` Right testPhrase24
  Hspec.it "accepts only 24 words with a valid checksum" $ do
    entropyFromMnemonic testPhrase24 `shouldSatisfy` isRight
    entropyFromMnemonic testPhrase12 `shouldBe` Left WEBadMnemonic
    entropyFromMnemonic (T.unwords $ replicate 24 "abandon") `shouldBe` Left WEBadMnemonic

walletTests :: SpecWith TestParams
walletTests = do
  it "creates a wallet only on the create command, and at most one" testWalletCreate
  it "binds the next free account, and re-binding an account the profile holds changes nothing" testWalletBind
  it "keeps each profile's accounts apart" testWalletAccountsPerProfile
  it "binding the next account skips one already bound by index" testWalletBindByIndexThenNext
  it "a deleted profile's account can be bound to another profile" testWalletDeletedProfileAccount
  it "derives an address without binding it" testWalletAddress
  it "exports the master phrase and one account's secret" testWalletExport
  it "does not bind the next account after an import" testWalletImport
  it "the wallet, the accounts and the counter persist across a restart" testWalletPersists
  it "deletes the wallet, and a new one can be created" testWalletDelete
  it "does not bind an account to a hidden profile" testWalletHiddenProfile
  it "exports only an account the profile holds" testWalletExportNotHeld
  it "rejects an account index at or above 2^31 on every command" testWalletIndexTooLarge

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet export master"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet bind 1"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet delete"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet create new"
  alice <## "wallet: this device already has a wallet"
  alice ##> "/_wallet delete"
  alice <## "ok"
  alice ##> ("/_wallet create mnemonic=" <> unwords (replicate 24 "abandon"))
  alice <## "wallet: not a valid 24 word recovery phrase"
  alice ##> ("/_wallet create mnemonic=" <> map toUpper (T.unpack testPhrase24))
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet export master"
  alice <## T.unpack testPhrase24

testWalletBind :: HasCallStack => TestParams -> IO ()
testWalletBind ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "0"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "1"
  alice ##> "/_wallet bind 1 account=0"
  alice `accountBound` "0"
  alice ##> "/_wallet 1"
  alice <## "accounts: 0, 1"

testWalletAccountsPerProfile :: HasCallStack => TestParams -> IO ()
testWalletAccountsPerProfile ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet 2"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 2"
  alice `accountBound` "1"
  alice ##> "/_wallet bind 2 account=0"
  alice <## "wallet: another profile holds this account"

testWalletBindByIndexThenNext :: HasCallStack => TestParams -> IO ()
testWalletBindByIndexThenNext ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1 account=2"
  alice `accountBound` "2"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "3"
  alice ##> "/_wallet bind 1 account=1"
  alice `accountBound` "1"
  alice ##> "/_wallet 1"
  alice <## "accounts: 1, 2, 3"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "4"

testWalletDeletedProfileAccount :: HasCallStack => TestParams -> IO ()
testWalletDeletedProfileAccount ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/delete user alice"
  alice <### ["ok", "completed deleting user"]
  alice ##> "/_wallet 2"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 2 account=0"
  alice `accountBound` "0"

testWalletAddress :: HasCallStack => TestParams -> IO ()
testWalletAddress ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet address"
  addr <- getTermLine alice
  alice ##> "/_wallet address"
  getTermLine alice `shouldReturn` addr
  words addr !! 1 `shouldBe` "m/44'/60'/0'/0/0"
  alice ##> "/_wallet address account=3"
  at3 <- getTermLine alice
  words at3 !! 1 `shouldBe` "m/44'/60'/3'/0/0"
  alice ##> "/_wallet address account=00000000003"
  getTermLine alice `shouldReturn` at3
  alice ##> "/_wallet bind 1 account=abc"
  alice <## "bad chat command: Failed reading: empty"
  alice ##> "/_wallet 1"
  alice <## "wallet, no accounts for this profile"

testWalletExport :: HasCallStack => TestParams -> IO ()
testWalletExport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> T.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet export master"
  alice <## T.unpack testPhrase24
  alice ##> "/_wallet bind 1 account=0"
  alice `accountBound` "0"
  alice ##> "/_wallet bind 1 account=1"
  alice `accountBound` "1"
  alice ##> "/_wallet export account 1 0"
  (idx, path, addr, secret) <- exportRow <$> getTermLine alice
  idx `shouldBe` "0"
  path `shouldBe` "m/44'/60'/0'/0/0"
  addr `shouldBe` "0xF278cF59F82eDcf871d630F28EcC8056f25C1cdb"
  addressFromSecret secret `shouldReturn` addr
  alice ##> "/_wallet export account 1 1"
  (idx', path', addr', _) <- exportRow <$> getTermLine alice
  idx' `shouldBe` "1"
  path' `shouldBe` "m/44'/60'/1'/0/0"
  (T.unpack . address . snd <$> walletAccount (seedEntropy testPhrase24) 1) `shouldReturn` addr'
  alice ##> "/_wallet address account=1"
  (words <$> getTermLine alice) `shouldReturn` ["1", "m/44'/60'/1'/0/0", addr']

testWalletImport :: HasCallStack => TestParams -> IO ()
testWalletImport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> T.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice <## "wallet: the next account is unknown after an import"
  alice ##> "/_wallet address"
  alice <## "wallet: the next account is unknown after an import"
  alice ##> "/_wallet bind 1 account=4"
  alice `accountBound` "4"
  alice ##> "/_wallet bind 1"
  alice <## "wallet: the next account is unknown after an import"

testWalletPersists :: HasCallStack => TestParams -> IO ()
testWalletPersists ps = do
  phrase <- withNewTestChat ps "alice" aliceProfile $ \alice -> do
    alice ##> "/_wallet create new"
    alice <## "wallet, no accounts for this profile"
    alice ##> "/_wallet bind 1 account=2"
    alice `accountBound` "2"
    alice ##> "/_wallet export master"
    getTermLine alice
  withTestChat ps "alice" $ \alice -> do
    alice ##> "/_wallet 1"
    alice <## "accounts: 2"
    alice ##> "/_wallet export master"
    alice <## phrase
    alice ##> "/_wallet bind 1"
    alice `accountBound` "3"

testWalletDelete :: HasCallStack => TestParams -> IO ()
testWalletDelete ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1 account=1"
  alice `accountBound` "1"
  alice ##> "/_wallet delete"
  alice <## "ok"
  alice ##> "/_wallet 1"
  alice <## "no wallet on this device"
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "0"

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
  alice ##> "/_wallet bind 2"
  alice <## "wallet: a hidden profile cannot hold an account"

testWalletExportNotHeld :: HasCallStack => TestParams -> IO ()
testWalletExportNotHeld ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind 1"
  alice `accountBound` "0"
  alice ##> "/_wallet export account 1 0"
  (_, path, _, _) <- exportRow <$> getTermLine alice
  path `shouldBe` "m/44'/60'/0'/0/0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/_wallet export account 2 0"
  alice <## "wallet: this profile does not hold this account"
  alice ##> "/_wallet export account 2 7"
  alice <## "wallet: this profile does not hold this account"

testWalletIndexTooLarge :: HasCallStack => TestParams -> IO ()
testWalletIndexTooLarge ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet address account=2147483648"
  alice <## "wallet: account index must be below 2^31"
  alice ##> "/_wallet bind 1 account=2147483648"
  alice <## "wallet: account index must be below 2^31"
  alice ##> "/_wallet export account 1 2147483648"
  alice <## "wallet: account index must be below 2^31"
  alice ##> "/_wallet address account=4294967296"
  alice <## "bad chat command: Failed reading: empty"
  alice ##> "/_wallet bind 1 account=4294967296"
  alice <## "bad chat command: Failed reading: empty"
  alice ##> "/_wallet bind 1 account=2147483647"
  alice `accountBound` "2147483647"
  alice ##> "/_wallet bind 1"
  alice <## "wallet: account index must be below 2^31"
