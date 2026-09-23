{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module WalletTests where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Database.SQLite.Simple (Only (..))
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Either (isRight)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import NameResolver (ownedName)
import Simplex.Chat.Wallet (AccountIndex, WalletError (..), accountSecret, deriveAccountKey, entropyFromMnemonic, renderAccountPath, seedMaster, seedMnemonic)
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Agent.Store.AgentStore (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Simplex.Messaging.Names.Record (NameRecord (..), NameRegistration (..), NameResponse (..))
import Simplex.Messaging.Util (decodeJSON, safeDecodeUtf8)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

-- | The standard BIP-39 test vector, 12 words, to check the addresses against another wallet.
testPhrase12 :: ByteString
testPhrase12 = B.unwords $ replicate 11 "abandon" <> ["about"]

-- | The 24 word all-zero-entropy vector, the length the commands take.
testPhrase24 :: ByteString
testPhrase24 = B.unwords $ replicate 23 "abandon" <> ["art"]

seedEntropy :: ByteString -> BA.ScrubbedBytes
seedEntropy phrase = BA.convert . B39.mnemonicToEntropy . either error id $ B39.parseMnemonic phrase

accountKey :: BA.ScrubbedBytes -> AccountIndex -> S.Secp256k1PrivateKey
accountKey entropy n = either (error . show) id $ seedMaster entropy >>= \m -> deriveAccountKey m n

-- | The address a wallet reaches when the secret is imported as a private key.
addressFromSecret :: String -> String
addressFromSecret secret =
  show . addressFromPrivateKey . either error id . S.mkPrivateKey . either error id $
    BAE.convertFromBase BAE.Base16 (B.drop 2 $ B.pack secret)

-- | The address of an account of the imported phrase, as the chain would hold it.
accountAddress :: AccountIndex -> Text
accountAddress = T.pack . show . addressFromPrivateKey . accountKey (seedEntropy testPhrase24)

-- | An @export account@ row: the index, the path, the address, the secret.
exportRow :: HasCallStack => String -> (String, String, String, String)
exportRow row = case words row of
  [idx, path, address, secret] -> (idx, path, address, secret)
  _ -> error $ "unexpected export row: " <> row

walletDerivationTests :: Spec
walletDerivationTests = do
  Hspec.it "accounts are the accounts another wallet derives for the same phrase" $ do
    let addrOf = show . addressFromPrivateKey . accountKey (seedEntropy testPhrase12)
    -- Ledger Live accounts 1 and 2 for this phrase, the published values for it
    addrOf 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 1 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  Hspec.it "the exported secret is the one another wallet shows for that account" $ do
    let k = accountKey (seedEntropy testPhrase12) 0
    -- as a wallet shows it for m/44'/60'/0'/0/0 of this phrase
    accountSecret k `shouldBe` "0x1ab42cc412b618bdea3a599e3c9bae199ebf030895b039e9db1e30dafb12b727"
  Hspec.it "every account has its own address" $ do
    let entropy = seedEntropy testPhrase12
        addrs = map (show . addressFromPrivateKey . accountKey entropy) [0 .. 9]
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
    seedMnemonic (seedEntropy testPhrase24) `shouldBe` Right (safeDecodeUtf8 testPhrase24)
  Hspec.it "takes 24 words only, with a valid checksum" $ do
    entropyFromMnemonic testPhrase24 `shouldSatisfy` isRight
    entropyFromMnemonic testPhrase12 `shouldBe` Left WEBadMnemonic
    entropyFromMnemonic (B.unwords $ replicate 24 "abandon") `shouldBe` Left WEBadMnemonic

walletTests :: SpecWith TestParams
walletTests = do
  it "creates no wallet until asked, and only one" testWalletCreate
  it "binds the next free account, and re-binding one it holds changes nothing" testWalletBind
  it "keeps each profile's accounts apart" testWalletAccountsPerProfile
  it "taking the next account skips one already bound by index" testWalletBindByIndexThenNext
  it "leaves a deleted profile's account for another profile to take" testWalletDeletedProfileAccount
  it "derives an address without taking it" testWalletAddress
  it "exports the master phrase and one account's secret" testWalletExport
  it "will not take a new account on an imported phrase" testWalletImport
  it "the wallet, the accounts and the counter come back after a restart" testWalletPersists
  it "deletes the wallet, and one can be made again" testWalletDelete
  it "a hidden profile is bound no account" testWalletHiddenProfile
  it "will not export an account another profile holds" testWalletExportNotMine
  it "refuses an index BIP-32 cannot harden, on every command" testWalletIndexTooLarge

-- | Its own group: the scan needs a names resolver, so it runs without the SMP server the other wallet tests share.
walletScanTests :: SpecWith TestParams
walletScanTests = do
  it "a scan of a recovered phrase finds the accounts in use" testWalletScan
  it "the counter a scan sets clears the accounts already bound" testWalletScanCounterPastBound
  it "a hidden profile cannot scan" testWalletScanHiddenProfile
  it "a profile with no names says so" testWalletNamesEmpty

testWalletScan :: HasCallStack => TestParams -> IO ()
testWalletScan ps = withSmpServerAndNames $ \reg -> withNewTestChat ps "alice" aliceProfile $ \alice -> do
  enableNamesRole alice
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  ownedName reg "alice.simplex" (accountAddress 1)
  alice ##> "/_wallet scan"
  alice <## "accounts: 1"
  -- the names the scan saw are recorded against the account that holds them
  alice ##> "/_wallet names"
  alice <## "1  alice.simplex"
  names <- withCCTransaction alice $ \db -> DB.query_ db "SELECT name_response FROM wallet_owned_names"
  map (\(Only r) -> registeredName <$> decodeJSON r) names `shouldBe` [Just (Just "alice.simplex")]
  -- the scan gives the imported phrase the counter it had none of
  alice ##> "/_wallet bind"
  alice <## "accounts: 1, 2"

testWalletCreate :: HasCallStack => TestParams -> IO ()
testWalletCreate ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet"
  alice <## "no wallet on this device"
  -- reading creates nothing
  alice ##> "/_wallet"
  alice <## "no wallet on this device"
  alice ##> "/_wallet export master"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet bind"
  alice <## "wallet: this device has no wallet"
  alice ##> "/_wallet delete"
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
  -- a phrase is taken as a backup card writes it, case and all
  alice ##> ("/_wallet create mnemonic=" <> map toUpper (B.unpack testPhrase24))
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet export master"
  alice <## B.unpack testPhrase24

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

testWalletDeletedProfileAccount :: HasCallStack => TestParams -> IO ()
testWalletDeletedProfileAccount ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind"
  alice <## "accounts: 0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- deleting a profile does not take its account with it
  alice ##> "/delete user alice"
  alice <### ["ok", "completed deleting user"]
  alice ##> "/_wallet"
  alice <## "wallet, no accounts for this profile"
  -- and another profile can take it, which is how a name outlives its profile
  alice ##> "/_wallet bind account=0"
  alice <## "accounts: 0"

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
  -- a zero-padded index is the same index
  alice ##> "/_wallet address account=0000000003"
  getTermLine alice `shouldReturn` at3
  -- a malformed index is a parse error, never a silent bind of the next account
  alice ##> "/_wallet bind account=abc"
  alice <## "bad chat command: Failed reading: empty"
  alice ##> "/_wallet"
  alice <## "wallet, no accounts for this profile"

testWalletExport :: HasCallStack => TestParams -> IO ()
testWalletExport ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet export master"
  alice <## B.unpack testPhrase24
  alice ##> "/_wallet export account 0"
  (idx, path, address, secret) <- exportRow <$> getTermLine alice
  idx `shouldBe` "0"
  path `shouldBe` "m/44'/60'/0'/0/0"
  -- m/44'/60'/0'/0/0 of the 24 word vector, pinned so a change of path fails here
  address `shouldBe` "0xF278cF59F82eDcf871d630F28EcC8056f25C1cdb"
  addressFromSecret secret `shouldBe` address
  -- the index reaches the key, not only the path printed beside it
  alice ##> "/_wallet export account 1"
  (idx', path', address', _) <- exportRow <$> getTermLine alice
  idx' `shouldBe` "1"
  path' `shouldBe` "m/44'/60'/1'/0/0"
  address' `shouldBe` show (addressFromPrivateKey $ accountKey (seedEntropy testPhrase24) 1)
  -- and an address is read from the account the command names, not the counter
  alice ##> "/_wallet address account=1"
  addressRow <- words <$> getTermLine alice
  addressRow `shouldBe` ["1", "m/44'/60'/1'/0/0", address']

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

-- | A scan that finds nothing must not point the counter at an account a profile already holds, or the next bind can never take one.
testWalletScanCounterPastBound :: HasCallStack => TestParams -> IO ()
testWalletScanCounterPastBound ps = withSmpServerAndNames $ \_reg -> withNewTestChat ps "alice" aliceProfile $ \alice -> do
  enableNamesRole alice
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet bind account=5"
  alice <## "accounts: 5"
  alice ##> "/_wallet scan"
  alice <## "accounts: 5"
  alice ##> "/_wallet bind"
  alice <## "accounts: 5, 6"

-- | Names come from a scan, so a profile that has not scanned has none to list.
testWalletNamesEmpty :: HasCallStack => TestParams -> IO ()
testWalletNamesEmpty ps = withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  alice ##> "/_wallet names"
  alice <## "wallet, no names for this profile"

-- | A scan binds what it finds, so it is refused where a bind is.
testWalletScanHiddenProfile :: HasCallStack => TestParams -> IO ()
testWalletScanHiddenProfile ps = withSmpServerAndNames $ \_reg -> withNewTestChat ps "alice" aliceProfile $ \alice -> do
  alice ##> ("/_wallet create mnemonic=" <> B.unpack testPhrase24)
  alice <## "wallet, no accounts for this profile"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  alice ##> "/hide user my_password"
  alice <## "current user alisa:"
  alice <## "messages are hidden (use /tail to view)"
  alice <## "profile is hidden"
  alice ##> "/_wallet scan"
  alice <## "wallet: a hidden profile cannot own an account"

-- | The name a stored NameResponse carries, which is what makes the row findable again.
registeredName :: NameResponse -> Maybe Text
registeredName NameResponse {registration} = case registration of
  NRRegistered {nameRecord} -> Just $ nrName nameRecord
  _ -> Nothing

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
    -- the counter came back too, so no account is handed out a second time
    alice ##> "/_wallet bind"
    alice <## "accounts: 2, 3"

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
  alice ##> "/_wallet create new"
  alice <## "wallet, no accounts for this profile"
  -- the new wallet holds no account and its counter starts over
  alice ##> "/_wallet bind"
  alice <## "accounts: 0"

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
  -- the profile's own account is its to export
  alice ##> "/_wallet export account 0"
  (_, path, _, _) <- exportRow <$> getTermLine alice
  path `shouldBe` "m/44'/60'/0'/0/0"
  alice ##> "/create user alisa"
  showActiveUser alice "alisa"
  -- account 0 is the other profile's, and its key is not this profile's to take
  alice ##> "/_wallet export account 0"
  alice <## "wallet: another profile holds this account"
  -- an account nobody holds is still derivable, which is what a scan needs
  alice ##> "/_wallet export account 7"
  (_, path', _, _) <- exportRow <$> getTermLine alice
  path' `shouldBe` "m/44'/60'/7'/0/0"

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
