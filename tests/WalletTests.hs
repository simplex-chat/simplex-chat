{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The wallet's derivation layout.
--
-- These tests exist because the layout is the one part of the design that
-- cannot be changed after release: altering a path once a user holds a name
-- means moving assets, and there is no safe migration. Everything here is
-- therefore a pin, not a behaviour check — if a test in this module fails, the
-- question is never "how do I update the expected value".
module WalletTests where

import Control.Concurrent.STM (TVar)
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Numeric (showHex)
import Simplex.Chat.Names.Service
import Simplex.Chat.Names.Service.Mock
import Simplex.Chat.Names.Snrc
import Simplex.Chat.Wallet
import Simplex.Chat.Wallet.Stealth
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Eth.Address (Address, checksumAddress)
import qualified Simplex.Messaging.Eth.Stealth as St
import Test.Hspec

walletTests :: Spec
walletTests = do
  describe "derivation paths" $ do
    it "main account is BIP-44 Ethereum" $
      mainPath ChainEth 0 `shouldBe` Right [0x8000002C, 0x8000003C, 0x80000000, 0, 0]
    it "main account varies only in the account level" $
      mainPath ChainEth 7 `shouldBe` Right [0x8000002C, 0x8000003C, 0x80000007, 0, 0]
    it "ethereum stealth keys use purpose 5564" $ do
      stealthSpendPath ChainEth 0 `shouldBe` Right [0x800015BC, 0x8000003C, 0x80000000, 0x80000000, 0]
      stealthViewPath ChainEth 0 `shouldBe` Right [0x800015BC, 0x8000003C, 0x80000000, 0x80000001, 0]
    it "bitcoin stealth keys use BIP-352 purpose 352" $ do
      stealthSpendPath ChainBtc 3 `shouldBe` Right [0x80000160, 0x80000000, 0x80000003, 0x80000000, 0]
      stealthViewPath ChainBtc 3 `shouldBe` Right [0x80000160, 0x80000000, 0x80000003, 0x80000001, 0]
    it "unimplemented chains fail rather than deriving something plausible" $ do
      mainPath ChainBtc 0 `shouldSatisfy` isLeft
      mainPath ChainXmr 0 `shouldSatisfy` isLeft
      stealthSpendPath ChainXmr 0 `shouldSatisfy` isLeft

  describe "account derivation" $ do
    it "derives the published BIP-44 address for the standard mnemonic" $
      -- m/44'/60'/0'/0/0 of "abandon abandon ... about", the BIP-39 all-zero
      -- entropy vector. Externally verifiable against any BIP-44 wallet.
      (checksumAddress . accountAddress <$> deriveAccount zeroSeed 0)
        `shouldBe` Right "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    it "gives every profile a distinct address" $ do
      a <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
      b <- expectRight $ accountAddress <$> deriveAccount zeroSeed 1
      a `shouldNotBe` b

  describe "stealth keys" $ do
    it "spend and view keys differ, and differ from the main key" $ do
      ks <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
      acct <- expectRight $ deriveAccount zeroSeed 0
      skSpend ks `shouldNotBe` skView ks
      skSpend ks `shouldNotBe` waKey acct
      skView ks `shouldNotBe` waKey acct
    it "differ per account, so profiles are not linkable through them" $ do
      k0 <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
      k1 <- expectRight $ deriveStealthKeys zeroSeed ChainEth 1
      accountMetaAddress k0 `shouldNotBe` accountMetaAddress k1
    it "publishes the spending key first, then the viewing key" $ do
      ks <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
      let bs = St.metaAddressBytes (accountMetaAddress ks)
      B.length bs `shouldBe` St.metaAddressSize
      B.index bs 0 `shouldSatisfy` isCompressedPrefix
      B.index bs 33 `shouldSatisfy` isCompressedPrefix
    it "is stable for a given seed and account" $ do
      -- Change-detector, not an external vector: purpose 5564 is ours, so no
      -- other wallet derives these keys. The ERC-5564 encoding around them is
      -- covered by simplexmq's own cross-checked vector. A diff here means the
      -- derivation layout moved.
      ks <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
      hex (St.metaAddressBytes $ accountMetaAddress ks)
        `shouldBe` "03dac487b400b6bdcfcbf258266638a76038d23b7c1665127eb8490c571b335b12"
          <> "024f660d285a9ab4e8e8906d423311e42c7c090289bd9a694f56ee8a0d4060918f"
    it "round-trips through the published encoding" $ do
      ks <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
      let ma = accountMetaAddress ks
      St.parseMetaAddress (St.metaAddressBytes ma) `shouldBe` Right ma

  describe "chain tags" $
    it "round-trip through their stored form" $
      map (parseChain . chainText) [ChainEth, ChainBtc, ChainXmr]
        `shouldBe` map Just [ChainEth, ChainBtc, ChainXmr]

  describe "receiving a gifted name" receivingTests

-- | The gifting path end to end against the mock chain: Bob derives a
-- destination from Alice's published meta-address with no handshake, the
-- transfer carries the announcement, and Alice finds the name by scanning
-- alone — no message, which is the case a recovery from the phrase faces.
receivingTests :: Spec
receivingTests = do
  it "the recipient finds a gift by scanning, with no message" $ do
    (c, svc, g) <- setup
    aliceKs <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
    bob <- expectRight $ deriveAccount zeroSeed 1
    _ <- giveName c "gifted.simplex" (accountAddress bob)

    -- Bob only ever sees the published meta-address.
    dest <- expectRight =<< giftDestination g (accountMetaAddress aliceKs)
    tx <- relayGift svc bob "gifted" (St.sdAddress dest) dest
    tx `shouldSatisfy` isRight

    -- Alice, with no chat message at all, scans the announcement log.
    (as, cursor) <- expectRightIO $ announcementsFrom svc Nothing
    length as `shouldBe` 1
    cursor `shouldBe` "1"
    case scanAnnouncements aliceKs as of
      [(an, addr)] -> do
        addr `shouldBe` St.sdAddress dest
        ota <- expectRight $ oneTimeAccount aliceKs (anEphemeralPubKey an)
        otaAddress ota `shouldBe` St.sdAddress dest
        owner <- expectRightIO $ resolveName svc "gifted.simplex"
        nrvOwner owner `shouldBe` otaAddress ota
      r -> expectationFailure $ "expected exactly one match, got " <> show (length r)

  it "a bystander holding the meta-address still cannot find it" $ do
    (c, svc, g) <- setup
    aliceKs <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
    malloryKs <- expectRight $ deriveStealthKeys zeroSeed ChainEth 9
    bob <- expectRight $ deriveAccount zeroSeed 1
    _ <- giveName c "gifted.simplex" (accountAddress bob)
    dest <- expectRight =<< giftDestination g (accountMetaAddress aliceKs)
    _ <- relayGift svc bob "gifted" (St.sdAddress dest) dest
    (as, _) <- expectRightIO $ announcementsFrom svc Nothing
    -- Mallory has Alice's meta-address — it is in her profile — but that is a
    -- public key pair, not a viewing key, so it locates nothing.
    scanAnnouncements malloryKs as `shouldBe` []

  it "the derived key is what signs for the name afterwards" $ do
    (c, svc, g) <- setup
    aliceKs <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
    bob <- expectRight $ deriveAccount zeroSeed 1
    _ <- giveName c "gifted.simplex" (accountAddress bob)
    dest <- expectRight =<< giftDestination g (accountMetaAddress aliceKs)
    _ <- relayGift svc bob "gifted" (St.sdAddress dest) dest
    ota <- expectRight $ oneTimeAccount aliceKs (St.sdEphemeralPubKey dest)
    -- The exported key is an ordinary secp256k1 key for that address and
    -- nothing else: this is what keeps a received name non-custodial.
    B.length (exportOneTimeKey ota) `shouldBe` 64
    owner <- expectRightIO $ resolveName svc "gifted.simplex"
    nrvOwner owner `shouldBe` otaAddress ota

  it "refuses a self-transfer, so announcements cost a real gift" $ do
    (c, svc, g) <- setup
    bob <- expectRight $ deriveAccount zeroSeed 1
    _ <- giveName c "gifted.simplex" (accountAddress bob)
    dest <- expectRight =<< giftDestination g (accountMetaAddress bobKs)
    r <- relayGift svc bob "gifted" (accountAddress bob) dest
    r `shouldBe` Left SESelfTransfer
    (as, _) <- expectRightIO $ announcementsFrom svc Nothing
    as `shouldBe` []
  where
    bobKs = either error id $ deriveStealthKeys zeroSeed ChainEth 1

setup :: IO (MockChain, NamesService, TVar ChaChaDRG)
setup = do
  c <- newMockChain
  -- No commit-reveal wait: these tests are about what happens after a name
  -- exists, not about registration timing.
  setPendingRounds c 0
  g <- C.newRandom
  pure (c, mockNamesService c, g)

-- | Put a name in the mock chain owned by @owner@, bypassing purchase.
giveName :: MockChain -> ByteString -> Address -> IO ()
giveName c name owner = do
  let svc = mockNamesService c
  pid <- expectRightIO $ buyName svc BuyRequest {brLabel = B.take (B.length name - 8) name, brOwner = owner, brYears = 1, brPayment = PPRedeemCode "test", brContactLink = Nothing, brChannelLink = Nothing}
  -- The mock writes the name on the status poll, mirroring the reveal step of
  -- commit-reveal rather than completing inside buy.
  registrationStatus svc pid >>= \case
    Right RegConfirmed {} -> pure ()
    r -> fail $ "registration did not confirm: " <> show r

relayGift :: NamesService -> WalletAccount -> ByteString -> Address -> St.StealthDestination -> IO (Either ServiceError ByteString)
relayGift svc from label to dest = do
  n <- either (error . show) id <$> currentNonce svc (accountAddress from)
  let intent = TransferName {tiFrom = accountAddress from, tiTo = to, tiLabel = label, tiNonce = n, tiDeadline = 4102444800}
      digest = either error id $ intentDigest mockDeployment intent
      sig = either error id $ signDigest from digest
  relayIntent svc SignedIntent {siIntent = intent, siSignature = sig} $
    Just Announcement {anEphemeralPubKey = St.sdEphemeralPubKey dest, anViewTag = St.sdViewTag dest}

expectRightIO :: Show a => IO (Either a b) -> IO b
expectRightIO = (>>= either (fail . show) pure)

-- | The BIP-39 all-zero entropy vector: "abandon abandon … about".
zeroSeed :: WalletSeed
zeroSeed = WalletSeed {wsId = SeedId 1, wsEntropy = B.replicate 16 0, wsBackedUp = False}

isCompressedPrefix :: Word8 -> Bool
isCompressedPrefix w = w == 0x02 || w == 0x03

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = not . isLeft

expectRight :: Show a => Either a b -> IO b
expectRight = either (fail . show) pure

hex :: ByteString -> String
hex = concatMap byte . B.unpack
  where
    byte w = let s = showHex w "" in if length s == 1 then '0' : s else s
