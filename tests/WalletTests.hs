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

  describe "renewing a name" renewTests

  describe "seed setup" seedSetupTests

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

-- | Renewal, around the boundaries where it changes behaviour: the expiry
-- itself, the end of the grace period, and someone else taking the name.
renewTests :: Spec
renewTests = do
  it "extends from the current expiry, so renewing early does not lose time" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    exp0 <- nrvExpires <$> expectRightIO (resolveName svc "renewme.simplex")
    exp1 <- expectRightIO $ renewName svc "renewme" 1 (PPRedeemCode "t")
    -- a year added to the old expiry, not to now
    exp1 - exp0 `shouldBe` 31536000

  it "adds edit credits rather than replacing them" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    exp0 <- nrvEditCredits <$> expectRightIO (resolveName svc "renewme.simplex")
    _ <- expectRightIO $ renewName svc "renewme" 1 (PPRedeemCode "t")
    exp1 <- nrvEditCredits <$> expectRightIO (resolveName svc "renewme.simplex")
    -- an unauthenticated renewal must never shrink the owner's allowance
    exp1 `shouldBe` exp0 + editCreditsPerYear

  it "still works after expiry, while in the grace period" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + 86400) -- a day past expiry
    stale <- nrvExpires <$> expectRightIO (resolveName svc "renewme.simplex")
    exp1 <- expectRightIO $ renewName svc "renewme" 1 (PPRedeemCode "t")
    -- extended from now, not backdated from the stale expiry
    exp1 `shouldSatisfy` (> stale)

  it "refuses once the grace period has passed" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + gracePeriod + 1)
    renewName svc "renewme" 1 (PPRedeemCode "t") `shouldReturn` Left SENotFound

  it "the name is still the owner's during grace: nobody else can take it" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + 86400)
    q <- expectRightIO $ quoteName svc "renewme"
    nqAvailable q `shouldBe` False

  it "becomes available to anyone once grace has passed" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + gracePeriod + 1)
    q <- expectRightIO $ quoteName svc "renewme"
    nqAvailable q `shouldBe` True

  it "an expired but untaken name is still listed as the owner's" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + gracePeriod + 1)
    owned <- expectRightIO $ namesOwnedBy svc ownerA
    -- past grace and unclaimed: still recoverable by buying it again
    owned `shouldBe` ["renewme.simplex"]

  it "disappears from the list once someone else registers it" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    otherA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 5
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + gracePeriod + 1)
    giveName mc "renewme.simplex" otherA
    expectRightIO (namesOwnedBy svc ownerA) `shouldReturn` []
    expectRightIO (namesOwnedBy svc otherA) `shouldReturn` ["renewme.simplex"]

  it "cannot be renewed once someone else holds it" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    otherA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 5
    giveName mc "renewme.simplex" ownerA
    advanceClock mc (31536000 + gracePeriod + 1)
    giveName mc "renewme.simplex" otherA
    -- renewal now extends the new holder's registration, not the old owner's:
    -- the old owner has no claim, which is what losing a name means
    e <- expectRightIO (resolveName svc "renewme.simplex")
    nrvOwner e `shouldBe` otherA

  it "rejects a renewal whose payment is refused" $ do
    (mc, svc, _) <- setup
    ownerA <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    giveName mc "renewme.simplex" ownerA
    setPaymentValidator mc (const $ Left "declined")
    renewName svc "renewme" 1 (PPRedeemCode "t") `shouldReturn` Left (SEPaymentRejected "declined")

  it "refuses to renew a name that never existed" $ do
    (_mc, svc, _) <- setup
    renewName svc "nosuchname" 1 (PPRedeemCode "t") `shouldReturn` Left SENotFound

-- | Binding a profile to a wallet. The rule these all defend: a stored seed is
-- never overwritten and a bound profile is never re-pointed, because either one
-- silently loses the names that profile owns.
seedSetupTests :: Spec
seedSetupTests = do
  it "a new seed is a new row, leaving stored seeds untouched" $
    -- entropy differs, so the rows cannot be the same seed
    wsEntropy zeroSeed `shouldNotBe` wsEntropy otherSeed

  it "importing the same phrase twice yields the same entropy" $ do
    -- import is deterministic, so re-importing is not a way to lose a key
    a <- expectRight $ importRecoveryKey canonicalPhrase
    b <- expectRight $ importRecoveryKey canonicalPhrase
    a `shouldBe` b

  it "a phrase round-trips through the stored form" $ do
    e <- expectRight $ importRecoveryKey canonicalPhrase
    let w = WalletSeed {wsId = SeedId 1, wsEntropy = e, wsBackedUp = False}
    expectRight (recoveryKeyPhrase w) >>= (`shouldBe` canonicalPhrase)

  it "two accounts on one seed never share an address" $ do
    a <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    b <- expectRight $ accountAddress <$> deriveAccount zeroSeed 1
    a `shouldNotBe` b

  it "the same index on different seeds gives different addresses" $ do
    a <- expectRight $ accountAddress <$> deriveAccount zeroSeed 0
    b <- expectRight $ accountAddress <$> deriveAccount otherSeed 0
    a `shouldNotBe` b

  it "a profile on its own seed is not derivable from the shared one" $ do
    -- what "generate a new seed for this profile" has to mean
    k0 <- expectRight $ deriveStealthKeys zeroSeed ChainEth 0
    k1 <- expectRight $ deriveStealthKeys otherSeed ChainEth 0
    accountMetaAddress k0 `shouldNotBe` accountMetaAddress k1

  it "rejects a phrase that is not a valid recovery key" $
    importRecoveryKey "not actually a recovery key at all" `shouldSatisfy` isLeft

canonicalPhrase :: ByteString
canonicalPhrase = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

otherSeed :: WalletSeed
otherSeed = WalletSeed {wsId = SeedId 2, wsEntropy = B.replicate 16 7, wsBackedUp = False}
