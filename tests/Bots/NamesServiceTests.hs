{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.NamesServiceTests where

import BadgeService.Service (NamesChain (..), devCodeTable, emptyNamesChain, handleNamesRequest)
import Bots.BadgeServiceTests (badgeProfile, mkBadgeServiceOpts, runBadgeService, serviceDbPrefix, withBadgeService)
import Control.Concurrent.STM (newTVarIO)
import Data.Time.Clock (getCurrentTime)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isHexDigit)
import Control.Exception (ErrorCall, try)
import Data.List (isPrefixOf)
import Simplex.Chat.Names.Protocol
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (parseAddress)
import Simplex.Messaging.Eth.Keccak (keccak256)
import Test.Hspec hiding (it)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32, Word8)
import Simplex.Messaging.Util (tshow)
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import Data.Either (isRight)
import Data.List (isInfixOf)
import Simplex.Chat.Names.Snrc (Intent (..), RecordKey (..), SnrcDeployment (..), devChainId, intent712)
import Simplex.Messaging.Eth.Address (Address, mkAddress)
import Simplex.Chat.Wallet (SeedId (..), WalletSeed (..), accountAddress, deriveNameKey, ethSignatureBytes, parseEthSignature, parseNameKeyPath, renderNameKeyPath, signIntent)
import qualified Test.Hspec as Hspec

namesServiceTests :: SpecWith TestParams
namesServiceTests = do
  it "registers a name via commit/reveal and rejects a taken name" testNamesRegister
  it "rejects a reveal with no matching commitment" testRevealWithoutCommit
  it "reads the wallet without creating one" testNameAddress
  it "gives each name its own key, still derivable after restart" testSeedPersists
  it "recovers the derivation marks from an imported phrase" testRecoverMarks
  it "buys with a code, then re-points the link with a signature" testBuyAndLink
  it "refuses a spent code, a reserved name and a short name" testBuyRefusals
  it "a quote reaches the registrar and renders" testNameQuote

-- | Pins the wire format. The end-to-end test cannot catch a key renamed on
-- both sides at once, so the encodings are asserted literally here.
namesProtocolTests :: Spec
namesProtocolTests = do
  -- A quote sends only this, so "is it reserved" is answered by hash equality.
  -- The whole label is hashed, so a reserved label cannot match as a prefix.
  it "labelhash covers the whole label" $ \_ ->
    mkLabelHash "acme" `shouldNotBe` mkLabelHash "acmecorp"
  -- Name keys are plain BIP-44, so they line up with wallets users already have.
  -- Pinned against the standard test mnemonic: profile 0's names are exactly
  -- MetaMask's account list (m/44'/60'/0'/0/k), and each profile's first name is
  -- the matching Ledger Live account (m/44'/60'/i'/0/0). That is what lets an
  -- owner move a single name into another wallet, and a name bought in a dapp be
  -- found here.
  it "name keys line up with other wallets' derivation" $ \_ -> do
    let mn = either error id $ B39.parseMnemonic "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        sd = WalletSeed {wsId = SeedId 1, wsEntropy = B39.mnemonicToEntropy mn}
        addrOf i k = either error (show . accountAddress) (deriveNameKey sd i k)
    -- MetaMask account 1 and 2 for this phrase
    addrOf 0 0 `shouldBe` "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"
    addrOf 0 1 `shouldBe` "0x6Fac4D18c912343BF86fa7049364Dd4E424Ab9C0"
    -- Ledger Live account 2 for this phrase
    addrOf 1 0 `shouldBe` "0x78839F6054d7ed13918bAe0473BA31b1Ca9D7265"
  -- EIP-2: for every signature there is a second one at n - s that recovers the
  -- same signer. Accepting both would give one authorisation two identities.
  it "refuses a signature whose s is not canonical" $ \_ -> do
    let mn = either error id $ B39.parseMnemonic "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
        sd = WalletSeed {wsId = SeedId 1, wsEntropy = B39.mnemonicToEntropy mn}
        acc = either error id $ deriveNameKey sd 0 0
        intent = SetTextRecord "alice.simplex" RKContact "simplex:/contact#/x" 0 9999999999
        sig = either error id $ signIntent acc (intent712 clientTestDeployment intent)
        bs = ethSignatureBytes sig
        s0 = beInt (B.take 32 (B.drop 32 bs))
        flipped = B.take 32 bs <> beBytes (secpN - s0) <> B.singleton (if B.last bs == 27 then 28 else 27)
    -- the signature the wallet produced is canonical
    isRight (parseEthSignature bs) `shouldBe` True
    -- its mirror image is refused rather than recovered
    case parseEthSignature flipped of
      Left e -> ("not canonical" `isInfixOf` e) `shouldBe` True
      Right _ -> expectationFailure "non-canonical s was accepted"

  -- The account a name sits under is read back out of its stored path: that is
  -- the only record of which profile owned it, and what the recovery scan reads
  -- to know which indices are taken. A path we did not generate has no account
  -- of ours and must say so rather than guess one.
  it "reads back the indices of a path it generated, and only those" $ \_ -> do
    parseNameKeyPath (renderNameKeyPath 0 0) `shouldBe` Just (0, 0)
    parseNameKeyPath (renderNameKeyPath 2 7) `shouldBe` Just (2, 7)
    -- a name bought in a dapp typically sits at the bare master key
    parseNameKeyPath "m" `shouldBe` Nothing
    -- neither of the two common wallet layouts the scan also probes is ours
    parseNameKeyPath "m/44'/60'/0'/1" `shouldBe` Nothing
    -- right shape, wrong coin
    parseNameKeyPath "m/44'/0'/0'/0/0" `shouldBe` Nothing
    -- the account level must be hardened, as BIP-44 requires
    parseNameKeyPath "m/44'/60'/0/0/0" `shouldBe` Nothing
  Hspec.it "encodes commit and reveal requests" $ do
    -- on-chain byte values are 0x-prefixed hex, as Ethereum writes them
    encodes (NamesRequest 1 (NRCommit $ Commitment "0123456789abcdef")) $
      "{\"version\":1,\"request\":{\"type\":\"commit\",\"commitment\":\"0x30313233343536373839616263646566\"}}"
    encodes (NamesRequest 1 (NRReveal "alicename.simplex" testOwner (NameSecret "s") 3600 "simplex:/contact#/x")) $
      "{\"version\":1,\"request\":{\"type\":\"reveal\",\"name\":\"alicename.simplex\""
        <> ",\"owner\":\"0x520110C7b1CE17f8C0a2778B41AB2F23D10B70B0\",\"secret\":\"0x73\""
        <> ",\"ttl\":3600,\"simplex_link\":\"simplex:/contact#/x\"}}"
    encodes (NRPError NECNameTaken Nothing Nothing) "{\"type\":\"error\",\"code\":\"name_taken\"}"
  Hspec.it "roundtrips requests and responses" $ do
    roundtrips $ NamesRequest 1 (NRCommit $ Commitment "0123456789abcdef")
    roundtrips $ NamesRequest 1 (NRReveal "alicename.simplex" testOwner (NameSecret "s") 3600 "simplex:/contact#/x")
    roundtrips $ NRPCommitted (TxHash "tx")
    roundtrips $ NamesRequest 1 (NRQuote (mkLabelHash "acme") 4 2)
    roundtrips $ NRPQuote (mkLabelHash "acme") False Nothing True 2000 2
    roundtrips $ NRPError NECNameTaken Nothing Nothing
    roundtrips $ NRPError (NECUnknown "future_code") (Just "why") (Just 30)
  Hspec.it "renders a 32-byte hash as 0x + 64 hex digits" $ do
    let h = strEncode $ TxHash (keccak256 "reveal")
    B.length h `shouldBe` 66
    B.take 2 h `shouldBe` "0x"
    B.all (\c -> isHexDigit (toEnum $ fromIntegral c)) (B.drop 2 h) `shouldBe` True
  -- The quote semantics, tested as a function call: no SMP connection, no
  -- forked service, no waiting on terminal output, so nothing here depends on
  -- how fast the machine is.
  Hspec.it "a reserved label is refused by its hash alone" $ do
    r <- freshChain >>= (`quoteFor` "acme")
    r `shouldSatisfy` isReserved
    r `shouldSatisfy` not . isAvailable
  Hspec.it "a longer label sharing a reserved prefix is not reserved" $ do
    r <- freshChain >>= (`quoteFor` "acmecorp")
    r `shouldSatisfy` not . isReserved
    r `shouldSatisfy` isAvailable
  -- the hash hides the length, so the caller reports it and the minimum still bites
  Hspec.it "a label below the minimum length is refused" $ do
    r <- freshChain >>= (`quoteFor` "abc")
    r `shouldSatisfy` not . isAvailable
    r `shouldSatisfy` not . isReserved
  Hspec.it "a registered name is found by its hash" $ do
    chain <- freshChain
    before' <- quoteFor chain "spender"
    before' `shouldSatisfy` isAvailable
    -- the fully-qualified name, as the client sends it
    bought <- decodeResp <$> (handleNamesRequest chain . NamesRequest currentNamesVersion $
      NRBuy (RequestId "req-1") "spender.simplex" testOwner (RedemptionCode (devCode 1)) "simplex:/contact#/x")
    bought `shouldSatisfy` \case NRPRegistered {} -> True; _ -> False
    after' <- quoteFor chain "spender"
    after' `shouldSatisfy` not . isAvailable
  Hspec.it "binds the commitment to every field" $ do
    let c = mkCommitment "alicename.simplex" testOwner (NameSecret "s") 3600
    -- the service recomputes it at reveal, so it must be deterministic
    c `shouldBe` mkCommitment "alicename.simplex" testOwner (NameSecret "s") 3600
    c `shouldNotBe` mkCommitment "bob.simplex" testOwner (NameSecret "s") 3600
    c `shouldNotBe` mkCommitment "alicename.simplex" testOwner (NameSecret "s2") 3600
    c `shouldNotBe` mkCommitment "alicename.simplex" testOwner (NameSecret "s") 7200
  where
    testOwner = either error id $ parseAddress "0x520110C7b1CE17f8C0a2778B41AB2F23D10B70B0"
    freshChain = do
      now <- getCurrentTime
      newTVarIO emptyNamesChain {chainCodes = devCodeTable now}
    -- what the client sends: the hash, and the length it cannot recover from it
    quoteFor chain label =
      decodeResp
        <$> ( handleNamesRequest chain . NamesRequest currentNamesVersion $
                NRQuote (mkLabelHash label) (fromIntegral $ T.length label) 2
            )
    decodeResp o = case J.fromJSON (J.Object o) of
      J.Success (r :: NamesResponse) -> r
      J.Error e -> error e
    isAvailable = \case NRPQuote {nrAvailable} -> nrAvailable; _ -> False
    isReserved = \case NRPQuote {nrReserved} -> nrReserved; _ -> False
    -- compares parsed values, so the assertion does not depend on key order
    encodes :: J.ToJSON a => a -> LB.ByteString -> Expectation
    encodes x s = Just (J.toJSON x) `shouldBe` J.decode s
    roundtrips :: (Eq a, Show a, J.ToJSON a, J.FromJSON a) => a -> Expectation
    roundtrips x = J.eitherDecode' (J.encode x) `shouldBe` Right x

-- | End-to-end: @/name register@ streams the commit → wait → reveal phases and
-- resolves to the owner address derived from the wallet seed; a second
-- registration of the same name fails with name_taken.
testNamesRegister :: HasCallStack => TestParams -> IO ()
testNamesRegister ps =
  withBadgeService ps $ \client bsLink -> do
    client ##> ("/name register " <> bsLink <> " alicename.simplex simplex:/contact#/first")
    commitPhases client
    -- the final progress event and the command response arrive on separate channels
    client
      <### [ ConsoleString "name alicename.simplex: registered",
             StartsWith "name registered: alicename.simplex -> 0x",
             -- one key per name: the profile's first name is address index 0
             ConsoleString "  derivation path: m/44'/60'/0'/0/0"
           ]
    -- re-running the identical command is rejected too, not silently accepted:
    -- the owner is the same within a session, so this is the duplicate a user hits.
    client ##> ("/name register " <> bsLink <> " alicename.simplex simplex:/contact#/first")
    commitPhases client
    client <## "name registration failed: name_taken"
    -- and the same name pointed at a different link is equally rejected
    client ##> ("/name register " <> bsLink <> " alicename.simplex simplex:/contact#/second")
    commitPhases client
    client <## "name registration failed: name_taken"
  where
    commitPhases client = do
      client <## "name alicename.simplex: committing"
      client <## "name alicename.simplex: committed. waiting 1s before revealing"
      client <## "name alicename.simplex: revealing"

-- | Asking about your wallet must not be what gives you one: only registering
-- creates a seed. @\/name keys@ is the read-only view now that @\/name address@
-- is retired in favour of @\/name info@.
testNameAddress :: HasCallStack => TestParams -> IO ()
testNameAddress ps =
  withBadgeService ps $ \client bsLink -> do
    -- asked repeatedly before any registration: still no address, none created
    client ##> "/name keys"
    client <## "no recovery keys yet - one is created when you buy a name"
    client ##> "/name keys"
    client <## "no recovery keys yet - one is created when you buy a name"
    client ##> ("/name register " <> bsLink <> " carolname.simplex simplex:/contact#/x")
    owner <- ownerOf client "carolname.simplex"
    -- now it exists, and reports the address and path the name was registered to
    -- and now exactly one key exists, controlling the name just bought
    client ##> "/name keys"
    client <## "1:  (in use)  (not written down)"
    client <## "     account 0"
    client <## "       name 0   carolname.simplex"

-- | Recovery on a new device must not re-use a key a recovered name already
-- owns. Neither high-water mark survives an import - the phrase records only
-- the entropy - so a scan is the only thing that can restore them, and without
-- that the next purchase derives the recovered name's key: the client would
-- discover the clash from the UNIQUE constraint on wallet_name_keys, after the
-- registration had already gone through.
--
-- Asserted through the paths: the second name must not land on the first's.
testRecoverMarks :: HasCallStack => TestParams -> IO ()
testRecoverMarks ps = do
  let opts = mkBadgeServiceOpts ps
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  runBadgeService testCfg opts (pure ())
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  runBadgeService testCfg opts $ do
    -- device one buys a name, then its phrase is written down
    phrase <- withNewTestChatCfg ps testCfg "client" bobProfile $ \client -> do
      client ##> ("/name register " <> bsLink <> " lostname.simplex simplex:/contact#/x")
      _ <- ownerOf client "lostname.simplex"
      client ##> "/name keys export"
      client <## "write these down - anyone who knows them can take the names they control:"
      client <## "1: lostname.simplex"
      phraseLine client
    -- device two is fresh: import, scan, then buy
    withNewTestChatCfg ps testCfg "client2" cathProfile $ \client -> do
      client ##> ("/name keys import " <> phrase)
      client <## "1:  (in use)  (not written down)"
      client <## "     no names yet"
      client ##> ("/name rescan " <> bsLink)
      awaitLine client "found:"
      client <## "  lostname.simplex  m/44'/60'/0'/0/0"
      client ##> ("/name register " <> bsLink <> " foundname.simplex simplex:/contact#/y")
      _ <- ownerOf client "foundname.simplex"
      -- the scan moved both marks past index 0, so the new name is elsewhere
      client ##> "/name keys"
      client <## "1:  (in use)  (not written down)"
      client <## "     account 0"
      client <## "       name 0   lostname.simplex"
      client <## "     account 1"
      client <## "       name 1   foundname.simplex"

-- | A scan is one round trip per candidate path, taken sequentially, and
-- outruns the harness's five second per-line read timeout. Retrying is not
-- polling: each attempt blocks on the output queue, so this waits as long as
-- the scan takes and no longer.
awaitLine :: HasCallStack => TestCC -> String -> Expectation
awaitLine cc expected = go (6 :: Int)
  where
    go 0 = expectationFailure $ "no line: " <> expected
    go n =
      try (getTermLine cc) >>= \case
        Left (_ :: ErrorCall) -> go (n - 1)
        Right l -> l `shouldBe` expected

-- | The phrase line printed under a key by @\/name keys export@.
phraseLine :: HasCallStack => TestCC -> IO String
phraseLine client = dropWhile (== ' ') <$> getTermLine client

-- | The front-running defence: a reveal only registers a name if that exact
-- commitment was published first. Sent as a raw service request, because the
-- core always commits before revealing and so cannot produce this on its own.
testRevealWithoutCommit :: HasCallStack => TestParams -> IO ()
testRevealWithoutCommit ps =
  withBadgeService ps $ \client bsLink -> do
    let reveal =
          "{\"version\":1,\"request\":{\"type\":\"reveal\",\"name\":\"eve.simplex\""
            <> ",\"owner\":\"0x520110c7b1ce17f8c0a2778b41ab2f23d10b70b0\",\"secret\":\"0x73\""
            <> ",\"ttl\":3600,\"simplex_link\":\"simplex:/contact#/x\"}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> reveal)
    client <## "service response: {\"code\":\"bad_request\",\"message\":\"no matching commitment\",\"type\":\"error\"}"

-- | The seed is persisted, so a name registered in one session is still owned by
-- an address the next session can derive. Without this the key is unrecoverable
-- after restart and the name is orphaned.
--
-- It also pins the other half of one-key-per-name: the second registration must
-- land on a /different/ address, at the next BIP-44 address index under the same
-- profile account. Sharing one key across a profile's names is what this
-- replaces — it would put every name behind one resolver nonce and make
-- exporting one name's key hand over all of them.
testSeedPersists :: HasCallStack => TestParams -> IO ()
testSeedPersists ps = do
  let opts = mkBadgeServiceOpts ps
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  runBadgeService testCfg opts (pure ())
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  runBadgeService testCfg opts $ do
    owner1 <- withNewTestChatCfg ps testCfg "client" bobProfile $ \client -> do
      client ##> ("/name register " <> bsLink <> " firstname.simplex simplex:/contact#/x")
      ownerOf client "firstname.simplex"
    -- same database, new session: the seed has to come back from the DB
    owner2 <- withTestChat ps "client" $ \client -> do
      client ##> ("/name register " <> bsLink <> " secondname.simplex simplex:/contact#/y")
      ownerOf client "secondname.simplex"
    owner2 `shouldNotBe` owner1
    -- and both are still derivable in a third session, each at its own path
    withTestChat ps "client" $ \client -> do
      client ##> "/name keys"
      client <## "1:  (in use)  (not written down)"
      client <## "     account 0"
      client <## "       name 0   firstname.simplex"
      client <## "       name 1   secondname.simplex"

-- | Reads past startup and progress lines to the registration result, returning
-- the owner address. Keeps reading until the final progress event has arrived
-- too — it races with the command response and would otherwise be left
-- unconsumed, failing the next assertion or the session close.
ownerOf :: HasCallStack => TestCC -> String -> IO String
ownerOf client nm = go (40 :: Int) Nothing False False
  where
    pfx = "name registered: " <> nm <> " -> "
    pathLine = "  derivation path: "
    lastEvt = "name " <> nm <> ": registered"
    -- three lines must be consumed before the next assertion: the progress
    -- event, the registration line, and the derivation path under it
    go _ (Just a) True True = pure a
    go 0 _ _ _ = error $ "no registration line for " <> nm
    go n addr seen path = do
      l <- getTermLine client
      let addr' = if pfx `isPrefixOf` l then Just (takeWhile (/= ' ') $ drop (length pfx) l) else addr
      go (n - 1) addr' (seen || l == lastEvt) (path || pathLine `isPrefixOf` l)

-- | The whole purchase path: verify a code on the device, buy, then change the
-- link with a signed intent the service verifies by recovering the signer.
testBuyAndLink :: HasCallStack => TestParams -> IO ()
testBuyAndLink ps =
  withBadgeService ps $ \client bsLink -> do
    let code = devCode 1
    client ##> ("/name verify-code " <> bsLink <> " " <> T.unpack code)
    client <## "code verified: names of 6 letters or more, 2 years"
    client <##. "  use before "
    client ##> ("/name buy " <> bsLink <> " purchased " <> T.unpack code <> " simplex:/contact#/first")
    client <## "name purchased.simplex: revealing"
    client <## "name purchased.simplex: registered"
    client <##. "name registered: purchased.simplex -> 0x"
    client <## "  derivation path: m/44'/60'/0'/0/0"
    -- the record the purchase wrote
    client ##> ("/name info " <> bsLink <> " purchased.simplex")
    client <## "purchased.simplex"
    client <##. "  owner   0x"
    client <## "  path    m/44'/60'/0'/0/0"
    client <## "  contact simplex:/contact#/first"
    client <##. "  expires "
    client <## "  10 relayed edits left"
    -- a signed edit: the service recovers the signer and refuses anyone else
    client ##> ("/name link " <> bsLink <> " contact purchased.simplex simplex:/contact#/second")
    client <##. "purchased.simplex: contact updated (tx 0x"
    client ##> ("/name info " <> bsLink <> " purchased.simplex")
    client <## "purchased.simplex"
    client <##. "  owner   0x"
    client <## "  path    m/44'/60'/0'/0/0"
    client <## "  contact simplex:/contact#/second"
    client <##. "  expires "
    -- one edit spent, and only one
    client <## "  9 relayed edits left"

-- | Every refusal has its own message. A user who types a reserved name must be
-- told that, not "bad request".
-- Wiring only: that the CLI command reaches the registrar and renders what
-- comes back. What a quote *means* is covered by the handler tests above,
-- which need no connection and so cannot fail for being slow. Kept because
-- neither those nor the encoding roundtrips can catch the two sides being
-- wired to different fields.
testNameQuote :: HasCallStack => TestParams -> IO ()
testNameQuote ps =
  withBadgeService ps $ \client bsLink -> do
    client ##> ("/name quote " <> bsLink <> " spender")
    client <##. "spender.simplex - available"
    -- charset is unanswerable from a hash, so the client refuses without asking
    client ##> ("/name quote " <> bsLink <> " acme!!")
    client <## "bad chat command: name may only contain a-z, 0-9 and inner hyphens"

testBuyRefusals :: HasCallStack => TestParams -> IO ()
testBuyRefusals ps =
  withBadgeService ps $ \client bsLink -> do
    let code1 = devCode 1
        code2 = devCode 2
    -- the registrar owns the code table, so every refusal now comes from it
    client ##> ("/name buy " <> bsLink <> " abc " <> T.unpack code1 <> " simplex:/contact#/x")
    client <## "name abc.simplex: revealing"
    client <##. "name registration failed: name_too_short"
    -- reserved
    -- long enough for the code, so this one reaches the service before failing
    client ##> ("/name buy " <> bsLink <> " support " <> T.unpack code1 <> " simplex:/contact#/x")
    client <## "name support.simplex: revealing"
    client <## "name registration failed: name_reserved"
    -- a real purchase, then the same code again
    client ##> ("/name buy " <> bsLink <> " spender " <> T.unpack code1 <> " simplex:/contact#/x")
    client <## "name spender.simplex: revealing"
    client <## "name spender.simplex: registered"
    client <##. "name registered: spender.simplex -> 0x"
    -- a refused purchase has already taken an index, so the path here is not 0.
    -- That is the documented cost of allocating before the service answers.
    client <##. "  derivation path: m/44'/60'/0'/0/"
    client ##> ("/name buy " <> bsLink <> " another " <> T.unpack code1 <> " simplex:/contact#/x")
    client <## "name another.simplex: revealing"
    client <## "name registration failed: code_spent"
    -- a different code works, and takes the next key
    client ##> ("/name buy " <> bsLink <> " another " <> T.unpack code2 <> " simplex:/contact#/x")
    client <## "name another.simplex: revealing"
    client <## "name another.simplex: registered"
    client <##. "name registered: another.simplex -> 0x"
    client <##. "  derivation path: m/44'/60'/0'/0/"
    -- A capital letter is not a different name, it is an invalid one. Without
    -- this the reserved set is bypassed by case: "Support" would register while
    -- "support" is held, and two names that read alike would both exist.
    client ##> ("/name buy " <> bsLink <> " Support " <> T.unpack (devCode 3) <> " simplex:/contact#/x")
    client <## "name Support.simplex: revealing"
    client <##. "name registration failed: name_invalid"
    -- and the code is not spent by a refusal, so it still works
    client ##> ("/name buy " <> bsLink <> " lowered " <> T.unpack (devCode 3) <> " simplex:/contact#/x")
    client <## "name lowered.simplex: revealing"
    client <## "name lowered.simplex: registered"
    client <##. "name registered: lowered.simplex -> 0x"
    client <##. "  derivation path: m/44'/60'/0'/0/"
    -- The charset alone admits xn--, which is ASCII that renders as something
    -- else. The reserved hyphen slot is refused wholesale, not just that prefix.
    client ##> ("/name buy " <> bsLink <> " xn--80ak6aa92e " <> T.unpack (devCode 4) <> " simplex:/contact#/x")
    client <## "name xn--80ak6aa92e.simplex: revealing"
    client <##. "name registration failed: name_invalid"
    -- and a hyphen cannot open or close a label either
    client ##> ("/name buy " <> bsLink <> " -leading " <> T.unpack (devCode 4) <> " simplex:/contact#/x")
    client <## "name -leading.simplex: revealing"
    client <##. "name registration failed: name_invalid"
    -- Expiry is a property of the key, not of the code, so a build with one
    -- cohort key cannot mint an expired code. Expiry refusal is covered by the
    -- unit test over verifyCode instead.


secpN :: Integer
secpN = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

beInt :: B.ByteString -> Integer
beInt = B.foldl' (\a w -> a * 256 + fromIntegral w) 0

beBytes :: Integer -> B.ByteString
beBytes n = B.pack [fromIntegral (n `div` (256 ^ i)) | i <- [31, 30 .. 0 :: Int]]

testAddr :: Word8 -> Address
testAddr n = either error id $ mkAddress (B.replicate 19 0 <> B.singleton n)

-- | Mirrors the client's placeholder deployment, so the digest is the real one.
clientTestDeployment :: SnrcDeployment
clientTestDeployment =
  SnrcDeployment {sdTld = "simplex", sdChainId = devChainId, sdRegistrar = testAddr 1, sdResolver = testAddr 2}

-- | The service's pre-issued table, mirrored so tests can name a code.
devCode :: Int -> Text
devCode i = ["SMPX-4K2P-7TQW-9XRM", "SMPX-8H3N-2VBD-6JYK", "SMPX-5L9C-4WFT-1ZQA", "SMPX-7R6M-8PGX-3NHV"] !! (i - 1)
