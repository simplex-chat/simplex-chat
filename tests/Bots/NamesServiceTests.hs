{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module Bots.NamesServiceTests where

import Bots.BadgeServiceTests (badgeProfile, mkBadgeServiceOpts, runBadgeService, serviceDbPrefix, withBadgeService)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Data.List (isPrefixOf)
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isHexDigit)
import Simplex.Chat.Names.Protocol
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (parseAddress)
import Simplex.Messaging.Eth.Keccak (keccak256)
import Test.Hspec hiding (it)
import qualified Test.Hspec as Hspec

namesServiceTests :: SpecWith TestParams
namesServiceTests = do
  it "registers a name via commit/reveal and rejects a taken name" testNamesRegister
  it "derives the same owner address after restart" testSeedPersists

-- | Pins the wire format. The end-to-end test cannot catch a key renamed on
-- both sides at once, so the encodings are asserted literally here.
namesProtocolTests :: Spec
namesProtocolTests = do
  Hspec.it "encodes commit and reveal requests" $ do
    -- on-chain byte values are 0x-prefixed hex, as Ethereum writes them
    encodes (NamesRequest 1 (NRCommit $ Commitment "0123456789abcdef")) $
      "{\"version\":1,\"request\":{\"type\":\"commit\",\"commitment\":\"0x30313233343536373839616263646566\"}}"
    encodes (NamesRequest 1 (NRReveal "alice.simplex" testOwner (NameSecret "s") 3600 "simplex:/contact#/x")) $
      "{\"version\":1,\"request\":{\"type\":\"reveal\",\"name\":\"alice.simplex\""
        <> ",\"owner\":\"0x520110C7b1CE17f8C0a2778B41AB2F23D10B70B0\",\"secret\":\"0x73\""
        <> ",\"ttl\":3600,\"simplex_link\":\"simplex:/contact#/x\"}}"
    encodes (NRPError NECNameTaken Nothing Nothing) "{\"type\":\"error\",\"code\":\"name_taken\"}"
  Hspec.it "roundtrips requests and responses" $ do
    roundtrips $ NamesRequest 1 (NRCommit $ Commitment "0123456789abcdef")
    roundtrips $ NamesRequest 1 (NRReveal "alice.simplex" testOwner (NameSecret "s") 3600 "simplex:/contact#/x")
    roundtrips $ NRPCommitted (TxHash "tx")
    roundtrips $ NRPError NECNameTaken Nothing Nothing
    roundtrips $ NRPError (NECUnknown "future_code") (Just "why") (Just 30)
  Hspec.it "renders a 32-byte hash as 0x + 64 hex digits" $ do
    let h = strEncode $ TxHash (keccak256 "reveal")
    B.length h `shouldBe` 66
    B.take 2 h `shouldBe` "0x"
    B.all (\c -> isHexDigit (toEnum $ fromIntegral c)) (B.drop 2 h) `shouldBe` True
  Hspec.it "binds the commitment to every field" $ do
    let c = mkCommitment "alice.simplex" testOwner (NameSecret "s") 3600
    -- the service recomputes it at reveal, so it must be deterministic
    c `shouldBe` mkCommitment "alice.simplex" testOwner (NameSecret "s") 3600
    c `shouldNotBe` mkCommitment "bob.simplex" testOwner (NameSecret "s") 3600
    c `shouldNotBe` mkCommitment "alice.simplex" testOwner (NameSecret "s2") 3600
    c `shouldNotBe` mkCommitment "alice.simplex" testOwner (NameSecret "s") 7200
  where
    testOwner = either error id $ parseAddress "0x520110C7b1CE17f8C0a2778B41AB2F23D10B70B0"
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
    client ##> ("/name register " <> bsLink <> " alice.simplex simplex:/contact#/first")
    commitPhases client
    -- the final progress event and the command response arrive on separate channels
    client
      <### [ ConsoleString "name alice.simplex: registered",
             StartsWith "name registered: alice.simplex -> 0x"
           ]
    -- re-running the identical command is rejected too, not silently accepted:
    -- the owner is the same within a session, so this is the duplicate a user hits.
    client ##> ("/name register " <> bsLink <> " alice.simplex simplex:/contact#/first")
    commitPhases client
    client <## "bad chat command: name registration failed: name_taken"
    -- and the same name pointed at a different link is equally rejected
    client ##> ("/name register " <> bsLink <> " alice.simplex simplex:/contact#/second")
    commitPhases client
    client <## "bad chat command: name registration failed: name_taken"
  where
    commitPhases client = do
      client <## "name alice.simplex: committing"
      client <## "name alice.simplex: committed. waiting 1s before revealing"
      client <## "name alice.simplex: revealing"

-- | The seed is persisted, so a name registered in one session is still owned by
-- an address the next session can derive. Without this the key is unrecoverable
-- after restart and the name is orphaned.
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
      client ##> ("/name register " <> bsLink <> " first.simplex simplex:/contact#/x")
      ownerOf client "first.simplex"
    -- same database, new session: the seed has to come back from the DB
    owner2 <- withTestChat ps "client" $ \client -> do
      client ##> ("/name register " <> bsLink <> " second.simplex simplex:/contact#/y")
      ownerOf client "second.simplex"
    owner2 `shouldBe` owner1
  where
    -- Reads past startup and progress lines to the registration result, and
    -- keeps reading until the final progress event has arrived too — it races
    -- with the command response and would otherwise be left unconsumed.
    ownerOf client nm = go (40 :: Int) Nothing False
      where
        pfx = "name registered: " <> nm <> " -> "
        lastEvt = "name " <> nm <> ": registered"
        go _ (Just a) True = pure a
        go 0 _ _ = error $ "no registration line for " <> nm
        go n addr seen = do
          l <- getTermLine client
          let addr' = if pfx `isPrefixOf` l then Just (takeWhile (/= ' ') $ drop (length pfx) l) else addr
          go (n - 1) addr' (seen || l == lastEvt)

