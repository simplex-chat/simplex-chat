{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesContractsTests
  ( namesContractsTests,
    withContracts,
    answerRegistrar,
    answerRegistered,
    registrarOf,
    resolverOf,
    rentPriceCall,
    rentPriceAnswer,
    now,
    year,
    gracePeriod,
    otherResolver,
    nameExpiresCall,
    reservedNamesCall,
    contactTextCall,
    channelTextCall,
    noncesCall,
    alice,
    availableCall,
    minCommitmentAgeCall,
    maxCommitmentAgeCall,
    unhex,
  )
where

import Bots.StoreService.FakeEthNode
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Network.HTTP.Types (hContentType, hLocation, status200, status302, status404, status429, status503)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash, SimplexTLD (..), labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi
import StoreService.Names.Chain
import StoreService.Names.Contracts
import StoreService.Names.Registration (labelNode, namehash, nodeBytes)
import StoreService.Names.Revert (RevertName (..))
import Test.Hspec

namesContractsTests :: Spec
namesContractsTests = describe "StoreService names: contracts" $ do
  -- the calldata and answers marked viem are viem 2.33.3's encodeFunctionData and encodeFunctionResult
  -- with the ABIs of ens-contracts PR #36, so a read the fake node answers sends viem's calldata
  g <- runIO C.newRandom
  it "builds a name's node from its label hash as viem's namehash" testNode
  describe "status" $ do
    it "reads a name nobody holds or reserved as available" $ testAvailable g
    it "reads a reserved name" $ testReserved g
    it "reads a registered name with the registry's owner and resolver" $ testRegistered g
    it "keeps a name registered to the last second of its grace period" $ testGracePeriod g
    it "reads the registry and the grace period once, and again after a failed read" $ testRegistrarOnce g
  describe "records" $ do
    it "reads the default resolver's records and the owner's nonce" $ testRecords g
    it "reads no records from another resolver" $ testOtherResolver g
  describe "prices" $ do
    it "quotes 6, 7 and 12 letters at the launch list for 2 years" $ testLaunchPrices g
  describe "other reads" $ do
    it "reads the sponsor's and the resolver's nonces from their own contracts" $ testNonces g
    it "reads ownerOf's revert without data as nobody" $ testOwnerOfNobody g
    it "reads any other ownerOf revert or failure as a failed read" $ testOwnerOfFailed g
    it "reads a commitment's time, none as Nothing" $ testCommitmentTime g
    it "reads the controller's availability and the name's expiry" $ testAvailability g
    it "reads the commitment ages once" $ testCommitmentAges g
  describe "name policy" $ do
    it "reads the policy from the controller once, and again after a zero answer or a failed read, and the reservations, listing, terms links, nonces and acceptances from it" $ testNamePolicy g
  describe "terms" $ do
    it "reads both terms links at one block, \"\" for one not set, and hashes a set link with keccak256 as viem does" $ testTermsLinks g
    it "fetches a terms text once, keeping it by its link's hash" $ testTermsTextKept g
    it "takes only a git.simplex.chat link of one .md file at one commit with the file's SHA-256, or a loopback link on the local chain, fetching nothing else" $ testTermsLinkForms g
    it "refuses an answer other than a 200 UTF-8 text of at most 256 KB with the link's SHA-256, following no redirect" $ testTermsTextRefused g
    it "fetches no terms text for 30 seconds after a failed fetch, or as long as its Retry-After asks, up to an hour" $ testTermsBackoff g
    it "gives up a terms fetch past the call timeout" $ testTermsTimeout g
  describe "failures" $ do
    it "fails a status on a failed read, never reading it as available" $ testFailedRead g
    it "fails a read whose answer does not decode or does not fit" $ testMalformed g
    it "fails a status within its deadline when one method hangs" $ testStatusDeadline g
    it "fails records within their deadline when the calls hang" $ testRecordsDeadline g
    it "reads a status and records within a deadline only concurrent reads meet" $ testConcurrentReads g

testNode :: IO ()
testNode = do
  nodeBytes (labelNode TLDSimplex $ labelHash "alicebakery") `shouldBe` aliceNode
  nodeBytes (namehash TLDSimplex "alicebakery") `shouldBe` aliceNode

testAvailable :: TVar ChaChaDRG -> IO ()
testAvailable g = withContracts g $ \node ct -> do
  answerRegistrar node
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  nameStatus ct alice `shouldReturn` Right NSAvailable

testReserved :: TVar ChaChaDRG -> IO ()
testReserved g = withContracts g $ \node ct -> do
  answerRegistrar node
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  -- viem: reservedNames answering Reason 1
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ unhex "0000000000000000000000000000000000000000000000000000000000000001"
  nameStatus ct alice `shouldReturn` Right NSReserved

testNamePolicy :: TVar ChaChaDRG -> IO ()
testNamePolicy g = withContracts g $ \node ct -> do
  answerRegistrar node
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (2 :: Int)
  answerCall node fakeController (selector "namePolicy()") $ Returns $ B.replicate 32 0
  namePolicy ct `shouldReturn` Left (ChainUnavailable "the controller has no name policy yet")
  answerCall node fakeController (selector "namePolicy()") $ Reverts ""
  namePolicy ct `shouldReturn` Left (ChainReverted $ Just RNWithoutData)
  answerCall node fakeController (selector "namePolicy()") $ Returns $ addressWord fakeNamePolicy
  namePolicy ct `shouldReturn` Right fakeNamePolicy
  -- read once: a controller answering no more keeps the policy read
  answerCall node fakeController (selector "namePolicy()") $ Reverts ""
  nameStatus ct alice `shouldReturn` Right NSReserved
  communityName ct alice `shouldReturn` Right False
  listCommunityName node alice
  communityName ct alice `shouldReturn` Right True
  termsLinks ct `shouldReturn` Right (fakeTermsLink fakeGeneralTermsText, fakeTermsLink fakeCommunityTermsText)
  answerCall node fakeNamePolicy noncesCall $ Returns $ uint256 (5 :: Int)
  policyNonce ct owner `shouldReturn` Right 5
  answerCall node fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> commitment <> addressWord owner) $ Returns fakeCommunityTerms
  acceptedTerms ct commitment owner `shouldReturn` Right fakeCommunityTerms

testTermsLinks :: TVar ChaChaDRG -> IO ()
testTermsLinks g = withContracts g $ \node ct -> do
  setBlock node 100 now
  answerCallFrom node 101 fakeNamePolicy (selector "generalTermsLink()") $ Returns $ abiTuple [Dynamic $ abiBytes ""]
  answerCallFrom node 101 fakeNamePolicy (selector "communityTermsLink()") $ Returns $ abiTuple [Dynamic $ abiBytes $ encodeUtf8 $ fakeTermsLink fakeNewCommunityTermsText]
  termsLinks ct `shouldReturn` Right (fakeTermsLink fakeGeneralTermsText, fakeTermsLink fakeCommunityTermsText)
  setBlock node 101 now
  termsLinks ct `shouldReturn` Right ("", fakeTermsLink fakeNewCommunityTermsText)
  termsOf "" `shouldBe` Nothing
  termsOf viemLink `shouldBe` Just Terms {tmLink = viemLink, tmHash = unhex "c2ff2149afa84a77e3a041b2925ebd08a22c1e91a74abd8b82fd17e45c15cb7f"}
  where
    viemLink = "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/4cb24444a1f2e3d4c5b6a7980716253443526170/docs/general-terms.md#sha256=0b2c3d4e5f60718293a4b5c6d7e8f90112233445566778899aabbccddeeff001"

testTermsTextKept :: TVar ChaChaDRG -> IO ()
testTermsTextKept g = withContracts g $ \node ct -> do
  termsText ct (termsAt fakeGeneralTermsText) `shouldReturn` Right "SimpleX general terms v1"
  answerText node (fakeTermsPath fakeGeneralTermsText) $ TextAnswer status404 [] ""
  termsText ct (termsAt fakeGeneralTermsText) `shouldReturn` Right "SimpleX general terms v1"
  fetchedTexts node `shouldReturn` [fakeTermsPath fakeGeneralTermsText]

testTermsLinkForms :: TVar ChaChaDRG -> IO ()
testTermsLinkForms g = withFakeEthNode $ \node -> do
  c <- fakeChainClient g node
  ct <- (\x -> x {ctTermsBackoff = 0}) <$> newContracts TLDSimplex c
  otherChain <- newContracts TLDSimplex c {ccConfig = (ccConfig c) {cChainId = 1}}
  let digest = T.drop 2 $ hex $ C.sha256Hash fakeGeneralTermsText
      commit = T.replicate 40 "a"
      commitPath path = "/simplex-chat/simplex-chat/raw/commit/" <> commit <> path
      taken = ["https://git.simplex.chat" <> commitPath "/docs/terms.md", "https://git.simplex.chat" <> commitPath "/a/b_c-d.e/terms.v1.md", "http://127.0.0.1:8098/terms/terms.md"]
  forM_ [commitPath "/docs/terms.md", commitPath "/a/b_c-d.e/terms.v1.md", "/terms/terms.md"] $ \path -> answerText node path $ servedText fakeGeneralTermsText
  forM_ taken $ \url -> termsText ct (fromJust $ termsOf $ url <> "#sha256=" <> digest) `shouldReturn` Right "SimpleX general terms v1"
  let refused link = (link,) <$> termsText ct (fromJust $ termsOf link) `shouldReturn` (link, Left TRLink)
  forM_
    [ "http://git.simplex.chat" <> commitPath "/docs/terms.md#sha256=" <> digest,
      "https://git.simplex.chat/simplex-chat/simplexmq/raw/commit/" <> commit <> "/docs/terms.md#sha256=" <> digest,
      "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/" <> T.replicate 39 "a" <> "/docs/terms.md#sha256=" <> digest,
      "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/" <> T.replicate 40 "A" <> "/docs/terms.md#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms.txt#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/../terms.md#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/./terms.md#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs//terms.md#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms%2E.md#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms.md?raw=1#sha256=" <> digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms.md",
      "https://git.simplex.chat" <> commitPath "/docs/terms.md#sha256=" <> T.toUpper digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms.md#sha256=" <> T.drop 1 digest,
      "https://git.simplex.chat" <> commitPath "/docs/terms.md#sha1=" <> digest,
      "http://localhost:8098/terms/terms.md#sha256=" <> digest,
      "http://127.0.0.1:08098/terms/terms.md#sha256=" <> digest,
      "http://127.0.0.1:65536/terms/terms.md#sha256=" <> digest
    ]
    refused
  termsText otherChain (fromJust $ termsOf $ "http://127.0.0.1:8098/terms/terms.md#sha256=" <> digest) `shouldReturn` Left TRLink
  setFault node "eth_chainId" (Answers (J.String "0x1"))
  termsText ct (fromJust $ termsOf $ "http://127.0.0.1:8099/terms/terms.md#sha256=" <> digest) `shouldReturn` Left TRLink
  length <$> fetchedTexts node `shouldReturn` 3

testTermsTextRefused :: TVar ChaChaDRG -> IO ()
testTermsTextRefused g = withContracts g $ \node ct0 -> do
  let ct = ct0 {ctTermsBackoff = 0}
      served n headers status body bytes = do
        let path = "/simplex-chat/simplex-chat/raw/commit/" <> T.justifyRight 40 '0' (tshow (n :: Int)) <> "/terms.md"
        answerText node path $ TextAnswer status headers body
        termsText ct $ fromJust $ termsOf $ "https://git.simplex.chat" <> path <> "#sha256=" <> T.drop 2 (hex $ C.sha256Hash bytes)
      markdown = [(hContentType, "text/markdown; charset=utf-8")]
      long = B.replicate (256 * 1024) 0x61
  served 1 [(hLocation, "/elsewhere.md")] status302 "" "" `shouldReturn` Left (TRWait 0)
  served 2 markdown status404 "terms" "terms" `shouldReturn` Left (TRWait 0)
  forM_ [[], [(hContentType, "text/html; charset=utf-8")], [(hContentType, "application/octet-stream")], [(hContentType, "text/plain; charset=iso-8859-1")]] $ \headers ->
    served 3 headers status200 "terms" "terms" `shouldReturn` Left (TRWait 0)
  forM_ [(4, "text/plain"), (5, "TEXT/Markdown; Charset=UTF-8"), (6, "text/plain;charset=utf-8")] $ \(n, contentType) ->
    served n [(hContentType, contentType)] status200 "terms" "terms" `shouldReturn` Right "terms"
  served 7 markdown status200 (LB.fromStrict $ long <> "a") (long <> "a") `shouldReturn` Left (TRWait 0)
  served 8 markdown status200 (LB.fromStrict long) long `shouldReturn` Right (decodeUtf8 long)
  served 9 markdown status200 "\xff" "\xff" `shouldReturn` Left (TRWait 0)
  served 10 markdown status200 "other terms" "terms" `shouldReturn` Left (TRWait 0)
  elem "/elsewhere.md" <$> fetchedTexts node `shouldReturn` False

testTermsBackoff :: TVar ChaChaDRG -> IO ()
testTermsBackoff g = withContracts g $ \node ct -> do
  let generalPath = fakeTermsPath fakeGeneralTermsText
      waitsUpTo seconds = \case
        Left (TRWait n) -> n >= 1 && n <= seconds
        _ -> False
  answerText node generalPath $ TextAnswer status429 [("Retry-After", "2")] ""
  termsText ct (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 30)
  termsText ct (termsAt fakeGeneralTermsText) >>= (`shouldSatisfy` waitsUpTo 30)
  termsText ct (termsAt fakeCommunityTermsText) >>= (`shouldSatisfy` waitsUpTo 30)
  fetchedTexts node `shouldReturn` [generalPath]
  short <- (\x -> x {ctTermsBackoff = 1}) <$> (newContracts TLDSimplex =<< fakeChainClient g node)
  answerText node generalPath $ TextAnswer status503 [] ""
  termsText short (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 1)
  termsText short (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 1)
  threadDelay 1100000
  answerText node generalPath $ TextAnswer status503 [("Retry-After", "2")] ""
  termsText short (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 2)
  answerText node generalPath $ servedText fakeGeneralTermsText
  threadDelay 1100000
  termsText short (termsAt fakeCommunityTermsText) >>= (`shouldSatisfy` waitsUpTo 1)
  threadDelay 1000000
  termsText short (termsAt fakeGeneralTermsText) `shouldReturn` Right "SimpleX general terms v1"
  length <$> fetchedTexts node `shouldReturn` 4
  capped <- newContracts TLDSimplex =<< fakeChainClient g node
  answerText node generalPath $ TextAnswer status429 [("Retry-After", "999999999")] ""
  termsText capped (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 3600)

testTermsTimeout :: TVar ChaChaDRG -> IO ()
testTermsTimeout g = withFakeEthNode $ \node -> do
  c <- fakeChainClient g node
  ct <- newContracts TLDSimplex c {ccTimeout = testDeadline}
  setFault node "GET" $ Slow 2000000
  elapsed <- timed $ termsText ct (termsAt fakeGeneralTermsText) `shouldReturn` Left (TRWait 30)
  elapsed `shouldSatisfy` (< 1)

termsAt :: ByteString -> Terms
termsAt = fromJust . termsOf . fakeTermsLink

testRegistered :: TVar ChaChaDRG -> IO ()
testRegistered g = withContracts g $ \node ct -> do
  answerRegistrar node
  answerRegistered node (resolverOf node)
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (now + year)
  nameStatus ct alice `shouldReturn` Right (NSRegistered $ registeredAlice (resolverOf node) (now + year))

testGracePeriod :: TVar ChaChaDRG -> IO ()
testGracePeriod g = withContracts g $ \node ct -> do
  answerRegistrar node
  answerRegistered node (resolverOf node)
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  let expired = now - year
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 expired
  setBlock node 100 $ expired + gracePeriod
  nameStatus ct alice `shouldReturn` Right (NSRegistered $ registeredAlice (resolverOf node) expired)
  setBlock node 101 $ expired + gracePeriod + 1
  nameStatus ct alice `shouldReturn` Right NSAvailable

testRegistrarOnce :: TVar ChaChaDRG -> IO ()
testRegistrarOnce g = withContracts g $ \node ct -> do
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  nameStatus ct alice `shouldReturn` Left (ChainUnavailable "eth_call: error -32000: no fake answer for this call")
  answerRegistrar node
  nameStatus ct alice `shouldReturn` Right NSAvailable
  answerCall node (registrarOf node) ensCall $ Reverts ""
  answerCall node (registrarOf node) gracePeriodCall $ Reverts ""
  nameStatus ct alice `shouldReturn` Right NSAvailable

testRecords :: TVar ChaChaDRG -> IO ()
testRecords g = withContracts g $ \node ct -> do
  let resolver = resolverOf node
  answerCall node resolver contactTextCall $ Returns contactTextAnswer
  -- an unset record is the empty string
  answerCall node resolver channelTextCall $ Returns $ abiTuple [Dynamic $ abiBytes ""]
  answerCall node resolver noncesCall $ Returns $ uint256 (3 :: Int)
  nameRecords ct alice (registeredAlice resolver $ now + year)
    `shouldReturn` Right (Just NameRecords {rdContact = "https://smp8.simplex.im/a#Y8m3", rdChannel = "", rdNonce = 3})
  textRecord ct alice "simplex.contact" `shouldReturn` Right "https://smp8.simplex.im/a#Y8m3"

testOtherResolver :: TVar ChaChaDRG -> IO ()
testOtherResolver g = withContracts g $ \_ ct -> do
  nameRecords ct alice (registeredAlice otherResolver $ now + year) `shouldReturn` Right Nothing

testLaunchPrices :: TVar ChaChaDRG -> IO ()
testLaunchPrices g = withContracts g $ \node ct -> do
  -- viem: rentPriceUSD answering {base: 40000 * 10^16, premium: 0}
  answerCall node fakeController (rentPriceCall 6) $ Returns $ unhex "000000000000000000000000000000000000000000000015af1d78b58c4000000000000000000000000000000000000000000000000000000000000000000000"
  answerCall node fakeController (rentPriceCall 7) $ Returns $ rentPriceAnswer 20000
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  mapM (\len -> chainPriceCents ct len 2) [6, 7, 12] `shouldReturn` [Right 40000, Right 20000, Right 10000]

testNonces :: TVar ChaChaDRG -> IO ()
testNonces g = withContracts g $ \node ct -> do
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (5 :: Int)
  answerCall node (resolverOf node) noncesCall $ Returns $ uint256 (3 :: Int)
  sponsorNonce ct owner `shouldReturn` Right 5
  resolverNonce ct owner `shouldReturn` Right 3

testOwnerOfNobody :: TVar ChaChaDRG -> IO ()
testOwnerOfNobody g = withContracts g $ \node ct -> do
  answerCall node (registrarOf node) ownerOfCall $ Returns $ addressWord owner
  ownerOfToken ct Latest alice `shouldReturn` Right (Just owner)
  -- the registrar's require(expiries[tokenId] > block.timestamp), as anvil answers it with "0x" and reth without data
  answerCall node (registrarOf node) ownerOfCall $ Reverts ""
  ownerOfToken ct Latest alice `shouldReturn` Right Nothing
  setFault node "eth_call" $ RpcError 3 "execution reverted" Nothing
  ownerOfToken ct Latest alice `shouldReturn` Right Nothing

testOwnerOfFailed :: TVar ChaChaDRG -> IO ()
testOwnerOfFailed g = withContracts g $ \node ct -> do
  -- viem's encodeErrorResult of Error("ERC721: invalid token ID")
  answerCall node (registrarOf node) ownerOfCall $ Reverts $ unhex "08c379a0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000184552433732313a20696e76616c696420746f6b656e2049440000000000000000"
  ownerOfToken ct Latest alice `shouldReturn` Left (ChainReverted Nothing)
  -- viem's encodeErrorResult of ReentrantCall()
  answerCall node (registrarOf node) ownerOfCall $ Reverts $ unhex "37ed32e8"
  ownerOfToken ct Latest alice `shouldReturn` Left (ChainReverted $ Just RNReentrantCall)
  -- a node or proxy that keeps the reason in the message but drops the data
  setFault node "eth_call" $ RpcError (-32000) "execution reverted: ERC721: invalid token ID" Nothing
  ownerOfToken ct Latest alice `shouldReturn` Left (ChainReverted Nothing)
  setFault node "eth_call" HttpFailure
  ownerOfToken ct Latest alice `shouldReturn` Left (ChainUnavailable "eth_call: HTTP 502")

testCommitmentTime :: TVar ChaChaDRG -> IO ()
testCommitmentTime g = withContracts g $ \node ct -> do
  answerCall node fakeController commitmentsCall $ Returns $ uint256 (0 :: Int)
  commitmentTime ct commitment `shouldReturn` Right Nothing
  answerCall node fakeController commitmentsCall $ Returns $ uint256 now
  commitmentTime ct commitment `shouldReturn` Right (Just now)

testAvailability :: TVar ChaChaDRG -> IO ()
testAvailability g = withContracts g $ \node ct -> do
  answerCall node fakeController availableCall $ Returns $ uint256 (1 :: Int)
  controllerAvailable ct "alicebakery" `shouldReturn` Right True
  answerCall node fakeController availableCall $ Returns $ uint256 (0 :: Int)
  controllerAvailable ct "alicebakery" `shouldReturn` Right False
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (now + year)
  nameExpires ct Latest alice `shouldReturn` Right (now + year)

testCommitmentAges :: TVar ChaChaDRG -> IO ()
testCommitmentAges g = withContracts g $ \node ct -> do
  answerCall node fakeController minCommitmentAgeCall $ Returns $ uint256 (60 :: Int)
  answerCall node fakeController maxCommitmentAgeCall $ Returns $ uint256 (86400 :: Int)
  commitmentAges ct `shouldReturn` Right CommitmentAges {caMin = 60, caMax = 86400}
  answerCall node fakeController maxCommitmentAgeCall $ Returns $ uint256 (3600 :: Int)
  commitmentAges ct `shouldReturn` Right CommitmentAges {caMin = 60, caMax = 86400}

testFailedRead :: TVar ChaChaDRG -> IO ()
testFailedRead g = withContracts g $ \node ct -> do
  answerRegistrar node
  setBlock node 100 now
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  answerCall node (registrarOf node) nameExpiresCall $ Reverts ""
  nameStatus ct alice `shouldReturn` Left (ChainReverted $ Just RNWithoutData)
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  setFault node "eth_getBlockByNumber" HttpFailure
  nameStatus ct alice `shouldReturn` Left (ChainUnavailable "eth_getBlockByNumber: HTTP 502")
  clearFault node "eth_getBlockByNumber"
  answerCall node fakeNamePolicy reservedNamesCall $ Reverts ""
  nameStatus ct alice `shouldReturn` Left (ChainReverted $ Just RNWithoutData)

testMalformed :: TVar ChaChaDRG -> IO ()
testMalformed g = withContracts g $ \node ct -> do
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ B.replicate 31 0
  nameExpires ct Latest alice `shouldReturn` Left (ChainUnavailable "eth_call: nameExpires(uint256) answered AEShort")
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (toInteger (maxBound :: Word64) + 1)
  sponsorNonce ct owner `shouldReturn` Left (ChainUnavailable "eth_call: nonces(address) answered AERange")

testStatusDeadline :: TVar ChaChaDRG -> IO ()
testStatusDeadline g = withContracts g $ \node ct -> do
  answerRegistrar node
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  setFault node "eth_getBlockByNumber" Hang
  elapsed <- timed $ nameStatus ct {ctDeadline = testDeadline} alice `shouldReturn` Left (ChainUnavailable "name status: timed out")
  elapsed `shouldSatisfy` (< 2)
  clearFault node "eth_getBlockByNumber"
  nameStatus ct {ctDeadline = testDeadline} alice `shouldReturn` Right NSAvailable

testRecordsDeadline :: TVar ChaChaDRG -> IO ()
testRecordsDeadline g = withContracts g $ \node ct -> do
  setFault node "eth_call" Hang
  elapsed <- timed $ nameRecords ct {ctDeadline = testDeadline} alice (registeredAlice (resolverOf node) $ now + year) `shouldReturn` Left (ChainUnavailable "name records: timed out")
  elapsed `shouldSatisfy` (< 2)

-- | Each slow read takes a third of the deadline or more, so the status's reads in sequence (four) or the records' (three)
-- pass it, while their concurrent rounds (two and one) meet it.
testConcurrentReads :: TVar ChaChaDRG -> IO ()
testConcurrentReads g = withContracts g $ \node ct -> do
  let resolver = resolverOf node
      ct' = ct {ctDeadline = concurrentDeadline}
      registered = registeredAlice resolver (now + year)
  answerRegistrar node
  answerRegistered node resolver
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (now + year)
  answerCall node resolver contactTextCall $ Returns contactTextAnswer
  answerCall node resolver channelTextCall $ Returns $ abiTuple [Dynamic $ abiBytes ""]
  answerCall node resolver noncesCall $ Returns $ uint256 (3 :: Int)
  -- the registrar's constants are read once, so this read takes them out of the timed ones
  nameStatus ct alice `shouldReturn` Right (NSRegistered registered)
  forM_ ["eth_call", "eth_getBlockByNumber"] $ \m -> setFault node m $ Slow $ concurrentDeadline * 2 `div` 5
  nameStatus ct' alice `shouldReturn` Right (NSRegistered registered)
  forM_ ["eth_call", "eth_getBlockByNumber"] $ \m -> setFault node m $ Slow $ concurrentDeadline * 3 `div` 5
  nameRecords ct' alice registered `shouldReturn` Right (Just NameRecords {rdContact = "https://smp8.simplex.im/a#Y8m3", rdChannel = "", rdNonce = 3})

-- fixtures

concurrentDeadline :: Int
concurrentDeadline = 1500000

-- | Far below a call's 10-second timeout, so only the deadline can end a hung read in time.
testDeadline :: Int
testDeadline = 300000

timed :: IO () -> IO NominalDiffTime
timed action = do
  start <- getCurrentTime
  action
  (`diffUTCTime` start) <$> getCurrentTime

withContracts :: TVar ChaChaDRG -> (FakeEthNode -> Contracts -> IO a) -> IO a
withContracts g action = withFakeEthNode $ \node -> do
  ct <- newContracts TLDSimplex =<< fakeChainClient g node
  action node ct

answerRegistrar :: FakeEthNode -> IO ()
answerRegistrar node = do
  answerCall node (registrarOf node) ensCall $ Returns $ addressWord registry
  answerCall node (registrarOf node) gracePeriodCall $ Returns $ uint256 gracePeriod

answerRegistered :: FakeEthNode -> Address -> IO ()
answerRegistered node resolver = do
  answerCall node registry registryOwnerCall $ Returns $ addressWord owner
  answerCall node registry registryResolverCall $ Returns $ addressWord resolver

registeredAlice :: Address -> Word64 -> RegisteredName
registeredAlice rnResolver rnExpiresAt = RegisteredName {rnOwner = owner, rnResolver, rnExpiresAt, rnGracePeriod = toInteger gracePeriod}

registrarOf, resolverOf :: FakeEthNode -> Address
registrarOf = cBaseRegistrar . fakeChainConfig
resolverOf = cResolver . fakeChainConfig

rentPriceAnswer :: Integer -> ByteString
rentPriceAnswer cents = uint256 (cents * 10 ^ (16 :: Int)) <> uint256 (0 :: Int)

-- viem: text answering "https://smp8.simplex.im/a#Y8m3"
contactTextAnswer :: ByteString
contactTextAnswer = unhex "0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000001e68747470733a2f2f736d70382e73696d706c65782e696d2f612359386d330000"

-- 2027-01-01, the term start
now :: Word64
now = 1798761600

year, gracePeriod :: Word64
year = 31536000
gracePeriod = 7776000

alice :: LabelHash
alice = labelHash "alicebakery"

-- viem: namehash("alicebakery.simplex")
aliceNode :: ByteString
aliceNode = unhex "0f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a"

commitment :: ByteString
commitment = unhex "f8096c3f3cfbadc9f3a108d2c586ccc816c0510711c8d6228aa4fa507324d30c"

owner, registry, otherResolver :: Address
owner = address "0x014f9c09cceedef1b713110d04d3a07d5b5d9b73"
registry = address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
otherResolver = address "0x4826533b4897376654bb4d4ad88b7fafd0c98528"

-- viem's calldata for the reads

ensCall, gracePeriodCall, nameExpiresCall, ownerOfCall, reservedNamesCall, registryOwnerCall, registryResolverCall :: ByteString
ensCall = unhex "3f15457f"
gracePeriodCall = unhex "c1a287e2"
nameExpiresCall = unhex "d6e4fa86bcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6"
ownerOfCall = unhex "6352211ebcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6"
reservedNamesCall = unhex "389c0748bcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6"
registryOwnerCall = unhex "02571be30f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a"
registryResolverCall = unhex "0178b8bf0f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a"

contactTextCall, channelTextCall, noncesCall :: ByteString
contactTextCall = unhex "59d1d43c0f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000"
channelTextCall = unhex "59d1d43c0f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a0000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000"
noncesCall = unhex "7ecebe00000000000000000000000000014f9c09cceedef1b713110d04d3a07d5b5d9b73"

-- | rentPriceUSD("a" × length, 2 years)
rentPriceCall :: Int -> ByteString
rentPriceCall = \case
  6 -> unhex "9ff7ec0900000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000003c2670000000000000000000000000000000000000000000000000000000000000000066161616161610000000000000000000000000000000000000000000000000000"
  7 -> unhex "9ff7ec0900000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000003c2670000000000000000000000000000000000000000000000000000000000000000076161616161616100000000000000000000000000000000000000000000000000"
  11 -> unhex "9ff7ec0900000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000003c26700000000000000000000000000000000000000000000000000000000000000000b6161616161616161616161000000000000000000000000000000000000000000"
  12 -> unhex "9ff7ec0900000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000003c26700000000000000000000000000000000000000000000000000000000000000000c6161616161616161616161610000000000000000000000000000000000000000"
  len -> error $ "no viem vector for length " <> show len

commitmentsCall, availableCall, minCommitmentAgeCall, maxCommitmentAgeCall :: ByteString
commitmentsCall = unhex "839df945f8096c3f3cfbadc9f3a108d2c586ccc816c0510711c8d6228aa4fa507324d30c"
availableCall = unhex "aeb8ce9b0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000b616c69636562616b657279000000000000000000000000000000000000000000"
minCommitmentAgeCall = unhex "8d839ffe"
maxCommitmentAgeCall = unhex "ce1e09c0"

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16

address :: ByteString -> Address
address = either error id . strDecode
