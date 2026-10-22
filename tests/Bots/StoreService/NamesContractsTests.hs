{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    pricesCall,
    launchPricesAnswer,
    pricesAnswer,
    oracle,
    alice,
    unhex,
  )
where

import Bots.StoreService.FakeEthNode
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash, SimplexTLD (..), labelHash)
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
    it "finds the code length for each launch price" $ testLaunchCodeLengths g
    it "requires every longer length to be covered when the list is not monotone" $ testNonMonotone g
    it "reads the oracle from the controller on each call" $ testOracleReplaced g
  describe "other reads" $ do
    it "reads the sponsor's and the resolver's nonces from their own contracts" $ testNonces g
    it "reads ownerOf's revert without data as nobody" $ testOwnerOfNobody g
    it "reads any other ownerOf revert or failure as a failed read" $ testOwnerOfFailed g
    it "reads a commitment's time, none as Nothing" $ testCommitmentTime g
    it "reads the controller's availability and the name's expiry" $ testAvailability g
    it "reads the commitment ages once" $ testCommitmentAges g
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
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  nameStatus ct alice `shouldReturn` Right NSAvailable

testReserved :: TVar ChaChaDRG -> IO ()
testReserved g = withContracts g $ \node ct -> do
  answerRegistrar node
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  -- viem: reservedNames answering Reason 1
  answerCall node fakeController reservedNamesCall $ Returns $ unhex "0000000000000000000000000000000000000000000000000000000000000001"
  nameStatus ct alice `shouldReturn` Right NSReserved

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
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
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
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
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

testLaunchCodeLengths :: TVar ChaChaDRG -> IO ()
testLaunchCodeLengths g = withContracts g $ \node ct -> do
  answerCall node fakeController pricesCall $ Returns $ addressWord oracle
  answerCall node oracle pricesCall $ Returns launchPricesAnswer
  mapM (\cents -> codeLengthFor ct cents 2) [40000, 20000, 10000, 9999]
    `shouldReturn` [Right (Just 6), Right (Just 7), Right (Just 8), Right Nothing]
  codeLengthFor ct 20000 4 `shouldReturn` Right (Just 8)

testNonMonotone :: TVar ChaChaDRG -> IO ()
testNonMonotone g = withContracts g $ \node ct -> do
  answerCall node fakeController pricesCall $ Returns $ addressWord oracle
  answerCall node oracle pricesCall $ Returns $ pricesAnswer 5000 [(6, 15000), (7, 20000)]
  mapM (\cents -> codeLengthFor ct cents 2) [30000, 40000] `shouldReturn` [Right (Just 8), Right (Just 6)]
  answerCall node oracle pricesCall $ Returns $ pricesAnswer 5000 [(63, 30000)]
  mapM (\cents -> codeLengthFor ct cents 2) [59999, 60000] `shouldReturn` [Right Nothing, Right (Just 6)]

testOracleReplaced :: TVar ChaChaDRG -> IO ()
testOracleReplaced g = withContracts g $ \node ct -> do
  let newOracle = address "0x8a791620dd6260079bf849dc5567adc3f2fdc318"
  answerCall node fakeController pricesCall $ Returns $ addressWord oracle
  answerCall node oracle pricesCall $ Returns launchPricesAnswer
  answerCall node newOracle pricesCall $ Returns $ pricesAnswer 10000 []
  codeLengthFor ct 20000 2 `shouldReturn` Right (Just 7)
  answerCall node fakeController pricesCall $ Returns $ addressWord newOracle
  codeLengthFor ct 20000 2 `shouldReturn` Right (Just 6)

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
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
  answerCall node (registrarOf node) nameExpiresCall $ Reverts ""
  nameStatus ct alice `shouldReturn` Left (ChainReverted $ Just RNWithoutData)
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  setFault node "eth_getBlockByNumber" HttpFailure
  nameStatus ct alice `shouldReturn` Left (ChainUnavailable "eth_getBlockByNumber: HTTP 502")
  clearFault node "eth_getBlockByNumber"
  answerCall node fakeController reservedNamesCall $ Reverts ""
  nameStatus ct alice `shouldReturn` Left (ChainReverted $ Just RNWithoutData)

testMalformed :: TVar ChaChaDRG -> IO ()
testMalformed g = withContracts g $ \node ct -> do
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ B.replicate 31 0
  nameExpires ct Latest alice `shouldReturn` Left (ChainUnavailable "eth_call: nameExpires(uint256) answered AEShort")
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (toInteger (maxBound :: Word64) + 1)
  sponsorNonce ct owner `shouldReturn` Left (ChainUnavailable "eth_call: nonces(address) answered AERange")
  answerCall node fakeController pricesCall $ Returns $ addressWord oracle
  answerCall node oracle pricesCall $ Returns $ B.dropEnd 32 launchPricesAnswer
  codeLengthFor ct 40000 2 `shouldReturn` Left (ChainUnavailable "eth_call: prices() answered AEShort")

testStatusDeadline :: TVar ChaChaDRG -> IO ()
testStatusDeadline g = withContracts g $ \node ct -> do
  answerRegistrar node
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 (0 :: Int)
  answerCall node fakeController reservedNamesCall $ Returns $ uint256 (0 :: Int)
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
  ct <- newContracts TLDSimplex =<< newChainClient g (fakeChainConfig node)
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

pricesAnswer :: Integer -> [(Integer, Integer)] -> ByteString
pricesAnswer base exceptions = abiTuple [Static $ uint256 base, Dynamic $ uint256 (length exceptions) <> B.concat [uint256 len <> uint256 cents | (len, cents) <- exceptions]]

-- viem: prices() answering the launch list, base 5000 and [{7, 10000}, {6, 20000}]
launchPricesAnswer :: ByteString
launchPricesAnswer = unhex "0000000000000000000000000000000000000000000000000000000000001388000000000000000000000000000000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000007000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000004e20"

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

owner, registry, oracle, otherResolver :: Address
owner = address "0x014f9c09cceedef1b713110d04d3a07d5b5d9b73"
registry = address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
oracle = address "0xa513e6e4b8f2a923d98304ec87f64353c4d5c853"
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

pricesCall, commitmentsCall, availableCall, minCommitmentAgeCall, maxCommitmentAgeCall :: ByteString
pricesCall = unhex "d3419bf3"
commitmentsCall = unhex "839df945f8096c3f3cfbadc9f3a108d2c586ccc816c0510711c8d6228aa4fa507324d30c"
availableCall = unhex "aeb8ce9b0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000b616c69636562616b657279000000000000000000000000000000000000000000"
minCommitmentAgeCall = unhex "8d839ffe"
maxCommitmentAgeCall = unhex "ce1e09c0"

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16

address :: ByteString -> Address
address = either error id . strDecode
