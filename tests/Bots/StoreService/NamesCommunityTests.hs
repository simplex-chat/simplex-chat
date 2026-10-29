{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesCommunityTests (namesCommunityTests) where

import Bots.StoreService.ConfigTests (chainSection, namesIni, namesRefusal, namesSection, parseIni, parsedNames, withIni)
import Bots.StoreService.FakeEthNode (CallAnswer (..), TextAnswer (..), answerCall, answerListing, answerText, fakeCommunityTerms, fakeCommunityTermsText, fakeController, fakeNamePolicy, fakeNameTerms, fakeNameTermsText, fakeNewCommunityTermsText, fakeTermsLink, fakeTermsPath, setCommunityTerms, setNameTerms)
import Bots.StoreService.NamesContractsTests (withContracts)
import Bots.StoreService.NamesSearchTests (aliceHex)
import Bots.StoreService.WebTests (capturingStderr)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_, void)
import Crypto.Random (ChaChaDRG)
import Data.Ini (readIniFile)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status404)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Config (NamesConfig (..), unknownKeys)
import StoreService.Names.Abi (AbiValue (..), abiBytes, abiTuple, bytes32FromHex, hex, selector)
import StoreService.Names.Community (agreedTermsHash, communityFor, termsFor)
import StoreService.Names.Contracts (Terms (..))
import StoreService.Names.View (addressText)
import StoreService.Service (logNamePolicy)
import Test.Hspec

namesCommunityTests :: Spec
namesCommunityTests = describe "StoreService names: community" $ do
  g <- runIO C.newRandom
  it "reads the list keyed by the label's keccak256, empty without [community]" testCommunityParsed
  it "refuses [community] without [names]" testCommunityWithoutNames
  it "refuses a section header written twice, naming it" testRepeatedHeader
  it "refuses a key written twice in any section, naming it" testRepeatedKey
  it "refuses a label that is not a valid name, upper case included, naming it" testBadLabel
  it "refuses an empty or over-long text, or one with a control, format or line separator character" testBadText
  it "names no community label as a setting nothing reads" testNoUnknownKeys
  it "logs how many community names it loaded" testLoadedCount
  it "gives the terms' hash for an unlisted name and community/ with it for a listed one, and reads a community hash back, not a version's" testTermsFor
  it "logs the name policy and both terms links at startup, and checks the [community] lines" $ testStartupCheck g
  it "checks no [community] line when the policy cannot be read at startup" $ testStartupUnread g
  it "warns at startup on terms not set, unread, of a link the service does not take, or whose text cannot be fetched" $ testStartupWarnings g

bakersText :: Text
bakersText = "bakers and bakeries in Portugal"

communityLines :: [Text] -> [Text]
communityLines entries = "[community]" : entries

withCommunity :: [Text] -> IO NamesConfig
withCommunity entries = parsedNames namesSection (chainSection <> communityLines entries)

refusedCommunity :: [Text] -> IO String
refusedCommunity entries = namesRefusal namesSection (chainSection <> communityLines entries)

-- viem: keccak256 of "alicebakery", as the page sends it
aliceHash :: LabelHash
aliceHash = LabelHash $ fromJust $ bytes32FromHex aliceHex

testCommunityParsed :: IO ()
testCommunityParsed = do
  NamesConfig {nCommunity} <- parsedNames namesSection chainSection
  nCommunity `shouldBe` M.empty
  cfg@NamesConfig {nCommunity = listed} <- withCommunity ["bakers = " <> bakersText, "alicebakery = Alice's bakery, Lisbon; open daily"]
  M.size listed `shouldBe` 2
  communityFor cfg aliceHash `shouldBe` Just "Alice's bakery, Lisbon; open daily"
  communityFor cfg (labelHash "bakers") `shouldBe` Just bakersText
  communityFor cfg (labelHash "bobsbakery") `shouldBe` Nothing

testCommunityWithoutNames :: IO ()
testCommunityWithoutNames =
  namesRefusal [] (communityLines ["bakers = " <> bakersText]) `shouldReturn` "[community] is set but [names] is absent, so nothing would use it"

testRepeatedHeader :: IO ()
testRepeatedHeader = do
  refusedCommunity ["bakers = " <> bakersText, "[community]", "florists = florists in Lisbon"]
    `shouldReturn` "[community] is written twice, and only the last of them would be read"
  namesRefusal (namesSection <> chainSection <> namesSection) [] `shouldReturn` "[names] is written twice, and only the last of them would be read"
  namesRefusal namesSection (chainSection <> ["[legacy]", "a = 1", " [ legacy ] "]) `shouldReturn` "[legacy] is written twice, and only the last of them would be read"

testRepeatedKey :: IO ()
testRepeatedKey = do
  refusedCommunity ["bakers = " <> bakersText, "florists = florists in Lisbon", "bakers = bakers of Porto"]
    `shouldReturn` "community.bakers is written twice, and only the first of them would be read"
  namesRefusal (namesSection <> ["tld = simplex"]) chainSection
    `shouldReturn` "names.tld is written twice, and only the first of them would be read"

testBadLabel :: IO ()
testBadLabel = do
  forM_ ["Bakers", "bak--ers", "bakers-", "bak_ers", "bäkers"] $ \l ->
    refusedCommunity [l <> " = " <> bakersText] `shouldReturn` ("community." <> T.unpack l <> " must be lower-case letters and digits, with single hyphens between them")
  forM_ ["baker", T.replicate 64 "a"] $ \l ->
    refusedCommunity [l <> " = " <> bakersText] `shouldReturn` ("community." <> T.unpack l <> " must be 6 to 63 characters")

testBadText :: IO ()
testBadText = do
  let badLength = "community.bakers must be 1 to 200 characters"
      invisible = "community.bakers has a control, format or line separator character"
  refusedCommunity ["bakers ="] `shouldReturn` badLength
  refusedCommunity ["bakers = " <> T.replicate 201 "a"] `shouldReturn` badLength
  forM_ ["bakers\tand bakeries", "bakers \ESC[31m", "bakers\DEL"] $ \t ->
    refusedCommunity ["bakers = " <> t] `shouldReturn` invisible
  -- right-to-left override and zero-width space (format), line and paragraph separators
  forM_ ["bakers \x202E and bakeries", "bakers\x200B and bakeries", "bakers\x2028 and bakeries", "bakers\x2029 and bakeries"] $ \t ->
    refusedCommunity ["bakers = " <> t] `shouldReturn` invisible
  NamesConfig {nCommunity} <- withCommunity ["bakers = " <> T.replicate 200 "é"]
  M.elems nCommunity `shouldBe` [T.replicate 200 "é"]

testNoUnknownKeys :: IO ()
testNoUnknownKeys =
  withIni (namesIni namesSection (chainSection <> communityLines ["bakers = " <> bakersText, "florists = florists in Lisbon"])) $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldBe` []

testLoadedCount :: IO ()
testLoadedCount = do
  listed <- capturingStderr $ void $ parseIni (namesIni namesSection (chainSection <> communityLines ["bakers = " <> bakersText, "florists = florists in Lisbon"]))
  listed `shouldSatisfy` T.isInfixOf "names: 2 community names loaded"
  none <- capturingStderr $ void $ parseIni (namesIni namesSection chainSection)
  none `shouldSatisfy` T.isInfixOf "names: 0 community names loaded"

testTermsFor :: IO ()
testTermsFor = do
  let hexDigits = "e2937ea655d8e743e2a8cecbab83acdf055846c80e13001ec1f8dc404e4ef2d9"
      h = fromJust $ bytes32FromHex $ "0x" <> hexDigits
      terms = Terms {tmLink = "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/4cb24444a1f2e3d4c5b6a7980716253443526170/docs/terms.md#sha256=" <> hexDigits, tmHash = h}
      listed = "community/" <> hexDigits
  termsFor False terms `shouldBe` hexDigits
  termsFor True terms `shouldBe` listed
  agreedTermsHash listed `shouldBe` Just h
  agreedTermsHash hexDigits `shouldBe` Nothing
  agreedTermsHash "community" `shouldBe` Nothing
  agreedTermsHash ("2026-10-23/community/" <> hexDigits) `shouldBe` Nothing
  agreedTermsHash ("community/" <> T.toUpper hexDigits) `shouldBe` Nothing

testStartupCheck :: TVar ChaChaDRG -> IO ()
testStartupCheck g = withContracts g $ \node ct -> do
  cfg <- withCommunity ["alicebakery = Alice's bakery"]
  logged <- capturingStderr $ logNamePolicy cfg ct
  logged `shouldSatisfy` T.isInfixOf ("names: name policy " <> addressText fakeNamePolicy)
  logged `shouldSatisfy` T.isInfixOf ("names: name terms " <> fakeTermsLink fakeNameTermsText <> ", accepted as " <> hex fakeNameTerms)
  logged `shouldSatisfy` T.isInfixOf ("names: community name terms " <> fakeTermsLink fakeCommunityTermsText <> ", accepted as " <> hex fakeCommunityTerms)
  logged `shouldSatisfy` T.isInfixOf "names: [community] lists a name the name policy does not list, so it is sold as a plain name: Alice's bakery"
  answerListing node aliceHash $ Reverts ""
  unread <- capturingStderr $ logNamePolicy cfg ct
  unread `shouldSatisfy` T.isInfixOf "names: a [community] name's listing could not be read: Alice's bakery"

testStartupUnread :: TVar ChaChaDRG -> IO ()
testStartupUnread g = withContracts g $ \node ct -> do
  cfg <- withCommunity ["alicebakery = a bakery the policy is not read for"]
  answerCall node fakeController (selector "namePolicy()") $ Reverts ""
  warned <- capturingStderr $ logNamePolicy cfg ct
  warned `shouldSatisfy` T.isInfixOf "names: the controller's name policy could not be read, it is read on first use"
  -- a text no other case logs, so a line another case logs late cannot match it
  warned `shouldNotSatisfy` T.isInfixOf "a bakery the policy is not read for"

testStartupWarnings :: TVar ChaChaDRG -> IO ()
testStartupWarnings g = withContracts g $ \node ct -> do
  cfg <- parsedNames namesSection chainSection
  let warns text = capturingStderr (logNamePolicy cfg ct) >>= (`shouldSatisfy` T.isInfixOf text)
      otherLink = "https://example.org/simplex-chat/terms.md"
  setNameTerms node ""
  warns "names: the name policy has no name terms yet, so plain names cannot be ordered until the owner sets them"
  setCommunityTerms node ""
  warns "names: the name policy has no community name terms yet, so community names cannot be ordered until the owner sets them"
  answerCall node fakeNamePolicy (selector "nameTermsLink()") $ Returns $ abiTuple [Dynamic $ abiBytes $ encodeUtf8 otherLink]
  warns $ "names: the name policy's name terms are not a link the service takes, so plain names cannot be ordered: " <> otherLink
  setCommunityTerms node fakeNewCommunityTermsText
  answerText node (fakeTermsPath fakeNewCommunityTermsText) $ TextAnswer status404 [] ""
  warns $ "names: the text of the community name terms could not be fetched, so community names cannot be ordered until it is: " <> fakeTermsLink fakeNewCommunityTermsText
  answerCall node fakeNamePolicy (selector "nameTermsLink()") $ Reverts ""
  warns "names: the name policy's terms could not be read: "
