{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesCommunityTests (namesCommunityTests) where

import Bots.StoreService.ConfigTests (chainSection, namesIni, namesRefusal, namesSection, parseIni, parsedNames, termsUrl, termsVersion, withIni)
import Bots.StoreService.FakeEthNode (CallAnswer (..), answerCall, answerListing, fakeCommunityTerms, fakeController, fakeNamePolicy, setCommunityTerms)
import Bots.StoreService.NamesContractsTests (withContracts)
import Bots.StoreService.NamesSearchTests (aliceHex)
import Bots.StoreService.WebTests (capturingStderr)
import Control.Concurrent.STM (TVar)
import Control.Monad (forM_, void)
import Crypto.Random (ChaChaDRG)
import Data.Functor ((<&>))
import Data.Ini (readIniFile)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Config (NamesConfig (..), TermsConfig (..), unknownKeys)
import StoreService.Names.Abi (bytes32FromHex, hex, selector)
import StoreService.Names.Community (agreedTermsHash, communityFor, termsFor)
import StoreService.Names.View (addressText)
import StoreService.Service (checkNamePolicy)
import Test.Hspec

namesCommunityTests :: Spec
namesCommunityTests = describe "StoreService names: community" $ do
  g <- runIO C.newRandom
  it "reads the terms, and the list keyed by the label's keccak256, empty without [community]" testCommunityParsed
  it "refuses [community] without [names]" testCommunityWithoutNames
  it "refuses a section header written twice, naming it" testRepeatedHeader
  it "refuses a key written twice in any section, naming it" testRepeatedKey
  it "refuses a label that is not a valid name, upper case included, naming it" testBadLabel
  it "refuses an empty or over-long text, or one with a control, format or line separator character" testBadText
  it "refuses a missing or malformed terms_version or terms_url" testBadTerms
  it "names no community label as a setting nothing reads" testNoUnknownKeys
  it "logs how many community names it loaded" testLoadedCount
  it "gives the version for an unlisted name and its community variant with the terms' hash for a listed one, and reads that hash back" testTermsFor
  it "logs the name policy and its terms at startup, and checks the [community] lines" $ testStartupCheck g
  it "checks no [community] line when the policy cannot be read at startup" $ testStartupUnread g
  it "warns at startup on a policy without terms, or whose terms cannot be read" $ testStartupWarnings g

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
  NamesConfig {nTerms, nCommunity} <- parsedNames namesSection chainSection
  nTerms `shouldBe` TermsConfig {tVersion = termsVersion, tUrl = termsUrl}
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
  namesRefusal (namesSection <> ["terms_version = 2026-11-01"]) chainSection
    `shouldReturn` "names.terms_version is written twice, and only the first of them would be read"

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

testBadTerms :: IO ()
testBadTerms = do
  let without k = filter (not . (k `T.isPrefixOf`)) namesSection
      withValue k v = without k <> [k <> " = " <> v]
      badVersion = "names.terms_version must be 1 to 32 letters, digits, dots, underscores or hyphens"
      badUrl = "names.terms_url must be an https URL"
  namesRefusal (without "terms_version") chainSection `shouldReturn` "names.terms_version is required"
  namesRefusal (withValue "terms_version" "") chainSection `shouldReturn` "names.terms_version is required"
  forM_ ["2026/10/23", "2026 10 23", "v1+2", "версия", T.replicate 33 "a"] $ \v ->
    namesRefusal (withValue "terms_version" v) chainSection `shouldReturn` badVersion
  NamesConfig {nTerms = TermsConfig {tVersion}} <- parsedNames (withValue "terms_version" ("Az09._-" <> T.replicate 25 "a")) chainSection
  tVersion `shouldBe` "Az09._-" <> T.replicate 25 "a"
  namesRefusal (without "terms_url") chainSection `shouldReturn` "names.terms_url is required"
  forM_ ["http://simplex.chat/terms", "simplex.chat/terms", "https://", "https://simplex.chat/name terms", "HTTPS://simplex.chat/terms"] $ \u ->
    namesRefusal (withValue "terms_url" u) chainSection `shouldReturn` badUrl

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
  cfg <- parsedNames namesSection chainSection
  termsFor cfg Nothing `shouldBe` termsVersion
  -- viem: keccak256(toHex("bona fide community terms v1")), as the contracts' tests set their terms
  let hexDigits = "e2937ea655d8e743e2a8cecbab83acdf055846c80e13001ec1f8dc404e4ef2d9"
      listed = termsVersion <> "/community/" <> hexDigits
  termsFor cfg (Just fakeCommunityTerms) `shouldBe` listed
  agreedTermsHash listed `shouldBe` Just fakeCommunityTerms
  agreedTermsHash termsVersion `shouldBe` Nothing
  agreedTermsHash (termsVersion <> "/community") `shouldBe` Nothing
  agreedTermsHash (termsVersion <> "/community/" <> T.toUpper hexDigits) `shouldBe` Nothing

testStartupCheck :: TVar ChaChaDRG -> IO ()
testStartupCheck g = withContracts g $ \node ct -> do
  cfg <- parsedNames namesSection chainSection <&> \c -> c {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) "Alice's bakery"}
  logged <- capturingStderr $ checkNamePolicy cfg ct
  logged `shouldSatisfy` T.isInfixOf ("names: name policy " <> addressText fakeNamePolicy)
  logged `shouldSatisfy` T.isInfixOf ("names: community terms " <> hex fakeCommunityTerms)
  logged `shouldSatisfy` T.isInfixOf "names: [community] lists a name the name policy does not list, so it is sold as a plain name: Alice's bakery"
  answerListing node aliceHash $ Reverts ""
  unread <- capturingStderr $ checkNamePolicy cfg ct
  unread `shouldSatisfy` T.isInfixOf "names: a [community] name's listing could not be read: Alice's bakery"

testStartupUnread :: TVar ChaChaDRG -> IO ()
testStartupUnread g = withContracts g $ \node ct -> do
  cfg <- parsedNames namesSection chainSection <&> \c -> c {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) "a bakery the policy is not read for"}
  answerCall node fakeController (selector "namePolicy()") $ Reverts ""
  warned <- capturingStderr $ checkNamePolicy cfg ct
  warned `shouldSatisfy` T.isInfixOf "names: the controller's name policy could not be read, it is read on first use"
  -- a text no other case logs, so a line another case logs late cannot match it
  warned `shouldNotSatisfy` T.isInfixOf "a bakery the policy is not read for"

testStartupWarnings :: TVar ChaChaDRG -> IO ()
testStartupWarnings g = withContracts g $ \node ct -> do
  cfg <- parsedNames namesSection chainSection
  let warns text = capturingStderr (checkNamePolicy cfg ct) >>= (`shouldSatisfy` T.isInfixOf text)
  setCommunityTerms node ""
  warns "names: the name policy has no community terms yet, so community names cannot be ordered until the owner sets them"
  answerCall node fakeNamePolicy (selector "termsHash()") $ Reverts ""
  warns "names: the name policy's community terms could not be read: "
