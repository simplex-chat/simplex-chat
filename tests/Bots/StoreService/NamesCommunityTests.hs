{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesCommunityTests (namesCommunityTests) where

import Bots.StoreService.ConfigTests (chainSection, namesIni, namesRefusal, namesSection, parseIni, parsedNames, termsUrl, termsVersion, withIni)
import Bots.StoreService.FakeEthNode (CallAnswer (..), answerCall, answerListing, fakeCommunityTerms, fakeCommunityTermsText, fakeController, fakeNewCommunityTerms, fakeTermsUrl, setCommunityTerms)
import Bots.StoreService.NamesContractsTests (withContracts)
import Bots.StoreService.NamesSearchTests (aliceHex)
import Bots.StoreService.WebTests (capturingStderr)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad (forM_, void)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Functor ((<&>))
import Data.Ini (readIniFile)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (Status, notFound404, ok200)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..), TermsConfig (..), unknownKeys)
import StoreService.Names.Abi (bytes32FromHex, hex, selector)
import StoreService.Names.Community (TermsError (..), agreedTermsHash, communityFor, currentCommunityTerms, maxTermsBytes, termsFor)
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
  it "reads community_terms_url: https, or http on loopback only on the local chain" testCommunityTermsUrl
  it "takes the policy's terms once the page's text hashes to them, and fetches again for new terms, none without terms on chain or community_terms_url" $ testCurrentTerms g
  it "checks the policy's terms and the [community] lines at startup, stopping only on terms the page does not hash to" $ testStartupCheck g
  it "checks neither the terms nor the [community] lines when the policy cannot be read at startup" $ testStartupUnread g
  it "warns at startup, without stopping, on no terms on chain, no community_terms_url or a page it cannot read" $ testStartupWarnings g
  it "takes no terms from a page that answers another status or more than 1 MiB, or that cannot be reached" $ testTermsPageRefused g

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

withTermsUrl :: Text -> IO NamesConfig
withTermsUrl url = parsedNames (namesSection <> ["community_terms_url = " <> url]) chainSection

-- | A page answering the status and text the test may change, at the URL it is given.
withTermsPage :: TVar (Status, B.ByteString) -> (Text -> IO a) -> IO a
withTermsPage page action =
  Warp.testWithApplication (pure $ \_ respond -> readTVarIO page >>= \(status, text) -> respond $ responseLBS status [] $ LB.fromStrict text) $ \port ->
    action $ "http://127.0.0.1:" <> T.pack (show port) <> "/terms"

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

testCurrentTerms :: TVar ChaChaDRG -> IO ()
testCurrentTerms g = withContracts g $ \node ct ->
  newTVarIO (ok200, fakeCommunityTermsText) >>= \page -> withTermsPage page $ \url -> do
    cfg <- withTermsUrl url
    currentCommunityTerms cfg ct `shouldReturn` Right fakeCommunityTerms
    -- checked once: a page edited without new terms on chain is not fetched again
    atomically $ writeTVar page (ok200, "other text")
    currentCommunityTerms cfg ct `shouldReturn` Right fakeCommunityTerms
    setCommunityTerms node fakeNewCommunityTerms
    currentCommunityTerms cfg ct `shouldReturn` Left (TEMismatch fakeNewCommunityTerms)
    atomically $ writeTVar page (ok200, "bona fide community terms v2")
    currentCommunityTerms cfg ct `shouldReturn` Right fakeNewCommunityTerms
    setCommunityTerms node $ B.replicate 32 0
    currentCommunityTerms cfg ct `shouldReturn` Left TENoTerms
    setCommunityTerms node $ C.keccak256 "bona fide community terms v3"
    currentCommunityTerms cfg {nCommunityTermsUrl = Nothing} ct `shouldReturn` Left TENoUrl

testStartupCheck :: TVar ChaChaDRG -> IO ()
testStartupCheck g = withContracts g $ \node ct -> do
  cfg <- withTermsUrl (fakeTermsUrl node) <&> \c -> c {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) "Alice's bakery"}
  warned <- capturingStderr $ checkNamePolicy cfg ct `shouldReturn` Right ()
  warned `shouldSatisfy` T.isInfixOf "names: [community] lists a name the name policy does not list, so it is sold as a plain name: Alice's bakery"
  setCommunityTerms node fakeNewCommunityTerms
  checkNamePolicy cfg ct `shouldReturn` Left ("names: the text at names.community_terms_url does not hash to the name policy's terms " <> T.unpack (hex fakeNewCommunityTerms))
  answerListing node aliceHash $ Reverts ""
  unread <- capturingStderr $ void $ checkNamePolicy cfg ct
  unread `shouldSatisfy` T.isInfixOf "names: a [community] name's listing could not be read: Alice's bakery"

testStartupUnread :: TVar ChaChaDRG -> IO ()
testStartupUnread g = withContracts g $ \node ct -> do
  cfg <- withTermsUrl (fakeTermsUrl node) <&> \c -> c {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) "a bakery the policy is not read for"}
  setCommunityTerms node fakeNewCommunityTerms
  answerCall node fakeController (selector "namePolicy()") $ Reverts ""
  warned <- capturingStderr $ checkNamePolicy cfg ct `shouldReturn` Right ()
  warned `shouldSatisfy` T.isInfixOf "names: the controller's name policy could not be read, it is read on first use"
  -- a text no other case logs, so a line another case logs late cannot match it
  warned `shouldNotSatisfy` T.isInfixOf "a bakery the policy is not read for"

testStartupWarnings :: TVar ChaChaDRG -> IO ()
testStartupWarnings g = withContracts g $ \node ct -> do
  cfg <- withTermsUrl (fakeTermsUrl node)
  let warns c text = capturingStderr (checkNamePolicy c ct `shouldReturn` Right ()) >>= (`shouldSatisfy` T.isInfixOf text)
  warns cfg {nCommunityTermsUrl = Nothing} "names: names.community_terms_url is not set, so community names cannot be searched, ordered or revealed until it is set and the service restarts"
  unreachable <- withTermsUrl "http://127.0.0.1:1/terms"
  warns unreachable "names: the community terms could not be checked now: names.community_terms_url could not be read: "
  setCommunityTerms node $ B.replicate 32 0
  -- no warning without community_terms_url; a text only this check logs, captured before this case logs it, as other cases log in parallel
  quiet <- capturingStderr $ checkNamePolicy cfg {nCommunityTermsUrl = Nothing} ct `shouldReturn` Right ()
  quiet `shouldNotSatisfy` T.isInfixOf "names: the name policy has no community terms yet"
  warns cfg "names: the name policy has no community terms yet"

testTermsPageRefused :: TVar ChaChaDRG -> IO ()
testTermsPageRefused g = withContracts g $ \_ ct ->
  newTVarIO (notFound404, fakeCommunityTermsText) >>= \page -> withTermsPage page $ \url -> do
    cfg <- withTermsUrl url
    currentCommunityTerms cfg ct `shouldReturn` Left (TEUnread "names.community_terms_url answered HTTP 404")
    atomically $ writeTVar page (ok200, B.replicate (maxTermsBytes + 1) 0x61)
    currentCommunityTerms cfg ct `shouldReturn` Left (TEUnread $ "names.community_terms_url answered more than " <> tshow maxTermsBytes <> " bytes")
    unreachable <- withTermsUrl "http://127.0.0.1:1/terms"
    Left (TEUnread e) <- currentCommunityTerms unreachable ct
    e `shouldSatisfy` T.isPrefixOf "names.community_terms_url could not be read: "
    atomically $ writeTVar page (ok200, fakeCommunityTermsText)
    currentCommunityTerms cfg ct `shouldReturn` Right fakeCommunityTerms

testCommunityTermsUrl :: IO ()
testCommunityTermsUrl = do
  (nCommunityTermsUrl <$> parsedNames namesSection chainSection) `shouldReturn` Nothing
  forM_ ["https://simplex.chat/community-terms", "http://127.0.0.1:8180/terms", "http://localhost/terms"] $ \u ->
    (nCommunityTermsUrl <$> withTermsUrl u) `shouldReturn` Just u
  let refused = "names.community_terms_url must be an https URL, or an http URL on 127.0.0.1 or localhost with the local chain"
      onMainnet = map (\l -> if l == "chain_id = 31337" then "chain_id = 1" else l) chainSection
  forM_ ["http://simplex.chat/terms", "http://127.0.0.1.example/terms", "ftp://127.0.0.1/terms", "https://", "simplex.chat/terms"] $ \u ->
    namesRefusal (namesSection <> ["community_terms_url = " <> u]) chainSection `shouldReturn` refused
  namesRefusal (namesSection <> ["community_terms_url = http://127.0.0.1:8180/terms"]) onMainnet `shouldReturn` refused
