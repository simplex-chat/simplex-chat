{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module JSONTests where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import JSONFixtures
import Simplex.Chat.Markdown (Format (BlockEnd, BlockStart, Bold, CellBreak, LineBreak), FormattedText (FormattedText), MarkdownBlock (CodeBlock, Heading, ListItem, Paragraph, Quote, Rule, Table, UnknownBlock), MarkdownList, TableAlign (..))
import Simplex.Chat.Remote.Protocol (owsf2tagged)
import Simplex.Messaging.Parsers
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary (..), property)

owsf2TaggedJSONTest :: IO ()
owsf2TaggedJSONTest = do
  noActiveUserSwift `to` noActiveUserTagged
  activeUserExistsSwift `to` activeUserExistsTagged
  activeUserSwift `to` activeUserTagged
  chatStartedSwift `to` chatStartedTagged
  connectionsDiffSwift `to` connectionsDiffTagged
  parsedMarkdownSwift `to` parsedMarkdownTagged
  markdownMarkersSwift `to` markdownMarkersTagged
  where
    to :: LB.ByteString -> LB.ByteString -> IO ()
    owsf `to` tagged =
      case J.eitherDecode owsf of
        Right json -> Right (owsf2tagged json) `shouldBe` J.eitherDecode tagged
        Left e -> expectationFailure e

data SomeType
  = Nullary
  | Unary (Maybe SomeType)
  | Product String (Maybe SomeType)
  | Record
      { testOne :: Int,
        testTwo :: Maybe Bool,
        testThree :: Maybe SomeType
      }
  | List [Int]
  deriving (Eq, Show, Generic)

$(pure [])

thToJSON :: SomeType -> J.Value
thToJSON = $(JQ.mkToJSON (singleFieldJSON_ (Just SingleFieldJSONTag) id) ''SomeType)

thToEncoding :: SomeType -> J.Encoding
thToEncoding = $(JQ.mkToEncoding (singleFieldJSON_ (Just SingleFieldJSONTag) id) ''SomeType)

thParseJSON :: J.Value -> JT.Parser SomeType
thParseJSON = $(JQ.mkParseJSON (taggedObjectJSON id) ''SomeType)

instance Arbitrary SomeType where arbitrary = genericArbitraryU

instance ToJSON SomeType where
  toJSON = J.genericToJSON $ singleFieldJSON_ (Just SingleFieldJSONTag) id
  toEncoding = J.genericToEncoding $ singleFieldJSON_ (Just SingleFieldJSONTag) id

instance FromJSON SomeType where
  parseJSON = J.genericParseJSON $ taggedObjectJSON id

owsf2TaggedSomeTypeTests :: Spec
owsf2TaggedSomeTypeTests = modifyMaxSuccess (const 10000) $ do
  it "should convert to tagged" $ property $ \x ->
    (JT.parseMaybe J.parseJSON . owsf2tagged . J.toJSON) x == Just (x :: SomeType)
  it "should convert to tagged via encoding" $ property $ \x ->
    (join . fmap (JT.parseMaybe J.parseJSON . owsf2tagged) . J.decode . J.encode) x == Just (x :: SomeType)
  it "should convert to tagged via TH" $ property $ \x ->
    (JT.parseMaybe thParseJSON . owsf2tagged . thToJSON) x == Just (x :: SomeType)
  it "should convert to tagged via TH encoding" $ property $ \x ->
    (join . fmap (JT.parseMaybe thParseJSON . owsf2tagged) . J.decode . toLazyByteString . J.fromEncoding . thToEncoding) x == Just (x :: SomeType)

markdownMarkersJSONTests :: Spec
markdownMarkersJSONTests = do
  it "should encode and decode each marker and block" $ do
    J.eitherDecode markdownMarkersTagged `shouldBe` Right markers
    J.encode markers `shouldBe` markdownMarkersTagged
  it "should decode an unknown block" $
    J.eitherDecode "{\"type\":\"blockStart\",\"block\":{\"type\":\"future\"}}"
      `shouldBe` Right (BlockStart $ UnknownBlock $ J.object ["type" J..= ("future" :: String)])
  where
    markers :: MarkdownList
    markers =
      [ marker (BlockStart Paragraph) "",
        FormattedText (Just Bold) "p",
        marker BlockEnd "",
        FormattedText Nothing "\n",
        marker (BlockStart $ Heading 2) "## ",
        marker (BlockStart $ CodeBlock $ Just "bash") "```bash\n",
        marker (BlockStart $ CodeBlock Nothing) "```\n",
        marker LineBreak "\n",
        marker (BlockStart Quote) "> ",
        marker (BlockStart $ ListItem (Just "2.") 1) "  2. ",
        marker (BlockStart $ ListItem Nothing 0) "- ",
        marker (BlockStart $ Table [TANone, TACenter]) "| ",
        marker CellBreak " | ",
        marker (BlockStart Rule) "---"
      ]
    marker = FormattedText . Just

jsonTests :: Spec
jsonTests = do
  describe "owsf2tagged" $ do
    it "should convert chat types" owsf2TaggedJSONTest
    describe "SomeType" owsf2TaggedSomeTypeTests
  describe "Markdown markers" markdownMarkersJSONTests
