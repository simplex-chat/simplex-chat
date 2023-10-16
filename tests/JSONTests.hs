{-# LANGUAGE DeriveGeneric #-}

module JSONTests where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import MobileTests
import Simplex.Chat.Remote (owsf2tagged)
import Simplex.Messaging.Parsers
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary (..), property)

jsonTests :: Spec
jsonTests = describe "owsf2tagged" $ do
  it "should convert chat types" owsf2TaggedJSONTest
  describe "SomeType" owsf2TaggedSomeTypeTests

owsf2TaggedJSONTest :: IO ()
owsf2TaggedJSONTest = do
  noActiveUserSwift `to` noActiveUserTagged
  activeUserExistsSwift `to` activeUserExistsTagged
  activeUserSwift `to` activeUserTagged
  chatStartedSwift `to` chatStartedTagged
  networkStatusesSwift `to` networkStatusesTagged
  memberSubSummarySwift `to` memberSubSummaryTagged
  userContactSubSummarySwift `to` userContactSubSummaryTagged
  pendingSubSummarySwift `to` pendingSubSummaryTagged
  parsedMarkdownSwift `to` parsedMarkdownTagged
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

instance Arbitrary SomeType where arbitrary = genericArbitraryU

instance ToJSON SomeType where
  toJSON = J.genericToJSON $ singleFieldJSON_ (Just SingleFieldJSONTag) id
  toEncoding = J.genericToEncoding $ singleFieldJSON_ (Just SingleFieldJSONTag) id

instance FromJSON SomeType where
  parseJSON = J.genericParseJSON $ taggedObjectJSON id

owsf2TaggedSomeTypeTests :: Spec
owsf2TaggedSomeTypeTests =
  modifyMaxSuccess (const 10000) $ it "should convert to tagged" $ property $ \x ->
    (JT.parseMaybe J.parseJSON . owsf2tagged . J.toJSON) x == Just (x :: SomeType)
