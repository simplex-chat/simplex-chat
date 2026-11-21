{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesLabelTests (namesLabelTests) where

import ChatTests.Utils (writeFixture)
import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import StoreService.Names.Label (LabelError (..), validLabel)
import Test.Hspec

namesLabelTests :: Spec
namesLabelTests = describe "StoreService names: label" $ do
  describe "accepts" $ do
    mapM_ accepted acceptedLabels
  describe "refuses as characters" $ do
    mapM_ (refused LEChars) charsRefused
  describe "refuses as length" $ do
    mapM_ (refused LELength) lengthRefused
  it "records the label vectors the names page's tests read" $
    writeFixture "store-service/rest-api/vectors-labels" $
      J.toJSON [J.object ["label" J..= l, "valid" J..= valid] | (l, valid) <- map (,True) acceptedLabels <> map (,False) (charsRefused <> lengthRefused)]
  it "applies the same rule under the testing TLD" $ do
    validLabel TLDTesting "bakery" `shouldBe` Right "bakery"
    validLabel TLDTesting "bak--ery" `shouldBe` Left LEChars
  it "has no bare web names" $
    validLabel TLDWeb "bakery" `shouldBe` Left LEChars
  where
    acceptedLabels = ["bakery", "bak-ery-2", "a-b-c-d-e-f", "123456", T.replicate 63 "a"]
    charsRefused = ["bak--ery", "--bakery", "bakery--", "-bakery", "bakery-", "Bakery", "bak.ery", "bäkery", "bak ery", "bak_ery", "bakery.simplex", "бакеry1"]
    lengthRefused = ["", "abcde", "a.b", T.replicate 64 "a", T.replicate 100 "-"]
    accepted l = it ("accepts " <> show l) $ validLabel TLDSimplex l `shouldBe` Right l
    refused :: LabelError -> Text -> Spec
    refused e l = it ("refuses " <> show l) $ validLabel TLDSimplex l `shouldBe` Left e
