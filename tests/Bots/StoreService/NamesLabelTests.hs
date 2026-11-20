{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesLabelTests (namesLabelTests) where

import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import StoreService.Names.Label
import Test.Hspec

namesLabelTests :: Spec
namesLabelTests = describe "StoreService names: label" $ do
  describe "accepts" $ do
    mapM_ accepted ["bakery", "bak-ery-2", "a-b-c-d-e-f", "123456", T.replicate 63 "a"]
  describe "refuses as characters" $ do
    mapM_ (refused LEChars) ["bak--ery", "--bakery", "bakery--", "-bakery", "bakery-", "Bakery", "bak.ery", "bäkery", "bak ery", "bak_ery", "bakery.simplex", "бакеry1"]
  describe "refuses as length" $ do
    mapM_ (refused LELength) ["", "abcde", "a.b", T.replicate 64 "a", T.replicate 100 "-"]
  it "applies the same rule under the testing TLD" $ do
    validLabel TLDTesting "bakery" `shouldBe` Right "bakery"
    validLabel TLDTesting "bak--ery" `shouldBe` Left LEChars
  it "has no bare web names" $
    validLabel TLDWeb "bakery" `shouldBe` Left LEChars
  where
    accepted l = it ("accepts " <> show l) $ validLabel TLDSimplex l `shouldBe` Right l
    refused :: LabelError -> Text -> Spec
    refused e l = it ("refuses " <> show l) $ validLabel TLDSimplex l `shouldBe` Left e
