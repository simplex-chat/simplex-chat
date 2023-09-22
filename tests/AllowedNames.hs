module AllowedNames where

import Simplex.Chat
import Test.Hspec

allowedNameTests :: Spec
allowedNameTests = describe "allowed chat names" $ do
  it "should allow single and multiple words" testAllowedNames

testAllowedNames :: IO ()
testAllowedNames = do
  allowedName "alice" `shouldBe` True
  allowedName "алиса" `shouldBe` True
  allowedName "John Doe" `shouldBe` True
  allowedName "J.Doe" `shouldBe` True
  allowedName "J. Doe" `shouldBe` True
  allowedName "@alice" `shouldBe` False
  allowedName "#alice" `shouldBe` False
  allowedName " alice" `shouldBe` False
  allowedName "alice " `shouldBe` False
  allowedName "John  Doe" `shouldBe` False
  allowedName "'John Doe'" `shouldBe` False
  allowedName "\"John Doe\"" `shouldBe` False
  allowedName "`John Doe`" `shouldBe` False
