module ValidNames where

import Simplex.Chat.Library.Commands
import Test.Hspec

validNameTests :: Spec
validNameTests = describe "valid chat names" $ do
  it "should keep valid and fix invalid names" testMkValidName

testMkValidName :: IO ()
testMkValidName = do
  mkValidName "alice" `shouldBe` "alice"
  mkValidName "алиса" `shouldBe` "алиса"
  mkValidName "John Doe" `shouldBe` "John Doe"
  mkValidName "J.Doe" `shouldBe` "J.Doe"
  mkValidName "J. Doe" `shouldBe` "J. Doe"
  mkValidName "J..Doe" `shouldBe` "J..Doe"
  mkValidName "J ..Doe" `shouldBe` "J ..Doe"
  mkValidName "J ... Doe" `shouldBe` "J ... Doe"
  mkValidName "J .... Doe" `shouldBe` "J ... Doe"
  mkValidName "J . . Doe" `shouldBe` "J . Doe"
  mkValidName "@alice" `shouldBe` "alice"
  mkValidName "#alice" `shouldBe` "alice"
  mkValidName " alice" `shouldBe` "alice"
  mkValidName "alice " `shouldBe` "alice"
  mkValidName "John  Doe" `shouldBe` "John Doe"
  mkValidName "'John Doe'" `shouldBe` "John Doe"
  mkValidName "\"John Doe\"" `shouldBe` "John Doe\""
  mkValidName "`John Doe`" `shouldBe` "`John Doe`"
  mkValidName "John \"Doe\"" `shouldBe` "John \"Doe\""
  mkValidName "John `Doe`" `shouldBe` "John `Doe`"
  mkValidName "alice/bob" `shouldBe` "alice/bob"
  mkValidName "alice / bob" `shouldBe` "alice / bob"
  mkValidName "alice /// bob" `shouldBe` "alice /// bob"
  mkValidName "alice //// bob" `shouldBe` "alice /// bob"
  mkValidName "alice >>= bob" `shouldBe` "alice >>= bob"
  mkValidName "alice@example.com" `shouldBe` "alice@example.com"
  mkValidName "alice <> bob" `shouldBe` "alice <> bob"
  mkValidName "alice -> bob" `shouldBe` "alice -> bob"
