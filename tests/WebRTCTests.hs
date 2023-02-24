module WebRTCTests where

import Control.Monad.Except
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Char8 as B
import Simplex.Chat.Mobile.WebRTC
import Test.Hspec

webRTCTests :: Spec
webRTCTests = describe "WebRTC crypto" $ do
  it "encrypts and decrypts media" $ do
    key <- U.encode <$> getRandomBytes 32
    frame <- getRandomBytes 1000
    Right frame' <- runExceptT $ chatEncryptMedia key $ frame <> B.replicate reservedSize '\NUL'
    B.length frame' `shouldBe` B.length frame + reservedSize
    Right frame'' <- runExceptT $ chatDecryptMedia key frame'
    frame'' `shouldBe` frame <> B.replicate reservedSize '\NUL'
