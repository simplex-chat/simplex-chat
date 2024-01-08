{-# LANGUAGE OverloadedStrings #-}

module WebRTCTests where

import Control.Monad.Except
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString.Base64.URL as U
import qualified Data.ByteString.Char8 as B
import Foreign.StablePtr
import Simplex.Chat.Mobile
import Simplex.Chat.Mobile.WebRTC
import qualified Simplex.Messaging.Crypto as C
import System.FilePath ((</>))
import Test.Hspec

webRTCTests :: SpecWith FilePath
webRTCTests = describe "WebRTC crypto" $ do
  it "encrypts and decrypts media" $ \tmp -> do
    Right c <- chatMigrateInit (tmp </> "1") "" "yesUp"
    cc <- newStablePtr c
    key <- U.encode <$> getRandomBytes 32
    frame <- getRandomBytes 1000
    Right frame' <- runExceptT $ chatEncryptMedia cc key $ frame <> B.replicate reservedSize '\NUL'
    B.length frame' `shouldBe` B.length frame + reservedSize
    Right frame'' <- runExceptT $ chatDecryptMedia key frame'
    frame'' `shouldBe` frame <> B.replicate reservedSize '\NUL'
  it "should fail on invalid frame size" $ \tmp -> do
    Right c <- chatMigrateInit (tmp </> "1") "" "yesUp"
    cc <- newStablePtr c
    key <- U.encode <$> getRandomBytes 32
    frame <- getRandomBytes 10
    runExceptT (chatEncryptMedia cc key frame) `shouldReturn` Left "frame has no [reserved space for] IV and/or auth tag"
    runExceptT (chatDecryptMedia key frame) `shouldReturn` Left "frame has no [reserved space for] IV and/or auth tag"
  it "should fail on invalid key" $ \tmp -> do
    Right c <- chatMigrateInit (tmp </> "1") "" "yesUp"
    cc <- newStablePtr c
    let key = B.replicate 32 '#'
    frame <- (<> B.replicate reservedSize '\NUL') <$> getRandomBytes 100
    runExceptT (chatEncryptMedia cc key frame) `shouldReturn` Left "invalid key: invalid character at offset: 0"
    runExceptT (chatDecryptMedia key frame) `shouldReturn` Left "invalid key: invalid character at offset: 0"
  it "should fail on invalid auth tag" $ \tmp -> do
    Right c <- chatMigrateInit (tmp </> "1") "" "yesUp"
    cc <- newStablePtr c
    key <- U.encode <$> getRandomBytes 32
    frame <- getRandomBytes 1000
    Right frame' <- runExceptT $ chatEncryptMedia cc key $ frame <> B.replicate reservedSize '\NUL'
    Right frame'' <- runExceptT $ chatDecryptMedia key frame'
    frame'' `shouldBe` frame <> B.replicate reservedSize '\NUL'
    let (encFrame, rest) = B.splitAt (B.length frame' - reservedSize) frame
        (_tag, iv) = B.splitAt C.authTagSize rest
        badFrame = encFrame <> B.replicate C.authTagSize '\NUL' <> iv
    runExceptT (chatDecryptMedia key badFrame) `shouldReturn` Left "AESDecryptError"
