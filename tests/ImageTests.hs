{-# LANGUAGE OverloadedStrings #-}

module ImageTests where

import Codec.Picture (DynamicImage (..), Image (..), PixelRGB16 (..), PixelRGB8 (..), PixelRGBA16 (..), PixelRGBA8 (..), decodeImage, dynamicMap, generateImage)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Simplex.Chat.Image
import Simplex.Chat.Library.Commands (fixedImagePreview, imagePreview)
import Simplex.Chat.Types (ImageData (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import qualified Simplex.Messaging.Crypto.File as CF
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.STM (atomically)
import UnliftIO.Temporary (withTempDirectory)

imageTests :: Spec
imageTests = do
  describe "data URI" $ do
    it "should resize JPEG to JPEG" $ testDataUri "tests/fixtures/test.jpg" "data:image/jpg;base64,"
    it "should resize PNG with transparency to PNG" $ testDataUri "tests/fixtures/logo-large-rgba.png" "data:image/png;base64,"
    it "should resize opaque PNG to JPEG" $ testDataUri "tests/fixtures/preview-issue1.png" "data:image/jpg;base64,"
    it "should convert 16-bit images to 8 bits" testDataUri16Bit
  describe "image data" $ do
    it "should reduce JPEG size" testResizeJpeg
    it "should reduce PNG size" testResizePng
    it "should fail when image does not fit" testImageTooLarge
    it "should refuse image with a side above 16384 pixels" testImageSideLimit
    it "should halve image to at most 4320 pixels per side" testDisplaySide
  describe "image preview" $ do
    it "should generate preview of an image file" $ testFilePreview $ CryptoFile "tests/fixtures/test.jpg" Nothing
    it "should generate preview of an encrypted image file" testEncryptedFilePreview
    it "should keep the fixed preview when the file is not an image" $
      imagePreview (CryptoFile "tests/fixtures/test.pdf" Nothing) `shouldReturn` fixedImagePreview

testDataUri :: FilePath -> LB.ByteString -> IO ()
testDataUri path prefix = do
  Right img <- decodeImage <$> B.readFile path
  Right uri <- resizeImageDataUri 14000 img
  LB.length uri `shouldSatisfy` (<= 14000)
  let (uriPrefix, body) = LB.splitAt (LB.length prefix) uri
  uriPrefix `shouldBe` prefix
  Right resized <- pure $ decodeImage . LB.toStrict =<< LB64.decode body
  imageSize resized `shouldKeepAspect` imageSize img

testDataUri16Bit :: IO ()
testDataUri16Bit = do
  let rgb = ImageRGB16 $ generateImage (\x y -> PixelRGB16 (fromIntegral x * 512) (fromIntegral y * 512) 0) 128 128
      rgba = ImageRGBA16 $ generateImage (\x y -> PixelRGBA16 (fromIntegral x * 512) (fromIntegral y * 512) 0 32768) 128 128
  Right jpegUri <- resizeImageDataUri 14000 rgb
  jpegUri `shouldSatisfy` ("data:image/jpg;base64," `LB.isPrefixOf`)
  Right pngUri <- resizeImageDataUri 14000 rgba
  Just body <- pure $ LB.stripPrefix "data:image/png;base64," pngUri
  Right (ImageRGBA8 _) <- pure $ decodeImage . LB.toStrict =<< LB64.decode body
  pure ()

testResizeJpeg :: IO ()
testResizeJpeg = do
  Right img <- decodeImage <$> B.readFile "tests/fixtures/test.jpg"
  Right jpeg <- resizeImage IFJpeg 10000 img
  LB.length jpeg `shouldSatisfy` (<= 10000)
  LB.take 2 jpeg `shouldBe` "\xFF\xD8"
  Right resized <- pure $ decodeImage $ LB.toStrict jpeg
  fst (imageSize resized) `shouldSatisfy` (< 1280)
  imageSize resized `shouldKeepAspect` imageSize img

testResizePng :: IO ()
testResizePng = do
  Right img <- decodeImage <$> B.readFile "tests/fixtures/logo-large-rgba.png"
  Right png <- resizeImage IFPng 10000 img
  LB.length png `shouldSatisfy` (<= 10000)
  Right resized@(ImageRGBA8 _) <- pure $ decodeImage $ LB.toStrict png
  fst (imageSize resized) `shouldSatisfy` (< 1630)
  imageSize resized `shouldKeepAspect` imageSize img

testImageTooLarge :: IO ()
testImageTooLarge = do
  let img = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) 1 1
  resizeImage IFPng 10 img >>= (`shouldSatisfy` isLeft)
  resizeImage IFJpeg 10 img >>= (`shouldSatisfy` isLeft)

testImageSideLimit :: IO ()
testImageSideLimit = do
  let blank w h = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
      resizeAll img = sequence [resizeImage IFPng 100000 img, resizeImage IFJpeg 100000 img, resizeImageDataUri 100000 img]
  forM_ [blank 16384 4, blank 4 16384] $ \img ->
    resizeAll img >>= (`shouldSatisfy` all isRight)
  forM_ [blank 16385 4, blank 4 16385] $ \img ->
    resizeAll img >>= (`shouldSatisfy` all isLeft)

testDisplaySide :: IO ()
testDisplaySide =
  forM_ [((8640, 8), (4320, 4)), ((8, 8640), (4, 4320)), ((4320, 4), (4320, 4))] $ \((w, h), size) -> do
    let opaque = ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 0 0 0) w h
        transparent = ImageRGBA8 $ generateImage (\_ _ -> PixelRGBA8 0 0 0 0) w h
    forM_ [opaque, transparent] $ \img -> do
      Right image <- resizeImage (imageFormat img) 261120 img
      decodedSize image `shouldBe` Right size
      forM_ [12500, 14000] $ \limit -> do
        Right uri <- resizeImageDataUri limit img
        (decodedSize =<< LB64.decode (LB.drop 1 $ LB.dropWhile (/= ',') uri)) `shouldBe` Right size
  where
    decodedSize = fmap imageSize . decodeImage . LB.toStrict

testFilePreview :: CryptoFile -> IO ()
testFilePreview f = do
  ImageData preview <- imagePreview f
  T.length preview `shouldSatisfy` (<= 14000)
  preview `shouldSatisfy` ("data:image/jpg;base64," `T.isPrefixOf`)

testEncryptedFilePreview :: IO ()
testEncryptedFilePreview = withTempDirectory "tests/tmp" "image-preview" $ \dir -> do
  args <- atomically . CF.randomArgs =<< C.newRandom
  let f = CryptoFile (dir </> "test.jpg") (Just args)
  Right () <- runExceptT . CF.writeFile f . LB.fromStrict =<< B.readFile "tests/fixtures/test.jpg"
  testFilePreview f

imageSize :: DynamicImage -> (Int, Int)
imageSize = dynamicMap (\img -> (imageWidth img, imageHeight img))

shouldKeepAspect :: (Int, Int) -> (Int, Int) -> Expectation
shouldKeepAspect (w, h) (w', h') = abs (w * h' - h * w') `shouldSatisfy` (<= w' + h')
