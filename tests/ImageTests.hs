{-# LANGUAGE OverloadedStrings #-}

module ImageTests where

import Codec.Picture (DynamicImage (..), Image (..), Pixel8, PixelCMYK8, PixelRGB16 (..), PixelRGB8 (..), PixelRGBA16 (..), PixelRGBA8 (..), convertRGB8, convertRGBA8, decodeImage, dynamicMap, encodeBitmap, encodeGifImage, generateImage, pixelAt)
import Codec.Picture.JPEGTurbo (encodeRGB)
import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata, encodeJpegAtQuality)
import Codec.Picture.Png (encodePng)
import Codec.Picture.Types (convertImage, extractLumaPlane, pixelFold)
import Control.Monad (forM_, when)
import Control.Monad.Except (runExceptT)
import Data.Bits (complement, shiftR, testBit, xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Data.Word (Word32)
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
    it "should fill 97-100% of the limit" testFill
    it "should raise JPEG quality when the image fits at its size" testFullSizeQuality
  describe "image preview" $ do
    it "should generate preview of an image file" $ testFilePreview $ CryptoFile "tests/fixtures/test.jpg" Nothing
    it "should generate preview of an encrypted image file" testEncryptedFilePreview
    it "should keep the fixed preview when the file is not an image" $
      imagePreview (CryptoFile "tests/fixtures/test.pdf" Nothing) `shouldReturn` fixedImagePreview
  describe "JPEG decoding" $ do
    it "should decode JPEG at the display size" testJpegDisplaySize
    it "should decode JPEG colour spaces" testJpegColourSpaces
    it "should refuse unsupported and malformed JPEG" testJpegRefused
    it "should apply EXIF orientation of JPEG" testJpegOrientation
  describe "PNG decoding" $ do
    it "should decode PNG as JuicyPixels does" testPngDecoding
    it "should refuse malformed PNG and other formats" testPngRefused
  describe "scaling" $ do
    it "should scale and sharpen as the research code" testScaleResearch
    it "should keep an image of the same size" testScaleSameSize
    it "should scale transparency in premultiplied alpha" testScaleTransparency

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

testFill :: IO ()
testFill = do
  forM_ ["tests/fixtures/test.jpg", "tests/fixtures/preview-issue1.png"] $ \path -> do
    Right img <- decodeImage <$> B.readFile path
    Right uri <- resizeImageDataUri 14000 img
    LB.length uri `shouldSatisfy` filled 14000
    Right image <- resizeImage IFJpeg 20000 img
    LB.length image `shouldSatisfy` filled 20000
  where
    filled limit n = n <= limit && fromIntegral n >= 0.97 * (fromIntegral limit :: Double)

testFullSizeQuality :: IO ()
testFullSizeQuality = do
  let rgb = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 4) (fromIntegral $ y * 5) (fromIntegral $ x * y)) 64 48
  q65 <- encodeRGB 65 rgb
  q95 <- encodeRGB 95 rgb
  let limit = fromIntegral $ (B.length q65 + B.length q95) `div` 2
  Right jpeg <- resizeImage IFJpeg limit $ ImageRGB8 rgb
  LB.length jpeg `shouldSatisfy` (\n -> n > fromIntegral (B.length q65) && n <= limit)
  imageSize <$> decodeImage (LB.toStrict jpeg) `shouldBe` Right (64, 48)

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

testJpegDisplaySize :: IO ()
testJpegDisplaySize =
  forM_ sizes $ \(w, (w', h')) ->
    forM_ [((w, 16), (w', h')), ((16, w), (h', w'))] $ \((iw, ih), size) -> do
      jpeg <- encodeRGB 90 $ generateImage (\_ _ -> PixelRGB8 128 128 128) iw ih
      Right img <- decodeResizable jpeg
      resizedSize img `shouldReturn` size
      when (w <= 8641) $ do
        Right full <- pure $ decodeImage jpeg
        resizedSize full `shouldReturn` size
  where
    sizes = [(4320, (4320, 16)), (4321, (2160, 8)), (8640, (4320, 8)), (8641, (4320, 8)), (17280, (4320, 4)), (17281, (4320, 4)), (17282, (4320, 4)), (34561, (4320, 2)), (65500, (4094, 1))]
    resizedSize img = do
      Right image <- resizeImage IFJpeg 261120 img
      either fail (pure . imageSize) $ decodeImage $ LB.toStrict image

testJpegColourSpaces :: IO ()
testJpegColourSpaces = do
  let rgb = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 4) (fromIntegral $ y * 5) 128) 64 48
  progressive <- encodeRGB 90 rgb
  Right (ImageRGB8 _) <- decodeResizable progressive
  Right (ImageRGB8 _) <- decodeResizable $ baselineJpeg rgb
  Right (ImageY8 _) <- decodeResizable $ LB.toStrict $ encodeDirectJpegAtQualityWithMetadata 90 mempty $ extractLumaPlane rgb
  Right (ImageCMYK8 _) <- decodeResizable $ LB.toStrict $ encodeDirectJpegAtQualityWithMetadata 90 mempty (convertImage rgb :: Image PixelCMYK8)
  pure ()

testJpegRefused :: IO ()
testJpegRefused =
  forM_ inputs $ \input -> isLeft <$> decodeResizable input `shouldReturn` True
  where
    jpeg = baselineJpeg $ generateImage (\_ _ -> PixelRGB8 128 128 128) 64 48
    sof = B.length $ fst $ B.breakSubstring "\xFF\xC0" jpeg
    patchAt i bytes = B.take i jpeg <> B.pack bytes <> B.drop (i + length bytes) jpeg
    sides w h = patchAt (sof + 5) $ concatMap (\v -> [fromIntegral (v `shiftR` 8), fromIntegral v]) [h, w :: Int]
    inputs =
      [ patchAt (sof + 4) [12],
        patchAt (sof + 1) [0xC3],
        sides 65501 16,
        sides 16385 16384,
        B.init jpeg,
        B.take sof jpeg <> "\0" <> B.drop sof jpeg
      ]

testJpegOrientation :: IO ()
testJpegOrientation = do
  jpeg <- encodeRGB 100 $ generateImage quadrant 64 32
  forM_ orientations $ \(orientation, size, colours) -> do
    Right img <- decodeResizable $ withExif (odd orientation) 8 orientation jpeg
    imageSize img `shouldBe` size
    quadrants img `shouldBe` colours
    Right resized <- resizeImage IFJpeg 261120 img
    "Exif" `B.isInfixOf` LB.toStrict resized `shouldBe` False
  Right img <- decodeResizable $ withExif True 1000 6 jpeg
  quadrants img `shouldBe` [red, green, blue, white]
  where
    quadrant x y = case (x < 32, y < 16) of
      (True, True) -> red
      (False, True) -> green
      (True, False) -> blue
      (False, False) -> white
    orientations :: [(Int, (Int, Int), [PixelRGB8])]
    orientations =
      [ (1, (64, 32), [red, green, blue, white]),
        (2, (64, 32), [green, red, white, blue]),
        (3, (64, 32), [white, blue, green, red]),
        (4, (64, 32), [blue, white, red, green]),
        (5, (32, 64), [red, blue, green, white]),
        (6, (32, 64), [blue, red, white, green]),
        (7, (32, 64), [white, green, blue, red]),
        (8, (32, 64), [green, white, red, blue])
      ]
    red = PixelRGB8 255 0 0
    green = PixelRGB8 0 255 0
    blue = PixelRGB8 0 0 255
    white = PixelRGB8 255 255 255
    quadrants img =
      let rgb = convertRGB8 img
          (w, h) = (imageWidth rgb, imageHeight rgb)
          snap (PixelRGB8 r g b) = PixelRGB8 (level r) (level g) (level b)
          level c = if c > 127 then 255 else 0
       in [snap $ pixelAt rgb x y | y <- [h `div` 4, 3 * h `div` 4], x <- [w `div` 4, 3 * w `div` 4]]
    withExif :: Bool -> Int -> Int -> B.ByteString -> B.ByteString
    withExif littleEndian ifdOffset orientation jpeg = B.take 2 jpeg <> "\xFF\xE1" <> bigEndian 2 (8 + B.length tiff) <> "Exif\0\0" <> tiff <> B.drop 2 jpeg
      where
        tiff = B.concat [if littleEndian then "II" else "MM", word 2 42, word 4 ifdOffset, word 2 1, word 2 0x0112, word 2 3, word 4 1, word 2 orientation, word 2 0, word 4 0]
        word n = (if littleEndian then B.reverse else id) . bigEndian n
    bigEndian :: Int -> Int -> B.ByteString
    bigEndian n v = B.pack [fromIntegral (v `shiftR` (8 * i)) | i <- [n - 1, n - 2 .. 0]]

testPngDecoding :: IO ()
testPngDecoding = do
  transparent <- B.readFile "tests/fixtures/logo-large-rgba.png"
  Right img@(ImageRGBA8 rgba) <- decodeResizable transparent
  Right reference <- pure $ decodeImage transparent
  imageData rgba `shouldBe` imageData (convertRGBA8 reference)
  IFPng <- pure $ imageFormat img
  opaque <- B.readFile "tests/fixtures/preview-issue1.png"
  Right (ImageRGB8 rgb) <- decodeResizable opaque
  Right reference' <- pure $ decodeImage opaque
  imageData rgb `shouldBe` imageData (convertRGB8 reference')

testPngRefused :: IO ()
testPngRefused =
  forM_ inputs $ \(input, err) ->
    either Just (const Nothing) <$> decodeResizable input `shouldReturn` Just err
  where
    inputs =
      [ (pngShortImageData, "IDAT stream too short"),
        (pngSideAbove16384, "image width exceeds user limit"),
        (LB.toStrict $ encodeGifImage $ generateImage (\_ _ -> 0) 16 16, "unsupported image format"),
        (LB.toStrict $ encodeBitmap $ generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16, "unsupported image format")
      ]

pngShortImageData :: B.ByteString
pngShortImageData = pngWithSides 2048 2048 $ blankPng 2048 1

pngSideAbove16384 :: B.ByteString
pngSideAbove16384 = pngWithSides 65537 65537 $ blankPng 1 1

blankPng :: Int -> Int -> B.ByteString
blankPng w h = LB.toStrict $ encodePng (generateImage (\_ _ -> 0) w h :: Image Pixel8)

pngWithSides :: Int -> Int -> B.ByteString -> B.ByteString
pngWithSides w h png = B.take 8 png <> word32 (B.length ihdr) <> "IHDR" <> ihdr <> word32 (crc32 $ "IHDR" <> ihdr) <> B.drop 33 png
  where
    ihdr = word32 w <> word32 h <> B.take 5 (B.drop 24 png)
    word32 :: Integral a => a -> B.ByteString
    word32 v = B.pack [fromIntegral (toInteger v `shiftR` (8 * i)) | i <- [3, 2, 1, 0]]

crc32 :: B.ByteString -> Word32
crc32 = complement . B.foldl' (\c b -> iterate step (c `xor` fromIntegral b) !! 8) 0xffffffff
  where
    step c = if testBit c 0 then 0xedb88320 `xor` shiftR c 1 else shiftR c 1

testScaleResearch :: IO ()
testScaleResearch = do
  Right src <- decodeImage <$> B.readFile "tests/fixtures/preview-issue1.png"
  Right (ImageRGB8 expected) <- decodeImage <$> B.readFile "tests/fixtures/preview-issue1-400x200-sharpened.png"
  let scaled = scaleRGB8 (convertRGB8 src) (400, 200)
  imageSize (ImageRGB8 scaled) `shouldBe` (400, 200)
  pixelFold (\n x y p -> if p == pixelAt expected x y then n else n + 1) (0 :: Int) scaled `shouldBe` 0

testScaleSameSize :: IO ()
testScaleSameSize = do
  let rgb = generateImage (\x y -> PixelRGB8 (fromIntegral $ x * 4) (fromIntegral $ y * 5) 128) 64 48
      rgba = generateImage (\x y -> PixelRGBA8 (fromIntegral $ x * 4) (fromIntegral $ y * 5) 128 (fromIntegral $ x + y)) 64 48
  imageData (scaleRGB8 rgb (64, 48)) `shouldBe` imageData rgb
  imageData (scaleRGBA8 rgba (64, 48)) `shouldBe` imageData rgba

testScaleTransparency :: IO ()
testScaleTransparency = do
  let scaled = scaleRGBA8 (generateImage pixel 64 64) (40, 40)
      visible = [p | y <- [0 .. 39], x <- [0 .. 39], let p@(PixelRGBA8 _ _ _ a) = pixelAt scaled x y, a > 0]
  filter (\(PixelRGBA8 r g b _) -> (r, g, b) /= (255, 0, 0)) visible `shouldBe` []
  any (\(PixelRGBA8 _ _ _ a) -> a < 255) visible `shouldBe` True
  where
    pixel x y
      | (x - 32) * (x - 32) + (y - 32) * (y - 32) < 400 = PixelRGBA8 255 0 0 255
      | even (x + y) = PixelRGBA8 0 255 0 0
      | otherwise = PixelRGBA8 0 0 255 0

baselineJpeg :: Image PixelRGB8 -> B.ByteString
baselineJpeg = LB.toStrict . encodeJpegAtQuality 90 . convertImage

imageSize :: DynamicImage -> (Int, Int)
imageSize = dynamicMap (\img -> (imageWidth img, imageHeight img))

shouldKeepAspect :: (Int, Int) -> (Int, Int) -> Expectation
shouldKeepAspect (w, h) (w', h') = abs (w * h' - h * w') `shouldSatisfy` (<= w' + h')
