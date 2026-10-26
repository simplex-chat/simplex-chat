{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Image
  ( ImageFormat (..),
    imageFormat,
    decodeResizable,
    resizeImage,
    resizeImageDataUri,
    scaleRGB8,
    scaleRGBA8,
  )
where

import Codec.Picture (DynamicImage (..), Image (..), Pixel (..), PixelRGB8, PixelRGBA8 (..), convertRGB8, convertRGBA8, decodeImage, dynamicMap, dynamicPixelMap, generateImage, pixelMap)
import Codec.Picture.JPEGTurbo (DecodeOptions (DecodeOptions), JpegHeader (..), Scale (..), decodeJpeg, encodeRGB, readJpegHeader)
import Codec.Picture.Png (encodePng)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (pixelFoldMap)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (All (..))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import GHC.Float (double2Float, float2Double)

data ImageFormat = IFJpeg | IFPng

imageFormat :: DynamicImage -> ImageFormat
imageFormat = \case
  ImageYA8 img -> alphaFormat img
  ImageYA16 img -> alphaFormat img
  ImageRGBA8 img -> alphaFormat img
  ImageRGBA16 img -> alphaFormat img
  _ -> IFJpeg
  where
    alphaFormat :: (Pixel a, Eq (PixelBaseComponent a), Bounded (PixelBaseComponent a)) => Image a -> ImageFormat
    alphaFormat img
      | getAll $ pixelFoldMap (All . (maxBound ==) . pixelOpacity) img = IFJpeg
      | otherwise = IFPng

decodeResizable :: ByteString -> IO (Either String DynamicImage)
decodeResizable s
  | "\xFF\xD8" `B.isPrefixOf` s = runExceptT $ decodeDisplayJpeg s
  | otherwise = pure $ decodeImage s

decodeDisplayJpeg :: ByteString -> ExceptT String IO DynamicImage
decodeDisplayJpeg s = do
  JpegHeader {jpegWidth, jpegHeight} <- ExceptT $ readJpegHeader s
  (halvings, size) <- maybe (throwError "image does not fit") pure $ displaySize jpegWidth jpegHeight
  orientImage (exifOrientation s) . resizeToSize size <$> ExceptT (decodeJpeg (DecodeOptions (jpegScale halvings) maxJpegPixels maxJpegMemoryMB) s)

displaySize :: Int -> Int -> Maybe (Int, (Int, Int))
displaySize = go 0
  where
    go n w h
      | w <= maxDisplaySide && h <= maxDisplaySide = Just (n, (w, h))
      | w < 2 || h < 2 = Nothing
      | otherwise = go (n + 1) (scaledSide 2 w) (scaledSide 2 h)

jpegScale :: Int -> Scale
jpegScale = \case
  0 -> Full
  1 -> Half
  2 -> Quarter
  _ -> Eighth

resizeToSize :: (Int, Int) -> DynamicImage -> DynamicImage
resizeToSize size@(w, h) img
  | dynamicMap (hasSize size) img = img
  | otherwise = ImageRGB8 $ STBIR.resize STBIR.defaultOptions w h $ convertRGB8 img

exifOrientation :: ByteString -> Int
exifOrientation s = fromMaybe 1 $ do
  tiff <- exifTiff $ B.drop 2 s
  order <- case B.take 2 tiff of
    "II" -> Just B.reverse
    "MM" -> Just id
    _ -> Nothing
  let field n i
        | i < 0 || B.length bytes /= n = Nothing
        | otherwise = Just $ B.foldl' (\a b -> a * 256 + fromIntegral b) 0 $ order bytes
        where
          bytes = B.take n $ B.drop i tiff
      u16 = field 2
  ifd <- field 4 4
  entries <- u16 ifd
  listToMaybe
    [ v
      | e <- [ifd + 2 + 12 * k | k <- [0 .. entries - 1]],
        u16 e == Just 0x0112,
        u16 (e + 2) == Just 3,
        Just v <- [u16 (e + 8)],
        v >= 1 && v <= 8
    ]

exifTiff :: ByteString -> Maybe ByteString
exifTiff s
  | B.length s < 4 || B.index s 0 /= 0xFF || marker == 0xDA = Nothing
  | marker == 0xE1 && "Exif\0\0" `B.isPrefixOf` segment = Just $ B.drop 6 segment
  | otherwise = exifTiff $ B.drop (2 + len) s
  where
    marker = B.index s 1
    len = fromIntegral (B.index s 2) * 256 + fromIntegral (B.index s 3)
    segment = B.take (len - 2) $ B.drop 4 s

orientImage :: Int -> DynamicImage -> DynamicImage
orientImage orientation = dynamicPixelMap orient
  where
    orient :: Pixel a => Image a -> Image a
    orient img@Image {imageWidth = w, imageHeight = h} = case orientation of
      2 -> generateImage (\x y -> pixelAt img (w - 1 - x) y) w h
      3 -> generateImage (\x y -> pixelAt img (w - 1 - x) (h - 1 - y)) w h
      4 -> generateImage (\x y -> pixelAt img x (h - 1 - y)) w h
      5 -> generateImage (\x y -> pixelAt img y x) h w
      6 -> generateImage (\x y -> pixelAt img y (h - 1 - x)) h w
      7 -> generateImage (\x y -> pixelAt img (w - 1 - y) (h - 1 - x)) h w
      8 -> generateImage (\x y -> pixelAt img (w - 1 - y) x) h w
      _ -> img

resizeImage :: ImageFormat -> Int64 -> DynamicImage -> IO (Either String LB.ByteString)
resizeImage = fitImage $ const id

resizeImageDataUri :: Int64 -> DynamicImage -> IO (Either String LB.ByteString)
resizeImageDataUri maxSize img = fitImage toDataUri (imageFormat img) maxSize img

fitImage :: (ImageFormat -> LB.ByteString -> LB.ByteString) -> ImageFormat -> Int64 -> DynamicImage -> IO (Either String LB.ByteString)
fitImage output format maxSize img
  | dynamicMap (hasSideAbove maxImageSide) img = tooLarge
  | otherwise = case format of
      IFJpeg -> fitDisplay (jpeg minJpegQuality) fitQuality $ convertRGB8 img
      IFPng -> fitDisplay png fitScale $ convertRGBA8 img
  where
    out = output format
    fits = (<= maxSize) . LB.length
    jpeg q = fmap (out . LB.fromStrict) . encodeRGB q
    png = pure . out . encodePng
    fitDisplay cheapest final = maybe tooLarge (fitHalving cheapest final Nothing) . halveToSide maxDisplaySide
    fitHalving :: STBIR.STBIRPixel a => (Image a -> IO LB.ByteString) -> (Maybe (Image a) -> Image a -> LB.ByteString -> IO LB.ByteString) -> Maybe (Image a) -> Image a -> IO (Either String LB.ByteString)
    fitHalving cheapest final larger i = do
      r <- cheapest i
      if fits r
        then Right <$> final larger i r
        else maybe tooLarge (fitHalving cheapest final (Just i)) $ halve i
    fitQuality _ rgb r = bisect maxSize qualityStep (`jpeg` rgb) (minJpegQuality, r) 95
    fitScale larger _ r = maybe (pure r) (\rgba -> bisect maxSize scaleStep (png . downscale rgba) (2, r) 1) larger
    tooLarge = pure $ Left "image does not fit"
    qualityStep good bad = if bad - good <= 1 then Nothing else Just $ (good + bad) `div` 2
    scaleStep good bad = if good - bad < 1 / 64 then Nothing else Just $ (good + bad) / 2

bisect :: Int64 -> (p -> p -> Maybe p) -> (p -> IO LB.ByteString) -> (p, LB.ByteString) -> p -> IO LB.ByteString
bisect maxSize step encode = go
  where
    go (good, r) bad = case step good bad of
      Nothing -> pure r
      Just m -> do
        r' <- encode m
        case compare (LB.length r') maxSize of
          LT -> go (m, r') bad
          EQ -> pure r'
          GT -> go (good, r) m

minJpegQuality :: Int
minJpegQuality = 20

maxImageSide :: Int
maxImageSide = 16384

maxDisplaySide :: Int
maxDisplaySide = 4320

maxJpegPixels :: Int
maxJpegPixels = 2 ^ (28 :: Int)

maxJpegMemoryMB :: Int
#if defined(mobileBuild)
maxJpegMemoryMB = 512
#else
maxJpegMemoryMB = 1024
#endif

hasSideAbove :: Int -> Image a -> Bool
hasSideAbove side img = imageWidth img > side || imageHeight img > side

hasSize :: (Int, Int) -> Image a -> Bool
hasSize (w, h) img = imageWidth img == w && imageHeight img == h

halveToSide :: STBIR.STBIRPixel a => Int -> Image a -> Maybe (Image a)
halveToSide side img
  | hasSideAbove side img = halveToSide side =<< halve img
  | otherwise = Just img

halve :: STBIR.STBIRPixel a => Image a -> Maybe (Image a)
halve img
  | imageWidth img < 2 || imageHeight img < 2 = Nothing
  | otherwise = Just $ downscale img 2

downscale :: STBIR.STBIRPixel a => Image a -> Float -> Image a
downscale img scale = STBIR.resize STBIR.defaultOptions (scaled imageWidth) (scaled imageHeight) img
  where
    scaled f = scaledSide scale (f img)

scaleRGB8 :: (Int, Int) -> Image PixelRGB8 -> Image PixelRGB8
scaleRGB8 size img
  | hasSize size img = img
  | otherwise = scaled {imageData = sharpen 3 size $ imageData scaled}
  where
    scaled = catmullRom [] size img

scaleRGBA8 :: (Int, Int) -> Image PixelRGBA8 -> Image PixelRGBA8
scaleRGBA8 size img
  | hasSize size img = img
  | otherwise = pixelMap unpremultiply $ scaled {imageData = sharpen 4 size $ imageData scaled}
  where
    scaled = catmullRom [STBIR.flag_ALPHA_PREMULTIPLIED] size $ pixelMap premultiply img

catmullRom :: STBIR.STBIRPixel a => [STBIR.Flag] -> (Int, Int) -> Image a -> Image a
catmullRom flags (w, h) = STBIR.resize options w h
  where
    options = STBIR.defaultOptions {STBIR.flags = flags, STBIR.filterHorizontal = STBIR.FILTER_CATMULLROM, STBIR.filterVertical = STBIR.FILTER_CATMULLROM}

premultiply :: PixelRGBA8 -> PixelRGBA8
premultiply (PixelRGBA8 r g b a) = PixelRGBA8 (multiply r) (multiply g) (multiply b) a
  where
    multiply c = fromIntegral ((fromIntegral c * fromIntegral a + 127) `div` 255 :: Int)

unpremultiply :: PixelRGBA8 -> PixelRGBA8
unpremultiply (PixelRGBA8 r g b a) = PixelRGBA8 (divide r) (divide g) (divide b) a
  where
    divide c
      | a == 0 = 0
      | otherwise = fromIntegral (min 255 $ (fromIntegral c * 255 + fromIntegral a `div` 2) `div` fromIntegral a :: Int)

sharpen :: Int -> (Int, Int) -> VS.Vector Word8 -> VS.Vector Word8
sharpen channels (w, h) samples = VS.generate (VS.length samples) $ \i -> unsharp (VS.unsafeIndex samples i) (VS.unsafeIndex blurred i)
  where
    rows = blurLines h w channels $ fromIntegral . VS.unsafeIndex samples
    blurred = blurLines 1 h (channels * w) $ float2Double . VS.unsafeIndex rows
    unsharp v b = fromIntegral (max 0 $ min 255 $ floor (fromIntegral v + sharpenAmount * float2Double (fromIntegral v - b) + 0.5) :: Int)

blurLines :: Int -> Int -> Int -> (Int -> Double) -> VS.Vector Float
blurLines count n step sample = VS.create $ do
  out <- VSM.unsafeNew $ count * n * step
  forM_ [0 .. count - 1] $ \line -> forM_ [0 .. n - 1] $ \p -> do
    let base = (line * n + p) * step
        offsets
          | p >= sharpenRadius && p < n - sharpenRadius = interior
          | otherwise = VS.map (\t -> (max 0 (min (n - 1) (p + t)) - p) * step) sharpenOffsets
        convolve i = go 0 0
          where
            go t s
              | t == VS.length sharpenWeights = s
              | otherwise = go (t + 1) (s + VS.unsafeIndex sharpenWeights t * sample (i + VS.unsafeIndex offsets t))
    forM_ [base .. base + step - 1] $ \i -> VSM.unsafeWrite out i $ double2Float $ convolve i
  pure out
  where
    interior = VS.map (* step) sharpenOffsets
{-# INLINE blurLines #-}

sharpenOffsets :: VS.Vector Int
sharpenOffsets = VS.enumFromTo (-radius) radius
  where
    radius = ceiling (3 * sharpenSigma)

sharpenWeights :: VS.Vector Double
sharpenWeights = VS.map (/ VS.sum weights) weights
  where
    weights = VS.map (\t -> let d = fromIntegral t in exp (negate d * d / (2 * sharpenSigma * sharpenSigma))) sharpenOffsets

sharpenRadius :: Int
sharpenRadius = VS.last sharpenOffsets

sharpenAmount :: Double
sharpenAmount = 0.5

sharpenSigma :: Double
sharpenSigma = 0.6

scaledSide :: Float -> Int -> Int
scaledSide scale side = round $ fromIntegral side / scale

toDataUri :: ImageFormat -> LB.ByteString -> LB.ByteString
toDataUri format body = "data:image/" <> mime <> ";base64," <> LB64.encode body
  where
    mime = case format of
      -- iOS and Android apps decode only the "jpg" mime type
      IFJpeg -> "jpg"
      IFPng -> "png"
