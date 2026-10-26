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
  )
where

import Codec.Picture (DynamicImage (..), Image (..), Pixel (..), convertRGB8, convertRGBA8, decodeImage, dynamicMap)
import Codec.Picture.JPEGTurbo (DecodeOptions (DecodeOptions), JpegHeader (..), Scale (..), decodeJpeg, encodeRGB, readJpegHeader)
import Codec.Picture.Png (encodePng)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (pixelFoldMap)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Monoid (All (..))

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
  resizeToSize size <$> ExceptT (decodeJpeg (DecodeOptions (jpegScale halvings) maxJpegPixels maxJpegMemoryMB) s)

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
resizeToSize (w, h) img
  | dynamicMap (\i -> imageWidth i == w && imageHeight i == h) img = img
  | otherwise = ImageRGB8 $ STBIR.resize STBIR.defaultOptions w h $ convertRGB8 img

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
maxJpegMemoryMB = 2047
#endif

hasSideAbove :: Int -> Image a -> Bool
hasSideAbove side img = imageWidth img > side || imageHeight img > side

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

scaledSide :: Float -> Int -> Int
scaledSide scale side = round $ fromIntegral side / scale

toDataUri :: ImageFormat -> LB.ByteString -> LB.ByteString
toDataUri format body = "data:image/" <> mime <> ";base64," <> LB64.encode body
  where
    mime = case format of
      -- iOS and Android apps decode only the "jpg" mime type
      IFJpeg -> "jpg"
      IFPng -> "png"
