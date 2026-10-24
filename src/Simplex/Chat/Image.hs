{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Image
  ( ImageFormat (..),
    imageFormat,
    resizeImage,
    resizeImageDataUri,
  )
where

import Codec.Picture (DynamicImage (..), Image (..), Pixel (..), convertRGB8, convertRGBA8, dynamicMap)
import Codec.Picture.JPEGTurbo (encodeRGB)
import Codec.Picture.Png (encodePng)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (pixelFoldMap)
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

resizeImage :: ImageFormat -> Int64 -> DynamicImage -> IO (Either String LB.ByteString)
resizeImage = fitImage $ const id

resizeImageDataUri :: Int64 -> DynamicImage -> IO (Either String LB.ByteString)
resizeImageDataUri maxSize img = fitImage toDataUri (imageFormat img) maxSize img

fitImage :: (ImageFormat -> LB.ByteString -> LB.ByteString) -> ImageFormat -> Int64 -> DynamicImage -> IO (Either String LB.ByteString)
fitImage output format maxSize img
  | dynamicMap (hasSideAbove maxImageSide) img = tooLarge
  | otherwise = case format of
      IFJpeg -> fitHalving (jpeg minJpegQuality) fitQuality Nothing $ convertRGB8 img
      IFPng -> fitHalving png fitScale Nothing $ convertRGBA8 img
  where
    out = output format
    fits = (<= maxSize) . LB.length
    jpeg q = fmap (out . LB.fromStrict) . encodeRGB q
    png = pure . out . encodePng
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
maxImageSide = 2 ^ (20 :: Int)

hasSideAbove :: Int -> Image a -> Bool
hasSideAbove side img = imageWidth img > side || imageHeight img > side

halve :: STBIR.STBIRPixel a => Image a -> Maybe (Image a)
halve img
  | imageWidth img < 2 || imageHeight img < 2 = Nothing
  | otherwise = Just $ downscale img 2

downscale :: STBIR.STBIRPixel a => Image a -> Float -> Image a
downscale img scale = STBIR.resize STBIR.defaultOptions (scaled imageWidth) (scaled imageHeight) img
  where
    scaled f = round $ fromIntegral (f img) / scale

toDataUri :: ImageFormat -> LB.ByteString -> LB.ByteString
toDataUri format body = "data:image/" <> mime <> ";base64," <> LB64.encode body
  where
    mime = case format of
      -- iOS and Android apps decode only the "jpg" mime type
      IFJpeg -> "jpg"
      IFPng -> "png"
