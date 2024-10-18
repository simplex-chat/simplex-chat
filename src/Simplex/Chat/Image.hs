{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Simplex.Chat.Image where

import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Codec.Picture as Picture
import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata)
import Codec.Picture.Png (encodePng)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (dropAlphaLayer, pixelFoldMap)
import Data.Int (Int64)
import Data.Monoid (All(..))
import Data.Word (Word8)

resizeImageToStrSize :: Int64 -> ResizeableImage -> Maybe LB.ByteString
resizeImageToStrSize maxSize (ResizeableImage fmt img encoder) = either resizePNG resizeJPG encoder
  where
    halveAndRetry = resizeImageToStrSize maxSize $ ResizeableImage fmt imgHalved encoder
    imgHalved = downscale img 2.0
    resizePNG enc
      | LB.length encoded <= maxSize = Just encoded
      | LB.length (encode imgHalved) > maxSize = halveAndRetry
      | otherwise = Just $ fitScale 1.0 2.0
      where
        encode = toDataUri "png" enc
        encoded = encode img
        fitScale l u
          | u - l < 1 / 64 = encode $ downscale img u -- prefer lower resolution
          | otherwise =
              case compare maxSize (LB.length result) of
                LT -> fitScale m u -- over budget, scale harder
                EQ -> result
                GT -> fitScale l m -- keep more pixels
              where
                m = (l + u) / 2
                result = encode $ downscale img m
    resizeJPG enc -- if minSize > maxSize then halveAndRetry else Just $ fitQuality 33 99
      | minSize > maxSize = halveAndRetry
      | otherwise = Just $ fitQuality 33 99
      where
        encode q = toDataUri "jpeg" (enc q) img
        minSize = LB.length $ encode 33
        -- fitQuality = binarySearch (\l u -> (l + u) `div` 2) (\l u -> u - l <= 1) encode score 50 99
        fitQuality l u
          | u - l <= 1 = encode l -- prefer higher compression
          | otherwise =
              case compare (LB.length result) maxSize of
                LT -> fitQuality m u -- keep more data
                EQ -> result
                GT -> fitQuality l m -- over budget, reduce quality
              where
                m = (l + u) `div` 2
                result = encode m

downscale :: STBIR.STBIRPixel pixel => Picture.Image pixel -> Float -> Picture.Image pixel
downscale img scale = STBIR.resize STBIR.defaultOptions (scaled Picture.imageWidth) (scaled Picture.imageHeight) img
  where
    scaled f = round $ fromIntegral (f img) / min 2.0 (max 0.5 scale)

toDataUri :: LB.ByteString -> (Picture.Image a -> LB.ByteString) -> Picture.Image a -> LB.ByteString
toDataUri fmt enc img = "data:image/" <> fmt <> ";base64," <> LB64.encode (enc img)

tryDropOpacity :: Picture.DynamicImage -> Picture.DynamicImage
tryDropOpacity dyn = case dyn of
  Picture.ImageRGBA16 img | opaque img -> Picture.ImageRGB16 $ dropAlphaLayer img
  Picture.ImageRGBA8 img | opaque img -> Picture.ImageRGB8 $ dropAlphaLayer img
  Picture.ImageYA16 img | opaque img -> Picture.ImageY16 $ dropAlphaLayer img
  Picture.ImageYA8 img | opaque img -> Picture.ImageY8 $ dropAlphaLayer img
  _ -> dyn
  where
    opaque :: (Picture.Pixel a, Eq (Picture.PixelBaseComponent a), Bounded (Picture.PixelBaseComponent a)) => Picture.Image a -> Bool
    opaque = getAll . pixelFoldMap (All . \pix -> Picture.pixelOpacity pix == maxBound)

data ResizeableImage where
  ResizeableImage
    :: (STBIR.STBIRPixel a) -- , ToDynamicImage a)
    => String -- ^ format label
    -> Picture.Image a -- ^ current image data
    -> ImageEncoder a
    -> ResizeableImage

type ImageEncoder a = Either (PNGEncoder a) (JPGEncoder a)
type PNGEncoder a = Picture.Image a -> LB.ByteString
type JPGEncoder a = Word8 -> Picture.Image a -> LB.ByteString

resizeableImage :: Picture.DynamicImage -> Maybe ResizeableImage
resizeableImage dyn = case dyn of
  Picture.ImageY8 img -> Just $ ResizeableImage "Y8" img $ Right $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty
  Picture.ImageY16 img -> Just $ ResizeableImage "Y16" img $ Left encodePng
  Picture.ImageY32 _ -> Nothing
  Picture.ImageYF _ -> Nothing
  Picture.ImageYA8 img -> Just $ ResizeableImage "YA8" img $ Left encodePng
  Picture.ImageYA16 img -> Just $ ResizeableImage "YA16" img $ Left encodePng
  Picture.ImageRGB8 img -> Just $ ResizeableImage "RGB8" img $ Right $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty
  Picture.ImageRGB16 img -> Just $ ResizeableImage "RGB16" img $ Left encodePng
  Picture.ImageRGBF _ -> Nothing
  Picture.ImageRGBA8 img -> Just $ ResizeableImage "RGBA8" img $ Left encodePng
  Picture.ImageRGBA16 img -> Just $ ResizeableImage "RGBA16" img $ Left encodePng
  Picture.ImageYCbCr8 img -> Just $ ResizeableImage "YCbCr8" img $ Right $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty
  Picture.ImageCMYK8 img -> Just $ ResizeableImage "CMYK8" img $ Right $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty
  Picture.ImageCMYK16 _ -> Nothing
  -- STBIR says all types currently covered by JP's 'Pixel' are supported.
