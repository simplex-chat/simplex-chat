{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Simplex.Chat.Image where

import qualified Codec.Picture as Picture
import Codec.Picture.Jpg (encodeDirectJpegAtQualityWithMetadata)
import Codec.Picture.Metadata (Metadatas)
import Codec.Picture.Png (encodePng)
import qualified Codec.Picture.STBIR as STBIR
import Codec.Picture.Types (dropAlphaLayer, pixelFoldMap)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Monoid (All (..))
import Data.Word (Word8)

resizeImageToSize :: Bool -> Int64 -> ResizeableImage -> LB.ByteString
resizeImageToSize toURI maxSize (ResizeableImage fmt img encoder) = either resizePNG resizeJPG encoder
  where
    halveAndRetry = resizeImageToSize toURI maxSize $ ResizeableImage fmt imgHalved encoder
    imgHalved = downscale img 2.0
    resizePNG enc
      | LB.length encoded <= maxSize = encoded
      | LB.length (encode imgHalved) > maxSize = halveAndRetry
      | otherwise = fitScale 1.0 2.0
      where
        encode
          | toURI = toDataUri "png" . enc
          | otherwise = enc
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
    resizeJPG enc
      | minSize > maxSize = halveAndRetry
      | otherwise = fitQuality 50 99
      where
        encode q
          | toURI = toDataUri "jpg" $ enc q img -- the correct mime type is "jpeg", but only "jpg" is supported by older clients
          | otherwise = enc q img
        minSize = LB.length $ encode 50
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

toDataUri :: LB.ByteString -> LB.ByteString -> LB.ByteString
toDataUri fmt body = "data:image/" <> fmt <> ";base64," <> LB64.encode body

decodeResizeable :: ByteString -> Either String (ResizeableImage, Metadatas)
decodeResizeable source = do
  (input, metadata) <- Picture.decodeImageWithMetadata source
  maybe (Left "unsupported image format") (pure . (,metadata)) $ resizeableImage (tryDropOpacity input)

readResizeable :: FilePath -> IO (Either String (ResizeableImage, Metadatas))
readResizeable inputPath = runExceptT $ do
  (input, metadata) <- ExceptT $ Picture.readImageWithMetadata inputPath -- will use mmap instead of reading the whole file
  maybe (throwError "unsupported image format") (pure . (,metadata)) $ resizeableImage (tryDropOpacity input)

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
  ResizeableImage ::
    STBIR.STBIRPixel a =>
    -- | format label
    String ->
    -- | current image data
    Picture.Image a ->
    ImageEncoder a ->
    ResizeableImage

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
