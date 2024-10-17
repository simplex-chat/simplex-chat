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

module LinkPreviewTests where

import Control.Monad
import Data.Maybe
import Data.Monoid (All(..))
import qualified Data.List as List
import Data.Kind (Constraint, Type)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import Generic.Random (genericArbitraryU)
import Data.Ord
import MobileTests
import Simplex.Chat.Remote.Protocol (owsf2tagged)
import Simplex.Messaging.Parsers
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary (..), property)

import Codec.Picture.Types (dropAlphaLayer, pixelFoldMap)
import Codec.Picture.Png (PngSavable(..), encodeDynamicPng)
import Codec.Picture.Jpg (JpgEncodable, encodeDirectJpegAtQualityWithMetadata)
import qualified Codec.Picture as Picture
import qualified Codec.Picture.STBIR as STBIR
import Control.Logger.Simple
import Simplex.Messaging.Util (tshow, (<$?>))
import qualified Data.ByteString.Base64.Lazy as LB64
import Data.Word (Word8)
import Debug.Trace

linkPreviewTests :: Spec
linkPreviewTests = do
  fdescribe "Image resize" $ do
    it "WIP" wipTest

wipTest :: IO ()
wipTest = do
  logError "huh"
  (input, metadata) <- Picture.readImageWithMetadata "tests/fixtures/test.jpg" >>= either error pure
  Picture.savePngImage "test-nop.png" input
  Picture.readImageWithMetadata "test-nop.png" >>= either error (print . snd)

  case resizeableImage (tryDropOpacity input) of
    Nothing -> error "Unsupported format"
    Just image@(ResizeableImage fmt asPNG asJPG img) -> do
      logNote $ tshow (metadata, fmt, isJust asPNG, isJust asJPG)
      case resizeImageToStrSize 14000 image of
        Nothing -> error "Unable to resize"
        Just lbs -> do
          logDebug $ "Final size: " <> tshow (LB.length lbs)
          let (fmt, b64) = fmap (LB.drop 8) . LB.break (== ';') $ LB.drop 11 lbs
              outFile = "out." ++ LB.unpack fmt
          either error (LB.writeFile outFile) $ LB64.decode b64
          Picture.readImageWithMetadata outFile >>= either error (logDebug . tshow . snd)

resizeImageToStrSize :: Int -> ResizeableImage -> Maybe LB.ByteString
resizeImageToStrSize maxSize huh@(ResizeableImage fmt asPNG asJPG img) =
  case debug candidates of
    (_, fits) : _ -> Just fits
    [] | Picture.imageWidth img < 4 || Picture.imageHeight img < 4 -> Nothing
    [] -> resizeImageToStrSize maxSize $! ResizeableImage fmt asPNG asJPG (halve img)
  where
    debug = traceShow $ map (\(s, c) -> (s, LB.length c, LB.takeWhile (/= ';') $ LB.drop 11 c)) candidates
    candidates = List.sortBy (comparing fst) . filter ((>= 0) . fst) $ map (\s -> (fromIntegral maxSize - LB.length s, s)) $ png ++ jpg
    png = [ toDataUri "png" encode img | encode <- maybeToList asPNG ]
    jpg = [ toDataUri "jpeg" (encode q) img | encode <- maybeToList asJPG, q <- [50, 75, 87, 93, 96, 97, 99] ]

halve :: STBIR.STBIRPixel pixel => Picture.Image pixel -> Picture.Image pixel
halve img = STBIR.resize opts halfW halfH img
  where
    (halfW, _chopW) = Picture.imageWidth img `divMod` 2
    (halfH, _chopH) = Picture.imageHeight img `divMod` 2
    opts = STBIR.defaultOptions
      { STBIR.filterHorizontal = STBIR.FILTER_MITCHELL
      , STBIR.filterVertical = STBIR.FILTER_MITCHELL
      -- -- TODO: chop region border to keep size even
      -- -- STBIR.transform = Left Scale ...
      }

toDataUriPNG :: PngSavable a => Picture.Image a -> LB.ByteString
toDataUriPNG = toDataUri "png" encodePng

toDataUriJPG :: JpgEncodable a => Word8 -> Picture.Image a -> LB.ByteString
toDataUriJPG quality = toDataUri "jpeg" (encodeDirectJpegAtQualityWithMetadata quality mempty)

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
    :: (STBIR.STBIRPixel a, ToDynamicImage a)
    => String -- ^ format label
    -> Maybe (Picture.Image a -> LB.ByteString) -- ^ png encoder
    -> Maybe (Word8 -> Picture.Image a -> LB.ByteString) -- ^ jpeg encoder
    -> Picture.Image a -- ^ current image data
    -> ResizeableImage

resizeableImage :: Picture.DynamicImage -> Maybe ResizeableImage
resizeableImage dyn = case dyn of
  Picture.ImageY8 img -> Just $ ResizeableImage "Y8" (Just encodePng) (Just $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty) img
  Picture.ImageY16 img -> Just $ ResizeableImage "Y16" (Just encodePng) Nothing img
  Picture.ImageY32 _ -> Nothing
  Picture.ImageYF _ -> Nothing
  Picture.ImageYA8 img -> Just $ ResizeableImage "YA8" (Just encodePng) Nothing img
  Picture.ImageYA16 img -> Just $ ResizeableImage "YA16" (Just encodePng) Nothing img
  Picture.ImageRGB8 img -> Just $ ResizeableImage "RGB8" (Just encodePng) (Just $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty) img
  Picture.ImageRGB16 img -> Just $ ResizeableImage "RGB16" (Just encodePng) Nothing img
  Picture.ImageRGBF _ -> Nothing
  Picture.ImageRGBA8 img -> Just $ ResizeableImage "RGBA8" (Just encodePng) Nothing img
  Picture.ImageRGBA16 img -> Just $ ResizeableImage "RGBA16" (Just encodePng) Nothing img
  Picture.ImageYCbCr8 img -> Just $ ResizeableImage "YCbCr8" Nothing (Just $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty) img
  Picture.ImageCMYK8 img -> Just $ ResizeableImage "CMYK8" Nothing (Just $ \q -> encodeDirectJpegAtQualityWithMetadata q mempty) img
  Picture.ImageCMYK16 _ -> Nothing
  -- STBIR says all types currently covered by JP's 'Pixel' are supported.

class ToDynamicImage a where toDynamicImage :: Picture.Image a -> Picture.DynamicImage
instance ToDynamicImage Picture.Pixel8 where toDynamicImage = Picture.ImageY8
instance ToDynamicImage Picture.Pixel16 where toDynamicImage = Picture.ImageY16
instance ToDynamicImage Picture.Pixel32 where toDynamicImage = Picture.ImageY32
instance ToDynamicImage Picture.PixelF where toDynamicImage = Picture.ImageYF
instance ToDynamicImage Picture.PixelYA8 where toDynamicImage = Picture.ImageYA8
instance ToDynamicImage Picture.PixelYA16 where toDynamicImage = Picture.ImageYA16
instance ToDynamicImage Picture.PixelRGB8 where toDynamicImage = Picture.ImageRGB8
instance ToDynamicImage Picture.PixelRGB16 where toDynamicImage = Picture.ImageRGB16
instance ToDynamicImage Picture.PixelRGBF where toDynamicImage = Picture.ImageRGBF
instance ToDynamicImage Picture.PixelRGBA8 where toDynamicImage = Picture.ImageRGBA8
instance ToDynamicImage Picture.PixelRGBA16 where toDynamicImage = Picture.ImageRGBA16
instance ToDynamicImage Picture.PixelYCbCr8 where toDynamicImage = Picture.ImageYCbCr8
instance ToDynamicImage Picture.PixelCMYK8 where toDynamicImage = Picture.ImageCMYK8
instance ToDynamicImage Picture.PixelCMYK16 where toDynamicImage = Picture.ImageCMYK16
