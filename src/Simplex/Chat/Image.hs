{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Image
  ( ImageFormat (..),
    ColourProfile (..),
    imageFormat,
    decodeResizable,
    resizeImage,
    resizeImageDataUri,
    maxDecodeMemoryMB,
    scaleRGB8,
    scaleRGBA8,
  )
where

import Codec.Picture (DynamicImage (..), Image (..), Pixel (..), PixelRGB8, PixelRGBA8 (..), convertRGB8, convertRGBA8, dynamicMap, dynamicPixelMap, generateImage, pixelMap)
import Codec.Picture.JPEGTurbo (DecodeOptions (DecodeOptions), JpegHeader (..), Scale (..), decodeJpeg, encodeRGB, readJpegHeader)
import qualified Codec.Picture.STBIR as STBIR
import qualified Codec.Picture.Spng as Spng
import Codec.Picture.Types (pixelFoldMap)
import Control.Monad (forM_, guard, when, (<=<))
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.FileEmbed (embedFile)
import Data.Int (Int32, Int64)
import Data.List (maximumBy, minimumBy, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (All (..))
import Data.Ord (comparing)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word8)
import GHC.Float (double2Float, float2Double)

data ImageFormat = IFJpeg | IFPng

data ColourProfile = CPNone | CPDisplayP3
  deriving (Eq, Show)

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

decodeResizable :: ByteString -> IO (Either String (DynamicImage, ColourProfile))
decodeResizable s = runExceptT $ do
  (img, profile) <- decode
  pure . (img,) $! maybe CPNone colourProfile profile
  where
    decode
      | "\xFF\xD8" `B.isPrefixOf` s = decodeDisplayJpeg s
      | "\x89PNG\r\n\x1a\n" `B.isPrefixOf` s = ExceptT $ Spng.decodePng (Spng.DecodeOptions $ maxDecodeMemoryMB `div` 2) s
      | otherwise = throwError "unsupported image format"

decodeDisplayJpeg :: ByteString -> ExceptT String IO (DynamicImage, Maybe ByteString)
decodeDisplayJpeg s = do
  JpegHeader {jpegWidth, jpegHeight} <- ExceptT $ readJpegHeader s
  (halvings, size) <- maybe (throwError "image does not fit") pure $ displaySize jpegWidth jpegHeight
  (,jpegIccProfile segments) . orientImage (exifOrientation segments) . resizeToSize size <$> ExceptT (decodeJpeg (DecodeOptions (jpegScale halvings) maxJpegPixels maxDecodeMemoryMB) s)
  where
    segments = jpegSegments $ B.drop 2 s

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

exifOrientation :: [(Word8, ByteString)] -> Int
exifOrientation segments = fromMaybe 1 $ do
  tiff <- exifTiff segments
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

exifTiff :: [(Word8, ByteString)] -> Maybe ByteString
exifTiff segments = listToMaybe [B.drop 6 segment | (0xE1, segment) <- segments, "Exif\0\0" `B.isPrefixOf` segment]

jpegIccProfile :: [(Word8, ByteString)] -> Maybe ByteString
jpegIccProfile segments = do
  chunks <- sortOn (\(n, _, _) -> n) <$> mapM iccChunk iccSegments
  let profile = B.concat [chunk | (_, _, chunk) <- chunks]
  guard $ [n | (n, _, _) <- chunks] == [1 .. length chunks] && all (\(_, count, _) -> count == length chunks) chunks && not (B.null profile)
  pure profile
  where
    iccSegments = [segment | (0xE2, segment) <- segments, "ICC_PROFILE\0" `B.isPrefixOf` segment]
    iccChunk segment
      | B.length segment < 14 = Nothing
      | otherwise = Just (fromIntegral $ B.index segment 12, fromIntegral $ B.index segment 13, B.drop 14 segment)

jpegSegments :: ByteString -> [(Word8, ByteString)]
jpegSegments s
  | B.length s < 4 || B.index s 0 /= 0xFF || marker == 0xDA = []
  | otherwise = (marker, B.take (len - 2) $ B.drop 4 s) : jpegSegments (B.drop (2 + len) s)
  where
    marker = B.index s 1
    len = fromIntegral (B.index s 2) * 256 + fromIntegral (B.index s 3)

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

colourProfile :: ByteString -> ColourProfile
colourProfile profile = fromMaybe CPNone $ do
  primaries <- iccPrimaries profile
  p3Primaries <- iccPrimaries displayP3
  CPDisplayP3 <$ guard (and $ zipWith (\v v' -> abs (v - v') <= 2) primaries p3Primaries)

iccPrimaries :: ByteString -> Maybe [Integer]
iccPrimaries profile = do
  guard $ slice 16 4 == Just "RGB " && slice 20 4 == Just "XYZ "
  count <- unsigned <$> slice 128 4
  table <- slice 132 (12 * count)
  concat <$> mapM (colorant <=< tagEntry table) ["rXYZ", "gXYZ", "bXYZ"]
  where
    tagEntry entries sig
      | B.null entries = Nothing
      | B.take 4 entries == sig = Just $ B.take 12 entries
      | otherwise = tagEntry (B.drop 12 entries) sig
    colorant entry = do
      let offset = unsigned $ B.take 4 $ B.drop 4 entry
          size = unsigned $ B.drop 8 entry
      guard $ size >= 20
      tag <- slice offset size
      guard $ "XYZ " `B.isPrefixOf` tag
      pure [s15Fixed16 $ B.take 4 $ B.drop i tag | i <- [8, 12, 16]]
    slice i n
      | i + n > toInteger (B.length profile) = Nothing
      | otherwise = Just $ B.take (fromInteger n) $ B.drop (fromInteger i) profile
    unsigned = B.foldl' (\a b -> a * 256 + toInteger b) 0
    s15Fixed16 = toInteger . (fromInteger :: Integer -> Int32) . unsigned

displayP3 :: ByteString
displayP3 = $(embedFile "src/Simplex/Chat/Image/DisplayP3-v4.icc")

data ImageOutput = ImageBytes | ImageDataUri

resizeImage :: ImageFormat -> Int64 -> DynamicImage -> ColourProfile -> IO (Either String LB.ByteString)
resizeImage = fitImage ImageBytes

resizeImageDataUri :: Int64 -> DynamicImage -> ColourProfile -> IO (Either String LB.ByteString)
resizeImageDataUri maxSize img = fitImage ImageDataUri (imageFormat img) maxSize img

fitImage :: ImageOutput -> ImageFormat -> Int64 -> DynamicImage -> ColourProfile -> IO (Either String LB.ByteString)
fitImage output format maxSize img profile
  | dynamicMap (hasSideAbove maxImageSide) img = pure imageDoesNotFit
  | otherwise = maybe (pure imageDoesNotFit) (fitLargest . snd) $ dynamicMap (\i -> displaySize (imageWidth i) (imageHeight i)) img
  where
    (out, quality, bitsPerPixel, jpegBytes, outputLength) = case output of
      ImageBytes -> (id, 65, 0.9, id, id)
      ImageDataUri -> (toDataUri format, 20, 0.35, \target -> 0.75 * max 0 (target - fromIntegral (LB.length $ toDataUri format "")), \n -> 4 * ((n + 2) `div` 3))
    keptProfile = displayP3 <$ guard (profile == CPDisplayP3 && fromIntegral (outputLength $ B.length displayP3) <= maxSize `div` 10)
    fitLargest largest = case format of
      IFJpeg -> do
        let rgb = convertRGB8 img
            largestRGB = scaleRGB8 rgb largest
            scale size = if size == largest then largestRGB else scaleRGB8 rgb size
            jpeg q = fmap (out . LB.fromStrict) . encodeRGB q keptProfile
        fitSize maxSize (Just $ \target -> 8 * jpegBytes target / bitsPerPixel) largest (jpeg quality . scale) >>= \case
          Right (size, r) | size == largest && fromIntegral (LB.length r) < minFill * fromIntegral maxSize -> Right <$> bisect maxSize (`jpeg` largestRGB) (quality, r) 96
          r -> pure $ snd <$> r
      IFPng -> do
        let png = fmap (out . LB.fromStrict) . encodeSmallerPng keptProfile
        fmap snd <$> fitSize maxSize Nothing largest (png . scaleRGBA8 (convertRGBA8 img))

fitSize :: Int64 -> Maybe (Double -> Double) -> (Int, Int) -> ((Int, Int) -> IO LB.ByteString) -> IO (Either String ((Int, Int), LB.ByteString))
fitSize maxSize firstPixels (lw, lh) encode = search [] $ maybe lw (widthFor . ($ target)) firstPixels
  where
    search tried w = do
      r <- encode $ sizeOf w
      let tried' = (w, r) : tried
      if fits r && (size r >= minFill * limit || w == lw)
        then pure $ Right (sizeOf w, r)
        else maybe (largestFit tried') (search tried') $ nextWidth tried' w r
    nextWidth tried w r
      | length tried >= 8 = Nothing
      | predicted `notElem` map fst tried = Just predicted
      | hi - lo > 1 = Just $ (lo + hi) `div` 2
      | otherwise = Nothing
      where
        predicted = widthFor $ pixels w * (target / size r) ** (1 / slope tried)
        lo = maximum $ 0 : map fst (filter (fits . snd) tried)
        hi = minimum $ lw + 1 : [x | (x, r') <- tried, not (fits r'), x > lo]
    slope ((w2, r2) : (w1, r1) : _)
      | w1 /= w2 && size r1 /= size r2 && a >= 0.4 && a <= 1.3 = a
      where
        a = log (size r2 / size r1) / log (pixels w2 / pixels w1)
    slope _ = 0.85
    largestFit tried = case filter (fits . snd) tried of
      [] -> shrink $ minimum $ map fst tried
      fitting -> let (w, r) = maximumBy (comparing fst) fitting in pure $ Right (sizeOf w, r)
    shrink w
      | w < 2 = pure imageDoesNotFit
      | otherwise = do
          let w' = w `div` 2
          r <- encode $ sizeOf w'
          if fits r then pure $ Right (sizeOf w', r) else shrink w'
    widthFor p = max 1 $ min lw $ round $ sqrt (p * fromIntegral lw / fromIntegral lh)
    sizeOf w = (w, max 1 $ floor (fromIntegral (w * lh) / fromIntegral lw + 0.5 :: Double))
    pixels w = let (w', h') = sizeOf w in fromIntegral (w' * h')
    fits r = LB.length r <= maxSize
    size = fromIntegral . LB.length
    limit = fromIntegral maxSize
    target = sqrt minFill * limit

minFill :: Double
minFill = 0.97

imageDoesNotFit :: Either String a
imageDoesNotFit = Left "image does not fit"

bisect :: Int64 -> (Int -> IO LB.ByteString) -> (Int, LB.ByteString) -> Int -> IO LB.ByteString
bisect maxSize encode = go
  where
    go (good, r) bad
      | bad - good <= 1 = pure r
      | otherwise = do
          let m = (good + bad) `div` 2
          r' <- encode m
          case compare (LB.length r') maxSize of
            LT -> go (m, r') bad
            EQ -> pure r'
            GT -> go (good, r) m

encodeSmallerPng :: Maybe ByteString -> Image PixelRGBA8 -> IO ByteString
encodeSmallerPng profile img = minimumBy (comparing B.length) <$> mapM encode [False, True]
  where
    encode filtering = either fail pure =<< Spng.encodePng (Spng.EncodeOptions 9 filtering profile) img

maxImageSide :: Int
maxImageSide = 16384

maxDisplaySide :: Int
maxDisplaySide = 4320

maxJpegPixels :: Int
maxJpegPixels = 2 ^ (28 :: Int)

maxDecodeMemoryMB :: Int
#if defined(mobileBuild)
maxDecodeMemoryMB = 512
#else
maxDecodeMemoryMB = 1024
#endif

hasSideAbove :: Int -> Image a -> Bool
hasSideAbove side img = imageWidth img > side || imageHeight img > side

hasSize :: (Int, Int) -> Image a -> Bool
hasSize (w, h) img = imageWidth img == w && imageHeight img == h

scaleRGB8 :: Image PixelRGB8 -> (Int, Int) -> Image PixelRGB8
scaleRGB8 img size
  | hasSize size img = img
  | otherwise = scaled {imageData = sharpen 3 size $ imageData scaled}
  where
    scaled = catmullRom [] size img

scaleRGBA8 :: Image PixelRGBA8 -> (Int, Int) -> Image PixelRGBA8
scaleRGBA8 img = scale
  where
    premultiplied = pixelMap premultiply img
    scale size
      | hasSize size img = img
      | otherwise = pixelMap unpremultiply $ scaled {imageData = sharpen 4 size $ imageData scaled}
      where
        scaled = catmullRom [STBIR.flag_ALPHA_PREMULTIPLIED] size premultiplied

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
sharpen channels (w, h) samples = VS.create $ do
  rows <- VSM.unsafeNew $ VS.length sharpenOffsets * stride
  out <- VSM.unsafeNew $ VS.length samples
  let blurRow r = forM_ [0 .. w - 1] $ \p -> do
        let offsets
              | p >= sharpenRadius && p < w - sharpenRadius = interior
              | otherwise = VS.map (\t -> (max 0 (min (w - 1) (p + t)) - p) * channels) sharpenOffsets
        forM_ [0 .. channels - 1] $ \c -> do
          let i = (r * w + p) * channels + c
          v <- convolve (\o -> pure $ fromIntegral $ VS.unsafeIndex samples $ i + o) offsets
          VSM.unsafeWrite rows (rowStart r + p * channels + c) $ double2Float v
  forM_ [0 .. min h sharpenRadius - 1] blurRow
  forM_ [0 .. h - 1] $ \y -> do
    when (y + sharpenRadius < h) $ blurRow $ y + sharpenRadius
    let offsets = VS.map (\t -> rowStart $ max 0 $ min (h - 1) $ y + t) sharpenOffsets
    forM_ [0 .. stride - 1] $ \x -> do
      b <- convolve (\o -> float2Double <$> VSM.unsafeRead rows (o + x)) offsets
      let i = y * stride + x
      VSM.unsafeWrite out i $ unsharp (VS.unsafeIndex samples i) $ double2Float b
  pure out
  where
    stride = channels * w
    rowStart r = (r `mod` VS.length sharpenOffsets) * stride
    interior = VS.map (* channels) sharpenOffsets
    unsharp v b = fromIntegral (max 0 $ min 255 $ floor (fromIntegral v + sharpenAmount * float2Double (fromIntegral v - b) + 0.5) :: Int)

convolve :: Monad m => (Int -> m Double) -> VS.Vector Int -> m Double
convolve sample offsets = go 0 0
  where
    go t s
      | t == VS.length sharpenWeights = pure s
      | otherwise = sample (VS.unsafeIndex offsets t) >>= \v -> go (t + 1) (s + VS.unsafeIndex sharpenWeights t * v)
{-# INLINE convolve #-}

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
