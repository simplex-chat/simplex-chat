module LinkPreviewTests where

import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Codec.Picture as Picture
import Control.Logger.Simple
import Control.Monad
import Simplex.Chat.Image (ResizeableImage (..))
import qualified Simplex.Chat.Image as Image
import Simplex.Messaging.Util (tshow)
import Test.Hspec

linkPreviewTests :: SpecWith FilePath
linkPreviewTests = do
  fdescribe "Image resize" $ do
    it "JPG" $ resizeToStrTest "tests/fixtures/test.jpg"
    it "PNG with alpha" $ resizeToStrTest "tests/fixtures/logo-large-rgba.png"
    it "PNG without alpha" $ resizeToStrTest "tests/fixtures/preview-issue1.png"

resizeToStrTest :: FilePath -> FilePath -> IO ()
resizeToStrTest inputPath tmp = do
  (ri@(ResizeableImage imgFormat _img encoder), metadata) <- either error pure =<< Image.readResizeable inputPath
  logDebug $ tshow (metadata, imgFormat, either (const "png") (const "jpeg") encoder)
  let res = Image.resizeImageToStrSize maxSize ri
      finalSize = LB.length res
  unless (finalSize <= maxSize) $ error $ "Final size larger than maximum size: " <> show (finalSize, maxSize)
  let (fmt, b64) = fmap (LB.drop 8) . LB.break (== ';') $ LB.drop 11 res
      outFile = tmp ++ "/out." ++ LB.unpack fmt
  either error (LB.writeFile outFile) $ LB64.decode b64
  Picture.readImageWithMetadata outFile >>= either error (logDebug . tshow . snd)
  where
    maxSize = 14000
