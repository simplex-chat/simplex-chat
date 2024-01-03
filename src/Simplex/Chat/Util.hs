{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Util (week, encryptFile, chunkSize, shuffle) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import System.Random (randomRIO)
import UnliftIO.IO (IOMode (..), withFile)

week :: NominalDiffTime
week = 7 * 86400

encryptFile :: FilePath -> FilePath -> CryptoFileArgs -> ExceptT String IO ()
encryptFile fromPath toPath cfArgs = do
  let toFile = CryptoFile toPath $ Just cfArgs
  -- uncomment to test encryption error in runTestFileTransferEncrypted
  -- throwError "test error"
  withExceptT show $
    withFile fromPath ReadMode $ \r -> CF.withFile toFile WriteMode $ \w -> do
      encryptChunks r w
      liftIO $ CF.hPutTag w
  where
    encryptChunks r w = do
      ch <- liftIO $ LB.hGet r chunkSize
      unless (LB.null ch) $ liftIO $ CF.hPut w ch
      unless (LB.length ch < chunkSize) $ encryptChunks r w

chunkSize :: Num a => a
chunkSize = 65536
{-# INLINE chunkSize #-}

shuffle :: [a] -> IO [a]
shuffle xs = map snd . sortBy (comparing fst) <$> mapM (\x -> (,x) <$> random) xs
  where
    random :: IO Word16
    random = randomRIO (0, 65535)
