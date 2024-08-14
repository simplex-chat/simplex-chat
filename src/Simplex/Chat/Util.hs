{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Simplex.Chat.Util (week, encryptFile, chunkSize, liftIOEither, safeLast, shuffle) where

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LB
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Ord (comparing)
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import System.Random (randomRIO)
import qualified UnliftIO.Exception as E
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

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : xs) = Just $ L.last (x :| xs)

liftIOEither :: (MonadIO m, MonadError e m) => IO (Either e a) -> m a
liftIOEither a = liftIO a >>= liftEither
{-# INLINE liftIOEither #-}

newtype InternalException e = InternalException {unInternalException :: e}
  deriving (Eq, Show)

instance Exception e => Exception (InternalException e)

instance Exception e => MonadUnliftIO (ExceptT e IO) where
  {-# INLINE withRunInIO #-}
  withRunInIO :: ((forall a. ExceptT e IO a -> IO a) -> IO b) -> ExceptT e IO b
  withRunInIO inner =
    ExceptT . fmap (first unInternalException) . E.try $
      withRunInIO $ \run ->
        inner $ run . (either (E.throwIO . InternalException) pure <=< runExceptT)

instance Exception e => MonadUnliftIO (ExceptT e (ReaderT r IO)) where
  {-# INLINE withRunInIO #-}
  withRunInIO :: ((forall a. ExceptT e (ReaderT r IO) a -> IO a) -> IO b) -> ExceptT e (ReaderT r IO) b
  withRunInIO inner =
    withExceptT unInternalException . ExceptT . E.try $
      withRunInIO $ \run ->
        inner $ run . (either (E.throwIO . InternalException) pure <=< runExceptT)
