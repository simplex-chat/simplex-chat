module Probe (probeObject) where

import Control.Monad (unless)
import Data.Word (Word8)
import Foreign.Ptr (castPtr, ptrToWordPtr)
import Foreign.Storable (peekElemOff)
import GHCi.ObjLink (ShouldRetainCAFs (RetainCAFs), initObjLinker, loadObj, lookupSymbol, resolveObjs)

probeObject :: FilePath -> IO ()
probeObject object = do
  initObjLinker RetainCAFs
  loadObj object
  resolved <- resolveObjs
  unless resolved (fail "resolveObjs returned False")
  address <- lookupSymbol "probe"
  case address of
    Nothing -> fail "probe symbol not found"
    Just pointer -> do
      first <- peekElemOff (castPtr pointer) 0 :: IO Word8
      lastByte <- peekElemOff (castPtr pointer) 8191 :: IO Word8
      unless (first == 1 && lastByte == 0) (fail "probe payload changed")
      print (ptrToWordPtr pointer `mod` 64, first, lastByte)
