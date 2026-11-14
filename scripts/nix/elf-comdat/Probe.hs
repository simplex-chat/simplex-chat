{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Control.Monad (unless)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, castPtrToFunPtr)
import GHCi.ObjLink (ShouldRetainCAFs (RetainCAFs), initObjLinker, loadArchive, loadObj, lookupSymbol, resolveObjs)
import System.Environment (getArgs)

foreign import ccall "dynamic" callInt :: FunPtr (IO CInt) -> IO CInt

main :: IO ()
main = do
  arguments <- getArgs
  initObjLinker RetainCAFs
  case arguments of
    [first, second] -> do
      mapM_ loadObj [first, second]
      resolve
      mapM invoke ["comdat_function", "reader_a", "reader_b"] >>= print
    ["--archive-conflict", first, second, strong] -> do
      -- Discovery initially selects the first archive's COMDAT definition.
      -- Loading an ordinary strong definition can replace that unused owner.
      -- The second archive must not retain stale permission to coalesce.
      mapM_ loadArchive [first, second]
      loadObj strong
      resolve
      invoke "reader_b" >>= print
    _ -> fail "expected two objects, or --archive-conflict and three paths"
  where
    resolve = resolveObjs >>= \ok -> unless ok (fail "resolveObjs returned False")
    invoke name = lookupSymbol name >>= maybe
      (fail (name <> " not found"))
      (callInt . castPtrToFunPtr)
