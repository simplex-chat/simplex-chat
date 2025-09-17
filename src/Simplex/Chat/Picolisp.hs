module Simplex.Chat.Picolisp where

import qualified Control.Exception as E
import Control.Monad
import Foreign
import Foreign.C

-- int picolisp(char*, int, int, char**);
foreign import ccall "picolisp" c_picolisp_init :: Ptr CChar -> CInt -> CInt -> Ptr CString -> IO CInt

-- char *evaluate(char*);
foreign import ccall "evaluate" c_picolisp_evaluate :: CString -> IO CString

-- void stoplisp(void);
foreign import ccall "stoplisp" c_picolisp_stop :: IO ()

picolispInit :: Int -> [String] -> IO ()
picolispInit size args = do
  stack <- mallocBytes size
  c_args <- mapM newCString args
  rc <- withArray c_args $ \arr ->
    c_picolisp_init stack (fromIntegral size) (fromIntegral $ length args) arr
  mapM_ free c_args
  when (rc == 0) $ do
    free stack
    E.throwIO $ userError "picolisp init failed"

picolispEvaluate :: String -> IO String
picolispEvaluate code = withCString code $ \s -> do
  r <- c_picolisp_evaluate s
  res <- peekCString r
  free r
  pure res

picolispStop :: IO ()
picolispStop = c_picolisp_stop
