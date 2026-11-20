{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.FakeHttp
  ( setSwitch,
    stalledAnswer,
    nextFakeId,
    withResetServer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.Text (Text)
import Network.HTTP.Types (hContentType, ok200)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import Network.Wai (Response, responseStream)
import Simplex.Messaging.Util (stateTVar')
import System.IO.Unsafe (unsafePerformIO)

setSwitch :: TVar s -> Text -> (Bool -> s -> s) -> J.Object -> IO (Maybe Text)
setSwitch state path set o = case KM.toList o of
  [("on", J.Bool on)] -> Nothing <$ atomically (modifyTVar' state (set on))
  _ -> pure (Just (path <> " takes {\"on\": <bool>}"))

stalledAnswer :: Response
stalledAnswer = responseStream ok200 [(hContentType, "application/json")] $ \write flush -> do
  write (byteString "[") >> flush
  forever $ threadDelay trickleMicros >> write (byteString " ") >> flush

trickleMicros :: Int
trickleMicros = 100000

withResetServer :: (String -> IO a) -> IO a
withResetServer action = do
  sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
  N.bind sock (N.SockAddrInet 0 $ N.tupleToHostAddress (127, 0, 0, 1))
  N.listen sock 5
  port <- N.socketPort sock
  withAsync (forever $ N.accept sock >>= resetAfterHeaders . fst) (\_ -> action $ "http://127.0.0.1:" <> show port <> "/") `finally` N.close sock
  where
    resetAfterHeaders conn = do
      readHeaders conn ""
      NB.sendAll conn "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 100000\r\n\r\n{\"part"
      threadDelay 200000
      N.setSockOpt conn N.Linger (N.StructLinger 1 0)
      N.close conn
    readHeaders conn received
      | "\r\n\r\n" `B.isInfixOf` received = pure ()
      | otherwise = NB.recv conn 4096 >>= \b -> if B.null b then pure () else readHeaders conn (received <> b)

nextFakeId :: STM Int
nextFakeId = stateTVar' fakeIdCounter $ \n -> (n, n + 1)

fakeIdCounter :: TVar Int
fakeIdCounter = unsafePerformIO $ newTVarIO 1
{-# NOINLINE fakeIdCounter #-}
