{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.FakeHttp
  ( setSwitch,
    stalledAnswer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar')
import Control.Monad (forever)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Builder (byteString)
import Data.Text (Text)
import Network.HTTP.Types (hContentType, ok200)
import Network.Wai (Response, responseStream)

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
