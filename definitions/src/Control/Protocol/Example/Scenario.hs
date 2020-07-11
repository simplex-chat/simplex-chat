{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Example.Scenario where

import Control.Protocol
import Control.Protocol.Example.Command
import Control.XMonad.Do
import Data.Proxy
import Prelude hiding ((>>), (>>=))

r :: Proxy Recipient
r = Proxy

b :: Proxy Broker
b = Proxy

s :: Proxy Sender
s = Proxy

scenario :: String -> MyProtocol (RNone |: BNone |: SNone) (RReady |: BEmpty |: SReady) String
scenario str = do
  r ->:: b $ Create
  r ->:: s $ Notify
  s ->:: b $ Send str
  b ->:: r $ Forward
