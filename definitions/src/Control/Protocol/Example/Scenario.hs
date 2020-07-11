{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Example.Scenario where

import Control.Protocol
import Control.Protocol.Example.Command
import Control.XMonad.Do
import Data.Singletons
import Prelude hiding ((>>), (>>=))

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

scenario :: String -> MyProtocol (RNone |: BNone |: SNone) (RReady |: BEmpty |: SReady) String
scenario str = do
  r ->: b $ Create
  r ->: s $ Notify
  s ->: b $ Send str
  b ->: r $ Forward
