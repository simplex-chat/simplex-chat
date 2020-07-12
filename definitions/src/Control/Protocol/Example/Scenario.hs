{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
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

scenario :: String -> MyProtocol '[None, None, None] '[Ready, Ready, Ready] String
scenario str = do
  r ->: b $ Create
  r ->: s $ Notify
  s ->: b $ Send str
  b ->: r $ Forward
