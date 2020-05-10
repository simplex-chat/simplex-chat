{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- below warning appears because of hiding Monad operators from prelude exports
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators    #-}

module Simplex.Messaging.Scenarios where

import ClassyPrelude hiding ((>>=), (>>), fail)
import Data.Singletons
import Simplex.Messaging.Protocol

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

establishConnection  :: Command ()
                          Recipient Broker
                          (None <==> None <==| None)
                          (Secured <==> Secured <==| Secured)
                          False False 0 0
establishConnection = do -- it is commands composition, not Monad
  r --> b $ CreateConn "123"     -- recipient's public key for broker
  r --> b $ Subscribe
  r --> s $ SendInvite "invite"  -- TODO invitation object
  s --> b $ ConfirmConn "456"    -- sender's public key for broker"
  key <- b --> r $ PushConfirm
  r --> b $ SecureConn key
  r --> b $ DeleteMsg
  s --> b $ SendWelcome
  b --> r $ PushMsg
  r --> b $ DeleteMsg
  s --> b $ SendMsg "Hello"
  b --> r $ PushMsg
  r --> b $ DeleteMsg
  r --> b $ Unsubscribe
