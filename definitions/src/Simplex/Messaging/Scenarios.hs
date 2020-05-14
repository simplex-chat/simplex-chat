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
import Simplex.Messaging.ProtocolDo
import Simplex.Messaging.Types

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

establishConnection  :: Command
                          CreateConnRequest ()
                          Recipient Broker
                          (None <==> None <==| None)
                          (Secured <==> Secured <==| Secured)
                          Idle Idle 0 0
establishConnection = do -- it is commands composition, not Monad
  r --> b $ CreateConn
  r --> b $ Subscribe
  r --> s $ SendInvite
  s --> b $ ConfirmConn
  b --> r $ PushConfirm
  r --> b $ SecureConn
  r --> b $ DeleteMsg
  s --> b $ SendWelcome
  b --> r $ PushMsg
  r --> b $ DeleteMsg
  s --> b $ SendMsg
  b --> r $ PushMsg
  r --> b $ DeleteMsg
  r --> b $ Unsubscribe
