{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Simplex.Messaging.Scenarios where

import Control.Protocol
import Control.XMonad.Do
import Data.Singletons
import Data.String
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types
import Prelude hiding ((>>), (>>=))

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

establishConnection :: SimplexProtocol (None |: None |: None) (Secured |: Secured |: Secured) ()
establishConnection = do
  comment "Establish simplex messaging connection and send first message"
  r ->: b $ CreateConn "BODbZxmtKUUF1l8pj4nVjQ"
  r ->: b $ Subscribe "RU"
  r ->: s $ SendInvite "invitation RU" -- invitation - TODo
  s ->: b $ ConfirmConn "SU" "encrypted"
  b ->: r $ PushConfirm "RU" Message {msgId = "abc", msg = "XPaVEVNunkYKqqK0dnAT5Q"}
  r ->: b $ SecureConn "RU" "XPaVEVNunkYKqqK0dnAT5Q"
  r ->: b $ DeleteMsg "RU" "abc"
  s ->: b $ SendMsg "SU" "welcome" -- welcome message
  b ->: r $ PushMsg "RU" Message {msgId = "def", msg = "welcome"}
  r ->: b $ DeleteMsg "RU" "def"
  s ->: b $ SendMsg "SU" "hello there"
  b ->: r $ PushMsg "RU" Message {msgId = "ghi", msg = "hello there"}
  r ->: b $ DeleteMsg "RU" "ghi"
  r ->: b $ Unsubscribe "RU"
