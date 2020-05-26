{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Scenarios where

import Data.Singletons
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

establishConnection :: Protocol None None None Secured Secured Secured ()
establishConnection =
  Start "Establish simplex messaging connection"
    :>> r :-> b |$ CreateConn "BODbZxmtKUUF1l8pj4nVjQ"
    :>> r :-> b |$ Subscribe "Qxz93A"
    :>> r :-> s |$ SendInvite "invitation Qxz93A" -- invitation - TODo
    :>> s :-> b |$ ConfirmConn "N9pA3g" "encrypted"
    :>> b :-> r |$ PushConfirm "Qxz93A" Message {msgId = "abc", msg = "XPaVEVNunkYKqqK0dnAT5Q"}
    :>> r :-> b |$ SecureConn "Qxz93A" "XPaVEVNunkYKqqK0dnAT5Q"
    :>> r :-> b |$ DeleteMsg "Qxz93A" "abc"
    :>> s :-> b |$ SendMsg "N9pA3g" "welcome" -- welcome message
    :>> b :-> r |$ PushMsg "Qxz93A" Message {msgId = "def", msg = "welcome"}
    :>> r :-> b |$ DeleteMsg "Qxz93A" "def"
    :>> s :-> b |$ SendMsg "N9pA3g" "hello there"
    :>> b :-> r |$ PushMsg "Qxz93A" Message {msgId = "ghi", msg = "hello there"}
    :>> r :-> b |$ DeleteMsg "Qxz93A" "ghi"
    :>> r :-> b |$ Unsubscribe "Qxz93A"
