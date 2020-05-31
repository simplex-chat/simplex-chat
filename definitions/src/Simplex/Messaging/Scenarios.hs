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
  Start "Establish simplex messaging connection and send first message"
    :>> r :-> b |$ CreateConn "BODbZxmtKUUF1l8pj4nVjQ"
    :>> r :-> b |$ Subscribe "RU"
    :>> r :-> s |$ SendInvite "invitation RU" -- invitation - TODo
    :>> s :-> b |$ ConfirmConn "SU" "encrypted"
    :>> b :-> r |$ PushConfirm "RU" Message {msgId = "abc", msg = "XPaVEVNunkYKqqK0dnAT5Q"}
    :>> r :-> b |$ SecureConn "RU" "XPaVEVNunkYKqqK0dnAT5Q"
    :>> r :-> b |$ DeleteMsg "RU" "abc"
    :>> s :-> b |$ SendMsg "SU" "welcome" -- welcome message
    :>> b :-> r |$ PushMsg "RU" Message {msgId = "def", msg = "welcome"}
    :>> r :-> b |$ DeleteMsg "RU" "def"
    :>> s :-> b |$ SendMsg "SU" "hello there"
    :>> b :-> r |$ PushMsg "RU" Message {msgId = "ghi", msg = "hello there"}
    :>> r :-> b |$ DeleteMsg "RU" "ghi"
    :>> r :-> b |$ Unsubscribe "RU"
