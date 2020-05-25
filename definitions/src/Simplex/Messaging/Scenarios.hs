{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Scenarios where

import Data.Singletons
import Simplex.Messaging.Protocol

r :: Sing Recipient
r = SRecipient

b :: Sing Broker
b = SBroker

s :: Sing Sender
s = SSender

scenario :: Protocol None None None Pending New Confirmed ()
scenario =
  Start
    :>> (r --> b $ CreateConn "")
    :>> (r --> b $ Subscribe "")
    :>> (r --> s $ SendInvite "")
    :>> (s --> b $ ConfirmConn "" "")
