{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Messaging.Scenarios where

import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

import ClassyPrelude

establishConnection  :: Command ()
                          Recipient Broker
                          (None <==> None <==| None)
                          (Secured <==> Secured <==| Secured)
                          False False 0 0
establishConnection =
  SRecipient ==> SBroker     &: CreateConn "123"     :>>=  -- recipient's public key for broker
                                \CreateConnResponse{..} ->
  SRecipient ==> SBroker     &: Subscribe            :>>
  SRecipient ==> SSender     &: SendInvite "invite"  :>>   -- TODO invitation object
  SSender    ==> SBroker     &: ConfirmConn "456"    :>>   -- sender's public key for broker"
  SBroker    ==> SRecipient  &: PushConfirm          :>>=
                                \senderKey ->
  SRecipient ==> SBroker     &: SecureConn senderKey :>>
  SRecipient ==> SBroker     &: DeleteMsg            :>>
  SSender    ==> SBroker     &: SendWelcome          :>>
  SBroker    ==> SRecipient  &: PushMsg              :>>
  SRecipient ==> SBroker     &: DeleteMsg            :>>
  SSender    ==> SBroker     &: SendMsg "Hello"      :>>
  SBroker    ==> SRecipient  &: PushMsg              :>>
  SRecipient ==> SBroker     &: DeleteMsg            :>>
  SRecipient ==> SBroker     &: Unsubscribe
