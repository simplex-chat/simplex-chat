{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Messaging.Scenarios where

import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

establishConnection  :: Command ()
                          'Recipient 'Broker
                          ('None <==> 'None <==| 'None)
                          ('Secured <==> 'Secured <==| 'Secured)
establishConnection =
  SRecipient ==> SBroker     &: CreateConn "123"     :>>=  --  recipient's public key for broker
                                \CreateConnResponse{..} ->
  SRecipient ==> SBroker     &: Subscribe            :>>   -- TODO add subscribed state
  SRecipient ==> SSender     &: SendInvite "invite"  :>>   -- TODO invitation object
  SSender    ==> SBroker     &: ConfirmConn "456"    :>>   -- sender's public key for broker"
  SBroker    ==> SRecipient  &: PushConfirm          :>>=
                                \senderKey ->
  SRecipient ==> SBroker     &: SecureConn senderKey :>>
  SSender    ==> SBroker     &: SendWelcome          :>>
  SBroker    ==> SRecipient  &: PushWelcome          :>>
  SSender    ==> SBroker     &: SendMsg "Hello"      :>>
  SBroker    ==> SRecipient  &: PushMsg              :>>
  SRecipient ==> SBroker     &: DeleteMsg
