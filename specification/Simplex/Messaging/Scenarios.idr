module Simplex.Messaging.Scenarios

import Protocol

establishConnection : Command ()
                        (Recipient &> Broker)
                        (Null <==> (Null, 0) <==| Null)
                        (>>> Secured <==> (Secured, 0) <==| Secured)
establishConnection = do
  Recipient &: CreateConn "recipient's public key for broker"
  Recipient &: Subscribe
  Recipient &: SendInvite newInvitation
  Sender    &: ConfirmConn "sender's public key for broker"
  Broker    &: PushConfirm
  Recipient &: SecureConn "sender's public key for broker"
  Sender    &: SendWelcome
  Broker    &: PushWelcome
  Sender    &: SendMsg "Hello"
  Broker    &: PushMsg
  Recipient &: DeleteMsg
