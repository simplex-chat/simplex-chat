module Simplex.Messaging.Scenarios

import Protocol

establishConnection : Command () Recipient Recipient
                        (Null <==> (Null, 0) <==| Null)
                        (>>> Secured <==> (Secured, 0) <==| Secured)
establishConnection = do
  ids <- CreateConn "recipient's public key for broker"
  Subscribe
  SendInvite newInvitation
  ConfirmConn "sender's public key for broker"
  PushConfirm
  SecureConn "sender's public key for broker"
  SendWelcome
  PushWelcome
  SendMsg "Hello"
  PushMsg
