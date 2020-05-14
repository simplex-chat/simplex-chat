{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -ddump-splices     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Simplex.Messaging.Client where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types


$(protocol Recipient [d|
  raCreateConn  = CreateConn  --> Broker
  raSubscribe   = Subscribe   --> Broker
  rcPushConfirm = PushConfirm <-- Broker
  rcPushMsg     = PushMsg     <-- Broker
  |])


raCreateConn  :: Connection Recipient None Idle
              -> CreateConnRequest
              -> Either String CreateConnResponse
              -> Either String (Connection Recipient New Idle)
raCreateConn = protoActionStub

raSubscribe   :: Connection Recipient s Idle
              -> ()
              -> Either String ()
              -> Either String (Connection Recipient s Subscribed)
raSubscribe = protoActionStub

rcPushConfirm :: Connection Recipient Pending Subscribed
              -> SecureConnRequest
              -> Either String ((), Connection Recipient Confirmed Subscribed)
rcPushConfirm = protoCmdStub

rcPushMsg     :: Connection Recipient Secured Subscribed
              -> MessagesResponse -- TODO, has to be a single message
              -> Either String ((), Connection Recipient Secured Subscribed)
rcPushMsg = protoCmdStub
