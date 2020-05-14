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

module Simplex.Messaging.Broker where

import ClassyPrelude
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types

$(protocol Broker [d|
  bcCreateConn  = CreateConn  <-- Recipient
  bcSubscribe   = Subscribe   <-- Recipient
  -- rcPushConfirm = PushConfirm <-- Broker
  -- rcPushMsg     = PushMsg     <-- Broker
  |])


bcCreateConn :: Connection Broker None Idle
             -> CreateConnRequest
             -> Either String (CreateConnResponse, Connection Broker New Idle)
bcCreateConn = protoCmdStub

bcSubscribe  :: Connection Broker s Idle
             -> ()
             -> Either String ((), Connection Broker s Subscribed)
bcSubscribe = protoCmdStub
