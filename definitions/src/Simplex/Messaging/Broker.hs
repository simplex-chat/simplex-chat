{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Simplex.Messaging.Broker where

import ClassyPrelude
import Data.Singletons.TH
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types


instance Prf HasState Sender s
         => ProtocolCommand Broker
              Recipient
              CreateConnRequest CreateConnResponse
              (None <==> None <==| s)
              (New <==> New <==| s)
              Idle Idle 0 0
  where
    command = CreateConn
    protoCmd = bCreateConn

instance ( (r /= None && r /= Disabled) ~ True
         , (b /= None && b /= Disabled) ~ True
         , Prf HasState Sender s )
         => ProtocolCommand Broker
              Recipient
              () ()
              (r <==> b <==| s)
              (r <==> b <==| s)
              Idle Subscribed n n
  where
    command  = Subscribe
    protoCmd = bSubscribe


bCreateConn :: Connection Broker None Idle
            -> CreateConnRequest
            -> Either String (CreateConnResponse, Connection Broker New Idle)
bCreateConn = protoCmdStub

bSubscribe  :: Connection Broker s Idle
            -> ()
            -> Either String ((), Connection Broker s Subscribed)
bSubscribe = protoCmdStub
