{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Simplex.Messaging.Client where

import ClassyPrelude
import Data.Singletons.TH
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types


instance Prf HasState Sender s
         => ProtocolActionOf Recipient
              CreateConnRequest CreateConnResponse
              Recipient Broker
              (None <==> None <==| s)
              (New <==> New <==| s)
              Idle Idle 0 0
  where
    action      = CreateConn
    protoAction = rCreateConn

instance ( (r /= None && r /= Disabled) ~ True
         , (b /= None && b /= Disabled) ~ True
         , Prf HasState Sender s )
         => ProtocolActionOf Recipient
              () ()
              Recipient Broker
              (r <==> b <==| s)
              (r <==> b <==| s)
              Idle Subscribed n n
  where
    action      = Subscribe
    protoAction = rSubscribe


rCreateConn :: Connection Recipient None Idle
            -> CreateConnRequest
            -> Either String CreateConnResponse
            -> Either String (Connection Recipient New Idle)
rCreateConn = protoActionStub

rSubscribe  :: Connection Recipient s Idle
            -> ()
            -> Either String ()
            -> Either String (Connection Recipient s Subscribed)
rSubscribe = protoActionStub
