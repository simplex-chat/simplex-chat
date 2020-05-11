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

rCreateConn :: Connection Recipient None Idle
            -> CreateConnRequest
            -> Either String CreateConnResponse
            -> Either String (Connection Recipient New Idle)
rCreateConn = protoActionStub
