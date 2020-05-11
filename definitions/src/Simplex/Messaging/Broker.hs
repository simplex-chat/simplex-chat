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
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types


instance Prf HasState Sender s
         => ProtocolCommandOf Broker
              CreateConnRequest CreateConnResponse
              Recipient Broker
              (None <==> None <==| s)
              (New <==> New <==| s)
              Idle Idle 0 0
  where
    command = CreateConn
    protoCmd = bCreateConn

bCreateConn :: Connection Broker None Idle
            -> CreateConnRequest
            -> Either String (CreateConnResponse, Connection Broker New Idle)
bCreateConn = protoCmdStub
