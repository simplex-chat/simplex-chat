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
import Data.Singletons.TH
import Simplex.Messaging.Protocol
import Simplex.Messaging.Types


$(protocol Recipient [d|
  raCreateConn  = CreateConn  --> Broker
  -- raSubscribe   = Subscribe   --> Broker
  -- rcPushConfirm = PushConfirm <-- Broker
  -- rcPushMsg     = PushMsg     <-- Broker
  |])

-- runQ [d| raCreateConn  = CreateConn  --> Broker |]
-- [
--   ValD
--     (VarP raCreateConn_0)
--     (NormalB
--       (InfixE
--         (Just (ConE Simplex.Messaging.Protocol.CreateConn))
--         (VarE Simplex.Messaging.Protocol.-->)
--         (Just (ConE Simplex.Messaging.Protocol.Broker))))      
--     []
-- ]

instance Prf HasState Sender s
         => ProtocolAction
              CreateConnRequest CreateConnResponse
              Recipient Broker
              (None <==> None <==| s)
              (New <==> New <==| s)
              Idle Idle 0 0
  where
    action      = CreateConn
    protoAction = raCreateConn

instance ( (r /= None && r /= Disabled) ~ True
         , (b /= None && b /= Disabled) ~ True
         , Prf HasState Sender s )
         => ProtocolAction
              () ()
              Recipient Broker
              (r <==> b <==| s)
              (r <==> b <==| s)
              Idle Subscribed n n
  where
    action      = Subscribe
    protoAction = rSubscribe


raCreateConn :: Connection Recipient None Idle
             -> CreateConnRequest
             -> Either String CreateConnResponse
             -> Either String (Connection Recipient New Idle)
raCreateConn = protoActionStub

rSubscribe  :: Connection Recipient s Idle
            -> ()
            -> Either String ()
            -> Either String (Connection Recipient s Subscribed)
rSubscribe = protoActionStub


decl = [d|
  instance Prf HasState Sender s
           => ProtocolAction
                CreateConnRequest CreateConnResponse
                Recipient Broker
                (None <==> None <==| s)
                (New <==> New <==| s)
                Idle Idle 0 0
    where
      action      = CreateConn
      protoAction = rCreateConn
  |]
