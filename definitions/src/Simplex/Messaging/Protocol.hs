{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Protocol where

import Control.Monad.Trans.Except
import Control.XFreer
import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Data.Type.Bool (type (||))
import Simplex.Messaging.Types

$( singletons
     [d|
       data Party = Recipient | Broker | Sender
         deriving (Show, Eq)

       data ConnState
         = None -- (all) not available or removed from the broker
         | New -- (recipient, broker) connection created (or received from sender)
         | Pending -- (recipient, sender) sent to sender out-of-band
         | Confirmed -- (recipient) confirmed by sender with the broker
         | Secured -- (all) secured with the broker
         | Disabled -- (broker, recipient) disabled with the broker by recipient
         deriving (Show, Eq)
       |]
 )

type family HasState (p :: Party) (s :: ConnState) :: Constraint where
  HasState Recipient s = ()
  HasState Broker None = ()
  HasState Broker New = ()
  HasState Broker Secured = ()
  HasState Broker Disabled = ()
  HasState Sender None = ()
  HasState Sender New = ()
  HasState Sender Confirmed = ()
  HasState Sender Secured = ()

type Enabled rs bs =
  ( (rs == New || rs == Pending || rs == Confirmed || rs == Secured) ~ True,
    (bs == New || bs == Secured) ~ True
  )

data
  Command
    (from :: Party)
    (fs :: ConnState)
    (fs' :: ConnState)
    (to :: Party)
    (ts :: ConnState)
    (ts' :: ConnState)
    (res :: Type) :: Type
  where
  CreateConn ::
    PublicKey ->
    Command Recipient None New Broker None New CreateConnResponse
  Subscribe ::
    Enabled rs bs =>
    ConnId ->
    Command Recipient rs rs Broker bs bs ()
  Unsubscribe ::
    Enabled rs bs =>
    ConnId ->
    Command Recipient rs rs Broker bs bs ()
  SendInvite ::
    Invitation ->
    Command Recipient New Pending Sender None New ()
  ConfirmConn ::
    SenderConnId ->
    Encrypted ->
    Command Sender New Confirmed Broker New New ()
  PushConfirm ::
    ConnId ->
    Message ->
    Command Broker New New Recipient Pending Confirmed ()
  SecureConn ::
    ConnId ->
    PublicKey ->
    Command Recipient Confirmed Secured Broker New Secured ()
  SendMsg ::
    (ss == Confirmed || ss == Secured) ~ True =>
    SenderConnId ->
    Encrypted ->
    Command Sender ss Secured Broker Secured Secured ()
  PushMsg ::
    ConnId ->
    Message ->
    Command Broker Secured Secured Recipient Secured Secured ()
  DeleteMsg ::
    ConnId ->
    MessageId ->
    Command Recipient Secured Secured Broker Secured Secured ()

-- connection type stub for all participants, TODO move from idris
data
  Connection
    (p :: Party)
    (s :: ConnState) :: Type
  where
  Connection :: String -> Connection p s -- TODO replace with real type definition

class Monad m => PartyProtocol m (p :: Party) where
  api ::
    Command from fs fs' p ps ps' res ->
    Connection p ps ->
    ExceptT String m (res, Connection p ps')
  action ::
    Command p ps ps' to ts ts' res ->
    Connection p ps ->
    ExceptT String m res ->
    ExceptT String m (Connection p ps')

apiStub :: Monad m => Connection p ps -> ExceptT String m (res, Connection p ps')
apiStub _ = throwE "api not implemented"

actionStub :: Monad m => Connection p ps -> ExceptT String m res -> ExceptT String m (Connection p ps')
actionStub _ _ = throwE "action not implemented"

type family AllowedStates s from fs' to ts' :: Constraint where
  AllowedStates '(rs, bs, ss) from fs' to ts' =
    ( HasState Recipient rs,
      HasState Broker bs,
      HasState Sender ss,
      HasState from fs',
      HasState to ts'
    )

type ProtocolState = (ConnState, ConnState, ConnState)

type family ConnSt (p :: Party) (s :: ProtocolState) :: ConnState where
  ConnSt Recipient '(rs, _, _) = rs
  ConnSt Broker '(_, bs, _) = bs
  ConnSt Sender '(_, _, ss) = ss

type family ProtoSt (s :: ProtocolState) from fs' to ts' where
  ProtoSt s from fs' to ts' =
    '( PartySt Recipient s from fs' to ts',
       PartySt Broker s from fs' to ts',
       PartySt Sender s from fs' to ts'
     )

type family PartySt (p :: Party) (s :: ProtocolState) from fs' to ts' where
  PartySt from _ from fs' _ _ = fs'
  PartySt to _ _ _ to ts' = ts'
  PartySt p s _ _ _ _ = ConnSt p s

data ProtocolEff (s :: ProtocolState) (s' :: ProtocolState) (a :: Type) :: Type where
  Start :: String -> ProtocolEff s s ()
  ProtocolCmd ::
    AllowedStates s from fs' to ts' =>
    Sing from ->
    Sing to ->
    Command from (ConnSt from s) fs' to (ConnSt to s) ts' a ->
    ProtocolEff s (ProtoSt s from fs' to ts') a

type Protocol = XFree ProtocolEff

infix 6 ->:

(->:) ::
  AllowedStates s from fs' to ts' =>
  Sing from ->
  Sing to ->
  Command from (ConnSt from s) fs' to (ConnSt to s) ts' a ->
  Protocol s (ProtoSt s from fs' to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

start :: String -> Protocol s s ()
start = xfree . Start

infix 5 |$

(|$) :: (a -> b) -> a -> b
f |$ x = f x
