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
  HasState Recipient _ = ()
  HasState Broker None = ()
  HasState Broker New = ()
  HasState Broker Secured = ()
  HasState Broker Disabled = ()
  HasState Sender None = ()
  HasState Sender New = ()
  HasState Sender Confirmed = ()
  HasState Sender Secured = ()

type family Enabled (rs :: ConnState) (bs :: ConnState) :: Constraint where
  Enabled New New = ()
  Enabled Pending New = ()
  Enabled Confirmed New = ()
  Enabled Secured Secured = ()

type PartyCmd = (Party, ConnState, ConnState)

data Command (from :: PartyCmd) (to :: PartyCmd) (a :: Type) :: Type where
  CreateConn ::
    PublicKey ->
    Command '(Recipient, None, New) '(Broker, None, New) CreateConnResponse
  Subscribe ::
    Enabled rs bs =>
    ConnId ->
    Command '(Recipient, rs, rs) '(Broker, bs, bs) ()
  Unsubscribe ::
    Enabled rs bs =>
    ConnId ->
    Command '(Recipient, rs, rs) '(Broker, bs, bs) ()
  SendInvite ::
    Invitation ->
    Command '(Recipient, New, Pending) '(Sender, None, New) ()
  ConfirmConn ::
    SenderConnId ->
    Encrypted ->
    Command '(Sender, New, Confirmed) '(Broker, New, New) ()
  PushConfirm ::
    ConnId ->
    Message ->
    Command '(Broker, New, New) '(Recipient, Pending, Confirmed) ()
  SecureConn ::
    ConnId ->
    PublicKey ->
    Command '(Recipient, Confirmed, Secured) '(Broker, New, Secured) ()
  SendMsg ::
    (ss == Confirmed || ss == Secured) ~ True =>
    SenderConnId ->
    Encrypted ->
    Command '(Sender, ss, Secured) '(Broker, Secured, Secured) ()
  PushMsg ::
    ConnId ->
    Message ->
    Command '(Broker, Secured, Secured) '(Recipient, Secured, Secured) ()
  DeleteMsg ::
    ConnId ->
    MessageId ->
    Command '(Recipient, Secured, Secured) '(Broker, Secured, Secured) ()

-- connection type stub for all participants, TODO move from idris
data
  Connection
    (p :: Party)
    (s :: ConnState) :: Type
  where
  Connection :: String -> Connection p s -- TODO replace with real type definition

class Monad m => PartyProtocol m (p :: Party) where
  api ::
    Command from '(p, s, s') a ->
    Connection p s ->
    ExceptT String m (a, Connection p s')
  action ::
    Command '(p, s, s') to a ->
    Connection p s ->
    ExceptT String m a ->
    ExceptT String m (Connection p s')

apiStub :: Monad m => Connection p s -> ExceptT String m (a, Connection p s')
apiStub _ = throwE "api not implemented"

actionStub :: Monad m => Connection p s -> ExceptT String m a -> ExceptT String m (Connection p s')
actionStub _ _ = throwE "action not implemented"

type ProtocolState = (ConnState, ConnState, ConnState)

type family HasProtoSt (s :: ProtocolState) :: Constraint where
  HasProtoSt '(rs, bs, ss) =
    ( HasState Recipient rs,
      HasState Broker bs,
      HasState Sender ss
    )

type family ConnSt (p :: Party) (s :: ProtocolState) :: ConnState where
  ConnSt Recipient '(rs, _, _) = rs
  ConnSt Broker '(_, bs, _) = bs
  ConnSt Sender '(_, _, ss) = ss

type family ProtoSt (s :: ProtocolState) from fs' to ts' :: ProtocolState where
  ProtoSt s from fs' to ts' =
    '( PartySt Recipient s from fs' to ts',
       PartySt Broker s from fs' to ts',
       PartySt Sender s from fs' to ts'
     )

type family PartySt (p :: Party) (s :: ProtocolState) from fs' to ts' :: ConnState where
  PartySt from _ from fs' _ _ = fs'
  PartySt to _ _ _ to ts' = ts'
  PartySt p s _ _ _ _ = ConnSt p s

data ProtocolCmd (s :: ProtocolState) (s' :: ProtocolState) (a :: Type) :: Type where
  Start :: String -> ProtocolCmd s s ()
  ProtocolCmd ::
    (HasProtoSt s, HasState from fs', HasState to ts') =>
    Sing from ->
    Sing to ->
    Command '(from, ConnSt from s, fs') '(to, ConnSt to s, ts') a ->
    ProtocolCmd s (ProtoSt s from fs' to ts') a

type Protocol = XFree ProtocolCmd

infix 6 ->:

(->:) ::
  (HasProtoSt s, HasState from fs', HasState to ts') =>
  Sing from ->
  Sing to ->
  Command '(from, ConnSt from s, fs') '(to, ConnSt to s, ts') a ->
  Protocol s (ProtoSt s from fs' to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

start :: String -> Protocol s s ()
start = xfree . Start
