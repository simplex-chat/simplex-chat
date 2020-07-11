{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Protocol where

import Control.Monad.Trans.Except
import Control.Protocol
import Control.XFreer
import Data.Kind
import Data.Proxy
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Simplex.Messaging.Types

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

-- type PartyCmd = (Party, ConnState, ConnState)

data Command (from :: PartyCmd Party) (to :: PartyCmd Party) (a :: Type) :: Type where
  CreateConn ::
    PublicKey ->
    Command (P Recipient None New) (P Broker None New) CreateConnResponse
  Subscribe ::
    Enabled rs bs =>
    ConnId ->
    Command (P Recipient rs rs) (P Broker bs bs) ()
  Unsubscribe ::
    Enabled rs bs =>
    ConnId ->
    Command (P Recipient rs rs) (P Broker bs bs) ()
  SendInvite ::
    Invitation ->
    Command (P Recipient New Pending) (P Sender None New) ()
  ConfirmConn ::
    SenderConnId ->
    Encrypted ->
    Command (P Sender New Confirmed) (P Broker New New) ()
  PushConfirm ::
    ConnId ->
    Message ->
    Command (P Broker New New) (P Recipient Pending Confirmed) ()
  SecureConn ::
    ConnId ->
    PublicKey ->
    Command (P Recipient Confirmed Secured) (P Broker New Secured) ()
  SendMsg ::
    (ss == Confirmed || ss == Secured) ~ True =>
    SenderConnId ->
    Encrypted ->
    Command (P Sender ss Secured) (P Broker Secured Secured) ()
  PushMsg ::
    ConnId ->
    Message ->
    Command (P Broker Secured Secured) (P Recipient Secured Secured) ()
  DeleteMsg ::
    ConnId ->
    MessageId ->
    Command (P Recipient Secured Secured) (P Broker Secured Secured) ()

-- connection type stub for all participants, TODO move from idris
data
  Connection
    (p :: Party)
    (s :: ConnState) :: Type
  where
  Connection :: String -> Connection p s -- TODO replace with real type definition

class Monad m => PartyProtocol m (p :: Party) where
  api ::
    Command from (P p s s') a ->
    Connection p s ->
    ExceptT String m (a, Connection p s')
  action ::
    Command (P p s s') to a ->
    Connection p s ->
    ExceptT String m a ->
    ExceptT String m (Connection p s')

apiStub :: Monad m => Connection p s -> ExceptT String m (a, Connection p s')
apiStub _ = throwE "api not implemented"

actionStub :: Monad m => Connection p s -> ExceptT String m a -> ExceptT String m (Connection p s')
actionStub _ _ = throwE "action not implemented"

data SimplexParty (p :: Party) m a where
  Api ::
    Command from (P p s s') x ->
    Connection p s ->
    SimplexParty p m (Either String (x, Connection p s'))
  Action ::
    Command (P p s s') to x ->
    Connection p s ->
    Either String x ->
    SimplexParty p m (Either String (Connection p s'))

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
    Proxy from ->
    Proxy to ->
    Command (P from (ConnSt from s) fs') (P to (ConnSt to s) ts') a ->
    ProtocolCmd s (ProtoSt s from fs' to ts') a

type Protocol' = XFree ProtocolCmd

type SimplexProtocol = Protocol '[Recipient, Broker, Sender] Command

infix 6 ->:

(->:) ::
  (HasProtoSt s, HasState from fs', HasState to ts') =>
  Proxy from ->
  Proxy to ->
  Command (P from (ConnSt from s) fs') (P to (ConnSt to s) ts') a ->
  Protocol' s (ProtoSt s from fs' to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

start :: String -> Protocol' s s ()
start = xfree . Start
