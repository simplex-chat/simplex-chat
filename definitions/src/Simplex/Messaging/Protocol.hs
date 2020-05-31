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
    (res :: Type) :: Type where
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
    (s :: ConnState) :: Type where
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

type AllowedStates from fs fs' to ts ts' =
  ( HasState from fs,
    HasState from fs',
    HasState to ts,
    HasState to ts'
  )

infix 6 :->

data PartiesCommand from fs fs' to ts ts' res :: Type where
  (:->) ::
    AllowedStates from fs fs' to ts ts' =>
    Sing from ->
    Sing to ->
    Command from fs fs' to ts ts' res ->
    PartiesCommand from fs fs' to ts ts' res

$( promote
     [d|
       cConnSt :: Party -> ConnState -> ConnState -> ConnState -> ConnState
       cConnSt p rs bs ss = case p of
         Recipient -> rs
         Broker -> bs
         Sender -> ss

       pConnSt :: Party -> ConnState -> Party -> ConnState -> Party -> ConnState -> ConnState
       pConnSt p ps from fs' to ts'
         | p == from = fs'
         | p == to = ts'
         | otherwise = ps
       |]
 )

infixl 4 :>>

data
  Protocol
    (rs :: ConnState)
    (bs :: ConnState)
    (ss :: ConnState)
    (rs' :: ConnState)
    (bs' :: ConnState)
    (ss' :: ConnState)
    (res :: Type) :: Type where
  Start :: String -> Protocol rs bs ss rs' bs' ss' ()
  (:>>) ::
    Protocol rs bs ss rs' bs' ss' a ->
    PartiesCommand from (CConnSt from rs' bs' ss') fs' to (CConnSt to rs' bs' ss') ts' b ->
    Protocol rs bs ss
      (PConnSt Recipient rs' from fs' to ts')
      (PConnSt Broker bs' from fs' to ts')
      (PConnSt Sender ss' from fs' to ts')
      b
  (:>>=) ::
    Protocol rs bs ss rs' bs' ss' a ->
    (a -> PartiesCommand from (CConnSt from rs' bs' ss') fs' to (CConnSt to rs' bs' ss') ts' b) ->
    Protocol rs bs ss
      (PConnSt Recipient rs' from fs' to ts')
      (PConnSt Broker bs' from fs' to ts')
      (PConnSt Sender ss' from fs' to ts')
      b

infix 5 |$

(|$) :: (a -> b) -> a -> b
f |$ x = f x
