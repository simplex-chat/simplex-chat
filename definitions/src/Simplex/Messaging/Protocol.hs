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

module Simplex.Messaging.Protocol
  ( module Simplex.Messaging.Core,
    module Simplex.Messaging.Protocol,
  )
where

import Control.Monad.Trans.Except
import Control.Protocol
import Data.Kind
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Simplex.Messaging.Core
import Simplex.Messaging.Types

type SimplexProtocol = Protocol SimplexCommand '[Recipient, Broker, Sender]

data SimplexCommand :: Command Party where
  CreateConn ::
    PublicKey ->
    SimplexCommand
      (Cmd Recipient None New)
      (Cmd Broker None New)
      CreateConnResponse
  Subscribe ::
    Enabled rs bs =>
    ConnId ->
    SimplexCommand
      (Cmd Recipient rs rs)
      (Cmd Broker bs bs)
      ()
  Unsubscribe ::
    Enabled rs bs =>
    ConnId ->
    SimplexCommand
      (Cmd Recipient rs rs)
      (Cmd Broker bs bs)
      ()
  SendInvite ::
    Invitation ->
    SimplexCommand
      (Cmd Recipient New Pending)
      (Cmd Sender None New)
      ()
  ConfirmConn ::
    SenderConnId ->
    Encrypted ->
    SimplexCommand
      (Cmd Sender New Confirmed)
      (Cmd Broker New New)
      ()
  PushConfirm ::
    ConnId ->
    Message ->
    SimplexCommand
      (Cmd Broker New New)
      (Cmd Recipient Pending Confirmed)
      ()
  SecureConn ::
    ConnId ->
    PublicKey ->
    SimplexCommand
      (Cmd Recipient Confirmed Secured)
      (Cmd Broker New Secured)
      ()
  SendMsg ::
    (ss == Confirmed || ss == Secured) ~ True =>
    SenderConnId ->
    Encrypted ->
    SimplexCommand
      (Cmd Sender ss Secured)
      (Cmd Broker Secured Secured)
      ()
  PushMsg ::
    ConnId ->
    Message ->
    SimplexCommand
      (Cmd Broker Secured Secured)
      (Cmd Recipient Secured Secured)
      ()
  DeleteMsg ::
    ConnId ->
    MessageId ->
    SimplexCommand
      (Cmd Recipient Secured Secured)
      (Cmd Broker Secured Secured)
      ()

-- connection type stub for all participants, TODO move from idris
data
  Connection
    (p :: Party)
    (s :: ConnState) :: Type
  where
  Connection :: String -> Connection p s -- TODO replace with real type definition

class Monad m => PartyProtocol m (p :: Party) where
  api ::
    SimplexCommand from (Cmd p s s') a ->
    Connection p s ->
    ExceptT String m (a, Connection p s')
  action ::
    SimplexCommand (Cmd p s s') to a ->
    Connection p s ->
    ExceptT String m a ->
    ExceptT String m (Connection p s')

apiStub :: Monad m => Connection p s -> ExceptT String m (a, Connection p s')
apiStub _ = throwE "api not implemented"

actionStub :: Monad m => Connection p s -> ExceptT String m a -> ExceptT String m (Connection p s')
actionStub _ _ = throwE "action not implemented"

data SimplexParty (p :: Party) m a where
  Api ::
    SimplexCommand from (Cmd p s s') x ->
    Connection p s ->
    SimplexParty p m (Either String (x, Connection p s'))
  Action ::
    SimplexCommand (Cmd p s s') to x ->
    Connection p s ->
    Either String x ->
    SimplexParty p m (Either String (Connection p s'))
