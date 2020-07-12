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

type SimplexProtocolCmd = ProtocolCmd SimplexCommand '[Recipient, Broker, Sender]

data SimplexCommand :: Command Party ConnState where
  CreateConn ::
    PublicKey ->
    SimplexCommand
      '(Recipient, None, New)
      '(Broker, None, New)
      CreateConnResponse
  Subscribe ::
    Enabled rs bs =>
    ConnId ->
    SimplexCommand
      '(Recipient, rs, rs)
      '(Broker, bs, bs)
      ()
  Unsubscribe ::
    Enabled rs bs =>
    ConnId ->
    SimplexCommand
      '(Recipient, rs, rs)
      '(Broker, bs, bs)
      ()
  SendInvite ::
    Invitation ->
    SimplexCommand
      '(Recipient, New, Pending)
      '(Sender, None, New)
      ()
  ConfirmConn ::
    SenderConnId ->
    Encrypted ->
    SimplexCommand
      '(Sender, New, Confirmed)
      '(Broker, New, New)
      ()
  PushConfirm ::
    ConnId ->
    Message ->
    SimplexCommand
      '(Broker, New, New)
      '(Recipient, Pending, Confirmed)
      ()
  SecureConn ::
    ConnId ->
    PublicKey ->
    SimplexCommand
      '(Recipient, Confirmed, Secured)
      '(Broker, New, Secured)
      ()
  SendMsg ::
    (ss == Confirmed || ss == Secured) ~ True =>
    SenderConnId ->
    Encrypted ->
    SimplexCommand
      '(Sender, ss, Secured)
      '(Broker, Secured, Secured)
      ()
  PushMsg ::
    ConnId ->
    Message ->
    SimplexCommand
      '(Broker, Secured, Secured)
      '(Recipient, Secured, Secured)
      ()
  DeleteMsg ::
    ConnId ->
    MessageId ->
    SimplexCommand
      '(Recipient, Secured, Secured)
      '(Broker, Secured, Secured)
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
    SimplexCommand from '(p, s, s') a ->
    Connection p s ->
    ExceptT String m (a, Connection p s')
  action ::
    SimplexCommand '(p, s, s') to a ->
    Connection p s ->
    ExceptT String m a ->
    ExceptT String m (Connection p s')

apiStub :: Monad m => Connection p s -> ExceptT String m (a, Connection p s')
apiStub _ = throwE "api not implemented"

actionStub :: Monad m => Connection p s -> ExceptT String m a -> ExceptT String m (Connection p s')
actionStub _ _ = throwE "action not implemented"

data SimplexParty (p :: Party) m a where
  Api ::
    SimplexCommand from '(p, s, s') x ->
    Connection p s ->
    SimplexParty p m (Either String (x, Connection p s'))
  Action ::
    SimplexCommand '(p, s, s') to x ->
    Connection p s ->
    Either String x ->
    SimplexParty p m (Either String (Connection p s'))
