{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Broker where

import Control.Monad.Trans.Except
import Polysemy.Internal
import Simplex.Messaging.Protocol

instance Monad m => PartyProtocol m Broker where
  api ::
    Command from '(Broker, s, s') a ->
    Connection Broker s ->
    ExceptT String m (a, Connection Broker s')
  api (CreateConn _) = apiStub
  api (Subscribe _) = apiStub
  api (Unsubscribe _) = apiStub
  api (ConfirmConn _ _) = apiStub
  api (SecureConn _ _) = apiStub
  api (SendMsg _ _) = apiStub
  api (DeleteMsg _ _) = apiStub

  action ::
    Command '(Broker, s, s') to a ->
    Connection Broker s ->
    ExceptT String m a ->
    ExceptT String m (Connection Broker s')
  action (PushConfirm _ _) = actionStub
  action (PushMsg _ _) = actionStub

type SimplexBroker = SimplexParty Broker

api' ::
  Member SimplexBroker r =>
  Command from '(Broker, s, s') a ->
  Connection Broker s ->
  Sem r (Either String (a, Connection Broker s'))
api' cmd conn = send $ Api cmd conn

action' ::
  Member SimplexBroker r =>
  Command '(Broker, s, s') to a ->
  Connection Broker s ->
  Either String a ->
  Sem r (Either String (Connection Broker s'))
action' cmd conn res = send $ Action cmd conn res
