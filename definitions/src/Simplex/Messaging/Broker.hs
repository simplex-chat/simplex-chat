{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Broker where

import Control.Monad.Trans.Except
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
