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
    Command from fs fs' Broker ps ps' res ->
    Connection Broker ps ->
    ExceptT String m (res, Connection Broker ps')
  api (CreateConn _) = apiStub
  api (Subscribe _) = apiStub
  api (Unsubscribe _) = apiStub
  api (ConfirmConn _ _) = apiStub
  api (SecureConn _ _) = apiStub
  api (SendMsg _ _) = apiStub
  api (DeleteMsg _ _) = apiStub

  action ::
    Command Broker ps ps' to ts ts' res ->
    Connection Broker ps ->
    ExceptT String m res ->
    ExceptT String m (Connection Broker ps')
  action (PushConfirm _ _) = actionStub
  action (PushMsg _ _) = actionStub
