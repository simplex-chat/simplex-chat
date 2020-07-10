{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Client where

import Control.Monad.Trans.Except
import Simplex.Messaging.Protocol

instance Monad m => PartyProtocol m Recipient where
  api ::
    Command from '(Recipient, s, s') a ->
    Connection Recipient s ->
    ExceptT String m (a, Connection Recipient s')
  api (PushConfirm _ _) = apiStub
  api (PushMsg _ _) = apiStub

  action ::
    Command '(Recipient, s, s') to a ->
    Connection Recipient s ->
    ExceptT String m a ->
    ExceptT String m (Connection Recipient s')
  action (CreateConn _) = actionStub
  action (Subscribe _) = actionStub
  action (Unsubscribe _) = actionStub
  action (SendInvite _) = actionStub
  action (SecureConn _ _) = actionStub
  action (DeleteMsg _ _) = actionStub

instance Monad m => PartyProtocol m Sender where
  api ::
    Command from '(Sender, s, s') a ->
    Connection Sender s ->
    ExceptT String m (a, Connection Sender s')
  api (SendInvite _) = apiStub

  action ::
    Command '(Sender, s, s') to a ->
    Connection Sender s ->
    ExceptT String m a ->
    ExceptT String m (Connection Sender s')
  action (ConfirmConn _ _) = actionStub
  action (SendMsg _ _) = actionStub
