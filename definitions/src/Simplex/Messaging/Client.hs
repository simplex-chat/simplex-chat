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
    Command from fs fs' Recipient ps ps' res ->
    Connection Recipient ps ->
    ExceptT String m (res, Connection Recipient ps')
  api (PushConfirm _ _) = apiStub
  api (PushMsg _ _) = apiStub

  action ::
    Command Recipient ps ps' to ts ts' res ->
    Connection Recipient ps ->
    ExceptT String m res ->
    ExceptT String m (Connection Recipient ps')
  action (CreateConn _) = actionStub
  action (Subscribe _) = actionStub
  action (Unsubscribe _) = actionStub
  action (SendInvite _) = actionStub
  action (SecureConn _ _) = actionStub
  action (DeleteMsg _ _) = actionStub

instance Monad m => PartyProtocol m Sender where
  api ::
    Command from fs fs' Sender ps ps' res ->
    Connection Sender ps ->
    ExceptT String m (res, Connection Sender ps')
  api (SendInvite _) = apiStub

  action ::
    Command Sender ps ps' to ts ts' res ->
    Connection Sender ps ->
    ExceptT String m res ->
    ExceptT String m (Connection Sender ps')
  action (ConfirmConn _ _) = actionStub
  action (SendMsg _ _) = actionStub
