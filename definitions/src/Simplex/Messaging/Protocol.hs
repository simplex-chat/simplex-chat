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
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Protocol where

import Data.Kind
import Data.Singletons
import Data.Singletons.TH
import Prf
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

$( proofs
     [d|
       -- allowed participant connection states
       data HasState (p :: Party) (s :: ConnState) :: Type where
         HSRcp :: HasState Recipient s
         HSBrkNone :: HasState Broker None
         HSBrkNew :: HasState Broker New
         HSBrkSecured :: HasState Broker Secured
         HSBrkDisabled :: HasState Broker Disabled
         HSSdrNone :: HasState Sender None
         HSSdrPending :: HasState Sender Pending
         HSSdrConfirmed :: HasState Sender Confirmed
         HSSdrSecured :: HasState Sender Secured
       |]
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
    CreateConnRequest ->
    Command Recipient None New Broker None New CreateConnResponse
  Subscribe ::
    (rs /= None && bs /= None) ~ True =>
    ConnectionId ->
    Command Recipient rs rs Broker bs bs ()
  SendInvite ::
    String -> -- invitation - TODO
    Command Recipient New Pending Sender None Pending ()
  ConfirmConn ::
    ConnectionId ->
    SecureConnRequest ->
    Command Sender Pending Confirmed Broker New New ()

class Monad m => PartyApi m (p :: Party) where
  process :: Command from fs fs' p ps ps' res -> m res

type AllowedState from fs fs' to ts ts' =
  ( Prf (HasState from fs),
    Prf (HasState from fs'),
    Prf (HasState to ts),
    Prf (HasState to ts')
  )

data PartiesAndCommand from fs fs' to ts ts' res :: Type where
  PartiesAndCommand ::
    AllowedState from fs fs' to ts ts' =>
    Sing from ->
    Sing to ->
    Command from fs fs' to ts ts' res ->
    PartiesAndCommand from fs fs' to ts ts' res

infix 6 -->

(-->) ::
  AllowedState from fs fs' to ts ts' =>
  Sing from ->
  Sing to ->
  ( Command from fs fs' to ts ts' res ->
    PartiesAndCommand from fs fs' to ts ts' res
  )
(-->) = PartiesAndCommand

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

data
  Protocol
    (rs :: ConnState)
    (bs :: ConnState)
    (ss :: ConnState)
    (rs' :: ConnState)
    (bs' :: ConnState)
    (ss' :: ConnState)
    (res :: Type) :: Type where
  Start :: Protocol rs bs ss rs' bs' ss' ()
  (:>>) ::
    Protocol rs bs ss rs' bs' ss' res ->
    PartiesAndCommand from (CConnSt from rs' bs' ss') fs' to (CConnSt to rs' bs' ss') ts' res' ->
    Protocol rs bs ss
      (PConnSt Recipient rs' from fs' to ts')
      (PConnSt Broker bs' from fs' to ts')
      (PConnSt Sender ss' from fs' to ts')
      res'
