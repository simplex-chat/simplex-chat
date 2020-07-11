{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Core where

import Data.Kind
import Data.Singletons.TH

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
