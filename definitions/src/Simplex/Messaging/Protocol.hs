{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Simplex.Messaging.Protocol where

import Simplex.Messaging.Types

import ClassyPrelude
import Data.Kind
import Data.Singletons
import Data.Singletons.ShowSing
import Data.Singletons.TH
import Data.Type.Predicate
import Data.Type.Predicate.Auto

$(singletons [d|
  data Participant = Recipient | Broker | Sender

  data ConnectionState =  New       -- (participants: all) connection created (or received from sender)
                        | Pending   -- (recipient) sent to sender out-of-band
                        | Confirmed -- (recipient) confirmed by sender with the broker
                        | Secured   -- (all) secured with the broker
                        | Disabled  -- (broker, recipient) disabled with the broker by recipient
                        | Drained   -- (broker, recipient) drained (no messages)
                        | None      -- (all) not available or removed from the broker
    deriving (Show, ShowSing, Eq)
  |])

-- broker connection states
type Prf1 t a = Auto (TyPred t) a

data BrokerCS :: ConnectionState -> Type where
  BrkNew      :: BrokerCS 'New
  BrkSecured  :: BrokerCS 'Secured
  BrkDisabled :: BrokerCS 'Disabled
  BrkDrained  :: BrokerCS 'Drained
  BrkNone     :: BrokerCS 'None

instance Auto (TyPred BrokerCS) 'New      where auto = autoTC
instance Auto (TyPred BrokerCS) 'Secured  where auto = autoTC
instance Auto (TyPred BrokerCS) 'Disabled where auto = autoTC
instance Auto (TyPred BrokerCS) 'Drained  where auto = autoTC
instance Auto (TyPred BrokerCS) 'None     where auto = autoTC

-- sender connection states
data SenderCS :: ConnectionState -> Type where
  SndNew       :: SenderCS 'New
  SndConfirmed :: SenderCS 'Confirmed
  SndSecured   :: SenderCS 'Secured
  SndNone      :: SenderCS 'None

instance Auto (TyPred SenderCS) 'New       where auto = autoTC
instance Auto (TyPred SenderCS) 'Confirmed where auto = autoTC
instance Auto (TyPred SenderCS) 'Secured   where auto = autoTC
instance Auto (TyPred SenderCS) 'None      where auto = autoTC

-- allowed participant connection states
data HasState (p :: Participant) (s :: ConnectionState) :: Type where
  RcpHasState :: HasState 'Recipient s
  BrkHasState :: Prf1 BrokerCS s => HasState 'Broker s
  SndHasState :: Prf1 SenderCS s => HasState 'Sender s

class Prf t p s where auto' :: t p s
instance                    Prf HasState 'Recipient s
  where auto' = RcpHasState
instance Prf1 BrokerCS s => Prf HasState 'Broker s
  where auto' = BrkHasState
instance Prf1 SenderCS s => Prf HasState 'Sender s
  where auto' = SndHasState

-- established connection states (used by broker and recipient)
data EstablishedState (s :: ConnectionState) :: Type where
  ESecured  :: EstablishedState 'Secured
  EDisabled :: EstablishedState 'Disabled
  EDrained  :: EstablishedState 'Drained


-- data types for connection states of all participants
infixl 7 <==>, <==|   -- types
infixl 7 :<==>, :<==| -- constructors

data (<==>) (rs :: ConnectionState) (bs :: ConnectionState) :: Type where
  (:<==>) :: (Prf HasState 'Recipient rs, Prf HasState 'Broker bs)
          => Sing rs
          -> Sing bs
          -> rs <==> bs

deriving instance Show (rs <==> bs)

data AllConnState (rs :: ConnectionState)
                  (bs :: ConnectionState)
                  (ss :: ConnectionState) :: Type where
  (:<==|) :: Prf HasState 'Sender ss
          => rs <==> bs
          -> Sing ss
          -> AllConnState rs bs ss

deriving instance Show (AllConnState rs bs ss)

type family (<==|) rb ss where
  (rs <==> bs) <==| (ss :: ConnectionState) = AllConnState rs bs ss

--   recipient <==> broker <==| sender
st2 :: 'Pending <==> 'New <==| 'Confirmed
st2 = SPending :<==> SNew :<==| SConfirmed

-- this must not type check
-- stBad :: 'Pending <==> 'Confirmed <==| 'Confirmed
-- stBad = SPending :<==> SConfirmed :<==| SConfirmed


-- data Command (res :: Type)
--              (ps :: (Participant, Participant))
--              (AllConnState r b s)
--              (AllConnState r b s) :: Type where

--   CreateConn  :: Prf HasState 'Sender s
--               => NewConnectionReqBody
--               -> Command NewConnectionResBody
--                     ('Recipient, 'Broker)
--                     ('None <==> 'None <==| s)
--                     ('New <==> 'New  <==| s)
