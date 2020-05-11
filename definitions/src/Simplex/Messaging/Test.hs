{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Simplex.Messaging.Test where

import ClassyPrelude
import Data.Kind
import Data.Singletons
import Data.Singletons.ShowSing
import Data.Singletons.TH
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import Simplex.Messaging.Types

$(singletons [d|
  data Participant = Recipient | Broker | Sender

  data ConnectionState =  None      -- (all) not available or removed from the broker
                        | New       -- (participants: all) connection created (or received from sender)
                        | Pending   -- (recipient) sent to sender out-of-band
                        | Confirmed -- (recipient) confirmed by sender with the broker
                        | Secured   -- (all) secured with the broker
                        | Disabled  -- (broker, recipient) disabled with the broker by recipient
                        | Drained   -- (broker, recipient) drained (no messages)
    deriving (Show, ShowSing, Eq)

  data ConnSubscription = Subscribed | Idle
  |])

-- broker connection states
type Prf1 t a = Auto (TyPred t) a

data BrokerCS :: ConnectionState -> Type where
  BrkNew      :: BrokerCS New
  BrkSecured  :: BrokerCS Secured
  BrkDisabled :: BrokerCS Disabled
  BrkDrained  :: BrokerCS Drained
  BrkNone     :: BrokerCS None

instance Auto (TyPred BrokerCS) New      where auto = autoTC
instance Auto (TyPred BrokerCS) Secured  where auto = autoTC
instance Auto (TyPred BrokerCS) Disabled where auto = autoTC
instance Auto (TyPred BrokerCS) Drained  where auto = autoTC
instance Auto (TyPred BrokerCS) None     where auto = autoTC

-- sender connection states
data SenderCS :: ConnectionState -> Type where
  SndNew       :: SenderCS New
  SndConfirmed :: SenderCS Confirmed
  SndSecured   :: SenderCS Secured
  SndNone      :: SenderCS None

instance Auto (TyPred SenderCS) New       where auto = autoTC
instance Auto (TyPred SenderCS) Confirmed where auto = autoTC
instance Auto (TyPred SenderCS) Secured   where auto = autoTC
instance Auto (TyPred SenderCS) None      where auto = autoTC

-- allowed participant connection states
data HasState (p :: Participant) (s :: ConnectionState) :: Type where
  RcpHasState :: HasState Recipient s
  BrkHasState :: Prf1 BrokerCS s => HasState Broker s
  SndHasState :: Prf1 SenderCS s => HasState Sender s

class Prf t p s where auto' :: t p s
instance                    Prf HasState Recipient s
  where auto' = RcpHasState
instance Prf1 BrokerCS s => Prf HasState Broker s
  where auto' = BrkHasState
instance Prf1 SenderCS s => Prf HasState Sender s
  where auto' = SndHasState


-- connection type stub for all participants, TODO move from idris
data Connection (p :: Participant)
                (s :: ConnectionState)
                (ss :: ConnSubscription) :: Type where
  Connection :: String -> Connection p s ss  -- TODO replace with real type definition


-- data types for connection states of all participants
infixl 7 <==>, <==|   -- types
infixl 7 :<==>, :<==| -- constructors

data (<==>) (rs :: ConnectionState) (bs :: ConnectionState) :: Type where
  (:<==>) :: (Prf HasState Recipient rs, Prf HasState Broker bs)
          => Sing rs
          -> Sing bs
          -> rs <==> bs

deriving instance Show (rs <==> bs)

data family (<==|) rcpBrk (ss :: ConnectionState) :: Type
data instance (<==|) (rs <==> bs) ss :: Type where
  (:<==|) :: Prf HasState Sender ss
          => rs <==> bs
          -> Sing ss
          -> rs <==> bs <==| ss

deriving instance Show (rs <==> bs <==| ss)

-- type family (<==|) rb ss where
--   (rs <==> bs) <==| (ss :: ConnectionState) = AllConnState rs bs ss

--   recipient <==> broker <==| sender
st2 :: Pending <==> New <==| Confirmed
st2 = SPending :<==> SNew :<==| SConfirmed

-- this must not type check
-- stBad :: 'Pending <==> 'Confirmed <==| 'Confirmed
-- stBad = SPending :<==> SConfirmed :<==| SConfirmed


-- data type with all available protocol commands
$(singletons [d|
  data ProtocolCmd =  CreateConnCmd
                    | SubscribeCmd
                    | Comp    -- result of composing multiple commands
  |])

data Command (cmd :: ProtocolCmd) arg result
             (from :: Participant) (to :: Participant)
             state state'
             (ss :: ConnSubscription) (ss' :: ConnSubscription)
             :: Type where
  CreateConn   :: Prf HasState Sender s
               => Command CreateConnCmd
                    CreateConnRequest CreateConnResponse
                    Recipient Broker
                    (None <==> None <==| s)
                    (New <==> New <==| s)
                    Idle Idle

  Subscribe    :: ( (r /= None && r /= Disabled) ~ True
                  , (b /= None && b /= Disabled) ~ True
                  , Prf HasState Sender s )
               => Command SubscribeCmd
                    () ()
                    Recipient Broker
                    (r <==> b <==| s)
                    (r <==> b <==| s)
                    Idle Subscribed

-- extract connection state of specfic participant
type family PConnSt (p :: Participant) state where
  PConnSt Recipient (r <==> b <==| s) = r
  PConnSt Broker    (r <==> b <==| s) = b
  PConnSt Sender    (r <==> b <==| s) = s


-- Type classes to ensure consistency of types of implementations
-- of participants actions/functions and connection state transitions (types)
-- with types of protocol commands defined above.

-- TODO for some reason this type class is not enforced -
-- it still allows to construct invalid implementations.
-- See comment in Broker.hs
-- As done in Client.hs it type-checks, but it looks ugly
-- class PrfCmd cmd arg res from to s1 s2 s3 s1' s2' s3' ss ss' where
--   command :: Command cmd arg res from to (AllConnState s1 s2 s3) (AllConnState s1' s2' s3') ss ss'
-- instance Prf HasState Sender s
--          => PrfCmd CreateConnCmd
--               CreateConnRequest CreateConnResponse
--               Recipient Broker
--               None None s
--               New New s
--               Idle Idle
--               where
--   command = CreateConn

-- data Cmd cmd arg res from to ss ss' :: Type where
--   Cmd ::  Command cmd arg res
--             from to
--             (AllConnState s1 s2 s3)
--             (AllConnState s1' s2' s3')
--             ss ss'
--       ->  Cmd cmd arg res from to ss ss'

class ProtocolFunc
        (cmd :: ProtocolCmd) arg res
        (from :: Participant) (p :: Participant)
        state state'
        (ss :: ConnSubscription) (ss' :: ConnSubscription)
        where
  pfCommand    :: Command (cmd :: ProtocolCmd) arg res
                  (from :: Participant) (p :: Participant)
                  state state'
                  (ss :: ConnSubscription) (ss' :: ConnSubscription)
  protoFunc    :: Connection p (PConnSt p state) ss
               -> arg
               -> Either String (res, Connection p (PConnSt p state') ss')


-- For some reason PrfCmd type-class requirement is not enforced here:
-- if I change one of the connection states to one for which
-- instance of PrfCmd does not exist (i.e. Command cannot be constructed),
-- it compiles anyway.
-- Creating Command instance here would prevent compilation
-- in case the types are incorrect, as it is done in Client.hs
instance Prf HasState Sender s
         => ProtocolFunc CreateConnCmd
              CreateConnRequest CreateConnResponse
              Recipient Broker
              (None <==> None <==| s)
              (New <==> New <==| s)
              Idle Idle                   -- connection states
              where
  pfCommand = CreateConn
  protoFunc = brkCreateConn

brkCreateConn :: Connection Broker None Idle
            -> CreateConnRequest
            -> Either String (CreateConnResponse, Connection Broker New Idle)
brkCreateConn (Connection str) _ = Right (CreateConnResponse "1" "2", Connection str)
-- TODO stub


-- -- below code should NOT compile, but it does
-- instance ProtocolFunc Broker CreateConnCmd
--          CreateConnRequest CreateConnResponse -- request response
--          None New Idle Idle -- "Secured" should not be allowed here,
--          where                  -- such command does not exist, so no instance of
--   protoFunc _ = \(Connection str) _ -> Right (CreateConnResponse "1" "2", Connection str)    -- PrfCmd exist...? But it compiles.

-- bCreateConn' :: Connection Broker None Idle
--             -> CreateConnRequest
--             -> Either String
--                       (CreateConnResponse, Connection Broker New Idle)
-- bCreateConn' = protoFunc $ CreateConn @None
