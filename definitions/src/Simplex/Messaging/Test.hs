{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE PolyKinds                      #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Simplex.Messaging.Test where

import ClassyPrelude

import Data.Kind
import Data.Singletons()
import Data.Singletons.TH
import Data.Type.Predicate
import Data.Type.Predicate.Auto

$(singletons [d|
  data Participant = Recipient | Broker | Sender

  data ConnectionState =  New       -- (participants: all) connection created (or received from sender)
                        | Pending   -- (recipient) sent to sender out-of-band
                        | Confirmed -- (recipient) confirmed by sender with the broker
                        | Secured   -- (all) secured with the broker
  |])

-- broker connection states
data BrokerCS :: ConnectionState -> Type where
  BrkNew      :: BrokerCS 'New
  BrkSecured  :: BrokerCS 'Secured

instance Auto (TyPred BrokerCS) 'New where auto = autoTC
instance Auto (TyPred BrokerCS) 'Secured where auto = autoTC

data RBState (rs :: ConnectionState) (bs :: ConnectionState) :: Type where
  RBState :: Auto (TyPred BrokerCS) bs
          => Sing rs -> Sing bs -> RBState rs bs

data Box a = Num a => Box a

goodBoxSample :: Box Int
goodBoxSample = Box 1

-- badBoxSample :: Box String
-- badBox = Box "foo"

goodSt :: RBState 'New 'New
goodSt = RBState SNew SNew

-- badSt :: RBState 'Pending 'Pending
-- badSt = RBState SPending SPending
