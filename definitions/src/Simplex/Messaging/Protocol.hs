{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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

module Simplex.Messaging.Protocol where

import ClassyPrelude
import Data.Kind
import Data.Singletons
import Data.Singletons.ShowSing
import Data.Singletons.TH
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import GHC.TypeLits
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

  data ConnectionSubscription = Subscribed | Idle
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

-- established connection states (used by broker and recipient)
data EstablishedState (s :: ConnectionState) :: Type where
  ESecured  :: EstablishedState Secured
  EDisabled :: EstablishedState Disabled
  EDrained  :: EstablishedState Drained


-- connection type stub for all participants, TODO move from idris
data Connection (p :: Participant)
                (s :: ConnectionState)
                (ss :: ConnectionSubscription) :: Type where
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

data AllConnState (rs :: ConnectionState)
                  (bs :: ConnectionState)
                  (ss :: ConnectionState) :: Type where
  (:<==|) :: Prf HasState Sender ss
          => rs <==> bs
          -> Sing ss
          -> AllConnState rs bs ss

deriving instance Show (AllConnState rs bs ss)

type family (<==|) rb ss where
  (rs <==> bs) <==| (ss :: ConnectionState) = AllConnState rs bs ss

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
                    | UnsubscribeCmd
                    | SendInviteCmd
                    | ConfirmConnCmd
                    | PushConfirmCmd
                    | SecureConnCmd
                    | SendWelcomeCmd
                    | SendMsgCmd
                    | PushMsgCmd
                    | DeleteMsgCmd
                    | Comp    -- result of composing multiple commands
  |])

infixl 4 :>>, :>>=

data Command (cmd :: ProtocolCmd) arg result
             (from :: Participant) (to :: Participant)
             state state'
             (ss :: ConnectionSubscription) (ss' :: ConnectionSubscription)
             (messages :: Nat) (messages' :: Nat)
             :: Type where
  CreateConn   :: Prf HasState Sender s
               => Command CreateConnCmd
                    CreateConnRequest CreateConnResponse
                    Recipient Broker
                    (None <==> None <==| s)
                    (New <==> New  <==| s)
                    Idle Idle 0 0

  Subscribe    :: ( (r /= None && r /= Disabled) ~ True
                  , (b /= None && b /= Disabled) ~ True
                  , Prf HasState Sender s )
               => Command SubscribeCmd
                    () ()
                    Recipient Broker
                    (r <==> b <==| s)
                    (r <==> b <==| s)
                    Idle Subscribed n n

  Unsubscribe  :: ( (r /= None && r /= Disabled) ~ True
                  , (b /= None && b /= Disabled) ~ True
                  , Prf HasState Sender s )
               => Command UnsubscribeCmd
                    () ()
                    Recipient Broker
                    (r <==> b <==| s)
                    (r <==> b <==| s)
                    Subscribed Idle n n

  SendInvite   :: Prf HasState Broker s
               => Command SendInviteCmd
                    String () -- invitation - TODO
                    Recipient Sender
                    (New <==> s <==| None)
                    (Pending <==> s <==| New)
                    ss ss n n

  ConfirmConn  :: Prf HasState Recipient s
               => Command ConfirmConnCmd
                    SecureConnRequest ()
                    Sender Broker
                    (s <==> New <==| New)
                    (s <==> New <==| Confirmed)
                    ss ss n (n+1)

  PushConfirm  :: Prf HasState Sender s
               => Command PushConfirmCmd
                    SecureConnRequest ()
                    Broker Recipient
                    (Pending <==> New <==| s)
                    (Confirmed <==> New <==| s)
                    Subscribed Subscribed n n

  SecureConn   :: Prf HasState Sender s
               => Command SecureConnCmd
                    SecureConnRequest ()
                    Recipient Broker
                    (Confirmed <==> New <==| s)
                    (Secured <==> Secured <==| s)
                    ss ss n n

  SendWelcome  :: Prf HasState Recipient s
               => Command SendWelcomeCmd
                    () ()
                    Sender Broker
                    (s <==> Secured <==| Confirmed)
                    (s <==> Secured <==| Secured)
                    ss ss n (n+1)

  SendMsg      :: Prf HasState Recipient s
               => Command SendMsgCmd
                    SendMessageRequest ()
                    Sender Broker
                    (s <==> Secured <==| Secured)
                    (s <==> Secured <==| Secured)
                    ss ss n (n+1)

  PushMsg      :: Prf HasState 'Sender s
               => Command PushMsgCmd
                    MessagesResponse () -- TODO, has to be a single message
                    Broker Recipient
                    (Secured <==> Secured <==| s)
                    (Secured <==> Secured <==| s)
                    Subscribed Subscribed n n

  DeleteMsg    :: Prf HasState Sender s
               => Command DeleteMsgCmd
                    () ()   -- TODO needs message ID parameter
                    Recipient Broker
                    (Secured <==> Secured <==| s)
                    (Secured <==> Secured <==| s)
                    ss ss n (n-1)

-- this command has to be removed - this is here to allow proof
-- that any command can be constructed
  AnyCmd       :: Command cmd a b from to s1 s2 ss1 ss2 n1 n2

  Return       :: b -> Command Comp a b from to state state ss ss n n

  (:>>=)       :: Command cmd1 a b from1 to1 s1 s2 ss1 ss2 n1 n2
               -> (b -> Command cmd2 b c from2 to2 s2 s3 ss2 ss3 n2 n3)
               -> Command Comp a c from1 to2 s1 s3 ss1 ss3 n1 n3

  (:>>)        :: Command cmd1 a b from1 to1 s1 s2 ss1 ss2 n1 n2
               -> Command cmd2 c d from2 to2 s2 s3 ss2 ss3 n2 n3
               -> Command Comp a d from1 to2 s1 s3 ss1 ss3 n1 n3

  Fail         :: String
               -> Command Comp a String from to state (None <==> None <==| None) ss ss n n

-- redifine Monad operators to compose commands
-- using `do` notation with RebindableSyntax extension
(>>=) :: Command cmd1 a b from1 to1 s1 s2 ss1 ss2 n1 n2
      -> (b -> Command cmd2 b c from2 to2 s2 s3 ss2 ss3 n2 n3)
      -> Command Comp a c from1 to2 s1 s3 ss1 ss3 n1 n3
(>>=) = (:>>=)

(>>)  :: Command cmd1 a b from1 to1 s1 s2 ss1 ss2 n1 n2
      -> Command cmd2 c d from2 to2 s2 s3 ss2 ss3 n2 n3
      -> Command Comp a d from1 to2 s1 s3 ss1 ss3 n1 n3
(>>) = (:>>)

fail :: String -> Command Comp a String from to state (None <==> None <==| None) ss ss n n
fail = Fail

-- show and validate expexcted command participants
infix 6 -->
(-->) :: Sing from -> Sing to
      -> ( Command cmd a b from to s s' ss ss' n n'
           -> Command cmd a b from to s s' ss ss' n n' )
(-->) _ _ = id


-- extract connection state of specfic participant
type family PConnSt (p :: Participant) state where
  PConnSt Recipient (AllConnState r b s) = r
  PConnSt Broker    (AllConnState r b s) = b
  PConnSt Sender    (AllConnState r b s) = s


-- Type classes to ensure consistency of types of implementations
-- of participants actions/functions and connection state transitions (types)
-- with types of protocol commands defined above.

-- TODO for some reason this type class is not enforced -
-- it still allows to construct invalid implementations.
-- See comment in Broker.hs
-- As done in Client.hs it type-checks, but it looks ugly
class PrfCmd cmd arg res from to s s' ss ss' n n' where
  command :: Command cmd arg res from to s s' ss ss' n n'
instance Prf HasState Sender s
         => PrfCmd CreateConnCmd
              CreateConnRequest CreateConnResponse
              Recipient Broker
              (AllConnState None None s)
              (AllConnState New New s)
              Idle Idle 0 0
              where
  command = CreateConn


class ProtocolFunc (p :: Participant) (cmd :: ProtocolCmd) arg res ps ps' ss ss' where
  protoFunc    :: ( PrfCmd cmd arg res from p s s' ss ss' n n'
                  , PConnSt p s ~ ps
                  , PConnSt p s' ~ ps' )
               => Command cmd arg res
                          from p  -- participant receives this command
                          s s' ss ss' n n'
               -> ( Connection p ps ss
                    -> arg
                    -> Either String (res, Connection p ps' ss') )

class ProtocolAction (p :: Participant) (cmd :: ProtocolCmd) arg res ps ps' ss ss' where
  protoAction  :: ( PrfCmd cmd arg res p to s s' ss ss' n n'
                  , PConnSt p s ~ ps
                  , PConnSt p s' ~ ps' )
               => Command cmd arg res
                          p to    -- participant sends this command
                          s s' ss ss' n n'
               -> ( Connection p ps ss
                    -> arg
                    -> Either String res
                    -> Connection p ps' ss' )
