{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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

module Simplex.Messaging.Protocol where

import ClassyPrelude
import Control.Monad.Fail
import Data.Kind
import Data.Singletons
import Data.Singletons.ShowSing
import Data.Singletons.TH
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import GHC.TypeLits
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import Predicate
import Simplex.Messaging.Types
import Text.Show.Pretty (ppShow)

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

type Prf1 t a = Auto (TyPred t) a

$(predicate [d|
-- broker connection states
  data BrokerCS :: ConnectionState -> Type where
    BrkNew      :: BrokerCS New
    BrkSecured  :: BrokerCS Secured
    BrkDisabled :: BrokerCS Disabled
    BrkDrained  :: BrokerCS Drained
    BrkNone     :: BrokerCS None

-- sender connection states
  data SenderCS :: ConnectionState -> Type where
    SndNew       :: SenderCS New
    SndConfirmed :: SenderCS Confirmed
    SndSecured   :: SenderCS Secured
    SndNone      :: SenderCS None
  |])


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

data family (<==|) rb (ss :: ConnectionState) :: Type
data instance (<==|) (rs <==> bs) ss :: Type where
  (:<==|) :: Prf HasState Sender ss
          => rs <==> bs
          -> Sing ss
          -> rs <==> bs <==| ss

deriving instance Show (rs <==> bs <==| ss)

-- example of states of connection for all participants
--   recipient <==> broker <==| sender
st2 :: Pending <==> New <==| Confirmed
st2 = SPending :<==> SNew :<==| SConfirmed

-- this must not type check
-- stBad :: 'Pending <==> 'Confirmed <==| 'Confirmed
-- stBad = SPending :<==> SConfirmed :<==| SConfirmed


infixl 4 :>>, :>>=

data Command arg res
             (from :: Participant) (to :: Participant)
             state state'
             (ss :: ConnSubscription) (ss' :: ConnSubscription)
             (messages :: Nat) (messages' :: Nat)
             :: Type where

  CreateConn   :: Prf HasState Sender s
               => Command
                    CreateConnRequest CreateConnResponse
                    Recipient Broker
                    (None <==> None <==| s)
                    (New <==> New  <==| s)
                    Idle Idle 0 0

  Subscribe    :: ( (r /= None && r /= Disabled) ~ True
                  , (b /= None && b /= Disabled) ~ True
                  , Prf HasState Sender s )
               => Command
                    () ()
                    Recipient Broker
                    (r <==> b <==| s)
                    (r <==> b <==| s)
                    Idle Subscribed n n

  Unsubscribe  :: ( (r /= None && r /= Disabled) ~ True
                  , (b /= None && b /= Disabled) ~ True
                  , Prf HasState Sender s )
               => Command
                    () ()
                    Recipient Broker
                    (r <==> b <==| s)
                    (r <==> b <==| s)
                    Subscribed Idle n n

  SendInvite   :: Prf HasState Broker s
               => Command
                    String () -- invitation - TODO
                    Recipient Sender
                    (New <==> s <==| None)
                    (Pending <==> s <==| New)
                    ss ss n n

  ConfirmConn  :: Prf HasState Recipient s
               => Command
                    SecureConnRequest ()
                    Sender Broker
                    (s <==> New <==| New)
                    (s <==> New <==| Confirmed)
                    ss ss n (n+1)

  PushConfirm  :: Prf HasState Sender s
               => Command
                    SecureConnRequest ()
                    Broker Recipient
                    (Pending <==> New <==| s)
                    (Confirmed <==> New <==| s)
                    Subscribed Subscribed n n

  SecureConn   :: Prf HasState Sender s
               => Command
                    SecureConnRequest ()
                    Recipient Broker
                    (Confirmed <==> New <==| s)
                    (Secured <==> Secured <==| s)
                    ss ss n n

  SendWelcome  :: Prf HasState Recipient s
               => Command
                    () ()
                    Sender Broker
                    (s <==> Secured <==| Confirmed)
                    (s <==> Secured <==| Secured)
                    ss ss n (n+1)

  SendMsg      :: Prf HasState Recipient s
               => Command
                    SendMessageRequest ()
                    Sender Broker
                    (s <==> Secured <==| Secured)
                    (s <==> Secured <==| Secured)
                    ss ss n (n+1)

  PushMsg      :: Prf HasState 'Sender s
               => Command
                    MessagesResponse () -- TODO, has to be a single message
                    Broker Recipient
                    (Secured <==> Secured <==| s)
                    (Secured <==> Secured <==| s)
                    Subscribed Subscribed n n

  DeleteMsg    :: Prf HasState Sender s
               => Command
                    () ()   -- TODO needs message ID parameter
                    Recipient Broker
                    (Secured <==> Secured <==| s)
                    (Secured <==> Secured <==| s)
                    ss ss n (n-1)

  Return       :: b -> Command a b from to state state ss ss n n

  (:>>=)       :: Command a b from1 to1 s1 s2 ss1 ss2 n1 n2
               -> (b -> Command b c from2 to2 s2 s3 ss2 ss3 n2 n3)
               -> Command a c from1 to2 s1 s3 ss1 ss3 n1 n3

  (:>>)        :: Command a b from1 to1 s1 s2 ss1 ss2 n1 n2
               -> Command c d from2 to2 s2 s3 ss2 ss3 n2 n3
               -> Command a d from1 to2 s1 s3 ss1 ss3 n1 n3

  Fail         :: String
               -> Command a String from to state (None <==> None <==| None) ss ss n n


-- show and validate expexcted command participants
infix 6 -->
(-->) :: Sing from -> Sing to
      -> ( Command a b from to s s' ss ss' n n'
           -> Command a b from to s s' ss ss' n n' )
(-->) _ _ = id


-- extract connection state of specfic participant
type family PConnSt (p :: Participant) state where
  PConnSt Recipient (r <==> b <==| s) = r
  PConnSt Broker    (r <==> b <==| s) = b
  PConnSt Sender    (r <==> b <==| s) = s


-- Type classes to ensure consistency of types of implementations
-- of participants commands/actions and connection state transitions (types)
-- with types of protocol commands defined above.
-- One participant's command is another participant's action.

class ProtocolCommand
        arg res
        (from :: Participant) (to :: Participant)
        state state'
        (ss :: ConnSubscription) (ss' :: ConnSubscription)
        (messages :: Nat) (messages' :: Nat)
  where
    command  :: Command arg res from to state state' ss ss' messages messages'
    protoCmd :: Connection to (PConnSt to state) ss
             -> arg
             -> Either String (res, Connection to (PConnSt to state') ss')

protoCmdStub :: Connection to ps ss
             -> arg
             -> Either String (res, Connection to ps' ss')
protoCmdStub _ _ = Left "Command not implemented"


class ProtocolAction
        arg res
        (from :: Participant) (to :: Participant)
        state state'
        (ss :: ConnSubscription) (ss' :: ConnSubscription)
        (messages :: Nat) (messages' :: Nat)
  where
    action      :: Command arg res from to state state' ss ss' messages messages'
    protoAction :: Connection from (PConnSt from state) ss
                -> arg
                -> Either String res
                -> Either String (Connection from (PConnSt from state') ss')

protoActionStub :: Connection from ps ss
                -> arg
                -> Either String res
                -> Either String (Connection from ps' ss')
protoActionStub _ _ _ = Left "Action not implemented"


-- Splice to generate typeclasse instances for ProtocolCommand
-- and ProtocolAction classes from implementation definition syntax.
--
-- Given this declaration:
--
-- $(protocol Recipient [d|
--     rcPushConfirm = PushConfirm <-- Broker
--     raCreateConn  = CreateConn  --> Broker
--   |])
--
-- two instance declarations will be generated:
--   - for ProtocolCommand with `protoCmd = rcPushConfirm` and `command = PushConfirm`
--   - for ProtocolAction class with `protoAction = raCreateConn` and `action = CreateConn`
-- matching PushConfirm and CreateConn constructors types
-- Type definitions for functions rcPushConfirm and raCreateConn have to be written manually -
-- they have to be consistent with the typeclass instances

data ProtocolOpeartion = POCommand | POAction | POUndefined

protocol :: Participant -> Q [Dec] -> Q [Dec]
protocol me ds = ds >>= mapM mkProtocolInstance
  where
    cmdCls = ("ProtocolCommand", "command", "protoCmd")
    actCls = ("ProtocolAction", "action", "protoAction")

    mkProtocolInstance :: Dec -> Q Dec
    mkProtocolInstance d@(ValD
                           (VarP fName)
                           (NormalB
                             (InfixE
                               (Just (ConE cmdCon))
                               op
                               (Just (ConE other))))
                           []) =
      let inst = instanceDecl d fName cmdCon other in
      case getOperation op of
        POUndefined -> failSyntax d
        POCommand -> inst cmdCls
        POAction  -> inst actCls
    mkProtocolInstance d = failSyntax d

    instanceDecl :: Dec
                 -> Name -> Name -> Name
                 -> (String, String, String)
                 -> Q Dec
    instanceDecl d fName cmdCon other (cls, mthd, protoMthd) = do
      info <- reify cmdCon
      case info of
        DataConI _ (ForallT _ ctxs ty) _ ->
          return $
            InstanceD Nothing ctxs (changeTyCon ty cls)
              [ mkMethod mthd (ConE cmdCon)
              , mkMethod protoMthd (VarE . mkName $ nameBase fName) ]
        _ -> failSyntax d

    mkMethod :: String -> Exp -> Dec
    mkMethod name d = ValD (VarP (mkName name)) (NormalB d) []

    changeTyCon :: TH.Type -> String -> TH.Type
    changeTyCon (AppT t1 t2) n = AppT (changeTyCon t1 n) t2
    changeTyCon (ConT _) n = ConT (mkName n)
    changeTyCon t _ = t

    getOperation :: Exp -> ProtocolOpeartion
    getOperation = \case
      VarE name        -> getOp name
      UnboundVarE name -> getOp name
      _ -> POUndefined
      where
        getOp n = case nameBase n of
          "<--" -> POCommand
          "-->" -> POAction
          _ -> POUndefined

    failSyntax :: Dec -> Q Dec
    failSyntax d = fail $ "invalid protocol syntax: " ++ pprint d


introspect :: Name -> Q Exp
introspect n = reify n >>= runIO . putStrLn . pack . ppShow  >> [|return ()|]

-- DataConI
--   Simplex.Messaging.Protocol.CreateConn
  
--   (ForallT
--     [KindedTV
--       s_6341068275337869648
--       (ConT Simplex.Messaging.Protocol.ConnectionState)]

--     contexts

--   (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT
--     (ConT Simplex.Messaging.Protocol.Command)
--       (ConT Simplex.Messaging.Types.CreateConnRequest))
--       (ConT Simplex.Messaging.Types.CreateConnResponse))
--       (PromotedT Simplex.Messaging.Protocol.Recipient))
--       (PromotedT Simplex.Messaging.Protocol.Broker))
--       (AppT (AppT (ConT Simplex.Messaging.Protocol.<==|) (AppT (AppT (ConT Simplex.Messaging.Protocol.<==>) (PromotedT Simplex.Messaging.Protocol.None)) (PromotedT Simplex.Messaging.Protocol.None))) (VarT s_6341068275337869648)))
--       (AppT (AppT (ConT Simplex.Messaging.Protocol.<==|) (AppT (AppT (ConT Simplex.Messaging.Protocol.<==>) (PromotedT Simplex.Messaging.Protocol.New)) (PromotedT Simplex.Messaging.Protocol.New))) (VarT s_6341068275337869648)))
--       (PromotedT Simplex.Messaging.Protocol.Idle))
--       (PromotedT Simplex.Messaging.Protocol.Idle))
--       (LitT (NumTyLit 0))) (LitT (NumTyLit 0))))
  
--   Simplex.Messaging.Protocol.Command




-- InstanceD
--   Nothing - always (non-overlapping)

--   contexts - same as in constructor

--   (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT (AppT
--     (ConT Simplex.Messaging.Protocol.ProtocolAction)
--       (ConT Simplex.Messaging.Types.CreateConnRequest))
--       (ConT Simplex.Messaging.Types.CreateConnResponse))
--       (PromotedT Simplex.Messaging.Protocol.Recipient))
--       (PromotedT Simplex.Messaging.Protocol.Broker))
--       (AppT (AppT (ConT Simplex.Messaging.Protocol.<==|) (AppT (AppT (ConT Simplex.Messaging.Protocol.<==>) (PromotedT Simplex.Messaging.Protocol.None)) (PromotedT Simplex.Messaging.Protocol.None))) (VarT s_0)))
--       (AppT (AppT (ConT Simplex.Messaging.Protocol.<==|) (AppT (AppT (ConT Simplex.Messaging.Protocol.<==>) (PromotedT Simplex.Messaging.Protocol.New)) (PromotedT Simplex.Messaging.Protocol.New))) (VarT s_0)))
--       (PromotedT Simplex.Messaging.Protocol.Idle))
--       (PromotedT Simplex.Messaging.Protocol.Idle))
--       (LitT (NumTyLit 0))) (LitT (NumTyLit 0)))
    

--   [ ValD
--       (VarP Simplex.Messaging.Protocol.action)
--       (NormalB (ConE Simplex.Messaging.Protocol.CreateConn))
--       []
--   , ValD
--       (VarP Simplex.Messaging.Protocol.protoAction)
--       (NormalB (VarE Simplex.Messaging.Client.rCreateConn))
--       [] ]