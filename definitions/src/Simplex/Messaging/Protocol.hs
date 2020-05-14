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
-- import Text.Show.Pretty (ppShow)

$(singletons [d|
  data Participant = Recipient | Broker | Sender
    deriving (Show, Eq)

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

-- | Using not allowed connection type results in type error
--
-- >>> :{
-- stBad :: 'Pending <==> 'Confirmed <==| 'Confirmed
-- stBad = SPending :<==> SConfirmed :<==| SConfirmed
-- :}
-- ...
-- ... No instance for (Auto (TyPred BrokerCS) 'Confirmed)
-- ... arising from a use of ‘:<==>’
-- ...


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
    cmdMe    :: Sing to
    cmdOther :: Sing from
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
    actionMe    :: Sing from
    actionOther :: Sing to
    protoAction :: Connection from (PConnSt from state) ss
                -> arg
                -> Either String res
                -> Either String (Connection from (PConnSt from state') ss')

protoActionStub :: Connection from ps ss
                -> arg
                -> Either String res
                -> Either String (Connection from ps' ss')
protoActionStub _ _ _ = Left "Action not implemented"


-- TH to generate typeclasse instances for ProtocolCommand
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
-- they have to be consistent with the typeclass.

data ProtocolOpeartion = POCommand | POAction | POUndefined
data ProtocolClassDescription = ProtocolClassDescription
                                  { clsName   :: String
                                  , mthd      :: String
                                  , meSing    :: String
                                  , otherSing :: String
                                  , protoMthd :: String }

protocol :: Participant -> Q [Dec] -> Q [Dec]
protocol me ds = ds >>= mapM mkProtocolInstance
  where
    getCls :: ProtocolOpeartion -> Maybe ProtocolClassDescription
    getCls POUndefined = Nothing
    getCls POCommand = Just ProtocolClassDescription
                              { clsName = "ProtocolCommand"
                              , mthd = "command"
                              , meSing = "cmdMe"
                              , otherSing = "cmdOther"
                              , protoMthd = "protoCmd" }
    getCls POAction  = Just ProtocolClassDescription
                              { clsName = "ProtocolAction"
                              , mthd = "action"
                              , meSing = "actionMe"
                              , otherSing = "actionOther"
                              , protoMthd = "protoAction" }

    mkProtocolInstance :: Dec -> Q Dec
    mkProtocolInstance d = case d of
      ValD
        (VarP fName)
        (NormalB
          (InfixE
            (Just (ConE cmd))
            opExp
            (Just (ConE other)))) [] ->
        case getCls (getOperation opExp) of
          Nothing  -> badSyntax d
          Just cls -> instanceDecl d fName cmd other cls
      _ -> badSyntax d

    instanceDecl :: Dec
                 -> Name -> Name -> Name
                 -> ProtocolClassDescription
                 -> Q Dec
    instanceDecl d fName cmd other ProtocolClassDescription{..} =
      reify cmd >>= \case
        DataConI _ (ForallT _ ctxs ty) _ -> do
          tc <- changeTyCon d ty clsName
          return $
            InstanceD Nothing ctxs tc
              [ mkMethod mthd (ConE cmd)
              , mkMethod meSing (mkSing $ show me) 
              , mkMethod otherSing (mkSing $ nameBase other) 
              , mkMethod protoMthd (VarE . mkName $ nameBase fName) ]
        _ -> badSyntax d

    mkMethod :: String -> Exp -> Dec
    mkMethod name rhs = ValD (VarP (mkName name)) (NormalB rhs) []

    mkSing :: String -> Exp
    mkSing = ConE . mkName . ('S':)

    changeTyCon :: Dec -> TH.Type -> String -> Q TH.Type
    changeTyCon d (AppT t1 t2) n =
      (`AppT` t2) <$> changeTyCon d t1 n
    changeTyCon d (ConT name) n =
      if nameBase name == "Command"
        then conT $ mkName n
        else badConstructor d
    changeTyCon d _ _ = badConstructor d

    badConstructor :: Dec -> Q TH.Type
    badConstructor d = fail $ "constructor type must be Command\n" ++ pprint d

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

    badSyntax :: Dec -> Q Dec
    badSyntax d = fail $ "invalid protocol syntax: " ++ pprint d


-- introspect :: Name -> Q Exp
-- introspect n = reify n >>= runIO . putStrLn . pack . ppShow  >> [|return ()|]
