{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol
  ( Protocol,
    ProtocolCmd (..),
    Command,
    (->:),
    comment,
  )
where

import Control.XFreer
import Data.Kind
import Data.Singletons
import GHC.TypeLits (ErrorMessage (..), TypeError)

type Command party state = (party, state, state) -> (party, state, state) -> Type -> Type

data ProtocolCmd (cmd :: Command p k) (parties :: [p]) (s :: [k]) (s' :: [k]) (a :: Type) where
  Comment :: String -> ProtocolCmd cmd ps s s ()
  ProtocolCmd ::
    Sing (from :: p) ->
    Sing (to :: p) ->
    cmd '(from, Prj ps s from, fs') '(to, Prj ps s to, ts') a ->
    ProtocolCmd cmd ps s (Inj ps (Inj ps s from fs') to ts') a

type Protocol cmd parties = XFree (ProtocolCmd cmd parties)

infix 6 ->:

(->:) ::
  Sing from ->
  Sing to ->
  cmd '(from, Prj ps s from, fs') '(to, Prj ps s to, ts') a ->
  Protocol cmd ps s (Inj ps (Inj ps s from fs') to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

comment :: String -> Protocol ps cmd s s ()
comment = xfree . Comment

type family Prj (parties :: [pk]) (state :: [k]) (party :: pk) :: k where
  Prj (p ': _) (s ': _) p = s
  Prj (_ ': ps) (_ ': ss) p = Prj ps ss p
  Prj '[] _ p = TypeError (NoParty p)
  Prj _ '[] p = TypeError (NoParty p :$$: StateError)

type family Inj (parties :: [pk]) (state :: [k]) (p :: pk) (s' :: k) :: [k] where
  Inj (p ': _) (_ ': ss) p s' = s' ': ss
  Inj (_ ': ps) (s ': ss) p s' = s ': Inj ps ss p s'
  Inj '[] _ p _ = TypeError (NoParty p)
  Inj _ '[] p _ = TypeError (NoParty p :$$: StateError)

type NoParty p = Text "Party " :<>: ShowType p :<>: Text " is not found."

type StateError = Text "Specified fewer protocol states than parties."
