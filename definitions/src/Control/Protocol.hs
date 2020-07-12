{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol
  ( Protocol,
    ProtocolCmd (..),
    Command,
    (->:),
    interpret,
  )
where

import Control.XFreer
import Data.Kind
import Data.Singletons
import GHC.TypeLits (ErrorMessage (..), TypeError)

type Command party state = (party, state, state) -> (party, state, state) -> Type -> Type

data ProtocolCmd (cmd :: Command p k) (parties :: [p]) (s :: [k]) (s' :: [k]) (a :: Type) where
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

interpret ::
  forall m cmd ps s s' a.
  Monad m =>
  (forall from to b. (Sing (P from) -> Sing (P to) -> cmd from to b -> m b)) ->
  Protocol cmd ps s s' a ->
  m a
interpret runCommand = loop
  where
    loop :: forall s1 s2 b. Protocol cmd ps s1 s2 b -> m b
    loop = \case
      Pure x -> return x
      Bind c f -> do
        x <- run c
        loop (f x)
    run :: forall s1 s2 b. ProtocolCmd cmd ps s1 s2 b -> m b
    run (ProtocolCmd from to cmd) = runCommand from to cmd

type family P (partyCmd :: (party, s, s)) where
  P '(p, _, _) = p

-- extracts the state of one party from the list of states
type family Prj (parties :: [pk]) (state :: [k]) (party :: pk) :: k where
  Prj (p ': _) (s ': _) p = s
  Prj (_ ': ps) (_ ': ss) p = Prj ps ss p
  Prj '[] _ p = TypeError (NoParty p)
  Prj _ '[] p = TypeError (NoParty p :$$: StateError)

-- updates the state of some party in the list of states
type family Inj (parties :: [pk]) (state :: [k]) (p :: pk) (s' :: k) :: [k] where
  Inj (p ': _) (_ ': ss) p s' = s' ': ss
  Inj (_ ': ps) (s ': ss) p s' = s ': Inj ps ss p s'
  Inj '[] _ p _ = TypeError (NoParty p)
  Inj _ '[] p _ = TypeError (NoParty p :$$: StateError)

type NoParty p = Text "Party " :<>: ShowType p :<>: Text " is not found."

type StateError = Text "Specified fewer protocol states than parties."
