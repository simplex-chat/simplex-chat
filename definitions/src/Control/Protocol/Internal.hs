{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Internal
  ( DList (..),
    type (|:),
    PartySt,
    ProtoSt,
  )
where

import GHC.TypeLits (ErrorMessage (..), TypeError)

infixr 3 :|, |:

data DList = DNil | forall a. a :| DList

type family (|:) (s :: k1) (t :: k2) :: DList where
  s |: DNil = s :| DNil
  s |: (t :| ss) = s :| t :| ss
  s |: t = s :| t :| DNil

type family PartySt (parties :: [k]) (state :: DList) (party :: k) where
  PartySt (p ': _) (s ':| _) p = s
  PartySt (_ ': ps) (_ ':| ss) p = PartySt ps ss p
  PartySt '[] DNil p = TypeError (NoParty p)
  PartySt '[] _ p = TypeError (NoParty p :<>: PartyError)
  PartySt _ DNil p = TypeError (NoParty p :<>: StateError)

type family ProtoSt (parties :: [k]) (state :: DList) (from :: k) fs' (to :: k) ts' where
  ProtoSt '[] DNil from _ to _ = TypeError (NoParties from to)
  ProtoSt '[] _ from _ to _ = TypeError (NoParties from to :<>: PartyError)
  ProtoSt _ DNil from _ to _ = TypeError (NoParties from to :<>: StateError)
  ProtoSt (from ': ps) (_ ':| ss) from fs' to ts' = fs' ':| ProtoSt1 ps ss to ts'
  ProtoSt (to ': ps) (_ ':| ss) from fs' to ts' = ts' ':| ProtoSt1 ps ss from fs'
  ProtoSt (_ ': ps) (s ':| ss) from fs' to ts' = s ':| ProtoSt ps ss from fs' to ts'

type family ProtoSt1 (parties :: [k]) (state :: DList) (p :: k) s' where
  ProtoSt1 '[] DNil p _ = TypeError (NoParty p)
  ProtoSt1 '[] _ p _ = TypeError (NoParty p :<>: PartyError)
  ProtoSt1 _ DNil p _ = TypeError (NoParty p :<>: StateError)
  ProtoSt1 (p ': _) (_ ':| ss) p s' = s' ':| ss
  ProtoSt1 (_ ': ps) (s ':| ss) p s' = s ':| ProtoSt1 ps ss p s'

type NoParties p1 p2 =
  Text "Parties " :<>: ShowType p1 :<>: Text " and " :<>: ShowType p2
    :<>: Text " are not found."

type NoParty p = Text "Party " :<>: ShowType p :<>: Text " is not found."

type PartyError = Text "\nSpecified fewer protocol parties than states."

type StateError = Text "\nSpecified fewer protocol states than parties."
