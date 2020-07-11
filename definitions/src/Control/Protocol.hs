{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol
  ( Protocol,
    -- ProtocolCmd (..),
    PartyCmd (..),
    type (|:),
    (->::),
    comment,
  )
where

import Control.XFreer
import Data.Kind
import Data.Proxy

data PartyCmd p = forall s. P p s s

infixr 3 :|, |:

data DList = DNil | forall a. a :| DList

-- this requires NoMonomorphismRestriction
type family (|:) (s :: k1) (t :: k2) :: DList where
  s |: DNil = s :| DNil
  s |: (t :| ss) = s :| t :| ss
  s |: t = s :| t :| DNil

data ProtocolCmd (parties :: [pk]) (cmd :: PartyCmd pk -> PartyCmd pk -> k -> Type) (s :: DList) (s' :: DList) (a :: k) where
  Comment :: String -> ProtocolCmd ps cmd s s ()
  ProtocolCmd ::
    Proxy (from :: pk) ->
    Proxy (to :: pk) ->
    cmd (P from (PartySt s ps from) fs') (P to (PartySt s ps to) ts') a ->
    ProtocolCmd ps cmd s (ProtoSt s ps from fs' to ts') a

type Protocol parties cmd = XFree (ProtocolCmd parties cmd)

infix 6 ->::

(->::) ::
  Proxy from ->
  Proxy to ->
  cmd (P from (PartySt s ps from) fs') (P to (PartySt s ps to) ts') a ->
  Protocol ps cmd s (ProtoSt s ps from fs' to ts') a
(->::) f t c = xfree $ ProtocolCmd f t c

comment :: String -> Protocol ps cmd s s ()
comment = xfree . Comment

type family PartySt (state :: DList) (parties :: [pk]) (party :: pk) where
  PartySt (s ':| _) (p ': _) p = s
  PartySt (_ ':| ss) (_ ': ps) p = PartySt ss ps p

-- PartySt _ '[] p = -- type error
-- PartySt DNil _ _ = -- type error

type family ProtoSt (state :: DList) (parties :: [pk]) (from :: pk) fs' (to :: pk) ts' where
  ProtoSt DNil '[] _ _ _ _ = DNil
-- ProtoSt DNil (_ ': _) _ _ _ _ = -- type error
-- ProtoSt (_ ':| _) '[] _ _ _ _ = -- type error
  ProtoSt (_ ':| ss) (from ': ps) from fs' to ts' = fs' ':| ProtoSt ss ps from fs' to ts'
  ProtoSt (_ ':| ss) (to ': ps) from fs' to ts' = ts' ':| ProtoSt ss ps from fs' to ts'
  ProtoSt (s ':| ss) (_ ': ps) from fs' to ts' = s ':| ProtoSt ss ps from fs' to ts'
