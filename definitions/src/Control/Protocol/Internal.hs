{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Internal where

infixr 3 :|, |:

data DList = DNil | forall a. a :| DList

type family (|:) (s :: k1) (t :: k2) :: DList where
  s |: DNil = s :| DNil
  s |: (t :| ss) = s :| t :| ss
  s |: t = s :| t :| DNil

type family PartySt (parties :: [k]) (state :: DList) (party :: k) where
  PartySt (p ': _) (s ':| _) p = s
  PartySt (_ ': ps) (_ ':| ss) p = PartySt ps ss p

-- PartySt '[] _ _ = -- type error
-- PartySt _ DNil _ = -- type error

type family ProtoSt (parties :: [k]) (state :: DList) (from :: k) fs' (to :: k) ts' where
  ProtoSt '[] DNil _ _ _ _ = DNil
-- ProtoSt _ DNil _ _ _ _ = -- type error
-- ProtoSt '[] _ _ _ _ _ = -- type error
  ProtoSt (from ': ps) (_ ':| ss) from fs' to ts' = fs' ':| ProtoSt ps ss from fs' to ts'
  ProtoSt (to ': ps) (_ ':| ss) from fs' to ts' = ts' ':| ProtoSt ps ss from fs' to ts'
  ProtoSt (_ ': ps) (s ':| ss) from fs' to ts' = s ':| ProtoSt ps ss from fs' to ts'
