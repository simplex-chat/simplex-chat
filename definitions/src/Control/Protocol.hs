{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol
  ( Protocol,
    ProtocolCmd (..),
    Command,
    PartyCmd (..),
    type (|:),
    (->:),
    comment,
  )
where

import Control.Protocol.Internal
import Control.XFreer
import Data.Kind
import Data.Singletons

data PartyCmd p = forall s. Cmd p s s

type Command p = PartyCmd p -> PartyCmd p -> Type -> Type

data ProtocolCmd (cmd :: Command p) (parties :: [p]) (s :: DList) (s' :: DList) (a :: Type) where
  Comment :: String -> ProtocolCmd cmd ps s s ()
  ProtocolCmd ::
    Sing (from :: p) ->
    Sing (to :: p) ->
    cmd (Cmd from (PartySt ps s from) fs') (Cmd to (PartySt ps s to) ts') a ->
    ProtocolCmd cmd ps s (ProtoSt ps s from fs' to ts') a

type Protocol parties cmd = XFree (ProtocolCmd parties cmd)

infix 6 ->:

(->:) ::
  Sing from ->
  Sing to ->
  cmd (Cmd from (PartySt ps s from) fs') (Cmd to (PartySt ps s to) ts') a ->
  Protocol cmd ps s (ProtoSt ps s from fs' to ts') a
(->:) f t c = xfree $ ProtocolCmd f t c

comment :: String -> Protocol ps cmd s s ()
comment = xfree . Comment
