{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Example.Command where

import Control.Protocol

data Party = Recipient | Broker | Sender
  deriving (Show, Eq)

data RState
  = RNone
  | RReady
  deriving (Show, Eq)

data BState
  = BNone
  | BEmpty
  | BFull
  deriving (Show, Eq)

data SState
  = SNone
  | SReady
  deriving (Show, Eq)

data MyCommand (fromCmd :: PartyCmd Party) (toCmd :: PartyCmd Party) a where
  Create :: MyCommand (P Recipient RNone RReady) (P Broker BNone BEmpty) ()
  Notify :: MyCommand (P Recipient RReady RReady) (P Sender SNone SReady) ()
  Send :: String -> MyCommand (P Sender SReady SReady) (P Broker BEmpty BFull) ()
  Forward :: MyCommand (P Broker BFull BEmpty) (P Recipient RReady RReady) String

type MyProtocol = Protocol '[Recipient, Broker, Sender] MyCommand
