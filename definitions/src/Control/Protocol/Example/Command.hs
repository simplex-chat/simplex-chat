{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Protocol.Example.Command where

import Control.Protocol
import Data.Singletons.TH

$( singletons
     [d|
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
       |]
 )

data MyCommand :: Command Party where
  Create :: MyCommand (Cmd Recipient RNone RReady) (Cmd Broker BNone BEmpty) ()
  Notify :: MyCommand (Cmd Recipient RReady RReady) (Cmd Sender SNone SReady) ()
  Send :: String -> MyCommand (Cmd Sender SReady SReady) (Cmd Broker BEmpty BFull) ()
  Forward :: MyCommand (Cmd Broker BFull BEmpty) (Cmd Recipient RReady RReady) String

type MyProtocol = Protocol MyCommand '[Recipient, Broker, Sender]
