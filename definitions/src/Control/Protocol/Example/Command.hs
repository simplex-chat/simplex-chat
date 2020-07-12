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

       data ChannelState
         = None
         | Ready
         | Busy
         deriving (Show, Eq)
       |]
 )

data MyCommand :: Command Party ChannelState where
  Create :: MyCommand '(Recipient, None, Ready) '(Broker, None, Ready) ()
  Notify :: MyCommand '(Recipient, Ready, Ready) '(Sender, None, Ready) ()
  Send :: String -> MyCommand '(Sender, Ready, Ready) '(Broker, Ready, Busy) ()
  Forward :: MyCommand '(Broker, Busy, Ready) '(Recipient, Ready, Ready) String

type MyProtocol = Protocol MyCommand '[Recipient, Broker, Sender]
