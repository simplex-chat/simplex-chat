{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Simplex.Messaging.Connection where

import Data.Kind
import Data.Text
import Data.Type.Bool (type (||))
import Data.Type.Equality (type (==))
import Simplex.Messaging.Core
import Simplex.Messaging.Types hiding (Invitation)

-- | 'Connection' for all participants
data Connection (p :: Party) (s :: ConnState) :: Type where
  -- | no connection with this ID, used by all parties
  NoConnection ::
    ConnId ->
    Connection p None
  -- | connection created by the broker
  ConnRcpNew ::
    (s == New || s == Pending) ~ True =>
    ClientConn ->
    SenderConnId ->
    PrivateKey -> -- key to decrypt messages from sender
    PublicKey -> -- key for sender to encrypt messages to recipient
    Connection Recipient s
  -- | After sender confirmed connection:
  --     * added sender's key for the broker (inside Conn)
  --     * removed PublicKey previously sent to sender
  ConnRcpConfirmed ::
    ClientConn ->
    Conn -> -- sender's connection info
    PrivateKey -> -- key to decrypt messages from the sender
    Connection Recipient Confirmed
  -- | Connection is secured and can be used by the sender or it is disabled.
  -- All sender connection information is removed now.
  ConnRcp ::
    (s == Secured || s == Disabled) ~ True =>
    ClientConn ->
    PrivateKey -> -- to decrypt messages from sender
    Connection Recipient s
  ConnSnd ::
    (HasState Sender s, (s == None) ~ False) =>
    ClientConn ->
    PublicKey -> -- to encrypt messages to recipient
    Connection Sender s
  ConnBrkNew ::
    Conn ->
    SenderConnId ->
    Connection Broker New
  ConnBrk ::
    (s == Secured || s == Disabled) ~ True =>
    {recipient :: Conn, sender :: Conn} ->
    Connection Broker s

data Conn = Conn
  { connId :: ConnId,
    brokerVerifyKey :: PublicKey
  }

data ClientConn = ClientConn
  { connId :: ConnId,
    brokerVerifyKey :: PublicKey,
    brokerKey :: PrivateKey,
    brokerUri :: Text
  }
