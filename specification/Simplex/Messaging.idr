module Simplex.Messaging

data Participant = Recipient | Sender | Broker

data Client : Participant -> Type where
  CRecipient : Client Recipient
  CSender    : Client Sender

Key : Type
Key = String

PrivateKey : Type
PrivateKey = String


-- Data structures for participants to store and pass connection information

data Conn : Type where   -- connection info shared between a client and broker
  MkConn : (id : String) -- connection ID to identify it with the broker
        -> (key : Key)   -- public key for broker to verify commands
        -> Conn

record ClientConn where
  constructor MkClientConn
  conn   : Conn   -- same info that broker has for this client
  label  : String -- label for the client to identify connection
  broker : String -- broker URI
  brokerPrivateKey : PrivateKey -- private key to sign commands to broker

newClientConn : ClientConn
newClientConn = MkClientConn (MkConn "" "") "" "" ""

record RcpConn where -- recipient connection data
  constructor MkRcpConn
  clientConn       : ClientConn
  senderPrivateKey : PrivateKey -- private key to decrypt sender messages

newRcpConn : RcpConn
newRcpConn = MkRcpConn newClientConn ""

record Invitation where -- out of band message to sender inviting to connect
  constructor MkInvitation
  conn   : Conn
  broker : String
  senderKey : Key -- public key for sender to encrypt messages

newInvitation : Invitation
newInvitation = MkInvitation (MkConn "" "") "" ""

record SndConn where -- sender connection data
  constructor MkSndConn
  clientConn : ClientConn
  senderKey  : Key -- public key for sender to encrypt messages

newSndConn : SndConn
newSndConn = MkSndConn newClientConn ""


-- connection states for all participants

data ConnectionState =  -- connection states for all participants
            New         -- (participants: all) connection created (or received from sender)
            | Pending   -- (recipient) sent to sender out-of-band
            | Confirmed -- (recipient) confirmed by sender with the broker
            | Secured   -- (all) secured with the broker
            | Disabled  -- (broker, recipient) disabled with the broker by recipient
            | Drained   -- (broker, recipient) drained (no messages)
            | Null      -- (all) not available or removed from the broker

-- broker connection states
data BrokerCS : ConnectionState -> Type where
  BNew      : BrokerCS New
  BSecured  : BrokerCS Secured
  BDisabled : BrokerCS Disabled
  BDrained  : BrokerCS Drained
  BNull     : BrokerCS Null

-- sender connection states
data SenderCS : ConnectionState -> Type where
  SNew       : SenderCS New
  SConfirmed : SenderCS Confirmed
  SSecured   : SenderCS Secured
  SNull      : SenderCS Null

-- established connection states (used by broker and recipient)
data EstablishedCS : ConnectionState -> Type where
  ESecured  : EstablishedCS Secured
  EDisabled : EstablishedCS Disabled
  EDrained  : EstablishedCS Drained


-- dependent types to represent connections for all participants

data BrokerConn : (state : ConnectionState) -> {auto prf : BrokerCS state} -> Type where
  BCNew      : (recipient : Conn) -> (senderId : String) -> BrokerConn New
  MkBrkConn  : (state : ConnectionState)
            -> (recipient : Conn)
            -> (sender : Conn)
            -> {auto prf : BrokerCS state}
            -> {auto prf : EstablishedCS state}
            -> BrokerConn state
  -- 3 constructors below are equivalent to MkBrkConn with some state
  BCSecured  : (recipient : Conn) -> (sender : Conn) -> BrokerConn Secured
  BCDisabled : (recipient : Conn) -> (sender : Conn) -> BrokerConn Disabled
  BCDrained  : (recipient : Conn) -> (sender : Conn) -> BrokerConn Drained
  --
  BCNull     : (id : String) -> BrokerConn Null


-- good broker connection sample
goodBrkConn : BrokerConn Secured
goodBrkConn = MkBrkConn Secured (MkConn "1" "1") (MkConn "2" "2")

-- bad broker connection sample - does not type check
-- badBrkConn : BrokerConn Null
-- badBrkConn = BCEstablished Null (MkConn "1" "1") (MkConn "2" "2")


data RecipientConn : (state : ConnectionState) -> Type where
  RCNew       : (conn : RcpConn) -> (sender : Invitation) -> RecipientConn New
  RCPending   : (conn : RcpConn) -> (sender : Invitation) -> RecipientConn Pending
  RCConfirmed : (conn : RcpConn) -> (sender : Conn) -> RecipientConn Confirmed
  MkRecipientConn   : (state : ConnectionState)
                   -> (conn : RcpConn)
                   -> {auto prf : EstablishedCS state}
                   -> RecipientConn state
  -- 3 constructors below are equivalent to MkRcpConn with some state
  RCSecured   : (conn : RcpConn) -> RecipientConn Secured
  RCDisabled  : (conn : RcpConn) -> RecipientConn Disabled
  RCDrained   : (conn : RcpConn) -> RecipientConn Drained
  --
  RCNull      : (conn : RcpConn) -> RecipientConn Null

-- recipient connection sample
goodRcpConn : RecipientConn Secured
goodRcpConn = MkRecipientConn Secured (record
                                        { clientConn = record
                                                        { conn   = MkConn "1" "1"
                                                        , label  = "label"
                                                        , broker = "broker"
                                                        , brokerPrivateKey = "2" }
                                                       newClientConn
                                        , senderPrivateKey = "3" }
                                       newRcpConn)


data SenderConn : (state : ConnectionState) -> {auto prf : SenderCS state} -> Type where
  SCNew       : (conn : Invitation) -> SenderConn New
  SCConfirmed : (conn : SndConn) -> SenderConn Confirmed
  SCSecured   : (conn : SndConn) -> SenderConn Secured
  SCNull      : (conn : SndConn) -> SenderConn Null

-- sender connection sample
goodSndConn : SenderConn Secured
goodSndConn = SCSecured (record
                          { clientConn = record
                                          { conn   = MkConn "1" "1"
                                          , label  = "label"
                                          , broker = "broker"
                                          , brokerPrivateKey = "2" }
                                         newClientConn
                          , senderKey = "3" }
                         newSndConn)



-- -- data Action  : (res : Type)
-- --             -> (snd : Participant)
-- --             -> (rcv : Participant)
-- --             -> (sndConn  : snd -> ConnState)
-- --             -> (sndConn' : snd -> res -> ConnState)
-- --             -> (rcvConn  : res -> ConnState)
-- --             -> (rcvConn' : rcv -> res -> ConnState)
-- --             -> Type where
-- --   SendMsg : Msg -> Action () Sender Server state (const state) state (conn state)
