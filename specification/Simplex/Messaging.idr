module Simplex.Messaging

data Participant = Recipient | Sender | Broker

Key : Type
Key = String

data Conn : Type where
  MkConn : (id : String) -> (key : Key) -> Conn

record ClientConn where
  constructor MkClientConn
  label     : String
  broker    : String
  conn      : Conn
  senderKey : Key -- public key for sender to encrypt messages

newClientConn : ClientConn
newClientConn = MkClientConn "" "" (MkConn "" "") ""

record RCData where -- recipient connection data
  constructor MkRCData
  conn             : ClientConn
  privateBrokerKey : Key
  privateSenderKey : Key

newRCData : RCData
newRCData = MkRCData newClientConn "" ""

record SCData where -- sender connection data
  constructor MkSCData
  conn   : ClientConn
  privateBrokerKey : Key

newSCData : SCData
newSCData = MkSCData newClientConn ""


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
  RCNew       : (conn : RCData) -> (senderId : String) -> RecipientConn New
  RCPending   : (conn : RCData) -> (senderId : String) -> RecipientConn Pending
  RCConfirmed : (conn : RCData) -> (sender : Conn) -> RecipientConn Confirmed
  MkRcpConn   : (state : ConnectionState)
             -> (conn : RCData)
             -> {auto prf : EstablishedCS state}
             -> RecipientConn state
  -- 3 constructors below are equivalent to MkRcpConn with some state
  RCSecured   : (conn : RCData) -> RecipientConn Secured
  RCDisabled  : (conn : RCData) -> RecipientConn Disabled
  RCDrained   : (conn : RCData) -> RecipientConn Drained
  --
  RCNull      : (conn : RCData) -> RecipientConn Null

-- recipient connection sample
goodRcpConn : RecipientConn Secured
goodRcpConn = MkRcpConn Secured (record
                                  { conn = record
                                            { label  = "label"
                                            , broker = "broker"
                                            , conn   = MkConn "1" "1"
                                            , senderKey = "2" } newClientConn
                                  , privateBrokerKey = "3"
                                  , privateSenderKey = "4" } newRCData)


data SenderConn : (state : ConnectionState) -> {auto prf : SenderCS state} -> Type where
  SCNew       : (conn : ClientConn) -> SenderConn New
  SCConfirmed : (conn : SCData) -> SenderConn Confirmed
  SCSecured   : (conn : SCData) -> SenderConn Secured
  SCNull      : (conn : SCData) -> SenderConn Null

-- sender connection sample
goodSndConn : SenderConn Secured
goodSndConn = SCSecured (record
                          { conn = record
                                    { label  = "label"
                                    , broker = "broker"
                                    , conn   = MkConn "1" "1"
                                    , senderKey = "2" } newClientConn
                          , privateBrokerKey = "3" } newSCData)



-- -- data Action  : (res : Type)
-- --             -> (snd : Participant)
-- --             -> (rcv : Participant)
-- --             -> (sndConn  : snd -> ConnState)
-- --             -> (sndConn' : snd -> res -> ConnState)
-- --             -> (rcvConn  : res -> ConnState)
-- --             -> (rcvConn' : rcv -> res -> ConnState)
-- --             -> Type where
-- --   SendMsg : Msg -> Action () Sender Server state (const state) state (conn state)
