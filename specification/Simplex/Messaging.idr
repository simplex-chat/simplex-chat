module Simplex.Messaging

data Participant = Recipient | Sender | Broker

data Conn : Type where
  MkConn : (id : String) -> (key : Key) -> Conn

type Key = String

record ClientConn where
  constructor MkClientConn
  label     : String
  broker    : String
  conn      : Conn
  senderKey : Key -- public key for sender to encrypt messages

record RecipientConnData where
  constructor MkRecipientConnData
  conn   : ClientConn
  privateBrokerKey : Key
  privateSenderKey : Key

record SenderConnData where
  constructor MkSenderConnData
  conn   : ClientConn
  privateBrokerKey : Key

data BrokerConnState =  -- broker connection state
          New           -- connection created by the receiver but not secured yet
          | Secured     -- connection is secured and active
          | Disabled    -- connection is disabled (by receiver)
          | Drained     -- connection is disabled and has no messages
          | NoRecipient -- connection for given receiver ID does not exist
          | NoSender    -- connection for given sender ID does not exist

data BrokerConn : BrokerConnState -> Type where
  BCNew         : (recipient : Conn) -> (senderId : String) -> BrokerConn New
  -- three constructors below can probably be merged into one
  MkBrokerConn  : (recipient : Conn) -> (sender : Conn) -> BrokerConn state
  -- with some restriction on the state
  BCSecured     : (recipient : Conn) -> (sender : Conn) -> BrokerConn Secured
  BCDisabled    : (recipient : Conn) -> (sender : Conn) -> BrokerConn Disabled
  BCDrained     : (recipient : Conn) -> (sender : Conn) -> BrokerConn Drained
  --
  BCNoRecipient : (id : String) -> BrokerConn NoRecipient
  BCNoSender    : (senderId : String) -> BrokerConn NoSender


data RecipientConnState = -- recipient connection state
              RNew        -- connection created with the broker
              | Pending   -- pending: connection informaiton sent to sender out-of-band
              | Confirmed -- sender confirmation received from the broker
              | RSecured  -- connection secured with the broker
              | RDisabled -- connection is disabled with the broker
              | RDrained  -- connection is disabled with the broker and drained
              | Deleted   -- connection is deleted from the broker

data RecipientConn : (state : RecipientConnState) -> Type where
  RCRcvNew         : (conn : RecipientConnData) -> (senderId : String) -> RecipientConn RNew
  RCPending        : (conn : RecipientConnData) -> (senderId : String) -> RecipientConn Pending
  RCConfirmed      : (conn : RecipientConnData) -> (sender : Conn) -> RecipientConn Confirmed
  -- 4 constructors below can probably be merged into this one
  MkRecipientConn  : (conn : RecipientConnData) -> RecipientConn state
  -- with some restriction on the state
  RCSecured        : (conn : RecipientConnData) -> RecipientConn RSecured
  RCDisabled       : (conn : RecipientConnData) -> RecipientConn RDisabled
  RCDrained        : (conn : RecipientConnData) -> RecipientConn RDrained
  RCDeleted        : (conn : RecipientConnData) -> RecipientConn Deleted
  --


data SenderConnState =  -- sender connection state
          Received      -- connection received from the recipient
          | Failed      -- failed to send confirmation message to broker
          | SConfirmed  -- sent confirmation message to broker
          | SSecured    -- succeeded sending the message after sending confirmation message
          | Unavailable -- connection is no longer available with the broker

data SenderConn : (state : SenderConnState) -> Type where
  SCReceived    : (conn : ClientConn) -> SenderConn Received
  SCFailed      : (conn : SenderConnData) -> SenderConn Failed
  SCConfirmed   : (conn : SenderConnData) -> SenderConn SConfirmed
  SCSecured     : (conn : SenderConnData) -> SenderConn SSecured
  SCUnavailable : (conn : SenderConnData) -> SenderConn Unavailable

  -- RDisabled



-- -- data Action  : (res : Type)
-- --             -> (snd : Participant)
-- --             -> (rcv : Participant)
-- --             -> (sndConn  : snd -> ConnState)
-- --             -> (sndConn' : snd -> res -> ConnState)
-- --             -> (rcvConn  : res -> ConnState)
-- --             -> (rcvConn' : rcv -> res -> ConnState)
-- --             -> Type where
-- --   SendMsg : Msg -> Action () Sender Server state (const state) state (conn state)
