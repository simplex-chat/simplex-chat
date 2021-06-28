module Simplex.Chat.Protocol where

import Data.Aeson (Value)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Simplex.Messaging.Agent.Protocol
import Simplex.Store.Types

type ChatTransmission = (MsgMeta, ChatMessage)

data ChatMessage = DirectChatMsg Contact ContentMsg

newtype ContentMsg = NewContentMsg ContentData

newtype ContentData = ContentText Text

data RawChatMessage = RawChatMessage
  { chatMsgId :: Maybe Int64,
    chatMsgEvent :: ByteString,
    chatMsgParams :: [ByteString],
    chatMsgBody :: [(ContentType, MsgBodyPart)]
  }

type ContentType = ByteString

data MsgBodyPart
  = MBFull MsgPart
  | MBPartial MsgPart Int ByteString -- MsgPart in this constructor is only used to define type, it's content can (should?) be empty
  | MBEmpty

data MsgPart = MsgPartBinary ByteString | MsgPartText Text | MsgPartJSON Value
