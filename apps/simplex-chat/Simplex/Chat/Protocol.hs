module Simplex.Chat.Protocol where

import Data.Text (Text)
import Simplex.Messaging.Agent.Protocol
import Simplex.Store.Types

data ChatMessage = ChatMessage MsgMeta ChatMsgData

data ChatMsgData = DirectChatMsg Contact ContentMsg

newtype ContentMsg = NewContentMsg ContentData

newtype ContentData = ContentText Text
