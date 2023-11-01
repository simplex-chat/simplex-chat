{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simplex.Chat.Messages.Events where

import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Version

data StoredGroupEvent d = StoredGroupEvent
  { chatVRange :: VersionRange,
    msgId :: SharedMsgId,
    eventData :: StoredGroupEventData,
    dagErrors :: [GroupEventIntegrityError],
    sharedHash :: ByteString,
    eventDir :: GEDirection d,
    parents :: [AStoredGroupEvent]
  }

data AStoredGroupEvent = forall d. MsgDirectionI d => AStoredGroupEvent (StoredGroupEvent d)

data GroupEventIntegrityError
  = GEErrInvalidHash
  | GEErrMissingParent SharedMsgId
  | GEErrParentHashMismatch SharedMsgId

data GEDirection (d :: MsgDirection) where
  GESent :: GEDirection 'MDSnd
  GEReceived :: ReceivedEventInfo -> GEDirection 'MDRcv

data StoredGroupEventData = SGEData (ChatMsgEvent 'Json) | SGEAvailable [GroupMemberId]

data ReceivedEventInfo = ReceivedEventInfo
  { authorMemberId :: MemberId,
    authorMemberName :: ContactName,
    authorMember :: Maybe GroupMemberRef,
    receivedFrom :: GroupMemberRef,
    processing :: EventProcessing
  }

data ReceivedFromRole = RFAuthor | RFSufficientPrivilege | RFLower

receviedFromRole :: ReceivedEventInfo -> ReceivedFromRole
receviedFromRole = undefined

data EventProcessing
  = EPProcessed UTCTime
  | EPScheduled UTCTime
  | EPPendingConfirmation -- e.g. till it's received from author or member with the same or higher privileges (depending on the event)
