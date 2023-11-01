{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Simplex.Chat.Messages.Events where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
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
  = GEErrInvalidHash                    -- content hash mismatch
  | GEErrUnconfirmedParent SharedMsgId  -- referenced parent wasn't previously received from author or admin
  | GEErrParentHashMismatch SharedMsgId -- referenced parent has different hash
  | GEErrChildHashMismatch SharedMsgId  -- child referencing this event has different hash (mirrors GEErrParentHashMismatch)
  deriving (Show, Generic)

instance ToJSON GroupEventIntegrityError where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "GEErr"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "GEErr"

-- data GroupEventMemberIntegrity = GroupEventMemberIntegrity
--   { groupMemberId :: GroupMemberId,
--     memberRole :: GroupMemberRole,
--     eventStatus :: GroupEventIntegrityStatus
--   }

-- data GroupEventIntegrityStatus
--   = GISOk                                  -- sent event; or received event status for sender with all parents known
--   | GISUnconfirmedParent SharedMsgId       -- unconfirmed parent for parent sender
--   | GISConfirmedParent SharedMsgId         -- 
--   | GISErrorParentHashMismatch SharedMsgId
--   | GISErrorChildHashMismatch SharedMsgId
--   | GISErrorInvalidHash

-- -- data GroupIntegrityStatusProgress
-- --   = GIPPartial
-- --   | GIPComplete

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
