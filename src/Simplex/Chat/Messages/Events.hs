{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Messages.Events where

import qualified Data.Aeson.TH as JQ
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Messages.CIContent
import Simplex.Chat.Protocol
import Simplex.Chat.Types
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)
import Simplex.Messaging.Version

data StoredGroupEvent d = StoredGroupEvent
  { chatVRange :: VersionRange,
    msgId :: SharedMsgId,
    eventData :: StoredGroupEventData,
    integrityErrors :: [GroupEventIntegrityError],
    integrityConfirmations :: [GroupEventIntegrityConfirmation],
    sharedHash :: ByteString,
    eventDir :: GEDirection d,
    parents :: [AStoredGroupEvent]
  }

data AStoredGroupEvent = forall d. MsgDirectionI d => AStoredGroupEvent (StoredGroupEvent d)

data GroupEventIntegrityError = GroupEventIntegrityError
  { groupMemberId :: GroupMemberId,
    memberRole :: GroupMemberRole,
    error :: GroupEventError
  }
  deriving (Show)

data GroupEventError
  = GEErrInvalidHash                    -- content hash mismatch
  | GEErrUnconfirmedParent SharedMsgId  -- referenced parent wasn't previously received from author or admin
  | GEErrParentHashMismatch SharedMsgId -- referenced parent has different hash
  | GEErrChildHashMismatch SharedMsgId  -- child referencing this event has different hash (mirrors GEErrParentHashMismatch)
  deriving (Show)

data GroupEventIntegrityConfirmation = GroupEventIntegrityConfirmation
  { groupMemberId :: GroupMemberId,
    memberRole :: GroupMemberRole
  }
  deriving (Show)

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
    authorMember :: GroupMemberRef,
    receivedFrom :: GroupMemberRef,
    processing :: EventProcessing
  }

data ReceivedFromRole = RFAuthor | RFSufficientPrivilege | RFLower

receivedFromRole' :: ReceivedEventInfo -> ReceivedFromRole
receivedFromRole' = undefined

data EventProcessing
  = EPProcessed UTCTime
  | EPScheduled UTCTime
  | EPPendingConfirmation -- e.g. till it's received from author or member with the same or higher privileges (depending on the event)

-- platform-specific JSON encoding (used in API)
$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "GEErr") ''GroupEventError)

$(JQ.deriveJSON defaultJSON ''GroupEventIntegrityError)

$(JQ.deriveJSON defaultJSON ''GroupEventIntegrityConfirmation)
