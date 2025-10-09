{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Responses where

import API.Docs.Types
import API.TypeInfo
import Data.List (find)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Messaging.Parsers (dropPrefix)

data CRDoc = CRDoc
  { consName :: ConsName,
    responseType :: ATUnionMember,
    responseDescr :: String
  }

instance ConstructorName CRDoc where consName' CRDoc {consName} = consName

chatResponsesDocs :: [CRDoc]
chatResponsesDocs = map toResp chatResponsesDocsData
  where
    toResp (consName, responseDescr)
      | consName == "CRChatCmdError" =
          let field = toAPIField consName $ FieldInfo "chatError" (ti "ChatError")
              responseType = ATUnionMember (dropPrefix "CR" consName) [field]
           in CRDoc {consName, responseType, responseDescr}
      | otherwise = case find ((consName ==) . consName') chatResponsesTypeInfo of
          Just RecordTypeInfo {fieldInfos} ->
            let fields = map (toAPIField consName) fieldInfos
                responseType = ATUnionMember (dropPrefix "CR" consName) fields
            in CRDoc {consName, responseType, responseDescr}
          Nothing -> error $ "Missing response type info for " <> consName

deriving instance Generic ChatResponse

chatResponsesTypeInfo :: [RecordTypeInfo]
chatResponsesTypeInfo = recordTypesInfo @ChatResponse

chatResponsesDocsData :: [(ConsName, String)]
chatResponsesDocsData =
  [ ("CRAcceptingContactRequest", "Contact request accepted"),
    ("CRActiveUser", "Active user profile"),
    ("CRChatItemNotChanged", "Message not changed"),
    ("CRChatItemReaction", "Message reaction"),
    ("CRChatItemUpdated", "Message updated"),
    ("CRChatItemsDeleted", "Messages deleted"),
    ("CRCmdOk", "Ok"),
    ("CRChatCmdError", "Command error"), -- only used in WebSockets API, Haskell code uses Either, with error in Left
    ("CRConnectionPlan", "Connection link information"),
    ("CRContactAlreadyExists", ""),
    ("CRContactConnectionDeleted", "Connection deleted"),
    ("CRContactDeleted", ""),
    ("CRContactPrefsUpdated", "Contact preferences updated"),
    ("CRContactRequestRejected", ""),
    ("CRContactsList", "Contacts"),
    ("CRGroupDeletedUser", "User deleted group"),
    ("CRGroupLink", ""),
    ("CRGroupLinkCreated", ""),
    ("CRGroupLinkDeleted", ""),
    ("CRGroupCreated", ""),
    ("CRGroupMembers", ""),
    ("CRGroupUpdated", ""),
    ("CRGroupsList", "Groups"),
    ("CRInvitation", "One-time invitation"),
    ("CRLeftMemberUser", "User left group"),
    ("CRMemberAccepted", "Member accepted to group"),
    ("CRMembersBlockedForAllUser", "Members blocked for all by admin"),
    ("CRMembersRoleUser", "Members role changed by user"),
    ("CRNewChatItems", "New messages"),
    ("CRRcvFileAccepted", "File accepted to be received"),
    ("CRRcvFileAcceptedSndCancelled", "File accepted, but no longer sent"),
    ("CRRcvFileCancelled", "Cancelled receiving file"),
    ("CRSentConfirmation", "Confirmation sent to one-time invitation"),
    ("CRSentGroupInvitation", "Group invitation sent"),
    ("CRSentInvitation", "Invitation sent to contact address"),
    ("CRSndFileCancelled", "Cancelled sending file"),
    ("CRUserAcceptedGroupSent", "User accepted group invitation"),
    ("CRUserContactLink", "User contact address"),
    ("CRUserContactLinkCreated", "User contact address created"),
    ("CRUserContactLinkDeleted", "User contact address deleted"),
    ("CRUserContactLinkUpdated", "User contact address updated"),
    ("CRUserDeletedMembers", "Members deleted"),
    ("CRUserProfileUpdated", "User profile updated"),
    ("CRUserProfileNoChange", "User profile was not changed"),
    ("CRUsersList", "Users")
    -- ("CRApiChat", "Chat and messages"),
    -- ("CRApiChats", "Chats with the most recent messages"),
    -- ("CRChatCleared", ""),
    -- ("CRChatItemInfo", "Message information"),
    -- ("CRChatItems", "The most recent messages"),
    -- ("CRConnectionAliasUpdated", ""),
    -- ("CRContactAliasUpdated", ""),
    -- ("CRContactRatchetSyncStarted", "Contact encryption synchronization started"),
    -- ("CRGroupAliasUpdated", ""),
    -- ("CRGroupMemberRatchetSyncStarted", "Member encryption synchronization started"),
    -- ("CRItemsReadForChat", "Messages marked as read"),
    -- ("CRReactionMembers", "Members who set reaction on the message"),
  ]

undocumentedResponses :: [ConsName]
undocumentedResponses =
  [ "CRAgentQueuesInfo",
    "CRAgentServersSummary",
    "CRAgentSubs",
    "CRAgentSubsDetails",
    "CRAgentSubsTotal",
    "CRAgentWorkersDetails",
    "CRAgentWorkersSummary",
    "CRApiChat",
    "CRApiChats",
    "CRAppSettings",
    "CRArchiveExported",
    "CRArchiveImported",
    "CRBroadcastSent",
    "CRCallInvitations",
    "CRChatCleared",
    "CRChatHelp",
    "CRChatItemId",
    "CRChatItemInfo",
    "CRChatItems",
    "CRChatItemTTL",
    "CRChatRunning",
    "CRChats",
    "CRChatStarted",
    "CRChatStopped",
    "CRConnectionsDiff",
    "CRChatTags",
    "CRConnectionAliasUpdated",
    "CRConnectionIncognitoUpdated",
    "CRConnectionUserChanged",
    "CRConnectionVerified",
    "CRConnNtfMessages",
    "CRContactAliasUpdated",
    "CRContactCode",
    "CRContactInfo",
    "CRContactRatchetSyncStarted",
    "CRContactSwitchAborted",
    "CRContactSwitchStarted",
    "CRContactUserChanged",
    "CRCurrentRemoteHost",
    "CRCustomChatResponse",
    "CRDebugLocks",
    "CRFileTransferStatus",
    "CRFileTransferStatusXFTP",
    "CRForwardPlan",
    "CRGroupAliasUpdated",
    "CRGroupChatItemsDeleted",
    "CRGroupDescription",
    "CRGroupInfo",
    "CRGroupMemberCode",
    "CRGroupMemberInfo",
    "CRGroupMemberRatchetSyncStarted",
    "CRGroupMemberSwitchAborted",
    "CRGroupMemberSwitchStarted",
    "CRGroupProfile",
    "CRGroupUserChanged",
    "CRItemsReadForChat",
    "CRJoinedGroupMember",
    "CRMemberSupportChatRead",
    "CRMemberSupportChatDeleted",
    "CRMemberSupportChats",
    "CRNetworkConfig",
    "CRNetworkStatuses",
    "CRNewMemberContact",
    "CRNewMemberContactSentInv",
    "CRMemberContactAccepted",
    "CRNewPreparedChat",
    "CRNtfConns",
    "CRNtfToken",
    "CRNtfTokenStatus",
    "CRQueueInfo",
    "CRRcvStandaloneFileCreated",
    "CRReactionMembers",
    "CRRemoteCtrlConnected",
    "CRRemoteCtrlConnecting",
    "CRRemoteCtrlList",
    "CRRemoteFileStored",
    "CRRemoteHostList",
    "CRRemoteHostStarted",
    "CRSentInvitationToContact",
    "CRServerOperatorConditions",
    "CRServerTestResult",
    "CRSlowSQLQueries",
    "CRSndStandaloneFileCreated",
    "CRSQLResult",
    "CRStandaloneFileInfo",
    "CRStartedConnectionToContact",
    "CRStartedConnectionToGroup",
    "CRTagsUpdated",
    "CRUsageConditions",
    "CRUserPrivacy",
    "CRUserProfile",
    "CRUserProfileImage",
    "CRUserServers",
    "CRUserServersValidation",
    "CRVersionInfo",
    "CRWelcome"
  ]
