{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Responses where

import API.TypeInfo
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller

data CRDoc = CRDoc {consName :: ConsName, responseDescr :: Text}

instance ConstructorName CRDoc where consName' CRDoc {consName} = consName

chatResponsesDocs :: [CRDoc]
chatResponsesDocs = map toResp chatResponsesDocsData
  where
    toResp (consName, responseDescr) = CRDoc {consName, responseDescr}

deriving instance Generic ChatResponse

chatResponsesTypeInfo :: [RecordTypeInfo]
chatResponsesTypeInfo = recordTypesInfo @ChatResponse

chatResponsesDocsData :: [(ConsName, Text)]
chatResponsesDocsData =
  [ ("CRAcceptingContactRequest", "Contact request accepted"),
    ("CRActiveUser", "Active user profile"),
    ("CRApiChat", "Chat and messages"),
    ("CRApiChats", "Chats with the most recent messages"),
    ("CRChatCleared", ""),
    ("CRChatItemInfo", "Message information"),
    ("CRChatItemNotChanged", "Message not changed"),
    ("CRChatItemReaction", "Message reaction"),
    ("CRChatItemUpdated", "Message updated"),
    ("CRChatItems", "The most recent messages"),
    ("CRChatItemsDeleted", "Messages deleted"),
    ("CRCmdOk", "Ok"),
    ("CRConnectionAliasUpdated", ""),
    ("CRConnectionPlan", "Connection link information"),
    ("CRContactAliasUpdated", ""),
    ("CRContactAlreadyExists", ""),
    ("CRContactConnectionDeleted", "Connection deleted"),
    ("CRContactDeleted", ""),
    ("CRContactPrefsUpdated", "Contact preferences updated"),
    ("CRContactRatchetSyncStarted", "Contact encryption synchronization started"),
    ("CRContactRequestRejected", ""),
    ("CRContactsList", "Contacts"),
    ("CRGroupAliasUpdated", ""),
    ("CRGroupCreated", ""),
    ("CRGroupDeletedUser", "User deleted group"),
    ("CRGroupLink", ""),
    ("CRGroupLinkCreated", ""),
    ("CRGroupLinkDeleted", ""),
    ("CRGroupMemberRatchetSyncStarted", "Member encryption synchronization started"),
    ("CRGroupMembers", ""),
    ("CRGroupUpdated", ""),
    ("CRGroupsList", "Group"),
    ("CRInvitation", "One-time invitation"),
    ("CRItemsReadForChat", "Messages marked as read"),
    ("CRLeftMemberUser", "User left group"),
    ("CRMemberAccepted", "Member accepted to group"),
    ("CRMembersBlockedForAllUser", "Members blocked for all by admin"),
    ("CRMembersRoleUser", "Members role changed by user"),
    ("CRNewChatItems", "New messages"),
    ("CRRcvFileAccepted", "File accepted to be received"),
    ("CRRcvFileAcceptedSndCancelled", "File accepted, but no longer sent"),
    ("CRRcvFileCancelled", "Cancelled receiving file"),
    ("CRReactionMembers", "Members who set reaction on the message"),
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
    ("CRUsersList", "Users")
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
    "CRAppSettings",
    "CRArchiveExported",
    "CRArchiveImported",
    "CRBroadcastSent",
    "CRCallInvitations",
    "CRChatHelp",
    "CRChatItemId",
    "CRChatItemTTL",
    "CRChatRunning",
    "CRChatStarted",
    "CRChatStopped",
    "CRChatTags",
    "CRChats",
    "CRConnNtfMessages",
    "CRConnectionIncognitoUpdated",
    "CRConnectionUserChanged",
    "CRConnectionVerified",
    "CRContactCode",
    "CRContactInfo",
    "CRContactSwitchAborted",
    "CRContactSwitchStarted",
    "CRContactUserChanged",
    "CRCurrentRemoteHost",
    "CRCustomChatResponse",
    "CRDebugLocks",
    "CRFileTransferStatus",
    "CRFileTransferStatusXFTP",
    "CRForwardPlan",
    "CRGroupChatItemsDeleted",
    "CRGroupDescription",
    "CRGroupInfo",
    "CRGroupMemberCode",
    "CRGroupMemberInfo",
    "CRGroupMemberSwitchAborted",
    "CRGroupMemberSwitchStarted",
    "CRGroupProfile",
    "CRGroupUserChanged",
    "CRJoinedGroupMember",
    "CRMemberSupportChatDeleted",
    "CRMemberSupportChats",
    "CRNetworkConfig",
    "CRNetworkStatuses",
    "CRNewMemberContact",
    "CRNewMemberContactSentInv",
    "CRNewPreparedChat",
    "CRNtfConns",
    "CRNtfToken",
    "CRNtfTokenStatus",
    "CRQueueInfo",
    "CRRcvStandaloneFileCreated",
    "CRRemoteCtrlConnected",
    "CRRemoteCtrlConnecting",
    "CRRemoteCtrlList",
    "CRRemoteFileStored",
    "CRRemoteHostList",
    "CRRemoteHostStarted",
    "CRSQLResult",
    "CRSentInvitationToContact",
    "CRServerOperatorConditions",
    "CRServerTestResult",
    "CRSlowSQLQueries",
    "CRSndStandaloneFileCreated",
    "CRStandaloneFileInfo",
    "CRStartedConnectionToContact",
    "CRStartedConnectionToGroup",
    "CRTagsUpdated",
    "CRUsageConditions",
    "CRUserPrivacy",
    "CRUserProfile",
    "CRUserProfileImage",
    "CRUserProfileNoChange",
    "CRUserServers",
    "CRUserServersValidation",
    "CRVersionInfo",
    "CRWelcome"
  ]
