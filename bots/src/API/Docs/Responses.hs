{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Responses where

import API.TypeInfo
import GHC.Generics
import Simplex.Chat.Controller

data CRDoc = CRDoc {consName :: ConsName, responseDescr :: String}

instance ConstructorName CRDoc where consName' CRDoc {consName} = consName

chatResponsesDocs :: [CRDoc]
chatResponsesDocs = map toResp chatResponsesDocsData
  where
    toResp (consName, responseDescr) = CRDoc {consName, responseDescr}

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
    ("CRConnectionPlan", "Connection link information"),
    ("CRContactAlreadyExists", ""),
    ("CRContactConnectionDeleted", "Connection deleted"),
    ("CRContactDeleted", ""),
    ("CRContactRequestRejected", ""),
    ("CRContactsList", "Contacts"),
    ("CRGroupDeletedUser", "User deleted group"),
    ("CRGroupLink", ""),
    ("CRGroupLinkCreated", ""),
    ("CRGroupLinkDeleted", ""),
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
    ("CRUsersList", "Users")
    -- ("CRApiChat", "Chat and messages"),
    -- ("CRApiChats", "Chats with the most recent messages"),
    -- ("CRChatCleared", ""),
    -- ("CRChatItemInfo", "Message information"),
    -- ("CRChatItems", "The most recent messages"),
    -- ("CRConnectionAliasUpdated", ""),
    -- ("CRContactAliasUpdated", ""),
    -- ("CRContactPrefsUpdated", "Contact preferences updated"),
    -- ("CRContactRatchetSyncStarted", "Contact encryption synchronization started"),
    -- ("CRGroupAliasUpdated", ""),
    -- ("CRGroupCreated", ""),
    -- ("CRGroupMemberRatchetSyncStarted", "Member encryption synchronization started"),
    -- ("CRGroupMembers", ""),
    -- ("CRGroupUpdated", ""),
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
    "CRChatTags",
    "CRConnectionAliasUpdated",
    "CRConnectionIncognitoUpdated",
    "CRConnectionUserChanged",
    "CRConnectionVerified",
    "CRConnNtfMessages",
    "CRContactAliasUpdated",
    "CRContactCode",
    "CRContactInfo",
    "CRContactPrefsUpdated",
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
    "CRGroupCreated",
    "CRGroupDescription",
    "CRGroupInfo",
    "CRGroupMemberCode",
    "CRGroupMemberInfo",
    "CRGroupMemberRatchetSyncStarted",
    "CRGroupMembers",
    "CRGroupMemberSwitchAborted",
    "CRGroupMemberSwitchStarted",
    "CRGroupProfile",
    "CRGroupUpdated",
    "CRGroupUserChanged",
    "CRItemsReadForChat",
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
    "CRUserProfileNoChange",
    "CRUserServers",
    "CRUserServersValidation",
    "CRVersionInfo",
    "CRWelcome"
  ]
