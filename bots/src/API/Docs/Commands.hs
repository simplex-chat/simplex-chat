{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Commands where

import API.TypeInfo
import Data.ByteString.Char8 (ByteString)
import Data.String
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller

chatCommandsDocs :: [CCCategory]
chatCommandsDocs = map toCategory chatCommandsDocsData
  where
    toCategory (categoryName, categoryDescr, commandsData) =
      CCCategory {categoryName, categoryDescr, commands = map toCmd commandsData}
    toCmd (consName, commandDescr, syntax, responses, errors, network) =
      CCDoc {consName, commandDescr, syntax, responses, errors, network}

deriving instance Generic ChatCommand

chatCommandsTypeInfo :: [RecordTypeInfo]
chatCommandsTypeInfo = gTypeInfo @(Rep ChatCommand)

data CCCategory = CCCategory
  { categoryName :: Text,
    categoryDescr :: Text,
    commands :: [CCDoc]
  }

data CCDoc = CCDoc
  { consName :: ConsName,
    commandDescr :: Text,
    syntax :: ByteString,
    responses :: [String],
    errors :: [TypeDoc],
    network :: Maybe UsesNetwork
  }

instance ConstructorName CCDoc where consName' CCDoc {consName} = consName

data TypeDoc = TD
  { consName :: ConsName,
    description :: Text
  }

type TypeDocTuple = (ConsName, Text)

data UsesNetwork = UNBackground | UNInteractive

instance IsString TypeDoc where fromString s = TD s ""


-- category name, category description, commands
-- inner: constructor, syntax, description, responses, errors (ChatErrorType constructors)
chatCommandsDocsData :: [(Text, Text, [(ConsName, Text, ByteString, [String], [TypeDoc], Maybe UsesNetwork)])]
chatCommandsDocsData =
  [ ( "Address commands",
      "Bots can use these commands to automatically check and create address when initialized",
      [ ("APICreateMyAddress", "Create bot address.", "", ["CRUserContactLinkCreated"], [], Just UNInteractive),
        ("APIDeleteMyAddress", "Delete bot address.", "", ["CRUserContactLinkDeleted"], [], Just UNBackground),
        ("APIShowMyAddress", "Get bot address and settings.", "", ["CRUserContactLink"], [], Nothing),
        ("APISetProfileAddress", "Add address to bot profile.", "", ["CRUserProfileUpdated"], [], Just UNInteractive),
        ("APISetAddressSettings", "Set bot address settings.", "", ["CRUserContactLinkUpdated"], [], Just UNInteractive)
      ]
    ),
    ( "Message commands",
      "Commands to send, update, delete, moderate messages and set message reactions",
      [ ("APISendMessages", "Send messages.", "", ["CRNewChatItems"], [], Just UNBackground),
        ("APIReportMessage", "Report message.", "", ["CRNewChatItems"], [], Just UNBackground),
        ("APIUpdateChatItem", "Update message.", "", ["CRChatItemUpdated", "CRChatItemNotChanged"], ["CEInvalidChatItemUpdate"], Just UNBackground),
        ("APIDeleteChatItem", "Delete message.", "", ["CRChatItemsDeleted"], [], Just UNBackground),
        ("APIDeleteMemberChatItem", "Moderate message.", "", ["CRChatItemsDeleted"], [], Just UNBackground),
        ("APIChatItemReaction", "Add/remove message reaction.", "", ["CRChatItemReaction"], [], Just UNBackground),
        ("APIGetReactionMembers", "Get reaction members.", "", ["CRReactionMembers"], [], Nothing)
      ]
    ),
    ( "File commands",
      "Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.",
      [ ("ReceiveFile", "Receive file.", "", ["CRRcvFileAccepted", "CRRcvFileAcceptedSndCancelled"], [], Nothing),
        ("CancelFile", "Cancel file.", "", ["CRSndFileCancelled", "CRRcvFileCancelled"], ["CEFileCancel"], Just UNBackground)
      ]
    ),
    ( "Group commands",
      "Commands to create and manage groups. These commands have to be used to manage business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.",
      [ ("APINewGroup", "Create group.", "", ["CRGroupCreated"], [], Nothing),
        ("APIAddMember", "Add contact to group.", "", ["CRSentGroupInvitation"], [], Just UNInteractive),
        ("APIJoinGroup", "Join group.", "", ["CRUserAcceptedGroupSent"], [], Just UNInteractive),
        ("APIAcceptMember", "Accept group member.", "", ["CRMemberAccepted"], ["CEGroupMemberNotActive"], Just UNBackground),
        ("APIMembersRole", "Set members role.", "", ["CRMembersRoleUser"], [], Just UNBackground),
        ("APIBlockMembersForAll", "Block members.", "", ["CRMembersBlockedForAllUser"], [], Just UNBackground),
        ("APIRemoveMembers", "Remove members.", "", ["CRUserDeletedMembers"], ["CEGroupMemberNotFound"], Just UNBackground),
        ("APILeaveGroup", "Leave group.", "", ["CRLeftMemberUser"], [], Just UNBackground),
        ("APIListMembers", "Get group members.", "", ["CRGroupMembers"], [], Nothing),
        ("APIUpdateGroupProfile", "Update group profile.", "", ["CRGroupUpdated"], [], Just UNBackground)
      ]
    ),
    ( "Group link commands",
      "These commands can be used by bots that manage multiple public groups",
      [ ("APICreateGroupLink", "Create group link.", "", ["CRGroupLinkCreated"], [], Just UNInteractive),
        ("APIGroupLinkMemberRole", "Set member role for group link.", "", ["CRGroupLink"], [], Nothing),
        ("APIDeleteGroupLink", "Delete group link.", "", ["CRGroupLinkDeleted"], [], Just UNBackground),
        ("APIGetGroupLink", "Get group link.", "", ["CRGroupLink"], [], Nothing)
      ]
    ),
    ( "Connection commands",
      "These commands may be used to establish connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.",
      [ ("APIAddContact", "Create 1-time invitation link.", "", ["CRInvitation"], [], Just UNInteractive),
        ("APIConnectPlan", "Determine SimpleX link type and if the bot is already connected via this link.", "", ["CRConnectionPlan"], [], Just UNInteractive),
        ("APIConnect", "Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link", "", ["CRSentConfirmation", "CRContactAlreadyExists", "CRSentInvitation"], [], Just UNInteractive),
        ("APIAcceptContact", "Accept contact request.", "", ["CRAcceptingContactRequest"], [], Just UNInteractive),
        ("APIRejectContact", "Reject contact request. The user who sent the request is **not notified**.", "", ["CRContactRequestRejected"], [], Nothing)
      ]
    ),
    ( "User profile commands",
      "Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).",
      [ ("ShowActiveUser", "Get active user profile", "/user", ["CRActiveUser"], [], Nothing),
        ("CreateActiveUser", "Create new user profile", "/_create user <json(NewUser)>", ["CRActiveUser"], ["CEUserExists", "CEInvalidDisplayName"], Nothing),
        ("ListUsers", "Get all user profiles", "/users", ["CRUsersList"], [], Nothing),
        ("APISetActiveUser", "Set active user profile", "/_user <UserId>[ <quoted(UserPwd)>]", ["CRActiveUser"], ["CEChatNotStarted"], Nothing),
        ("APIDeleteUser", "Delete user profile.", "", ["CRCmdOk"], [], Just UNBackground),
        ("APIUpdateProfile", "Update user profile.", "", ["CRUserProfileUpdated"], [], Just UNBackground)
      ]
    ),
    ( "Chat commands",
      "Commands to get and to manage coversations.",
      [ ("APIGetChats", "Get chats.", "", ["CRApiChats"], [], Nothing),
        ("APIGetChat", "Get chat.", "", ["CRApiChat"], [], Nothing),
        ("APIGetChatItems", "Get the most recent messages from all chats.", "", ["CRChatItems"], [], Nothing),
        ("APIGetChatItemInfo", "Get message information.", "", ["CRChatItemInfo"], [], Nothing),
        ("APIChatRead", "Mark chat as read.", "", ["CRCmdOk"], [], Nothing),
        ("APIChatItemsRead", "Mark items as read.", "", ["CRItemsReadForChat"], [], Nothing),
        ("APIChatUnread", "Mark chat as unread.", "", ["CRCmdOk"], [], Nothing),
        ("APIDeleteChat", "Delete chat.", "", ["CRContactDeleted", "CRContactConnectionDeleted", "CRGroupDeletedUser"], [], Just UNBackground),
        ("APIClearChat", "Clear chat.", "", ["CRChatCleared"], [], Nothing),
        ("APISetContactPrefs", "Set contact preferences.", "", ["CRContactPrefsUpdated"], [], Just UNBackground),
        ("APISetContactAlias", "Set contact alias.", "", ["CRContactAliasUpdated"], [], Nothing),
        ("APISetGroupAlias", "Set group alias.", "", ["CRGroupAliasUpdated"], [], Nothing),
        ("APISetConnectionAlias", "Set connection alias.", "", ["CRConnectionAliasUpdated"], [], Nothing),
        ("APISetChatTTL", "Set TTL for chat messages.", "", ["CRCmdOk"], [], Nothing),
        ("APISetChatSettings", "Set chat settings.", "", ["CRCmdOk"], [], Nothing),
        ("APISyncContactRatchet", "Synchronize encryption with contact.", "", ["CRContactRatchetSyncStarted"], [], Just UNBackground),
        ("APISyncGroupMemberRatchet", "Synchronize encryption with member.", "", ["CRGroupMemberRatchetSyncStarted"], [], Just UNBackground),
        ("APIListContacts", "Get contacts.", "", ["CRContactsList"], [], Nothing),
        ("APIListGroups", "Get groups.", "", ["CRGroupsList"], [], Nothing)
      ]
    )
  ]

cliCommands :: [ConsName]
cliCommands =
  [ "SetActiveUser",
    "SetUserContactReceipts",
    "SetUserGroupReceipts",
    "HideUser",
    "UnhideUser",
    "MuteUser",
    "UnmuteUser",
    "DeleteUser",
    "ReportMessage",
    "UserRead",
    "SendCallInvitation",
    "SetChatTTL",
    "GetChatTTL",
    "SetShowMessages",
    "SetSendReceipts",
    "SetShowMemberMessages",
    "ContactInfo",
    "ShowGroupInfo",
    "GroupMemberInfo",
    "ContactQueueInfo",
    "GroupMemberQueueInfo",
    "SwitchContact",
    "SwitchGroupMember",
    "AbortSwitchContact",
    "AbortSwitchGroupMember",
    "SyncContactRatchet",
    "SyncGroupMemberRatchet",
    "GetContactCode",
    "GetGroupMemberCode",
    "VerifyContact",
    "VerifyGroupMember",
    "EnableContact",
    "EnableGroupMember",
    "ChatHelp",
    "Welcome",
    "AddContact",
    "Connect",
    "ConnectSimplex",
    "DeleteContact",
    "ClearContact",
    "ListContacts",
    "CreateMyAddress",
    "DeleteMyAddress",
    "ShowMyAddress",
    "SetProfileAddress",
    "SetAddressSettings",
    "AcceptContact",
    "RejectContact",
    "ForwardMessage",
    "ForwardGroupMessage",
    "ForwardLocalMessage",
    "SendMessage",
    "SendMemberContactMessage",
    "SendLiveMessage",
    "SendMessageQuote",
    "SendMessageBroadcast",
    "DeleteMessage",
    "DeleteMemberMessage",
    "EditMessage",
    "UpdateLiveMessage",
    "ReactToMessage",
    "NewGroup",
    "AddMember",
    "JoinGroup",
    "AcceptMember",
    "MemberRole",
    "BlockForAll",
    "RemoveMembers",
    "LeaveGroup",
    "DeleteGroup",
    "ClearGroup",
    "ListMembers",
    "ListMemberSupportChats",
    "ListGroups",
    "UpdateGroupNames",
    "ShowGroupProfile",
    "UpdateGroupDescription",
    "ShowGroupDescription",
    "CreateGroupLink",
    "GroupLinkMemberRole",
    "DeleteGroupLink",
    "ShowGroupLink",
    "SendGroupMessageQuote",
    "ClearNoteFolder",
    "LastChats",
    "LastMessages",
    "LastChatItemId",
    "ShowChatItem",
    "ShowChatItemInfo",
    "ShowLiveItems",
    "SendFile",
    "SendImage",
    "ForwardFile",
    "ForwardImage",
    "SendFileDescription",
    "FileStatus",
    "ShowProfile",
    "UpdateProfile",
    "UpdateProfileImage",
    "ShowProfileImage",
    "SetUserFeature",
    "SetContactFeature",
    "SetGroupFeature",
    "SetGroupFeatureRole",
    "SetGroupMemberAdmissionReview",
    "SetUserTimedMessages",
    "SetContactTimedMessages",
    "SetGroupTimedMessages",
    "SetLocalDeviceName",
    "QuitChat",
    "ShowVersion"
  ]

undocumentedCommands :: [ConsName]
undocumentedCommands =
  [ "SetAllContactReceipts",
    "APISetUserContactReceipts",
    "APISetUserGroupReceipts",
    "APIHideUser",
    "APIUnhideUser",
    "APIMuteUser",
    "APIUnmuteUser",
    "StartChat",
    "CheckChatRunning",
    "APIStopChat",
    "APIActivateChat",
    "APISuspendChat",
    "ResubscribeAllConnections",
    "SetTempFolder",
    "SetFilesFolder",
    "SetRemoteHostsFolder",
    "APISetAppFilePaths",
    "APISetEncryptLocalFiles",
    "SetContactMergeEnabled",
    "APIExportArchive",
    "ExportArchive",
    "APIImportArchive",
    "APIDeleteStorage",
    "APIStorageEncryption",
    "TestStorageEncryption",
    "SlowSQLQueries",
    "ExecChatStoreSQL",
    "ExecAgentStoreSQL",
    "APISaveAppSettings",
    "APIGetAppSettings",
    "APIGetChatTags",
    "APICreateChatTag",
    "APISetChatTags",
    "APIDeleteChatTag",
    "APIUpdateChatTag",
    "APIReorderChatTags",
    "APICreateChatItems",
    "APIPlanForwardChatItems",
    "APIForwardChatItems",
    "APIArchiveReceivedReports",
    "APIDeleteReceivedReports",
    "APIUserRead",
    "APISendCallInvitation",
    "APIRejectCall",
    "APISendCallOffer",
    "APISendCallAnswer",
    "APISendCallExtraInfo",
    "APIEndCall",
    "APIGetCallInvitations",
    "APICallStatus",
    "APIGetNetworkStatuses",
    "APISetUserUIThemes",
    "APISetChatUIThemes",
    "APIGetNtfToken",
    "APIRegisterToken",
    "APIVerifyToken",
    "APICheckToken",
    "APIDeleteToken",
    "APIGetNtfConns",
    "APIGetConnNtfMessages",
    "APIDeleteMemberSupportChat",
    "APIAddGroupShortLink",
    "APICreateMemberContact",
    "APISendMemberContactInvitation",
    "GetUserProtoServers",
    "SetUserProtoServers",
    "APITestProtoServer",
    "TestProtoServer",
    "APIGetServerOperators",
    "APISetServerOperators",
    "SetServerOperators",
    "APIGetUserServers",
    "APISetUserServers",
    "APIValidateServers",
    "APIGetUsageConditions",
    "APISetConditionsNotified",
    "APIAcceptConditions",
    "APISetChatItemTTL",
    "SetChatItemTTL",
    "APIGetChatItemTTL",
    "GetChatItemTTL",
    "APISetNetworkConfig",
    "APIGetNetworkConfig",
    "SetNetworkConfig",
    "APISetNetworkInfo",
    "ReconnectAllServers",
    "ReconnectServer",
    "APISetMemberSettings",
    "APIContactInfo",
    "APIGroupInfo",
    "APIGroupMemberInfo",
    "APIContactQueueInfo",
    "APIGroupMemberQueueInfo",
    "APISwitchContact",
    "APISwitchGroupMember",
    "APIAbortSwitchContact",
    "APIAbortSwitchGroupMember",
    "APIEnableContact",
    "APIEnableGroupMember",
    "APIGetContactCode",
    "APIGetGroupMemberCode",
    "APIVerifyContact",
    "APIVerifyGroupMember",
    "APISetConnectionIncognito",
    "APIChangeConnectionUser",
    "APIPrepareContact",
    "APIPrepareGroup",
    "APIChangePreparedContactUser",
    "APIChangePreparedGroupUser",
    "APIConnectPreparedContact",
    "APIConnectPreparedGroup",
    "APIConnectContactViaAddress",
    "APIAddMyAddressShortLink",
    "SetFileToReceive",
    "ListRemoteHosts",
    "StartRemoteHost",
    "SwitchRemoteHost",
    "StopRemoteHost",
    "DeleteRemoteHost",
    "StoreRemoteFile",
    "GetRemoteFile",
    "ConnectRemoteCtrl",
    "FindKnownRemoteCtrl",
    "ConfirmRemoteCtrl",
    "VerifyRemoteCtrlSession",
    "ListRemoteCtrls",
    "StopRemoteCtrl",
    "DeleteRemoteCtrl",
    "APIUploadStandaloneFile",
    "APIDownloadStandaloneFile",
    "APIStandaloneFileInfo",
    "DebugLocks",
    "DebugEvent",
    "GetAgentSubsTotal",
    "GetAgentServersSummary",
    "ResetAgentServersStats",
    "GetAgentSubs",
    "GetAgentSubsDetails",
    "GetAgentWorkers",
    "GetAgentWorkersDetails",
    "GetAgentQueuesInfo",
    "CustomChatCommand"
  ]
