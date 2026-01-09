{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Docs.Commands where

import API.Docs.Responses
import API.Docs.Syntax.Types
import API.Docs.Types
import API.TypeInfo
import Data.String
import Data.List (find)
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Messaging.Parsers (dropPrefix, fstToLower)

chatCommandsDocs :: [CCCategory]
chatCommandsDocs = map toCategory chatCommandsDocsData
  where
    toCategory (categoryName, categoryDescr, commandsData) =
      CCCategory {categoryName, categoryDescr, commands = map toCmd commandsData}
    toCmd (consName, hideParams, commandDescr, respNames, errors, network, syntax) = case find ((consName ==) . consName') chatCommandsTypeInfo of
      Just RecordTypeInfo {fieldInfos} ->
        let fields = filter ((`notElem` hideParams) . fieldName') $ map (toAPIField consName) fieldInfos
            commandType = ATUnionMember (fstToLower consName) fields
            findResp name = case find ((name ==) . consName') chatResponsesDocs of
              Just resp -> resp
              Nothing -> error $ "Missing response doc for " <> name
            responses = map findResp respNames
            errors' = map (\(TD err descr) -> TD (dropPrefix "CE" err) descr) errors
         in CCDoc {consName, commandType, commandDescr, responses, errors = errors', network, syntax}
      Nothing -> error $ "Missing command type info for " <> consName

deriving instance Generic ChatCommand

chatCommandsTypeInfo :: [RecordTypeInfo]
chatCommandsTypeInfo = recordTypesInfo @ChatCommand

data CCCategory = CCCategory
  { categoryName :: String,
    categoryDescr :: String,
    commands :: [CCDoc]
  }

data CCDoc = CCDoc
  { consName :: ConsName,
    commandType :: ATUnionMember,
    commandDescr :: Text,
    responses :: [CRDoc],
    errors :: [ErrorTypeDoc],
    network :: Maybe UsesNetwork,
    syntax :: Expr
  }

instance ConstructorName CCDoc where consName' CCDoc {consName} = consName

data ErrorTypeDoc = TD
  { consName :: ConsName,
    description :: String
  }

data UsesNetwork = UNBackground | UNInteractive

instance IsString ErrorTypeDoc where fromString s = TD s ""


-- category name, category description, commands
-- inner: constructor, description, responses, errors (ChatErrorType constructors), network usage, syntax
chatCommandsDocsData :: [(String, String, [(ConsName, [String], Text, [ConsName], [ErrorTypeDoc], Maybe UsesNetwork, Expr)])]
chatCommandsDocsData =
  [ ( "Address commands",
      "Bots can use these commands to automatically check and create address when initialized",
      [ ("APICreateMyAddress", [], "Create bot address.", ["CRUserContactLinkCreated", "CRChatCmdError"], [], Just UNInteractive, "/_address " <> Param "userId"),
        ("APIDeleteMyAddress", [], "Delete bot address.", ["CRUserContactLinkDeleted", "CRChatCmdError"], [], Just UNBackground, "/_delete_address " <> Param "userId"),
        ("APIShowMyAddress", [], "Get bot address and settings.", ["CRUserContactLink", "CRChatCmdError"], [], Nothing, "/_show_address " <> Param "userId"),
        ("APISetProfileAddress", [], "Add address to bot profile.", ["CRUserProfileUpdated", "CRChatCmdError"], [], Just UNInteractive, "/_profile_address " <> Param "userId" <> " " <> OnOff "enable"),
        ("APISetAddressSettings", [], "Set bot address settings.", ["CRUserContactLinkUpdated", "CRChatCmdError"], [], Just UNInteractive, "/_address_settings " <> Param "userId" <> " " <> Json "settings")
      ]
    ),
    ( "Message commands",
      "Commands to send, update, delete, moderate messages and set message reactions",
      [ ("APISendMessages", [], "Send messages.", ["CRNewChatItems", "CRChatCmdError"], [], Just UNBackground, "/_send " <> Param "sendRef" <> OnOffParam "live" "liveMessage" (Just False) <> Optional "" (" ttl=" <> Param "$0") "ttl" <> " json " <> Json "composedMessages"),
        ( "APIUpdateChatItem",
          [],
          "Update message.",
          ["CRChatItemUpdated", "CRChatItemNotChanged", "CRChatCmdError"],
          [TD "CEInvalidChatItemUpdate" "Not user's message or cannot be edited"],
          Just UNBackground,
          "/_update item " <> Param "chatRef" <> " " <> Param "chatItemId" <> OnOffParam "live" "liveMessage" (Just False) <> " json " <> Json "updatedMessage"
        ),
        ("APIDeleteChatItem", [], "Delete message.", ["CRChatItemsDeleted", "CRChatCmdError"], [], Just UNBackground, "/_delete item " <> Param "chatRef" <> " " <> Join ',' "chatItemIds" <> " " <> Param "deleteMode"),
        ("APIDeleteMemberChatItem", [], "Moderate message. Requires Moderator role (and higher than message author's).", ["CRChatItemsDeleted", "CRChatCmdError"], [], Just UNBackground, "/_delete member item #" <> Param "groupId" <> " " <> Join ',' "chatItemIds"),
        ("APIChatItemReaction", [], "Add/remove message reaction.", ["CRChatItemReaction", "CRChatCmdError"], [], Just UNBackground, "/_reaction " <> Param "chatRef" <> " " <> Param "chatItemId" <> " " <> OnOff "add" <> " " <> Json "reaction")
      ]
    ),
    ( "File commands",
      "Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.",
      [ ("ReceiveFile", [], "Receive file.", ["CRRcvFileAccepted", "CRRcvFileAcceptedSndCancelled", "CRChatCmdError"], [], Nothing, "/freceive " <> Param "fileId" <> OnOffParam "approved_relays" "userApprovedRelays" (Just False) <> OnOffParam "encrypt" "storeEncrypted" Nothing <> OnOffParam "inline" "fileInline" Nothing <> Optional "" (" " <> Param "$0") "filePath"),
        ("CancelFile", [], "Cancel file.", ["CRSndFileCancelled", "CRRcvFileCancelled", "CRChatCmdError"], [TD "CEFileCancel" "Cannot cancel file"], Just UNBackground, "/fcancel " <> Param "fileId")
      ]
    ),
    ( "Group commands",
      "Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.",
      [ ("APIAddMember", [], "Add contact to group. Requires bot to have Admin role.", ["CRSentGroupInvitation", "CRChatCmdError"], [], Just UNInteractive, "/_add #" <> Param "groupId" <> " " <> Param "contactId" <> " " <> Param "memberRole"),
        ("APIJoinGroup", ["enableNtfs"], "Join group.", ["CRUserAcceptedGroupSent", "CRChatCmdError"], [], Just UNInteractive, "/_join #" <> Param "groupId"),
        ("APIAcceptMember", [], "Accept group member. Requires Admin role.", ["CRMemberAccepted", "CRChatCmdError"], [TD "CEGroupMemberNotActive" "Member is not connected yet"], Just UNBackground, "/_accept member #" <> Param "groupId" <> " " <> Param "groupMemberId" <> " " <> Param "memberRole"),
        ("APIMembersRole", [], "Set members role. Requires Admin role.", ["CRMembersRoleUser", "CRChatCmdError"], [], Just UNBackground, "/_member role #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> " " <> Param "memberRole"),
        ("APIBlockMembersForAll", [], "Block members. Requires Moderator role.", ["CRMembersBlockedForAllUser", "CRChatCmdError"], [], Just UNBackground, "/_block #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> OnOffParam "blocked" "blocked" Nothing),
        ("APIRemoveMembers", [], "Remove members. Requires Admin role.", ["CRUserDeletedMembers", "CRChatCmdError"], ["CEGroupMemberNotFound"], Just UNBackground, "/_remove #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> OnOffParam "messages" "withMessages" (Just False)),
        ("APILeaveGroup", [], "Leave group.", ["CRLeftMemberUser", "CRChatCmdError"], [], Just UNBackground, "/_leave #" <> Param "groupId"),
        ("APIListMembers", [], "Get group members.", ["CRGroupMembers", "CRChatCmdError"], [], Nothing, "/_members #" <> Param "groupId"),
        ("APINewGroup", [], "Create group.", ["CRGroupCreated", "CRChatCmdError"], [], Nothing, "/_group " <> Param "userId" <> OnOffParam "incognito" "incognito" (Just False) <> " " <> Json "groupProfile"),
        ("APIUpdateGroupProfile", [], "Update group profile.", ["CRGroupUpdated", "CRChatCmdError"], [], Just UNBackground, "/_group_profile #" <> Param "groupId" <> " " <> Json "groupProfile")
      ]
    ),
    ( "Group link commands",
      "These commands can be used by bots that manage multiple public groups",
      [ ("APICreateGroupLink", [], "Create group link.", ["CRGroupLinkCreated", "CRChatCmdError"], [], Just UNInteractive, "/_create link #" <> Param "groupId" <> " " <> Param "memberRole"),
        ("APIGroupLinkMemberRole", [], "Set member role for group link.", ["CRGroupLink", "CRChatCmdError"], [], Nothing, "/_set link role #" <> Param "groupId" <> " " <> Param "memberRole"),
        ("APIDeleteGroupLink", [], "Delete group link.", ["CRGroupLinkDeleted", "CRChatCmdError"], [], Just UNBackground, "/_delete link #" <> Param "groupId"),
        ("APIGetGroupLink", [], "Get group link.", ["CRGroupLink", "CRChatCmdError"], [], Nothing, "/_get link #" <> Param "groupId")
      ]
    ),
    ( "Connection commands",
      "These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.",
      [ ("APIAddContact", [], "Create 1-time invitation link.", ["CRInvitation", "CRChatCmdError"], [], Just UNInteractive, "/_connect " <> Param "userId" <> OnOffParam "incognito" "incognito" (Just False)),
        ("APIConnectPlan", [], "Determine SimpleX link type and if the bot is already connected via this link.", ["CRConnectionPlan", "CRChatCmdError"], [], Just UNInteractive, "/_connect plan " <> Param "userId" <> " " <> Param "connectionLink"),
        ("APIConnect", [], "Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link", ["CRSentConfirmation", "CRContactAlreadyExists", "CRSentInvitation", "CRChatCmdError"], [], Just UNInteractive, "/_connect " <> Param "userId" <> Optional "" (" " <> Param "$0") "preparedLink_"),
        ("Connect", [], "Connect via SimpleX link as string in the active user profile.", ["CRSentConfirmation", "CRContactAlreadyExists", "CRSentInvitation", "CRChatCmdError"], [], Just UNInteractive, "/connect" <> Optional "" (" " <> Param "$0") "connLink_"),
        ("APIAcceptContact", ["incognito"], "Accept contact request.", ["CRAcceptingContactRequest", "CRChatCmdError"], [], Just UNInteractive, "/_accept " <> Param "contactReqId"),
        ("APIRejectContact", [], "Reject contact request. The user who sent the request is **not notified**.", ["CRContactRequestRejected", "CRChatCmdError"], [], Nothing, "/_reject " <> Param "contactReqId")
      ]
    ),
    ( "Chat commands",
      "Commands to list and delete conversations.",
      [ ("APIListContacts", [], "Get contacts.", ["CRContactsList", "CRChatCmdError"], [], Nothing, "/_contacts " <> Param "userId"),
        ("APIListGroups", [], "Get groups.", ["CRGroupsList", "CRChatCmdError"], [], Nothing, "/_groups " <> Param "userId" <> Optional "" (" @" <> Param "$0") "contactId_" <> Optional "" (" " <> Param "$0") "search"),
        ("APIDeleteChat", [], "Delete chat.", ["CRContactDeleted", "CRContactConnectionDeleted", "CRGroupDeletedUser", "CRChatCmdError"], [], Just UNBackground, "/_delete " <> Param "chatRef" <> " " <> Param "chatDeleteMode")
        -- ("APIChatItemsRead", [], "Mark items as read.", ["CRItemsReadForChat"], [], Nothing, ""),
        -- ("APIChatRead", [], "Mark chat as read.", ["CRCmdOk"], [], Nothing, ""),
        -- ("APIChatUnread", [], "Mark chat as unread.", ["CRCmdOk"], [], Nothing, ""),
        -- ("APIClearChat", [], "Clear chat.", ["CRChatCleared"], [], Nothing, ""),
        -- ("APIGetChat", [], "Get chat.", ["CRApiChat"], [], Nothing, ""),
        -- ("APIGetChatItemInfo", [], "Get message information.", ["CRChatItemInfo"], [], Nothing, ""),
        -- ("APIGetChatItems", [], "Get the most recent messages from all chats.", ["CRChatItems"], [], Nothing, ""),
        -- ("APIGetChats", [], "Get chats.", ["CRApiChats"], [], Nothing, ""),
        -- ("APISetChatSettings", [], "Set chat settings.", ["CRCmdOk"], [], Nothing, ""),
        -- ("APISetChatTTL", [], "Set TTL for chat messages.", ["CRCmdOk"], [], Nothing, ""),
        -- ("APISetConnectionAlias", [], "Set connection alias.", ["CRConnectionAliasUpdated"], [], Nothing, ""),
        -- ("APISetContactAlias", [], "Set contact alias.", ["CRContactAliasUpdated"], [], Nothing, ""),
        -- ("APISetContactPrefs", [], "Set contact preferences.", ["CRContactPrefsUpdated"], [], Just UNBackground, ""),
        -- ("APISetGroupAlias", [], "Set group alias.", ["CRGroupAliasUpdated"], [], Nothing, ""),
        -- ("APISyncContactRatchet", [], "Synchronize encryption with contact.", ["CRContactRatchetSyncStarted"], [], Just UNBackground, ""),
        -- ("APISyncGroupMemberRatchet", [], "Synchronize encryption with member.", ["CRGroupMemberRatchetSyncStarted"], [], Just UNBackground, ""),
      ]
    ),
    ( "User profile commands",
      "Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).",
      [ ("ShowActiveUser", [], "Get active user profile", ["CRActiveUser", "CRChatCmdError"], [], Nothing, "/user"),
        ( "CreateActiveUser",
          [],
          "Create new user profile",
          ["CRActiveUser", "CRChatCmdError"],
          [TD "CEUserExists" "User or contact with this name already exists", TD "CEInvalidDisplayName" "Invalid user display name"],
          Nothing,
          "/_create user " <> Json "newUser"
        ),
        ("ListUsers", [], "Get all user profiles", ["CRUsersList", "CRChatCmdError"], [], Nothing, "/users"),
        ("APISetActiveUser", [], "Set active user profile", ["CRActiveUser", "CRChatCmdError"], ["CEChatNotStarted"], Nothing, "/_user " <> Param "userId" <> Optional "" (" " <> Json "$0") "viewPwd"),
        ("APIDeleteUser", [], "Delete user profile.", ["CRCmdOk", "CRChatCmdError"], [], Just UNBackground, "/_delete user " <> Param "userId" <> OnOffParam "del_smp" "delSMPQueues" Nothing <> Optional "" (" " <> Json "$0") "viewPwd"),
        ("APIUpdateProfile", [], "Update user profile.", ["CRUserProfileUpdated", "CRUserProfileNoChange", "CRChatCmdError"], [], Just UNBackground, "/_profile " <> Param "userId" <> " " <> Json "profile"),
        ("APISetContactPrefs", [], "Configure chat preference overrides for the contact.", ["CRContactPrefsUpdated", "CRChatCmdError"], [], Just UNBackground, "/_set prefs @" <> Param "contactId" <> " " <> Json "preferences")
      ]
    )
  ]

cliCommands :: [ConsName]
cliCommands =
  [ "AbortSwitchContact",
    "AbortSwitchGroupMember",
    "AcceptContact",
    "AcceptMember",
    "AddContact",
    "AddMember",
    "BlockForAll",
    "ChatHelp",
    "ClearContact",
    "ClearGroup",
    "ClearNoteFolder",
    "ConnectSimplex",
    "ContactInfo",
    "ContactQueueInfo",
    "CreateGroupLink",
    "CreateMyAddress",
    "DeleteContact",
    "DeleteGroup",
    "DeleteGroupLink",
    "DeleteMemberMessage",
    "DeleteMessage",
    "DeleteMyAddress",
    "DeleteUser",
    "EditMessage",
    "EnableContact",
    "EnableGroupMember",
    "FileStatus",
    "ForwardFile",
    "ForwardGroupMessage",
    "ForwardImage",
    "ForwardLocalMessage",
    "ForwardMessage",
    "GetChatTTL",
    "GetContactCode",
    "GetGroupMemberCode",
    "GroupLinkMemberRole",
    "GroupMemberInfo",
    "GroupMemberQueueInfo",
    "HideUser",
    "JoinGroup",
    "LastChatItemId",
    "LastChats",
    "LastMessages",
    "LeaveGroup",
    "ListContacts",
    "ListGroups",
    "ListMembers",
    "ListMemberSupportChats",
    "MemberRole",
    "MuteUser",
    "NewGroup",
    "QuitChat",
    "ReactToMessage",
    "RejectContact",
    "RemoveMembers",
    "ReportMessage",
    "SendCallInvitation",
    "SendFile",
    "SendFileDescription",
    "SendGroupMessageQuote",
    "SendImage",
    "SendLiveMessage",
    "SendMemberContactMessage",
    "AcceptMemberContact",
    "SendMessage",
    "SendMessageBroadcast",
    "SendMessageQuote",
    "SetActiveUser",
    "SetAddressSettings",
    "SetBotCommands",
    "SetChatTTL",
    "SetContactFeature",
    "SetContactTimedMessages",
    "SetGroupFeature",
    "SetGroupFeatureRole",
    "SetGroupMemberAdmissionReview",
    "SetGroupTimedMessages",
    "SetLocalDeviceName",
    "SetProfileAddress",
    "SetSendReceipts",
    "SetShowMemberMessages",
    "SetShowMessages",
    "SetUserContactReceipts",
    "SetUserFeature",
    "SetUserGroupReceipts",
    "SetUserAutoAcceptMemberContacts",
    "SetUserTimedMessages",
    "ShowChatItem",
    "ShowChatItemInfo",
    "ShowGroupDescription",
    "ShowGroupInfo",
    "ShowGroupLink",
    "ShowGroupProfile",
    "ShowLiveItems",
    "ShowMyAddress",
    "ShowProfile",
    "ShowProfileImage",
    "ShowVersion",
    "SwitchContact",
    "SwitchGroupMember",
    "SyncContactRatchet",
    "SyncGroupMemberRatchet",
    "UnhideUser",
    "UnmuteUser",
    "UpdateGroupDescription",
    "UpdateGroupNames",
    "UpdateLiveMessage",
    "UpdateProfile",
    "UpdateProfileImage",
    "UserRead",
    "VerifyContact",
    "VerifyGroupMember",
    "Welcome"
  ]

undocumentedCommands :: [ConsName]
undocumentedCommands =
  [ "APIAbortSwitchContact",
    "APIAbortSwitchGroupMember",
    "APIAcceptConditions",
    "APIActivateChat",
    "APIAddGroupShortLink",
    "APIAddMyAddressShortLink",
    "APIArchiveReceivedReports",
    "APICallStatus",
    "APIChangeConnectionUser",
    "APIChangePreparedContactUser",
    "APIChangePreparedGroupUser",
    "APIChatItemsRead",
    "APIChatRead",
    "APIChatUnread",
    "APICheckToken",
    "APIClearChat",
    "APIConnectContactViaAddress",
    "APIConnectPreparedContact",
    "APIConnectPreparedGroup",
    "APIContactInfo",
    "APIContactQueueInfo",
    "APICreateChatItems",
    "APICreateChatTag",
    "APICreateMemberContact",
    "APISendMemberContactInvitation",
    "APIAcceptMemberContact",
    "APIDeleteChatTag",
    "APIDeleteMemberSupportChat",
    "APIDeleteReceivedReports",
    "APIDeleteStorage",
    "APIDeleteToken",
    "APIDownloadStandaloneFile",
    "APIEnableContact",
    "APIEnableGroupMember",
    "APIEndCall",
    "APIExportArchive",
    "APIForwardChatItems",
    "APIGetAppSettings",
    "APIGetCallInvitations",
    "APIGetChat",
    "APIGetChatContentTypes",
    "APIGetChatItemInfo",
    "APIGetChatItems",
    "APIGetChatItemTTL",
    "APIGetChats",
    "APIGetChatTags",
    "APIGetConnNtfMessages",
    "APIGetContactCode",
    "APIGetGroupMemberCode",
    "APIGetNetworkConfig",
    "APIGetNtfConns",
    "APIGetNtfToken",
    "APIGetReactionMembers",
    "APIGetServerOperators",
    "APIGetUsageConditions",
    "APIGetUserServers",
    "APIGroupInfo",
    "APIGroupMemberInfo",
    "APIGroupMemberQueueInfo",
    "APIHideUser",
    "APIImportArchive",
    "APIMuteUser",
    "APIPlanForwardChatItems",
    "APIPrepareContact",
    "APIPrepareGroup",
    "APIRegisterToken",
    "APIRejectCall",
    "APIReorderChatTags",
    "APIReportMessage",
    "APISaveAppSettings",
    "APISendCallAnswer",
    "APISendCallExtraInfo",
    "APISendCallInvitation",
    "APISendCallOffer",
    "APISetAppFilePaths",
    "APISetChatItemTTL",
    "APISetChatSettings",
    "APISetChatTags",
    "APISetChatTTL",
    "APISetChatUIThemes",
    "APISetConditionsNotified",
    "APISetConnectionAlias",
    "APISetConnectionIncognito",
    "APISetContactAlias",
    "APISetEncryptLocalFiles",
    "APISetGroupAlias",
    "APISetMemberSettings",
    "APISetNetworkConfig",
    "APISetNetworkInfo",
    "APISetServerOperators",
    "APISetUserContactReceipts",
    "APISetUserGroupReceipts",
    "APISetUserAutoAcceptMemberContacts",
    "APISetUserServers",
    "APISetUserUIThemes",
    "APIStandaloneFileInfo",
    "APIStopChat",
    "APIStorageEncryption",
    "APISuspendChat",
    "APISwitchContact",
    "APISwitchGroupMember",
    "APISyncContactRatchet",
    "APISyncGroupMemberRatchet",
    "APITestProtoServer",
    "APIUnhideUser",
    "APIUnmuteUser",
    "APIUpdateChatTag",
    "APIUploadStandaloneFile",
    "APIUserRead",
    "APIValidateServers",
    "APIVerifyContact",
    "APIVerifyGroupMember",
    "APIVerifyToken",
    "CheckChatRunning",
    "ConfirmRemoteCtrl",
    "ConnectRemoteCtrl",
    "CustomChatCommand",
    "DebugEvent",
    "DebugLocks",
    "DeleteRemoteCtrl",
    "DeleteRemoteHost",
    "ExecAgentStoreSQL",
    "ExecChatStoreSQL",
    "ExportArchive",
    "FindKnownRemoteCtrl",
    "GetAgentQueuesInfo",
    "GetAgentServersSummary",
    "GetAgentSubs",
    "GetAgentSubsDetails",
    "GetAgentSubsTotal",
    "GetAgentWorkers",
    "GetAgentWorkersDetails",
    "GetChatItemTTL",
    "GetRemoteFile",
    "GetUserProtoServers",
    "GetUserChatRelays",
    "ListRemoteCtrls",
    "ListRemoteHosts",
    "ReconnectAllServers",
    "ReconnectServer",
    "ResetAgentServersStats",
    "ShowConnectionsDiff",
    "ResubscribeAllConnections",
    "SetAllContactReceipts",
    "SetChatItemTTL",
    "SetContactMergeEnabled",
    "SetFilesFolder",
    "SetFileToReceive",
    "SetNetworkConfig",
    "SetRemoteHostsFolder",
    "SetServerOperators",
    "SetTempFolder",
    "SetUserProtoServers",
    "SetUserChatRelays",
    "SlowSQLQueries",
    "StartChat",
    "StartRemoteHost",
    "StopRemoteCtrl",
    "StopRemoteHost",
    "StoreRemoteFile",
    "SwitchRemoteHost",
    "TestProtoServer",
    "TestStorageEncryption",
    "VerifyRemoteCtrlSession"
  ]
