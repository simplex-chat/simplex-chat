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
import API.Docs.Syntax
import API.Docs.Types
import API.TypeInfo
import Data.String
import Data.List (find)
import Data.Text (Text)
import GHC.Generics
import Simplex.Chat.Controller
import Simplex.Messaging.Parsers (fstToLower)

chatCommandsDocs :: [CCCategory]
chatCommandsDocs = map toCategory chatCommandsDocsData
  where
    toCategory (categoryName, categoryDescr, commandsData) =
      CCCategory {categoryName, categoryDescr, commands = map toCmd commandsData}
    toCmd (consName, commandDescr, respNames, errors, network, syntax) = case find ((consName ==) . consName') chatCommandsTypeInfo of
      Just RecordTypeInfo {fieldInfos} ->
        let fields = map (toAPIField consName) fieldInfos
            commandType = ATUnionMember (fstToLower consName) fields
            findResp name = case find ((name ==) . consName') chatResponsesDocs of
              Just resp -> resp
              Nothing -> error $ "Missing response doc for " <> name
            responses = map findResp respNames
         in CCDoc {consName, commandType, commandDescr, responses, errors, network, syntax}
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
    errors :: [TypeDoc], -- TODO add to doc or remove
    network :: Maybe UsesNetwork, -- TODO add to doc
    syntax :: [Expr]
  }

instance ConstructorName CCDoc where consName' CCDoc {consName} = consName

-- TODO remove?
data TypeDoc = TD
  { consName :: ConsName,
    description :: Text
  }

data UsesNetwork = UNBackground | UNInteractive

instance IsString TypeDoc where fromString s = TD s ""


-- category name, category description, commands
-- inner: constructor, description, syntax, responses, errors (ChatErrorType constructors), network usage
chatCommandsDocsData :: [(String, String, [(ConsName, Text, [String], [TypeDoc], Maybe UsesNetwork, [Expr])])]
chatCommandsDocsData =
  [ ( "Address commands",
      "Bots can use these commands to automatically check and create address when initialized",
      [ ("APICreateMyAddress", "Create bot address.", ["CRUserContactLinkCreated"], [], Just UNInteractive, ["/_address " <> Param "userId"]), -- /_address <userId>
        ("APIDeleteMyAddress", "Delete bot address.", ["CRUserContactLinkDeleted"], [], Just UNBackground, ["/_delete_address " <> Param "userId"]), -- /_delete_address <userId>
        ("APIShowMyAddress", "Get bot address and settings.", ["CRUserContactLink"], [], Nothing, ["/_show_address " <> Param "userId"]), -- /_show_address <userId>
        ("APISetProfileAddress", "Add address to bot profile.", ["CRUserProfileUpdated"], [], Just UNInteractive, ["/_profile_address " <> Param "userId" <> " " <> OnOff "enable"]), -- /_profile_address <userId> on/off
        ("APISetAddressSettings", "Set bot address settings.", ["CRUserContactLinkUpdated"], [], Just UNInteractive, ["/_address_settings " <> Param "userId" <> " " <> Json "settings"]) -- /_address_settings <userId> <json(settings)>
      ]
    ),
    ( "Message commands",
      "Commands to send, update, delete, moderate messages and set message reactions", -- TODO chatRef command encoding
      [ ("APISendMessages", "Send messages.", ["CRNewChatItems"], [], Just UNBackground, [Func "chat" "r" (ChatRefExpr "$0"), "/_send " <> Call "chat" "sendRef" <> OnOffParam "live" "liveMessage" (Just False) <> Optional "" (" ttl=" <> Param "$0") "ttl" <> " json " <> Json "composedMessages"]), -- /_send \(ref(type, id, scope: scope))[ live=on|off][ ttl=<ttl>] json <json(composedMessages)>
        ("APIUpdateChatItem", "Update message.", ["CRChatItemUpdated", "CRChatItemNotChanged"], ["CEInvalidChatItemUpdate"], Just UNBackground, ["/_update item " <> Call "ref" "chatRef" <> " " <> Param "chatItemId" <> OnOffParam "live" "liveMessage" (Just False) <> " json " <> Json "updatedMessage"]), -- /_update item <ref(chatRef)> <chatItemId>[ live=on|off] \(updatedMessage.cmdString)"
        ("APIDeleteChatItem", "Delete message.", ["CRChatItemsDeleted"], [], Just UNBackground, ["/_delete item " <> Call "ref" "chatRef" <> " " <> Join ',' "chatItemIds" <> " " <> Param "deleteMode"]), -- /_delete item <ref(chatRef)> <chatItemIds> <mode>
        ("APIDeleteMemberChatItem", "Moderate message. Requires Moderator role (and higher than message author's).", ["CRChatItemsDeleted"], [], Just UNBackground, ["/_delete member item #" <> Param "groupId" <> " " <> Join ',' "chatItemIds"]), -- "/_delete member item #<groupId> <chatItemIds>
        ("APIChatItemReaction", "Add/remove message reaction.", ["CRChatItemReaction"], [], Just UNBackground, ["/_reaction " <> Call "ref" "chatRef" <> " " <> Param "chatItemId" <> " " <> OnOff "add" <> " " <> Json "reaction"]) -- "/_reaction <ref(chatRef)> <itemId> on|off <json(reaction)>
        -- ("APIGetReactionMembers", "Get reaction members.", ["CRReactionMembers"], [], Nothing, []),
        -- ("APIReportMessage", "Report message.", ["CRNewChatItems"], [], Just UNBackground, []),
      ]
    ),
    ( "File commands",
      "Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.",
      [ ("ReceiveFile", "Receive file.", ["CRRcvFileAccepted", "CRRcvFileAcceptedSndCancelled"], [], Nothing, ["/freceive " <> Param "fileId" <> OnOffParam "approved_relays" "userApprovedRelays" (Just False) <> OnOffParam "encrypt" "storeEncrypted" Nothing <> OnOffParam "inline" "fileInline" Nothing <> Optional "" (" " <> Param "$0") "filePath"]), -- "/freceive <fileId> <onOffParam("approved_relays", userApprovedRelays)><onOffParam("encrypt", encrypt)><onOffParam("inline", inline)>"
        ("CancelFile", "Cancel file.", ["CRSndFileCancelled", "CRRcvFileCancelled"], ["CEFileCancel"], Just UNBackground, ["/fcancel " <> Param "fileId"]) -- "/fcancel <fileId>"
      ]
    ),
    ( "Group commands",
      "Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.",
      [ ("APIAddMember", "Add contact to group. Requires bot to have Admin role.", ["CRSentGroupInvitation"], [], Just UNInteractive, ["/_add #" <> Param "groupId" <> " " <> Param "contactId" <> " " <> Param "memberRole"]), -- /_add #<groupId> <contactId> <memberRole>
        -- TODO note that enableNtfs is not supported
        ("APIJoinGroup", "Join group.", ["CRUserAcceptedGroupSent"], [], Just UNInteractive, ["/_join #" <> Param "groupId"]), -- /_join #<groupId>
        ("APIAcceptMember", "Accept group member. Requires Admin role.", ["CRMemberAccepted"], ["CEGroupMemberNotActive"], Just UNBackground, ["/_accept member #" <> Param "groupId" <> " " <> Param "groupMemberId" <> " " <> Param "memberRole"]), -- /_accept member #<groupId> <groupMemberId> <memberRole>
        ("APIMembersRole", "Set members role. Requires Admin role.", ["CRMembersRoleUser"], [], Just UNBackground, ["/_member role #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> " " <> Param "memberRole"]), -- /_member role #<groupId> <memberIds> <memberRole>
        ("APIBlockMembersForAll", "Block members. Requires Moderator role.", ["CRMembersBlockedForAllUser"], [], Just UNBackground, ["/_block #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> OnOffParam "blocked" "blocked" Nothing]), -- /_block #<groupId> <memberIds> blocked=on|off
        ("APIRemoveMembers", "Remove members. Requires Admin role.", ["CRUserDeletedMembers"], ["CEGroupMemberNotFound"], Just UNBackground, ["/_remove #" <> Param "groupId" <> " " <> Join ',' "groupMemberIds" <> OnOffParam "messages" "withMessages" (Just False)]), -- /_remove #<groupId> <memberIds> messages=on|off
        ("APILeaveGroup", "Leave group.", ["CRLeftMemberUser"], [], Just UNBackground, ["/_leave #" <> Param "groupId"]) -- /_leave #<groupId>
        -- ("APIListMembers", "Get group members.", ["CRGroupMembers"], [], Nothing, []),
        -- ("APINewGroup", "Create group.", ["CRGroupCreated"], [], Nothing, []),
        -- ("APIUpdateGroupProfile", "Update group profile.", ["CRGroupUpdated"], [], Just UNBackground, [])
      ]
    ),
    ( "Group link commands",
      "These commands can be used by bots that manage multiple public groups",
      [ ("APICreateGroupLink", "Create group link.", ["CRGroupLinkCreated"], [], Just UNInteractive, ["/_create link #" <> Param "groupId" <> " " <> Param "memberRole"]), -- /_create link #<groupId> <memberRole>
        ("APIGroupLinkMemberRole", "Set member role for group link.", ["CRGroupLink"], [], Nothing, ["/_set link role #" <> Param "groupId" <> " " <> Param "memberRole"]), -- /_set link role #<groupId> <memberRole>
        ("APIDeleteGroupLink", "Delete group link.", ["CRGroupLinkDeleted"], [], Just UNBackground, ["/_delete link #" <> Param "groupId"]), -- /_delete link #<groupId>
        ("APIGetGroupLink", "Get group link.", ["CRGroupLink"], [], Nothing, ["/_get link #" <> Param "groupId"]) -- /_get link #<groupId>
      ]
    ),
    ( "Connection commands",
      "These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.",
      [ ("APIAddContact", "Create 1-time invitation link.", ["CRInvitation"], [], Just UNInteractive, ["/_connect " <> Param "userId" <> OnOffParam "incognito" "incognito" (Just False)]), -- /_connect <userId>[ incognito=on|off]
        ("APIConnectPlan", "Determine SimpleX link type and if the bot is already connected via this link.", ["CRConnectionPlan"], [], Just UNInteractive, ["/_connect plan " <> Param "userId" <> " " <> Param "connectionLink"]), -- /_connect plan <userId> <connLink>
        -- TODO link command encoding
        ("APIConnect", "Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link", ["CRSentConfirmation", "CRContactAlreadyExists", "CRSentInvitation"], [], Just UNInteractive, ["/_connect " <> Param "userId" <> " " <> Call "link" "connLink_"]), -- /_connect <userId>[ incognito=on|off] connLink.connFullLink[ connLink.connShortLink]
        -- TODO remove incognito
        ("APIAcceptContact", "Accept contact request.", ["CRAcceptingContactRequest"], [], Just UNInteractive, ["/_accept " <> Param "contactReqId"]), -- /_accept[ incognito=on|off] <contactReqId>
        ("APIRejectContact", "Reject contact request. The user who sent the request is **not notified**.", ["CRContactRequestRejected"], [], Nothing, ["/_reject " <> Param "contactReqId"]) -- /_reject <contactReqId>
      ]
    ),
    ( "Chat commands",
      "Commands to list and delete coversations.",
      [ ("APIListContacts", "Get contacts.", ["CRContactsList"], [], Nothing, ["/_contacts " <> Param "userId"]), -- /_contacts <userId>
        ("APIListGroups", "Get groups.", ["CRGroupsList"], [], Nothing, ["/_groups " <> Param "userId" <> Optional "" (" @" <> Param "$0") "contactId_" <> Optional "" (" " <> Param "$0") "search"]), --       /_groups <userId>[ @<contactId>][ search]
        ("APIDeleteChat", "Delete chat.", ["CRContactDeleted", "CRContactConnectionDeleted", "CRGroupDeletedUser"], [], Just UNBackground, ["/_delete " <> Call "ref" "chatRef" <> " " <> Param "chatDeleteMode"]) -- /_delete <ref(chatRef)>[ full|entity|messages][ notify=on|off]
        -- ("APIChatItemsRead", "Mark items as read.", ["CRItemsReadForChat"], [], Nothing, []),
        -- ("APIChatRead", "Mark chat as read.", ["CRCmdOk"], [], Nothing, []),
        -- ("APIChatUnread", "Mark chat as unread.", ["CRCmdOk"], [], Nothing, []),
        -- ("APIClearChat", "Clear chat.", ["CRChatCleared"], [], Nothing, []),
        -- ("APIGetChat", "Get chat.", ["CRApiChat"], [], Nothing, []),
        -- ("APIGetChatItemInfo", "Get message information.", ["CRChatItemInfo"], [], Nothing, []),
        -- ("APIGetChatItems", "Get the most recent messages from all chats.", ["CRChatItems"], [], Nothing, []),
        -- ("APIGetChats", "Get chats.", ["CRApiChats"], [], Nothing, []),
        -- ("APISetChatSettings", "Set chat settings.", ["CRCmdOk"], [], Nothing, []),
        -- ("APISetChatTTL", "Set TTL for chat messages.", ["CRCmdOk"], [], Nothing, []),
        -- ("APISetConnectionAlias", "Set connection alias.", ["CRConnectionAliasUpdated"], [], Nothing, []),
        -- ("APISetContactAlias", "Set contact alias.", ["CRContactAliasUpdated"], [], Nothing, []),
        -- ("APISetContactPrefs", "Set contact preferences.", ["CRContactPrefsUpdated"], [], Just UNBackground, []),
        -- ("APISetGroupAlias", "Set group alias.", ["CRGroupAliasUpdated"], [], Nothing, []),
        -- ("APISyncContactRatchet", "Synchronize encryption with contact.", ["CRContactRatchetSyncStarted"], [], Just UNBackground, []),
        -- ("APISyncGroupMemberRatchet", "Synchronize encryption with member.", ["CRGroupMemberRatchetSyncStarted"], [], Just UNBackground, []),
      ]
    ),
    ( "User profile commands",
      "Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).",
      [ ("ShowActiveUser", "Get active user profile", ["CRActiveUser"], [], Nothing, ["/user"]),
        ("CreateActiveUser", "Create new user profile", ["CRActiveUser"], ["CEUserExists", "CEInvalidDisplayName"], Nothing, ["/_create user " <> Json "newUser"]), -- "/_create user <json(NewUser)>"
        ("ListUsers", "Get all user profiles", ["CRUsersList"], [], Nothing, ["/users"]),
        ("APISetActiveUser", "Set active user profile", ["CRActiveUser"], ["CEChatNotStarted"], Nothing, ["/_user " <> Param "userId" <> Optional "" (" " <> Json "$0") "viewPwd"]), -- "/_user <UserId>[ <quoted(UserPwd)>]"
        ("APIDeleteUser", "Delete user profile.", ["CRCmdOk"], [], Just UNBackground, ["/_delete user " <> Param "userId" <> OnOffParam "del_smp" "delSMPQueues" Nothing <> Optional "" (" " <> Json "$0") "viewPwd"]), -- /_delete user <userId>[ del_smp=on|off][ quoted(viewPwd)]
        ("APIUpdateProfile", "Update user profile.", ["CRUserProfileUpdated"], [], Just UNBackground, ["/_profile " <> Param "userId" <> " " <> Json "profile"]) -- /_profile <userId> <json(profile)>
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
    "Connect",
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
    "SendMessage",
    "SendMessageBroadcast",
    "SendMessageQuote",
    "SetActiveUser",
    "SetAddressSettings",
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
    "APIGetChatItemInfo",
    "APIGetChatItems",
    "APIGetChatItemTTL",
    "APIGetChats",
    "APIGetChatTags",
    "APIGetConnNtfMessages",
    "APIGetContactCode",
    "APIGetGroupMemberCode",
    "APIGetNetworkConfig",
    "APIGetNetworkStatuses",
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
    "APIListMembers",
    "APIMuteUser",
    "APINewGroup",
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
    "APISendMemberContactInvitation",
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
    "APISetContactPrefs",
    "APISetEncryptLocalFiles",
    "APISetGroupAlias",
    "APISetMemberSettings",
    "APISetNetworkConfig",
    "APISetNetworkInfo",
    "APISetServerOperators",
    "APISetUserContactReceipts",
    "APISetUserGroupReceipts",
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
    "APIUpdateGroupProfile",
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
    "ListRemoteCtrls",
    "ListRemoteHosts",
    "ReconnectAllServers",
    "ReconnectServer",
    "ResetAgentServersStats",
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
