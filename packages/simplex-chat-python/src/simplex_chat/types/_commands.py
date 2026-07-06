# API Commands
# This file is generated automatically.
from __future__ import annotations
import json
from typing import NotRequired, TypedDict
from . import _types as T
from . import _responses as CR

# Address commands
# Bots can use these commands to automatically check and create address when initialized

# Create bot address.
# Network usage: interactive.
class APICreateMyAddress(TypedDict):
    userId: int  # int64


def APICreateMyAddress_cmd_string(self: APICreateMyAddress) -> str:
    return '/_address ' + str(self['userId'])

APICreateMyAddress_Response = CR.UserContactLinkCreated | CR.ChatCmdError


# Delete bot address.
# Network usage: background.
class APIDeleteMyAddress(TypedDict):
    userId: int  # int64


def APIDeleteMyAddress_cmd_string(self: APIDeleteMyAddress) -> str:
    return '/_delete_address ' + str(self['userId'])

APIDeleteMyAddress_Response = CR.UserContactLinkDeleted | CR.ChatCmdError


# Get bot address and settings.
# Network usage: no.
class APIShowMyAddress(TypedDict):
    userId: int  # int64


def APIShowMyAddress_cmd_string(self: APIShowMyAddress) -> str:
    return '/_show_address ' + str(self['userId'])

APIShowMyAddress_Response = CR.UserContactLink | CR.ChatCmdError


# Add address to bot profile.
# Network usage: interactive.
class APISetProfileAddress(TypedDict):
    userId: int  # int64
    enable: bool


def APISetProfileAddress_cmd_string(self: APISetProfileAddress) -> str:
    return '/_profile_address ' + str(self['userId']) + ' ' + ('on' if self['enable'] else 'off')

APISetProfileAddress_Response = CR.UserProfileUpdated | CR.ChatCmdError


# Set bot address settings.
# Network usage: interactive.
class APISetAddressSettings(TypedDict):
    userId: int  # int64
    settings: "T.AddressSettings"


def APISetAddressSettings_cmd_string(self: APISetAddressSettings) -> str:
    return '/_address_settings ' + str(self['userId']) + ' ' + json.dumps(self['settings'])

APISetAddressSettings_Response = CR.UserContactLinkUpdated | CR.ChatCmdError


# Message commands
# Commands to send, update, delete, moderate messages and set message reactions

# Send messages.
# Network usage: background.
class APISendMessages(TypedDict):
    sendRef: "T.ChatRef"
    liveMessage: bool
    ttl: NotRequired[int]  # int
    composedMessages: list["T.ComposedMessage"]  # non-empty


def APISendMessages_cmd_string(self: APISendMessages) -> str:
    return '/_send ' + T.ChatRef_cmd_string(self['sendRef']) + (' live=on' if self['liveMessage'] else '') + ((' ttl=' + str(self.get('ttl'))) if self.get('ttl') is not None else '') + ' json ' + json.dumps(self['composedMessages'])

APISendMessages_Response = CR.NewChatItems | CR.ChatCmdError


# Update message.
# Network usage: background.
class APIUpdateChatItem(TypedDict):
    chatRef: "T.ChatRef"
    chatItemId: int  # int64
    liveMessage: bool
    updatedMessage: "T.UpdatedMessage"


def APIUpdateChatItem_cmd_string(self: APIUpdateChatItem) -> str:
    return '/_update item ' + T.ChatRef_cmd_string(self['chatRef']) + ' ' + str(self['chatItemId']) + (' live=on' if self['liveMessage'] else '') + ' json ' + json.dumps(self['updatedMessage'])

APIUpdateChatItem_Response = CR.ChatItemUpdated | CR.ChatItemNotChanged | CR.ChatCmdError


# Delete message.
# Network usage: background.
class APIDeleteChatItem(TypedDict):
    chatRef: "T.ChatRef"
    chatItemIds: list[int]  # int64, non-empty
    deleteMode: "T.CIDeleteMode"


def APIDeleteChatItem_cmd_string(self: APIDeleteChatItem) -> str:
    return '/_delete item ' + T.ChatRef_cmd_string(self['chatRef']) + ' ' + ','.join(map(str, self['chatItemIds'])) + ' ' + str(self['deleteMode'])

APIDeleteChatItem_Response = CR.ChatItemsDeleted | CR.ChatCmdError


# Moderate message. Requires Moderator role (and higher than message author's).
# Network usage: background.
class APIDeleteMemberChatItem(TypedDict):
    groupId: int  # int64
    chatItemIds: list[int]  # int64, non-empty


def APIDeleteMemberChatItem_cmd_string(self: APIDeleteMemberChatItem) -> str:
    return '/_delete member item #' + str(self['groupId']) + ' ' + ','.join(map(str, self['chatItemIds']))

APIDeleteMemberChatItem_Response = CR.ChatItemsDeleted | CR.ChatCmdError


# Add/remove message reaction.
# Network usage: background.
class APIChatItemReaction(TypedDict):
    chatRef: "T.ChatRef"
    chatItemId: int  # int64
    add: bool
    reaction: "T.MsgReaction"


def APIChatItemReaction_cmd_string(self: APIChatItemReaction) -> str:
    return '/_reaction ' + T.ChatRef_cmd_string(self['chatRef']) + ' ' + str(self['chatItemId']) + ' ' + ('on' if self['add'] else 'off') + ' ' + json.dumps(self['reaction'])

APIChatItemReaction_Response = CR.ChatItemReaction | CR.ChatCmdError


# File commands
# Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.

# Receive file.
# Network usage: no.
class ReceiveFile(TypedDict):
    fileId: int  # int64
    userApprovedRelays: bool
    storeEncrypted: NotRequired[bool]
    fileInline: NotRequired[bool]
    filePath: NotRequired[str]


def ReceiveFile_cmd_string(self: ReceiveFile) -> str:
    return '/freceive ' + str(self['fileId']) + (' approved_relays=on' if self['userApprovedRelays'] else '') + ((' encrypt=' + ('on' if self.get('storeEncrypted') else 'off')) if self.get('storeEncrypted') is not None else '') + ((' inline=' + ('on' if self.get('fileInline') else 'off')) if self.get('fileInline') is not None else '') + ((' ' + self.get('filePath')) if self.get('filePath') is not None else '')

ReceiveFile_Response = CR.RcvFileAccepted | CR.RcvFileAcceptedSndCancelled | CR.ChatCmdError


# Cancel file.
# Network usage: background.
class CancelFile(TypedDict):
    fileId: int  # int64


def CancelFile_cmd_string(self: CancelFile) -> str:
    return '/fcancel ' + str(self['fileId'])

CancelFile_Response = CR.SndFileCancelled | CR.RcvFileCancelled | CR.ChatCmdError


# Group commands
# Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.

# Add contact to group. Requires bot to have Admin role.
# Network usage: interactive.
class APIAddMember(TypedDict):
    groupId: int  # int64
    contactId: int  # int64
    memberRole: "T.GroupMemberRole"


def APIAddMember_cmd_string(self: APIAddMember) -> str:
    return '/_add #' + str(self['groupId']) + ' ' + str(self['contactId']) + ' ' + str(self['memberRole'])

APIAddMember_Response = CR.SentGroupInvitation | CR.ChatCmdError


# Join group.
# Network usage: interactive.
class APIJoinGroup(TypedDict):
    groupId: int  # int64


def APIJoinGroup_cmd_string(self: APIJoinGroup) -> str:
    return '/_join #' + str(self['groupId'])

APIJoinGroup_Response = CR.UserAcceptedGroupSent | CR.ChatCmdError


# Accept group member. Requires Admin role.
# Network usage: background.
class APIAcceptMember(TypedDict):
    groupId: int  # int64
    groupMemberId: int  # int64
    memberRole: "T.GroupMemberRole"


def APIAcceptMember_cmd_string(self: APIAcceptMember) -> str:
    return '/_accept member #' + str(self['groupId']) + ' ' + str(self['groupMemberId']) + ' ' + str(self['memberRole'])

APIAcceptMember_Response = CR.MemberAccepted | CR.ChatCmdError


# Set members role. Requires Admin role.
# Network usage: background.
class APIMembersRole(TypedDict):
    groupId: int  # int64
    groupMemberIds: list[int]  # int64, non-empty
    memberRole: "T.GroupMemberRole"


def APIMembersRole_cmd_string(self: APIMembersRole) -> str:
    return '/_member role #' + str(self['groupId']) + ' ' + ','.join(map(str, self['groupMemberIds'])) + ' ' + str(self['memberRole'])

APIMembersRole_Response = CR.MembersRoleUser | CR.ChatCmdError


# Block members. Requires Moderator role.
# Network usage: background.
class APIBlockMembersForAll(TypedDict):
    groupId: int  # int64
    groupMemberIds: list[int]  # int64, non-empty
    blocked: bool


def APIBlockMembersForAll_cmd_string(self: APIBlockMembersForAll) -> str:
    return '/_block #' + str(self['groupId']) + ' ' + ','.join(map(str, self['groupMemberIds'])) + ' blocked=' + ('on' if self['blocked'] else 'off')

APIBlockMembersForAll_Response = CR.MembersBlockedForAllUser | CR.ChatCmdError


# Remove members. Requires Admin role.
# Network usage: background.
class APIRemoveMembers(TypedDict):
    groupId: int  # int64
    groupMemberIds: list[int]  # int64, non-empty
    withMessages: bool


def APIRemoveMembers_cmd_string(self: APIRemoveMembers) -> str:
    return '/_remove #' + str(self['groupId']) + ' ' + ','.join(map(str, self['groupMemberIds'])) + (' messages=on' if self['withMessages'] else '')

APIRemoveMembers_Response = CR.UserDeletedMembers | CR.ChatCmdError


# Leave group.
# Network usage: background.
class APILeaveGroup(TypedDict):
    groupId: int  # int64


def APILeaveGroup_cmd_string(self: APILeaveGroup) -> str:
    return '/_leave #' + str(self['groupId'])

APILeaveGroup_Response = CR.LeftMemberUser | CR.ChatCmdError


# Get group members.
# Network usage: no.
class APIListMembers(TypedDict):
    groupId: int  # int64


def APIListMembers_cmd_string(self: APIListMembers) -> str:
    return '/_members #' + str(self['groupId'])

APIListMembers_Response = CR.GroupMembers | CR.ChatCmdError


# Create group.
# Network usage: no.
class APINewGroup(TypedDict):
    userId: int  # int64
    incognito: bool
    groupProfile: "T.GroupProfile"


def APINewGroup_cmd_string(self: APINewGroup) -> str:
    return '/_group ' + str(self['userId']) + (' incognito=on' if self['incognito'] else '') + ' ' + json.dumps(self['groupProfile'])

APINewGroup_Response = CR.GroupCreated | CR.ChatCmdError


# Create public group.
# Network usage: interactive.
class APINewPublicGroup(TypedDict):
    userId: int  # int64
    incognito: bool
    relayIds: list[int]  # int64, non-empty
    groupProfile: "T.GroupProfile"


def APINewPublicGroup_cmd_string(self: APINewPublicGroup) -> str:
    return '/_public group ' + str(self['userId']) + (' incognito=on' if self['incognito'] else '') + ' ' + ','.join(map(str, self['relayIds'])) + ' ' + json.dumps(self['groupProfile'])

APINewPublicGroup_Response = CR.PublicGroupCreated | CR.PublicGroupCreationFailed | CR.ChatCmdError


# Get group relays.
# Network usage: no.
class APIGetGroupRelays(TypedDict):
    groupId: int  # int64


def APIGetGroupRelays_cmd_string(self: APIGetGroupRelays) -> str:
    return '/_get relays #' + str(self['groupId'])

APIGetGroupRelays_Response = CR.GroupRelays | CR.ChatCmdError


# Add relays to group.
# Network usage: interactive.
class APIAddGroupRelays(TypedDict):
    groupId: int  # int64
    relayIds: list[int]  # int64, non-empty


def APIAddGroupRelays_cmd_string(self: APIAddGroupRelays) -> str:
    return '/_add relays #' + str(self['groupId']) + ' ' + ','.join(map(str, self['relayIds']))

APIAddGroupRelays_Response = CR.GroupRelaysAdded | CR.GroupRelaysAddFailed | CR.ChatCmdError


# Clear relay rejection for a channel (relay operator).
# Network usage: background.
class APIAllowRelayGroup(TypedDict):
    groupId: int  # int64


def APIAllowRelayGroup_cmd_string(self: APIAllowRelayGroup) -> str:
    return '/_relay allow #' + str(self['groupId'])

APIAllowRelayGroup_Response = CR.RelayGroupAllowed | CR.ChatCmdError


# Update group profile.
# Network usage: background.
class APIUpdateGroupProfile(TypedDict):
    groupId: int  # int64
    groupProfile: "T.GroupProfile"


def APIUpdateGroupProfile_cmd_string(self: APIUpdateGroupProfile) -> str:
    return '/_group_profile #' + str(self['groupId']) + ' ' + json.dumps(self['groupProfile'])

APIUpdateGroupProfile_Response = CR.GroupUpdated | CR.ChatCmdError


# Group link commands
# These commands can be used by bots that manage multiple public groups

# Create group link.
# Network usage: interactive.
class APICreateGroupLink(TypedDict):
    groupId: int  # int64
    memberRole: "T.GroupMemberRole"


def APICreateGroupLink_cmd_string(self: APICreateGroupLink) -> str:
    return '/_create link #' + str(self['groupId']) + ' ' + str(self['memberRole'])

APICreateGroupLink_Response = CR.GroupLinkCreated | CR.ChatCmdError


# Set member role for group link.
# Network usage: no.
class APIGroupLinkMemberRole(TypedDict):
    groupId: int  # int64
    memberRole: "T.GroupMemberRole"


def APIGroupLinkMemberRole_cmd_string(self: APIGroupLinkMemberRole) -> str:
    return '/_set link role #' + str(self['groupId']) + ' ' + str(self['memberRole'])

APIGroupLinkMemberRole_Response = CR.GroupLink | CR.ChatCmdError


# Delete group link.
# Network usage: background.
class APIDeleteGroupLink(TypedDict):
    groupId: int  # int64


def APIDeleteGroupLink_cmd_string(self: APIDeleteGroupLink) -> str:
    return '/_delete link #' + str(self['groupId'])

APIDeleteGroupLink_Response = CR.GroupLinkDeleted | CR.ChatCmdError


# Get group link.
# Network usage: no.
class APIGetGroupLink(TypedDict):
    groupId: int  # int64


def APIGetGroupLink_cmd_string(self: APIGetGroupLink) -> str:
    return '/_get link #' + str(self['groupId'])

APIGetGroupLink_Response = CR.GroupLink | CR.ChatCmdError


# Connection commands
# These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.

# Create 1-time invitation link.
# Network usage: interactive.
class APIAddContact(TypedDict):
    userId: int  # int64
    incognito: bool


def APIAddContact_cmd_string(self: APIAddContact) -> str:
    return '/_connect ' + str(self['userId']) + (' incognito=on' if self['incognito'] else '')

APIAddContact_Response = CR.Invitation | CR.ChatCmdError


# Determine SimpleX link type and if the bot is already connected via this link or name.
# Network usage: interactive.
class APIConnectPlan(TypedDict):
    userId: int  # int64
    connectTarget: NotRequired[str]
    resolveMode: "T.PlanResolveMode"
    linkOwnerSig: NotRequired["T.LinkOwnerSig"]


def APIConnectPlan_cmd_string(self: APIConnectPlan) -> str:
    return '/_connect plan ' + str(self['userId']) + ' ' + self.get('connectTarget')

APIConnectPlan_Response = CR.ConnectionPlan | CR.ChatCmdError


# Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link.
# Network usage: interactive.
class APIConnect(TypedDict):
    userId: int  # int64
    incognito: bool
    preparedLink_: NotRequired["T.CreatedConnLink"]


def APIConnect_cmd_string(self: APIConnect) -> str:
    return '/_connect ' + str(self['userId']) + ((' ' + T.CreatedConnLink_cmd_string(self.get('preparedLink_'))) if self.get('preparedLink_') is not None else '')

APIConnect_Response = CR.SentConfirmation | CR.ContactAlreadyExists | CR.SentInvitation | CR.ChatCmdError


# Connect via SimpleX link or name as string in the active user profile.
# Network usage: interactive.
class Connect(TypedDict):
    incognito: bool
    connTarget_: NotRequired[str]


def Connect_cmd_string(self: Connect) -> str:
    return '/connect' + ((' ' + self.get('connTarget_')) if self.get('connTarget_') is not None else '')

Connect_Response = CR.SentConfirmation | CR.ContactAlreadyExists | CR.SentInvitation | CR.ChatCmdError


# Accept contact request.
# Network usage: interactive.
class APIAcceptContact(TypedDict):
    contactReqId: int  # int64


def APIAcceptContact_cmd_string(self: APIAcceptContact) -> str:
    return '/_accept ' + str(self['contactReqId'])

APIAcceptContact_Response = CR.AcceptingContactRequest | CR.ChatCmdError


# Reject contact request. The user who sent the request is **not notified**.
# Network usage: no.
class APIRejectContact(TypedDict):
    contactReqId: int  # int64


def APIRejectContact_cmd_string(self: APIRejectContact) -> str:
    return '/_reject ' + str(self['contactReqId'])

APIRejectContact_Response = CR.ContactRequestRejected | CR.ChatCmdError


# Chat commands
# Commands to list and delete conversations.

# Get contacts.
# Network usage: no.
class APIListContacts(TypedDict):
    userId: int  # int64


def APIListContacts_cmd_string(self: APIListContacts) -> str:
    return '/_contacts ' + str(self['userId'])

APIListContacts_Response = CR.ContactsList | CR.ChatCmdError


# Get groups.
# Network usage: no.
class APIListGroups(TypedDict):
    userId: int  # int64
    contactId_: NotRequired[int]  # int64
    search: NotRequired[str]


def APIListGroups_cmd_string(self: APIListGroups) -> str:
    return '/_groups ' + str(self['userId']) + ((' @' + str(self.get('contactId_'))) if self.get('contactId_') is not None else '') + ((' ' + self.get('search')) if self.get('search') is not None else '')

APIListGroups_Response = CR.GroupsList | CR.ChatCmdError


# Get chat previews. Supports time-based pagination — use this instead of APIListContacts / APIListGroups when scanning at scale (those load every record into memory and fail on large databases).
# Network usage: no.
class APIGetChats(TypedDict):
    userId: int  # int64
    pendingConnections: bool
    pagination: "T.PaginationByTime"
    query: "T.ChatListQuery"


def APIGetChats_cmd_string(self: APIGetChats) -> str:
    return '/_get chats ' + str(self['userId']) + (' pcc=on' if self['pendingConnections'] else '') + ' ' + T.PaginationByTime_cmd_string(self['pagination']) + ' ' + json.dumps(self['query'])

APIGetChats_Response = CR.ApiChats | CR.ChatCmdError


# Delete chat.
# Network usage: background.
class APIDeleteChat(TypedDict):
    chatRef: "T.ChatRef"
    chatDeleteMode: "T.ChatDeleteMode"


def APIDeleteChat_cmd_string(self: APIDeleteChat) -> str:
    return '/_delete ' + T.ChatRef_cmd_string(self['chatRef']) + ' ' + T.ChatDeleteMode_cmd_string(self['chatDeleteMode'])

APIDeleteChat_Response = CR.ContactDeleted | CR.ContactConnectionDeleted | CR.GroupDeletedUser | CR.ChatCmdError


# Set group custom data.
# Network usage: no.
class APISetGroupCustomData(TypedDict):
    groupId: int  # int64
    customData: NotRequired[dict[str, object]]


def APISetGroupCustomData_cmd_string(self: APISetGroupCustomData) -> str:
    return '/_set custom #' + str(self['groupId']) + ((' ' + json.dumps(self.get('customData'))) if self.get('customData') is not None else '')

APISetGroupCustomData_Response = CR.CmdOk | CR.ChatCmdError


# Set contact custom data.
# Network usage: no.
class APISetContactCustomData(TypedDict):
    contactId: int  # int64
    customData: NotRequired[dict[str, object]]


def APISetContactCustomData_cmd_string(self: APISetContactCustomData) -> str:
    return '/_set custom @' + str(self['contactId']) + ((' ' + json.dumps(self.get('customData'))) if self.get('customData') is not None else '')

APISetContactCustomData_Response = CR.CmdOk | CR.ChatCmdError


# Set auto-accept member contacts.
# Network usage: no.
class APISetUserAutoAcceptMemberContacts(TypedDict):
    userId: int  # int64
    onOff: bool


def APISetUserAutoAcceptMemberContacts_cmd_string(self: APISetUserAutoAcceptMemberContacts) -> str:
    return '/_set accept member contacts ' + str(self['userId']) + ' ' + ('on' if self['onOff'] else 'off')

APISetUserAutoAcceptMemberContacts_Response = CR.CmdOk | CR.ChatCmdError


# User profile commands
# Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).

# Get active user profile.
# Network usage: no.
class ShowActiveUser(TypedDict):
    pass


def ShowActiveUser_cmd_string(self: ShowActiveUser) -> str:
    return '/user'

ShowActiveUser_Response = CR.ActiveUser | CR.ChatCmdError


# Create new user profile.
# Network usage: no.
class CreateActiveUser(TypedDict):
    newUser: "T.NewUser"


def CreateActiveUser_cmd_string(self: CreateActiveUser) -> str:
    return '/_create user ' + json.dumps(self['newUser'])

CreateActiveUser_Response = CR.ActiveUser | CR.ChatCmdError


# Get all user profiles.
# Network usage: no.
class ListUsers(TypedDict):
    pass


def ListUsers_cmd_string(self: ListUsers) -> str:
    return '/users'

ListUsers_Response = CR.UsersList | CR.ChatCmdError


# Set active user profile.
# Network usage: no.
class APISetActiveUser(TypedDict):
    userId: int  # int64
    viewPwd: NotRequired[str]


def APISetActiveUser_cmd_string(self: APISetActiveUser) -> str:
    return '/_user ' + str(self['userId']) + ((' ' + json.dumps(self.get('viewPwd'))) if self.get('viewPwd') is not None else '')

APISetActiveUser_Response = CR.ActiveUser | CR.ChatCmdError


# Delete user profile.
# Network usage: background.
class APIDeleteUser(TypedDict):
    userId: int  # int64
    delSMPQueues: bool
    viewPwd: NotRequired[str]


def APIDeleteUser_cmd_string(self: APIDeleteUser) -> str:
    return '/_delete user ' + str(self['userId']) + ' del_smp=' + ('on' if self['delSMPQueues'] else 'off') + ((' ' + json.dumps(self.get('viewPwd'))) if self.get('viewPwd') is not None else '')

APIDeleteUser_Response = CR.CmdOk | CR.ChatCmdError


# Update user profile.
# Network usage: background.
class APIUpdateProfile(TypedDict):
    userId: int  # int64
    profile: "T.Profile"


def APIUpdateProfile_cmd_string(self: APIUpdateProfile) -> str:
    return '/_profile ' + str(self['userId']) + ' ' + json.dumps(self['profile'])

APIUpdateProfile_Response = CR.UserProfileUpdated | CR.UserProfileNoChange | CR.ChatCmdError


# Configure chat preference overrides for the contact.
# Network usage: background.
class APISetContactPrefs(TypedDict):
    contactId: int  # int64
    preferences: "T.Preferences"


def APISetContactPrefs_cmd_string(self: APISetContactPrefs) -> str:
    return '/_set prefs @' + str(self['contactId']) + ' ' + json.dumps(self['preferences'])

APISetContactPrefs_Response = CR.ContactPrefsUpdated | CR.ChatCmdError


# Chat management
# These commands should not be used with CLI-based bots

# Start chat controller.
# Network usage: no.
class StartChat(TypedDict):
    mainApp: bool
    enableSndFiles: bool


def StartChat_cmd_string(self: StartChat) -> str:
    return '/_start'

StartChat_Response = CR.ChatStarted | CR.ChatRunning


# Stop chat controller.
# Network usage: no.
class APIStopChat(TypedDict):
    pass


def APIStopChat_cmd_string(self: APIStopChat) -> str:
    return '/_stop'

APIStopChat_Response = CR.ChatStopped

