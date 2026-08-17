# API Responses
# This file is generated automatically.
from __future__ import annotations
from typing import Literal, NotRequired, TypedDict
from . import _types as T

class AcceptingContactRequest(TypedDict):
    type: Literal["acceptingContactRequest"]
    user: "T.User"
    contact: "T.Contact"

class ActiveUser(TypedDict):
    type: Literal["activeUser"]
    user: "T.User"

class ChatItemNotChanged(TypedDict):
    type: Literal["chatItemNotChanged"]
    user: "T.User"
    chatItem: "T.AChatItem"

class ChatItemReaction(TypedDict):
    type: Literal["chatItemReaction"]
    user: "T.User"
    added: bool
    reaction: "T.ACIReaction"

class ChatItemUpdated(TypedDict):
    type: Literal["chatItemUpdated"]
    user: "T.User"
    chatItem: "T.AChatItem"

class ChatItemsDeleted(TypedDict):
    type: Literal["chatItemsDeleted"]
    user: "T.User"
    chatItemDeletions: list["T.ChatItemDeletion"]
    byUser: bool
    timed: bool

class ChatRunning(TypedDict):
    type: Literal["chatRunning"]

class ChatStarted(TypedDict):
    type: Literal["chatStarted"]

class ChatStopped(TypedDict):
    type: Literal["chatStopped"]

class CmdOk(TypedDict):
    type: Literal["cmdOk"]
    user_: NotRequired["T.User"]

class ChatCmdError(TypedDict):
    type: Literal["chatCmdError"]
    chatError: "T.ChatError"

class ConnectionPlan(TypedDict):
    type: Literal["connectionPlan"]
    user: "T.User"
    connLink: "T.CreatedConnLink"
    planSimplexName: NotRequired["T.SimplexNameInfo"]
    otherSimplexName: NotRequired["T.SimplexNameInfo"]
    connectionPlan: "T.ConnectionPlan"

class ContactAlreadyExists(TypedDict):
    type: Literal["contactAlreadyExists"]
    user: "T.User"
    contact: "T.Contact"

class ContactConnectionDeleted(TypedDict):
    type: Literal["contactConnectionDeleted"]
    user: "T.User"
    connection: "T.PendingContactConnection"

class ContactDeleted(TypedDict):
    type: Literal["contactDeleted"]
    user: "T.User"
    contact: "T.Contact"

class ContactPrefsUpdated(TypedDict):
    type: Literal["contactPrefsUpdated"]
    user: "T.User"
    fromContact: "T.Contact"
    toContact: "T.Contact"

class ContactRequestRejected(TypedDict):
    type: Literal["contactRequestRejected"]
    user: "T.User"
    contactRequest: "T.UserContactRequest"
    contact_: NotRequired["T.Contact"]

class ContactsList(TypedDict):
    type: Literal["contactsList"]
    user: "T.User"
    contacts: list["T.Contact"]

class GroupDeletedUser(TypedDict):
    type: Literal["groupDeletedUser"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    msgSigned: bool

class GroupLink(TypedDict):
    type: Literal["groupLink"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupLink: "T.GroupLink"

class GroupLinkCreated(TypedDict):
    type: Literal["groupLinkCreated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupLink: "T.GroupLink"

class GroupLinkDeleted(TypedDict):
    type: Literal["groupLinkDeleted"]
    user: "T.User"
    groupInfo: "T.GroupInfo"

class GroupCreated(TypedDict):
    type: Literal["groupCreated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"

class PublicGroupCreated(TypedDict):
    type: Literal["publicGroupCreated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupLink: "T.GroupLink"
    groupRelays: list["T.GroupRelay"]

class PublicGroupCreationFailed(TypedDict):
    type: Literal["publicGroupCreationFailed"]
    user: "T.User"
    addRelayResults: list["T.AddRelayResult"]

class GroupRelays(TypedDict):
    type: Literal["groupRelays"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupRelays: list["T.GroupRelay"]

class GroupRelaysAdded(TypedDict):
    type: Literal["groupRelaysAdded"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupLink: "T.GroupLink"
    groupRelays: list["T.GroupRelay"]

class GroupRelaysAddFailed(TypedDict):
    type: Literal["groupRelaysAddFailed"]
    user: "T.User"
    addRelayResults: list["T.AddRelayResult"]

class RelayGroupAllowed(TypedDict):
    type: Literal["relayGroupAllowed"]
    user: "T.User"
    groupInfo: "T.GroupInfo"

class GroupMembers(TypedDict):
    type: Literal["groupMembers"]
    user: "T.User"
    group: "T.Group"

class GroupUpdated(TypedDict):
    type: Literal["groupUpdated"]
    user: "T.User"
    fromGroup: "T.GroupInfo"
    toGroup: "T.GroupInfo"
    member_: NotRequired["T.GroupMember"]
    msgSigned: bool

class GroupsList(TypedDict):
    type: Literal["groupsList"]
    user: "T.User"
    groups: list["T.GroupInfo"]

class Invitation(TypedDict):
    type: Literal["invitation"]
    user: "T.User"
    connLinkInvitation: "T.CreatedConnLink"
    connection: "T.PendingContactConnection"

class LeftMemberUser(TypedDict):
    type: Literal["leftMemberUser"]
    user: "T.User"
    groupInfo: "T.GroupInfo"

class MemberAccepted(TypedDict):
    type: Literal["memberAccepted"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"

class MembersBlockedForAllUser(TypedDict):
    type: Literal["membersBlockedForAllUser"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    members: list["T.GroupMember"]
    blocked: bool
    msgSigned: bool

class MembersRoleUser(TypedDict):
    type: Literal["membersRoleUser"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    members: list["T.GroupMember"]
    toRole: "T.GroupMemberRole"
    msgSigned: bool

class NewChatItems(TypedDict):
    type: Literal["newChatItems"]
    user: "T.User"
    chatItems: list["T.AChatItem"]

class RcvFileAccepted(TypedDict):
    type: Literal["rcvFileAccepted"]
    user: "T.User"
    chatItem: "T.AChatItem"

class RcvFileAcceptedSndCancelled(TypedDict):
    type: Literal["rcvFileAcceptedSndCancelled"]
    user: "T.User"
    rcvFileTransfer: "T.RcvFileTransfer"

class RcvFileCancelled(TypedDict):
    type: Literal["rcvFileCancelled"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    rcvFileTransfer: "T.RcvFileTransfer"

class SentConfirmation(TypedDict):
    type: Literal["sentConfirmation"]
    user: "T.User"
    connection: "T.PendingContactConnection"
    customUserProfile: NotRequired["T.Profile"]

class SentGroupInvitation(TypedDict):
    type: Literal["sentGroupInvitation"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    contact: "T.Contact"
    member: "T.GroupMember"

class SentInvitation(TypedDict):
    type: Literal["sentInvitation"]
    user: "T.User"
    connection: "T.PendingContactConnection"
    customUserProfile: NotRequired["T.Profile"]

class SndFileCancelled(TypedDict):
    type: Literal["sndFileCancelled"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    fileTransferMeta: "T.FileTransferMeta"
    sndFileTransfers: list["T.SndFileTransfer"]

class UserAcceptedGroupSent(TypedDict):
    type: Literal["userAcceptedGroupSent"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    hostContact: NotRequired["T.Contact"]

class UserContactLink(TypedDict):
    type: Literal["userContactLink"]
    user: "T.User"
    contactLink: "T.UserContactLink"

class UserContactLinkCreated(TypedDict):
    type: Literal["userContactLinkCreated"]
    user: "T.User"
    connLinkContact: "T.CreatedConnLink"

class UserContactLinkDeleted(TypedDict):
    type: Literal["userContactLinkDeleted"]
    user: "T.User"

class UserContactLinkUpdated(TypedDict):
    type: Literal["userContactLinkUpdated"]
    user: "T.User"
    contactLink: "T.UserContactLink"

class UserDeletedMembers(TypedDict):
    type: Literal["userDeletedMembers"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    members: list["T.GroupMember"]
    withMessages: bool
    msgSigned: bool

class UserProfileUpdated(TypedDict):
    type: Literal["userProfileUpdated"]
    user: "T.User"
    fromProfile: "T.Profile"
    toProfile: "T.Profile"
    updateSummary: "T.UserProfileUpdateSummary"

class UserProfileNoChange(TypedDict):
    type: Literal["userProfileNoChange"]
    user: "T.User"

class UsersList(TypedDict):
    type: Literal["usersList"]
    users: list["T.UserInfo"]

class ApiChats(TypedDict):
    type: Literal["apiChats"]
    user: "T.User"
    chats: list["T.AChat"]

ChatResponse = (
    AcceptingContactRequest
    | ActiveUser
    | ChatItemNotChanged
    | ChatItemReaction
    | ChatItemUpdated
    | ChatItemsDeleted
    | ChatRunning
    | ChatStarted
    | ChatStopped
    | CmdOk
    | ChatCmdError
    | ConnectionPlan
    | ContactAlreadyExists
    | ContactConnectionDeleted
    | ContactDeleted
    | ContactPrefsUpdated
    | ContactRequestRejected
    | ContactsList
    | GroupDeletedUser
    | GroupLink
    | GroupLinkCreated
    | GroupLinkDeleted
    | GroupCreated
    | PublicGroupCreated
    | PublicGroupCreationFailed
    | GroupRelays
    | GroupRelaysAdded
    | GroupRelaysAddFailed
    | RelayGroupAllowed
    | GroupMembers
    | GroupUpdated
    | GroupsList
    | Invitation
    | LeftMemberUser
    | MemberAccepted
    | MembersBlockedForAllUser
    | MembersRoleUser
    | NewChatItems
    | RcvFileAccepted
    | RcvFileAcceptedSndCancelled
    | RcvFileCancelled
    | SentConfirmation
    | SentGroupInvitation
    | SentInvitation
    | SndFileCancelled
    | UserAcceptedGroupSent
    | UserContactLink
    | UserContactLinkCreated
    | UserContactLinkDeleted
    | UserContactLinkUpdated
    | UserDeletedMembers
    | UserProfileUpdated
    | UserProfileNoChange
    | UsersList
    | ApiChats
)

ChatResponse_Tag = Literal["acceptingContactRequest", "activeUser", "chatItemNotChanged", "chatItemReaction", "chatItemUpdated", "chatItemsDeleted", "chatRunning", "chatStarted", "chatStopped", "cmdOk", "chatCmdError", "connectionPlan", "contactAlreadyExists", "contactConnectionDeleted", "contactDeleted", "contactPrefsUpdated", "contactRequestRejected", "contactsList", "groupDeletedUser", "groupLink", "groupLinkCreated", "groupLinkDeleted", "groupCreated", "publicGroupCreated", "publicGroupCreationFailed", "groupRelays", "groupRelaysAdded", "groupRelaysAddFailed", "relayGroupAllowed", "groupMembers", "groupUpdated", "groupsList", "invitation", "leftMemberUser", "memberAccepted", "membersBlockedForAllUser", "membersRoleUser", "newChatItems", "rcvFileAccepted", "rcvFileAcceptedSndCancelled", "rcvFileCancelled", "sentConfirmation", "sentGroupInvitation", "sentInvitation", "sndFileCancelled", "userAcceptedGroupSent", "userContactLink", "userContactLinkCreated", "userContactLinkDeleted", "userContactLinkUpdated", "userDeletedMembers", "userProfileUpdated", "userProfileNoChange", "usersList", "apiChats"]
