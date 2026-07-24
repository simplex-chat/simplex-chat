# API Events
# This file is generated automatically.
from __future__ import annotations
from collections.abc import Awaitable, Callable
from typing import Literal, NotRequired, Protocol, TypedDict, overload
from . import _types as T

class ContactConnected(TypedDict):
    type: Literal["contactConnected"]
    user: "T.User"
    contact: "T.Contact"
    userCustomProfile: NotRequired["T.Profile"]

class ContactUpdated(TypedDict):
    type: Literal["contactUpdated"]
    user: "T.User"
    fromContact: "T.Contact"
    toContact: "T.Contact"

class ContactDeletedByContact(TypedDict):
    type: Literal["contactDeletedByContact"]
    user: "T.User"
    contact: "T.Contact"

class ReceivedContactRequest(TypedDict):
    type: Literal["receivedContactRequest"]
    user: "T.User"
    contactRequest: "T.UserContactRequest"
    chat_: NotRequired["T.AChat"]

class NewMemberContactReceivedInv(TypedDict):
    type: Literal["newMemberContactReceivedInv"]
    user: "T.User"
    contact: "T.Contact"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"

class ContactSndReady(TypedDict):
    type: Literal["contactSndReady"]
    user: "T.User"
    contact: "T.Contact"

class NewChatItems(TypedDict):
    type: Literal["newChatItems"]
    user: "T.User"
    chatItems: list["T.AChatItem"]

class ChatItemReaction(TypedDict):
    type: Literal["chatItemReaction"]
    user: "T.User"
    added: bool
    reaction: "T.ACIReaction"

class ChatItemsDeleted(TypedDict):
    type: Literal["chatItemsDeleted"]
    user: "T.User"
    chatItemDeletions: list["T.ChatItemDeletion"]
    byUser: bool
    timed: bool

class ChatItemUpdated(TypedDict):
    type: Literal["chatItemUpdated"]
    user: "T.User"
    chatItem: "T.AChatItem"

class GroupChatItemsDeleted(TypedDict):
    type: Literal["groupChatItemsDeleted"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    chatItemIDs: list[int]  # int64
    byUser: bool
    member_: NotRequired["T.GroupMember"]

class ChatItemsStatusesUpdated(TypedDict):
    type: Literal["chatItemsStatusesUpdated"]
    user: "T.User"
    chatItems: list["T.AChatItem"]

class ReceivedGroupInvitation(TypedDict):
    type: Literal["receivedGroupInvitation"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    contact: "T.Contact"
    fromMemberRole: "T.GroupMemberRole"
    memberRole: "T.GroupMemberRole"

class UserJoinedGroup(TypedDict):
    type: Literal["userJoinedGroup"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    hostMember: "T.GroupMember"

class GroupUpdated(TypedDict):
    type: Literal["groupUpdated"]
    user: "T.User"
    fromGroup: "T.GroupInfo"
    toGroup: "T.GroupInfo"
    member_: NotRequired["T.GroupMember"]
    msgSigned: NotRequired["T.MsgSigStatus"]

class JoinedGroupMember(TypedDict):
    type: Literal["joinedGroupMember"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"

class MemberRole(TypedDict):
    type: Literal["memberRole"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    byMember: "T.GroupMember"
    member: "T.GroupMember"
    fromRole: "T.GroupMemberRole"
    toRole: "T.GroupMemberRole"
    msgSigned: NotRequired["T.MsgSigStatus"]

class DeletedMember(TypedDict):
    type: Literal["deletedMember"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    byMember: "T.GroupMember"
    deletedMember: "T.GroupMember"
    withMessages: bool
    msgSigned: NotRequired["T.MsgSigStatus"]

class LeftMember(TypedDict):
    type: Literal["leftMember"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"
    msgSigned: NotRequired["T.MsgSigStatus"]

class DeletedMemberUser(TypedDict):
    type: Literal["deletedMemberUser"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"
    withMessages: bool
    msgSigned: NotRequired["T.MsgSigStatus"]

class GroupDeleted(TypedDict):
    type: Literal["groupDeleted"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"
    msgSigned: NotRequired["T.MsgSigStatus"]

class ConnectedToGroupMember(TypedDict):
    type: Literal["connectedToGroupMember"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"
    memberContact: NotRequired["T.Contact"]

class MemberAcceptedByOther(TypedDict):
    type: Literal["memberAcceptedByOther"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    acceptingMember: "T.GroupMember"
    member: "T.GroupMember"

class MemberBlockedForAll(TypedDict):
    type: Literal["memberBlockedForAll"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    byMember: "T.GroupMember"
    member: "T.GroupMember"
    blocked: bool
    msgSigned: NotRequired["T.MsgSigStatus"]

class GroupMemberUpdated(TypedDict):
    type: Literal["groupMemberUpdated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    fromMember: "T.GroupMember"
    toMember: "T.GroupMember"

class GroupLinkDataUpdated(TypedDict):
    type: Literal["groupLinkDataUpdated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    groupLink: "T.GroupLink"
    groupRelays: list["T.GroupRelay"]
    relaysChanged: bool

class GroupRelayUpdated(TypedDict):
    type: Literal["groupRelayUpdated"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    member: "T.GroupMember"
    groupRelay: "T.GroupRelay"

class RcvFileDescrReady(TypedDict):
    type: Literal["rcvFileDescrReady"]
    user: "T.User"
    chatItem: "T.AChatItem"
    rcvFileTransfer: "T.RcvFileTransfer"
    rcvFileDescr: "T.RcvFileDescr"

class RcvFileComplete(TypedDict):
    type: Literal["rcvFileComplete"]
    user: "T.User"
    chatItem: "T.AChatItem"

class SndFileCompleteXFTP(TypedDict):
    type: Literal["sndFileCompleteXFTP"]
    user: "T.User"
    chatItem: "T.AChatItem"
    fileTransferMeta: "T.FileTransferMeta"

class RcvFileStart(TypedDict):
    type: Literal["rcvFileStart"]
    user: "T.User"
    chatItem: "T.AChatItem"

class RcvFileSndCancelled(TypedDict):
    type: Literal["rcvFileSndCancelled"]
    user: "T.User"
    chatItem: "T.AChatItem"
    rcvFileTransfer: "T.RcvFileTransfer"

class RcvFileAccepted(TypedDict):
    type: Literal["rcvFileAccepted"]
    user: "T.User"
    chatItem: "T.AChatItem"

class RcvFileError(TypedDict):
    type: Literal["rcvFileError"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    agentError: "T.AgentErrorType"
    rcvFileTransfer: "T.RcvFileTransfer"

class RcvFileWarning(TypedDict):
    type: Literal["rcvFileWarning"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    agentError: "T.AgentErrorType"
    rcvFileTransfer: "T.RcvFileTransfer"

class SndFileError(TypedDict):
    type: Literal["sndFileError"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    fileTransferMeta: "T.FileTransferMeta"
    errorMessage: str

class SndFileWarning(TypedDict):
    type: Literal["sndFileWarning"]
    user: "T.User"
    chatItem_: NotRequired["T.AChatItem"]
    fileTransferMeta: "T.FileTransferMeta"
    errorMessage: str

class AcceptingContactRequest(TypedDict):
    type: Literal["acceptingContactRequest"]
    user: "T.User"
    contact: "T.Contact"

class AcceptingBusinessRequest(TypedDict):
    type: Literal["acceptingBusinessRequest"]
    user: "T.User"
    groupInfo: "T.GroupInfo"

class ContactConnecting(TypedDict):
    type: Literal["contactConnecting"]
    user: "T.User"
    contact: "T.Contact"

class BusinessLinkConnecting(TypedDict):
    type: Literal["businessLinkConnecting"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    hostMember: "T.GroupMember"
    fromContact: "T.Contact"

class JoinedGroupMemberConnecting(TypedDict):
    type: Literal["joinedGroupMemberConnecting"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    hostMember: "T.GroupMember"
    member: "T.GroupMember"

class SentGroupInvitation(TypedDict):
    type: Literal["sentGroupInvitation"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    contact: "T.Contact"
    member: "T.GroupMember"

class GroupLinkConnecting(TypedDict):
    type: Literal["groupLinkConnecting"]
    user: "T.User"
    groupInfo: "T.GroupInfo"
    hostMember: "T.GroupMember"

class HostConnected(TypedDict):
    type: Literal["hostConnected"]
    protocol: str
    transportHost: str

class HostDisconnected(TypedDict):
    type: Literal["hostDisconnected"]
    protocol: str
    transportHost: str

class SubscriptionStatus(TypedDict):
    type: Literal["subscriptionStatus"]
    server: str
    subscriptionStatus: "T.SubscriptionStatus"
    connections: list[str]

class ServiceRequest(TypedDict):
    type: Literal["serviceRequest"]
    user: "T.User"
    requestId: str
    requestData: dict[str, object]

class ServiceReplySent(TypedDict):
    type: Literal["serviceReplySent"]
    connectionId: str

class MessageError(TypedDict):
    type: Literal["messageError"]
    user: "T.User"
    severity: str
    errorMessage: str

class ChatError(TypedDict):
    type: Literal["chatError"]
    chatError: "T.ChatError"

class ChatErrors(TypedDict):
    type: Literal["chatErrors"]
    chatErrors: list["T.ChatError"]

ChatEvent = (
    ContactConnected
    | ContactUpdated
    | ContactDeletedByContact
    | ReceivedContactRequest
    | NewMemberContactReceivedInv
    | ContactSndReady
    | NewChatItems
    | ChatItemReaction
    | ChatItemsDeleted
    | ChatItemUpdated
    | GroupChatItemsDeleted
    | ChatItemsStatusesUpdated
    | ReceivedGroupInvitation
    | UserJoinedGroup
    | GroupUpdated
    | JoinedGroupMember
    | MemberRole
    | DeletedMember
    | LeftMember
    | DeletedMemberUser
    | GroupDeleted
    | ConnectedToGroupMember
    | MemberAcceptedByOther
    | MemberBlockedForAll
    | GroupMemberUpdated
    | GroupLinkDataUpdated
    | GroupRelayUpdated
    | RcvFileDescrReady
    | RcvFileComplete
    | SndFileCompleteXFTP
    | RcvFileStart
    | RcvFileSndCancelled
    | RcvFileAccepted
    | RcvFileError
    | RcvFileWarning
    | SndFileError
    | SndFileWarning
    | AcceptingContactRequest
    | AcceptingBusinessRequest
    | ContactConnecting
    | BusinessLinkConnecting
    | JoinedGroupMemberConnecting
    | SentGroupInvitation
    | GroupLinkConnecting
    | HostConnected
    | HostDisconnected
    | SubscriptionStatus
    | ServiceRequest
    | ServiceReplySent
    | MessageError
    | ChatError
    | ChatErrors
)

ChatEvent_Tag = Literal["contactConnected", "contactUpdated", "contactDeletedByContact", "receivedContactRequest", "newMemberContactReceivedInv", "contactSndReady", "newChatItems", "chatItemReaction", "chatItemsDeleted", "chatItemUpdated", "groupChatItemsDeleted", "chatItemsStatusesUpdated", "receivedGroupInvitation", "userJoinedGroup", "groupUpdated", "joinedGroupMember", "memberRole", "deletedMember", "leftMember", "deletedMemberUser", "groupDeleted", "connectedToGroupMember", "memberAcceptedByOther", "memberBlockedForAll", "groupMemberUpdated", "groupLinkDataUpdated", "groupRelayUpdated", "rcvFileDescrReady", "rcvFileComplete", "sndFileCompleteXFTP", "rcvFileStart", "rcvFileSndCancelled", "rcvFileAccepted", "rcvFileError", "rcvFileWarning", "sndFileError", "sndFileWarning", "acceptingContactRequest", "acceptingBusinessRequest", "contactConnecting", "businessLinkConnecting", "joinedGroupMemberConnecting", "sentGroupInvitation", "groupLinkConnecting", "hostConnected", "hostDisconnected", "subscriptionStatus", "serviceRequest", "serviceReplySent", "messageError", "chatError", "chatErrors"]


class OnEventDecorator(Protocol):
    """Per-tag narrowing protocol for ``Client.on_event``.

    ``@client.on_event("contactConnected")`` types the handler's
    ``evt`` parameter as :class:`ContactConnected` rather than the
    unnarrowed :data:`ChatEvent` union.
    """

    @overload
    def __call__(self, event: Literal["contactConnected"], /) -> Callable[
        [Callable[["ContactConnected"], Awaitable[None]]],
        Callable[["ContactConnected"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["contactUpdated"], /) -> Callable[
        [Callable[["ContactUpdated"], Awaitable[None]]],
        Callable[["ContactUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["contactDeletedByContact"], /) -> Callable[
        [Callable[["ContactDeletedByContact"], Awaitable[None]]],
        Callable[["ContactDeletedByContact"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["receivedContactRequest"], /) -> Callable[
        [Callable[["ReceivedContactRequest"], Awaitable[None]]],
        Callable[["ReceivedContactRequest"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["newMemberContactReceivedInv"], /) -> Callable[
        [Callable[["NewMemberContactReceivedInv"], Awaitable[None]]],
        Callable[["NewMemberContactReceivedInv"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["contactSndReady"], /) -> Callable[
        [Callable[["ContactSndReady"], Awaitable[None]]],
        Callable[["ContactSndReady"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["newChatItems"], /) -> Callable[
        [Callable[["NewChatItems"], Awaitable[None]]],
        Callable[["NewChatItems"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatItemReaction"], /) -> Callable[
        [Callable[["ChatItemReaction"], Awaitable[None]]],
        Callable[["ChatItemReaction"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatItemsDeleted"], /) -> Callable[
        [Callable[["ChatItemsDeleted"], Awaitable[None]]],
        Callable[["ChatItemsDeleted"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatItemUpdated"], /) -> Callable[
        [Callable[["ChatItemUpdated"], Awaitable[None]]],
        Callable[["ChatItemUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupChatItemsDeleted"], /) -> Callable[
        [Callable[["GroupChatItemsDeleted"], Awaitable[None]]],
        Callable[["GroupChatItemsDeleted"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatItemsStatusesUpdated"], /) -> Callable[
        [Callable[["ChatItemsStatusesUpdated"], Awaitable[None]]],
        Callable[["ChatItemsStatusesUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["receivedGroupInvitation"], /) -> Callable[
        [Callable[["ReceivedGroupInvitation"], Awaitable[None]]],
        Callable[["ReceivedGroupInvitation"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["userJoinedGroup"], /) -> Callable[
        [Callable[["UserJoinedGroup"], Awaitable[None]]],
        Callable[["UserJoinedGroup"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupUpdated"], /) -> Callable[
        [Callable[["GroupUpdated"], Awaitable[None]]],
        Callable[["GroupUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["joinedGroupMember"], /) -> Callable[
        [Callable[["JoinedGroupMember"], Awaitable[None]]],
        Callable[["JoinedGroupMember"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["memberRole"], /) -> Callable[
        [Callable[["MemberRole"], Awaitable[None]]],
        Callable[["MemberRole"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["deletedMember"], /) -> Callable[
        [Callable[["DeletedMember"], Awaitable[None]]],
        Callable[["DeletedMember"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["leftMember"], /) -> Callable[
        [Callable[["LeftMember"], Awaitable[None]]],
        Callable[["LeftMember"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["deletedMemberUser"], /) -> Callable[
        [Callable[["DeletedMemberUser"], Awaitable[None]]],
        Callable[["DeletedMemberUser"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupDeleted"], /) -> Callable[
        [Callable[["GroupDeleted"], Awaitable[None]]],
        Callable[["GroupDeleted"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["connectedToGroupMember"], /) -> Callable[
        [Callable[["ConnectedToGroupMember"], Awaitable[None]]],
        Callable[["ConnectedToGroupMember"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["memberAcceptedByOther"], /) -> Callable[
        [Callable[["MemberAcceptedByOther"], Awaitable[None]]],
        Callable[["MemberAcceptedByOther"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["memberBlockedForAll"], /) -> Callable[
        [Callable[["MemberBlockedForAll"], Awaitable[None]]],
        Callable[["MemberBlockedForAll"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupMemberUpdated"], /) -> Callable[
        [Callable[["GroupMemberUpdated"], Awaitable[None]]],
        Callable[["GroupMemberUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupLinkDataUpdated"], /) -> Callable[
        [Callable[["GroupLinkDataUpdated"], Awaitable[None]]],
        Callable[["GroupLinkDataUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupRelayUpdated"], /) -> Callable[
        [Callable[["GroupRelayUpdated"], Awaitable[None]]],
        Callable[["GroupRelayUpdated"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileDescrReady"], /) -> Callable[
        [Callable[["RcvFileDescrReady"], Awaitable[None]]],
        Callable[["RcvFileDescrReady"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileComplete"], /) -> Callable[
        [Callable[["RcvFileComplete"], Awaitable[None]]],
        Callable[["RcvFileComplete"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["sndFileCompleteXFTP"], /) -> Callable[
        [Callable[["SndFileCompleteXFTP"], Awaitable[None]]],
        Callable[["SndFileCompleteXFTP"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileStart"], /) -> Callable[
        [Callable[["RcvFileStart"], Awaitable[None]]],
        Callable[["RcvFileStart"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileSndCancelled"], /) -> Callable[
        [Callable[["RcvFileSndCancelled"], Awaitable[None]]],
        Callable[["RcvFileSndCancelled"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileAccepted"], /) -> Callable[
        [Callable[["RcvFileAccepted"], Awaitable[None]]],
        Callable[["RcvFileAccepted"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileError"], /) -> Callable[
        [Callable[["RcvFileError"], Awaitable[None]]],
        Callable[["RcvFileError"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["rcvFileWarning"], /) -> Callable[
        [Callable[["RcvFileWarning"], Awaitable[None]]],
        Callable[["RcvFileWarning"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["sndFileError"], /) -> Callable[
        [Callable[["SndFileError"], Awaitable[None]]],
        Callable[["SndFileError"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["sndFileWarning"], /) -> Callable[
        [Callable[["SndFileWarning"], Awaitable[None]]],
        Callable[["SndFileWarning"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["acceptingContactRequest"], /) -> Callable[
        [Callable[["AcceptingContactRequest"], Awaitable[None]]],
        Callable[["AcceptingContactRequest"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["acceptingBusinessRequest"], /) -> Callable[
        [Callable[["AcceptingBusinessRequest"], Awaitable[None]]],
        Callable[["AcceptingBusinessRequest"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["contactConnecting"], /) -> Callable[
        [Callable[["ContactConnecting"], Awaitable[None]]],
        Callable[["ContactConnecting"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["businessLinkConnecting"], /) -> Callable[
        [Callable[["BusinessLinkConnecting"], Awaitable[None]]],
        Callable[["BusinessLinkConnecting"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["joinedGroupMemberConnecting"], /) -> Callable[
        [Callable[["JoinedGroupMemberConnecting"], Awaitable[None]]],
        Callable[["JoinedGroupMemberConnecting"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["sentGroupInvitation"], /) -> Callable[
        [Callable[["SentGroupInvitation"], Awaitable[None]]],
        Callable[["SentGroupInvitation"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["groupLinkConnecting"], /) -> Callable[
        [Callable[["GroupLinkConnecting"], Awaitable[None]]],
        Callable[["GroupLinkConnecting"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["hostConnected"], /) -> Callable[
        [Callable[["HostConnected"], Awaitable[None]]],
        Callable[["HostConnected"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["hostDisconnected"], /) -> Callable[
        [Callable[["HostDisconnected"], Awaitable[None]]],
        Callable[["HostDisconnected"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["subscriptionStatus"], /) -> Callable[
        [Callable[["SubscriptionStatus"], Awaitable[None]]],
        Callable[["SubscriptionStatus"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["serviceRequest"], /) -> Callable[
        [Callable[["ServiceRequest"], Awaitable[None]]],
        Callable[["ServiceRequest"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["serviceReplySent"], /) -> Callable[
        [Callable[["ServiceReplySent"], Awaitable[None]]],
        Callable[["ServiceReplySent"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["messageError"], /) -> Callable[
        [Callable[["MessageError"], Awaitable[None]]],
        Callable[["MessageError"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatError"], /) -> Callable[
        [Callable[["ChatError"], Awaitable[None]]],
        Callable[["ChatError"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: Literal["chatErrors"], /) -> Callable[
        [Callable[["ChatErrors"], Awaitable[None]]],
        Callable[["ChatErrors"], Awaitable[None]],
    ]: ...

    @overload
    def __call__(self, event: str, /) -> Callable[
        [Callable[["ChatEvent"], Awaitable[None]]],
        Callable[["ChatEvent"], Awaitable[None]],
    ]: ...
