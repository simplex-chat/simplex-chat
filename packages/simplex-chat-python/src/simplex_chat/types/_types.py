# API Types
# This file is generated automatically.
from __future__ import annotations
from typing import Literal, NotRequired, TypedDict

class ACIReaction(TypedDict):
    chatInfo: "ChatInfo"
    chatReaction: "CIReaction"

class AChat(TypedDict):
    chatInfo: "ChatInfo"
    chatItems: list["ChatItem"]
    chatStats: "ChatStats"

class AChatItem(TypedDict):
    chatInfo: "ChatInfo"
    chatItem: "ChatItem"

class AddRelayResult(TypedDict):
    relay: "UserChatRelay"
    relayError: NotRequired["ChatError"]

class AddressSettings(TypedDict):
    businessAddress: bool
    autoAccept: NotRequired["AutoAccept"]
    autoReply: NotRequired["MsgContent"]

class AgentCryptoError_DECRYPT_AES(TypedDict):
    type: Literal["DECRYPT_AES"]

class AgentCryptoError_DECRYPT_CB(TypedDict):
    type: Literal["DECRYPT_CB"]

class AgentCryptoError_RATCHET_HEADER(TypedDict):
    type: Literal["RATCHET_HEADER"]

class AgentCryptoError_RATCHET_SYNC(TypedDict):
    type: Literal["RATCHET_SYNC"]

AgentCryptoError = (
    AgentCryptoError_DECRYPT_AES
    | AgentCryptoError_DECRYPT_CB
    | AgentCryptoError_RATCHET_HEADER
    | AgentCryptoError_RATCHET_SYNC
)

AgentCryptoError_Tag = Literal["DECRYPT_AES", "DECRYPT_CB", "RATCHET_HEADER", "RATCHET_SYNC"]

class AgentErrorType_CMD(TypedDict):
    type: Literal["CMD"]
    cmdErr: "CommandErrorType"
    errContext: str

class AgentErrorType_CONN(TypedDict):
    type: Literal["CONN"]
    connErr: "ConnectionErrorType"
    errContext: str

class AgentErrorType_NO_USER(TypedDict):
    type: Literal["NO_USER"]

class AgentErrorType_SMP(TypedDict):
    type: Literal["SMP"]
    serverAddress: str
    smpErr: "ErrorType"

class AgentErrorType_NTF(TypedDict):
    type: Literal["NTF"]
    serverAddress: str
    ntfErr: "ErrorType"

class AgentErrorType_XFTP(TypedDict):
    type: Literal["XFTP"]
    serverAddress: str
    xftpErr: "XFTPErrorType"

class AgentErrorType_FILE(TypedDict):
    type: Literal["FILE"]
    fileErr: "FileErrorType"

class AgentErrorType_NO_NAME_SERVERS(TypedDict):
    type: Literal["NO_NAME_SERVERS"]

class AgentErrorType_PROXY(TypedDict):
    type: Literal["PROXY"]
    proxyServer: str
    relayServer: str
    proxyErr: "ProxyClientError"

class AgentErrorType_RCP(TypedDict):
    type: Literal["RCP"]
    rcpErr: "RCErrorType"

class AgentErrorType_BROKER(TypedDict):
    type: Literal["BROKER"]
    brokerAddress: str
    brokerErr: "BrokerErrorType"

class AgentErrorType_AGENT(TypedDict):
    type: Literal["AGENT"]
    agentErr: "SMPAgentError"

class AgentErrorType_NOTICE(TypedDict):
    type: Literal["NOTICE"]
    server: str
    preset: bool
    expiresAt: NotRequired[str]  # ISO-8601 timestamp

class AgentErrorType_INTERNAL(TypedDict):
    type: Literal["INTERNAL"]
    internalErr: str

class AgentErrorType_CRITICAL(TypedDict):
    type: Literal["CRITICAL"]
    offerRestart: bool
    criticalErr: str

class AgentErrorType_INACTIVE(TypedDict):
    type: Literal["INACTIVE"]

AgentErrorType = (
    AgentErrorType_CMD
    | AgentErrorType_CONN
    | AgentErrorType_NO_USER
    | AgentErrorType_SMP
    | AgentErrorType_NTF
    | AgentErrorType_XFTP
    | AgentErrorType_FILE
    | AgentErrorType_NO_NAME_SERVERS
    | AgentErrorType_PROXY
    | AgentErrorType_RCP
    | AgentErrorType_BROKER
    | AgentErrorType_AGENT
    | AgentErrorType_NOTICE
    | AgentErrorType_INTERNAL
    | AgentErrorType_CRITICAL
    | AgentErrorType_INACTIVE
)

AgentErrorType_Tag = Literal["CMD", "CONN", "NO_USER", "SMP", "NTF", "XFTP", "FILE", "NO_NAME_SERVERS", "PROXY", "RCP", "BROKER", "AGENT", "NOTICE", "INTERNAL", "CRITICAL", "INACTIVE"]

class AgentServiceError_rejected(TypedDict):
    type: Literal["rejected"]
    rejectReason: str

class AgentServiceError_timeout(TypedDict):
    type: Literal["timeout"]

class AgentServiceError_noPendingRequest(TypedDict):
    type: Literal["noPendingRequest"]

class AgentServiceError_notDRAddress(TypedDict):
    type: Literal["notDRAddress"]

AgentServiceError = (
    AgentServiceError_rejected
    | AgentServiceError_timeout
    | AgentServiceError_noPendingRequest
    | AgentServiceError_notDRAddress
)

AgentServiceError_Tag = Literal["rejected", "timeout", "noPendingRequest", "notDRAddress"]

class AutoAccept(TypedDict):
    acceptIncognito: bool

class BadgeInfo(TypedDict):
    badgeType: "BadgeType"
    badgeExpiry: NotRequired[str]  # ISO-8601 timestamp
    badgeExtra: str

class BadgeProof(TypedDict):
    badgeKeyIdx: int  # int
    presHeader: str
    proof: str
    badgeInfo: "BadgeInfo"

BadgeStatus = Literal["active", "expired", "expiredOld", "failed", "unknownKey"]

BadgeType = Literal["supporter", "legend", "investor"]

class BlockingInfo(TypedDict):
    reason: "BlockingReason"
    notice: NotRequired["ClientNotice"]

BlockingReason = Literal["spam", "content"]

class BrokerErrorType_RESPONSE(TypedDict):
    type: Literal["RESPONSE"]
    respErr: str

class BrokerErrorType_UNEXPECTED(TypedDict):
    type: Literal["UNEXPECTED"]
    respErr: str

class BrokerErrorType_NETWORK(TypedDict):
    type: Literal["NETWORK"]
    networkError: "NetworkError"

class BrokerErrorType_HOST(TypedDict):
    type: Literal["HOST"]

class BrokerErrorType_NO_SERVICE(TypedDict):
    type: Literal["NO_SERVICE"]

class BrokerErrorType_TRANSPORT(TypedDict):
    type: Literal["TRANSPORT"]
    transportErr: "TransportError"

class BrokerErrorType_TIMEOUT(TypedDict):
    type: Literal["TIMEOUT"]

BrokerErrorType = (
    BrokerErrorType_RESPONSE
    | BrokerErrorType_UNEXPECTED
    | BrokerErrorType_NETWORK
    | BrokerErrorType_HOST
    | BrokerErrorType_NO_SERVICE
    | BrokerErrorType_TRANSPORT
    | BrokerErrorType_TIMEOUT
)

BrokerErrorType_Tag = Literal["RESPONSE", "UNEXPECTED", "NETWORK", "HOST", "NO_SERVICE", "TRANSPORT", "TIMEOUT"]

class BusinessChatInfo(TypedDict):
    chatType: "BusinessChatType"
    businessId: str
    customerId: str
    businessDomain: NotRequired["SimplexDomainClaim"]

BusinessChatType = Literal["business", "customer"]

CICallStatus = Literal["pending", "missed", "rejected", "accepted", "negotiated", "progress", "ended", "error"]

class CIContent_sndMsgContent(TypedDict):
    type: Literal["sndMsgContent"]
    msgContent: "MsgContent"

class CIContent_rcvMsgContent(TypedDict):
    type: Literal["rcvMsgContent"]
    msgContent: "MsgContent"

class CIContent_sndDeleted(TypedDict):
    type: Literal["sndDeleted"]
    deleteMode: "CIDeleteMode"

class CIContent_rcvDeleted(TypedDict):
    type: Literal["rcvDeleted"]
    deleteMode: "CIDeleteMode"

class CIContent_sndCall(TypedDict):
    type: Literal["sndCall"]
    status: "CICallStatus"
    duration: int  # int

class CIContent_rcvCall(TypedDict):
    type: Literal["rcvCall"]
    status: "CICallStatus"
    duration: int  # int

class CIContent_rcvIntegrityError(TypedDict):
    type: Literal["rcvIntegrityError"]
    msgError: "MsgErrorType"

class CIContent_rcvDecryptionError(TypedDict):
    type: Literal["rcvDecryptionError"]
    msgDecryptError: "MsgDecryptError"
    msgCount: int  # word32

class CIContent_rcvMsgError(TypedDict):
    type: Literal["rcvMsgError"]
    rcvMsgError: "RcvMsgError"

class CIContent_rcvGroupInvitation(TypedDict):
    type: Literal["rcvGroupInvitation"]
    groupInvitation: "CIGroupInvitation"
    memberRole: "GroupMemberRole"

class CIContent_sndGroupInvitation(TypedDict):
    type: Literal["sndGroupInvitation"]
    groupInvitation: "CIGroupInvitation"
    memberRole: "GroupMemberRole"

class CIContent_rcvDirectEvent(TypedDict):
    type: Literal["rcvDirectEvent"]
    rcvDirectEvent: "RcvDirectEvent"

class CIContent_rcvGroupEvent(TypedDict):
    type: Literal["rcvGroupEvent"]
    rcvGroupEvent: "RcvGroupEvent"

class CIContent_sndGroupEvent(TypedDict):
    type: Literal["sndGroupEvent"]
    sndGroupEvent: "SndGroupEvent"

class CIContent_rcvConnEvent(TypedDict):
    type: Literal["rcvConnEvent"]
    rcvConnEvent: "RcvConnEvent"

class CIContent_sndConnEvent(TypedDict):
    type: Literal["sndConnEvent"]
    sndConnEvent: "SndConnEvent"

class CIContent_rcvChatFeature(TypedDict):
    type: Literal["rcvChatFeature"]
    feature: "ChatFeature"
    enabled: "PrefEnabled"
    param: NotRequired[int]  # int

class CIContent_sndChatFeature(TypedDict):
    type: Literal["sndChatFeature"]
    feature: "ChatFeature"
    enabled: "PrefEnabled"
    param: NotRequired[int]  # int

class CIContent_rcvChatPreference(TypedDict):
    type: Literal["rcvChatPreference"]
    feature: "ChatFeature"
    allowed: "FeatureAllowed"
    param: NotRequired[int]  # int

class CIContent_sndChatPreference(TypedDict):
    type: Literal["sndChatPreference"]
    feature: "ChatFeature"
    allowed: "FeatureAllowed"
    param: NotRequired[int]  # int

class CIContent_rcvGroupFeature(TypedDict):
    type: Literal["rcvGroupFeature"]
    groupFeature: "GroupFeature"
    preference: "GroupPreference"
    param: NotRequired[int]  # int
    memberRole_: NotRequired["GroupMemberRole"]

class CIContent_sndGroupFeature(TypedDict):
    type: Literal["sndGroupFeature"]
    groupFeature: "GroupFeature"
    preference: "GroupPreference"
    param: NotRequired[int]  # int
    memberRole_: NotRequired["GroupMemberRole"]

class CIContent_rcvChatFeatureRejected(TypedDict):
    type: Literal["rcvChatFeatureRejected"]
    feature: "ChatFeature"

class CIContent_rcvGroupFeatureRejected(TypedDict):
    type: Literal["rcvGroupFeatureRejected"]
    groupFeature: "GroupFeature"

class CIContent_sndModerated(TypedDict):
    type: Literal["sndModerated"]

class CIContent_rcvModerated(TypedDict):
    type: Literal["rcvModerated"]

class CIContent_rcvBlocked(TypedDict):
    type: Literal["rcvBlocked"]

class CIContent_sndDirectE2EEInfo(TypedDict):
    type: Literal["sndDirectE2EEInfo"]
    e2eeInfo: "E2EInfo"

class CIContent_rcvDirectE2EEInfo(TypedDict):
    type: Literal["rcvDirectE2EEInfo"]
    e2eeInfo: "E2EInfo"

class CIContent_sndGroupE2EEInfo(TypedDict):
    type: Literal["sndGroupE2EEInfo"]
    e2eeInfo: "E2EInfo"

class CIContent_rcvGroupE2EEInfo(TypedDict):
    type: Literal["rcvGroupE2EEInfo"]
    e2eeInfo: "E2EInfo"

class CIContent_chatBanner(TypedDict):
    type: Literal["chatBanner"]

CIContent = (
    CIContent_sndMsgContent
    | CIContent_rcvMsgContent
    | CIContent_sndDeleted
    | CIContent_rcvDeleted
    | CIContent_sndCall
    | CIContent_rcvCall
    | CIContent_rcvIntegrityError
    | CIContent_rcvDecryptionError
    | CIContent_rcvMsgError
    | CIContent_rcvGroupInvitation
    | CIContent_sndGroupInvitation
    | CIContent_rcvDirectEvent
    | CIContent_rcvGroupEvent
    | CIContent_sndGroupEvent
    | CIContent_rcvConnEvent
    | CIContent_sndConnEvent
    | CIContent_rcvChatFeature
    | CIContent_sndChatFeature
    | CIContent_rcvChatPreference
    | CIContent_sndChatPreference
    | CIContent_rcvGroupFeature
    | CIContent_sndGroupFeature
    | CIContent_rcvChatFeatureRejected
    | CIContent_rcvGroupFeatureRejected
    | CIContent_sndModerated
    | CIContent_rcvModerated
    | CIContent_rcvBlocked
    | CIContent_sndDirectE2EEInfo
    | CIContent_rcvDirectE2EEInfo
    | CIContent_sndGroupE2EEInfo
    | CIContent_rcvGroupE2EEInfo
    | CIContent_chatBanner
)

CIContent_Tag = Literal["sndMsgContent", "rcvMsgContent", "sndDeleted", "rcvDeleted", "sndCall", "rcvCall", "rcvIntegrityError", "rcvDecryptionError", "rcvMsgError", "rcvGroupInvitation", "sndGroupInvitation", "rcvDirectEvent", "rcvGroupEvent", "sndGroupEvent", "rcvConnEvent", "sndConnEvent", "rcvChatFeature", "sndChatFeature", "rcvChatPreference", "sndChatPreference", "rcvGroupFeature", "sndGroupFeature", "rcvChatFeatureRejected", "rcvGroupFeatureRejected", "sndModerated", "rcvModerated", "rcvBlocked", "sndDirectE2EEInfo", "rcvDirectE2EEInfo", "sndGroupE2EEInfo", "rcvGroupE2EEInfo", "chatBanner"]

CIDeleteMode = Literal["broadcast", "internal", "internalMark", "history"]

class CIDeleted_deleted(TypedDict):
    type: Literal["deleted"]
    deletedTs: NotRequired[str]  # ISO-8601 timestamp
    chatType: "ChatType"

class CIDeleted_blocked(TypedDict):
    type: Literal["blocked"]
    deletedTs: NotRequired[str]  # ISO-8601 timestamp

class CIDeleted_blockedByAdmin(TypedDict):
    type: Literal["blockedByAdmin"]
    deletedTs: NotRequired[str]  # ISO-8601 timestamp

class CIDeleted_moderated(TypedDict):
    type: Literal["moderated"]
    deletedTs: NotRequired[str]  # ISO-8601 timestamp
    byGroupMember: "GroupMember"

CIDeleted = CIDeleted_deleted | CIDeleted_blocked | CIDeleted_blockedByAdmin | CIDeleted_moderated

CIDeleted_Tag = Literal["deleted", "blocked", "blockedByAdmin", "moderated"]

class CIDirection_directSnd(TypedDict):
    type: Literal["directSnd"]

class CIDirection_directRcv(TypedDict):
    type: Literal["directRcv"]

class CIDirection_groupSnd(TypedDict):
    type: Literal["groupSnd"]

class CIDirection_groupRcv(TypedDict):
    type: Literal["groupRcv"]
    groupMember: "GroupMember"

class CIDirection_channelRcv(TypedDict):
    type: Literal["channelRcv"]

class CIDirection_localSnd(TypedDict):
    type: Literal["localSnd"]

class CIDirection_localRcv(TypedDict):
    type: Literal["localRcv"]

CIDirection = (
    CIDirection_directSnd
    | CIDirection_directRcv
    | CIDirection_groupSnd
    | CIDirection_groupRcv
    | CIDirection_channelRcv
    | CIDirection_localSnd
    | CIDirection_localRcv
)

CIDirection_Tag = Literal["directSnd", "directRcv", "groupSnd", "groupRcv", "channelRcv", "localSnd", "localRcv"]

class CIFile(TypedDict):
    fileId: int  # int64
    fileName: str
    fileSize: int  # int64
    fileSource: NotRequired["CryptoFile"]
    fileStatus: "CIFileStatus"
    fileProtocol: "FileProtocol"

class CIFileStatus_sndStored(TypedDict):
    type: Literal["sndStored"]

class CIFileStatus_sndTransfer(TypedDict):
    type: Literal["sndTransfer"]
    sndProgress: int  # int64
    sndTotal: int  # int64

class CIFileStatus_sndCancelled(TypedDict):
    type: Literal["sndCancelled"]

class CIFileStatus_sndComplete(TypedDict):
    type: Literal["sndComplete"]

class CIFileStatus_sndError(TypedDict):
    type: Literal["sndError"]
    sndFileError: "FileError"

class CIFileStatus_sndWarning(TypedDict):
    type: Literal["sndWarning"]
    sndFileError: "FileError"

class CIFileStatus_rcvInvitation(TypedDict):
    type: Literal["rcvInvitation"]

class CIFileStatus_rcvAccepted(TypedDict):
    type: Literal["rcvAccepted"]

class CIFileStatus_rcvTransfer(TypedDict):
    type: Literal["rcvTransfer"]
    rcvProgress: int  # int64
    rcvTotal: int  # int64

class CIFileStatus_rcvAborted(TypedDict):
    type: Literal["rcvAborted"]

class CIFileStatus_rcvComplete(TypedDict):
    type: Literal["rcvComplete"]

class CIFileStatus_rcvCancelled(TypedDict):
    type: Literal["rcvCancelled"]

class CIFileStatus_rcvError(TypedDict):
    type: Literal["rcvError"]
    rcvFileError: "FileError"

class CIFileStatus_rcvWarning(TypedDict):
    type: Literal["rcvWarning"]
    rcvFileError: "FileError"

class CIFileStatus_invalid(TypedDict):
    type: Literal["invalid"]
    text: str

CIFileStatus = (
    CIFileStatus_sndStored
    | CIFileStatus_sndTransfer
    | CIFileStatus_sndCancelled
    | CIFileStatus_sndComplete
    | CIFileStatus_sndError
    | CIFileStatus_sndWarning
    | CIFileStatus_rcvInvitation
    | CIFileStatus_rcvAccepted
    | CIFileStatus_rcvTransfer
    | CIFileStatus_rcvAborted
    | CIFileStatus_rcvComplete
    | CIFileStatus_rcvCancelled
    | CIFileStatus_rcvError
    | CIFileStatus_rcvWarning
    | CIFileStatus_invalid
)

CIFileStatus_Tag = Literal["sndStored", "sndTransfer", "sndCancelled", "sndComplete", "sndError", "sndWarning", "rcvInvitation", "rcvAccepted", "rcvTransfer", "rcvAborted", "rcvComplete", "rcvCancelled", "rcvError", "rcvWarning", "invalid"]

class CIForwardedFrom_unknown(TypedDict):
    type: Literal["unknown"]

class CIForwardedFrom_contact(TypedDict):
    type: Literal["contact"]
    chatName: str
    msgDir: "MsgDirection"
    contactId: NotRequired[int]  # int64
    chatItemId: NotRequired[int]  # int64

class CIForwardedFrom_group(TypedDict):
    type: Literal["group"]
    chatName: str
    msgDir: "MsgDirection"
    groupId: NotRequired[int]  # int64
    chatItemId: NotRequired[int]  # int64

CIForwardedFrom = CIForwardedFrom_unknown | CIForwardedFrom_contact | CIForwardedFrom_group

CIForwardedFrom_Tag = Literal["unknown", "contact", "group"]

class CIGroupInvitation(TypedDict):
    groupId: int  # int64
    groupMemberId: int  # int64
    localDisplayName: str
    groupProfile: "GroupProfile"
    status: "CIGroupInvitationStatus"

CIGroupInvitationStatus = Literal["pending", "accepted", "rejected", "expired"]

class CIMention(TypedDict):
    memberId: str
    memberRef: NotRequired["CIMentionMember"]

class CIMentionMember(TypedDict):
    groupMemberId: int  # int64
    displayName: str
    localAlias: NotRequired[str]
    memberRole: "GroupMemberRole"

class CIMeta(TypedDict):
    itemId: int  # int64
    itemTs: str  # ISO-8601 timestamp
    itemText: str
    itemStatus: "CIStatus"
    sentViaProxy: NotRequired[bool]
    itemSharedMsgId: NotRequired[str]
    itemForwarded: NotRequired["CIForwardedFrom"]
    itemDeleted: NotRequired["CIDeleted"]
    itemEdited: bool
    itemTimed: NotRequired["CITimed"]
    itemLive: NotRequired[bool]
    userMention: bool
    hasLink: bool
    deletable: bool
    editable: bool
    forwardedByMember: NotRequired[int]  # int64
    showGroupAsSender: bool
    msgVerified: NotRequired["MsgVerified"]
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp

class CIQuote(TypedDict):
    chatDir: NotRequired["CIDirection"]
    itemId: NotRequired[int]  # int64
    sharedMsgId: NotRequired[str]
    sentAt: str  # ISO-8601 timestamp
    content: "MsgContent"
    formattedText: NotRequired[list["FormattedText"]]

class CIReaction(TypedDict):
    chatDir: "CIDirection"
    chatItem: "ChatItem"
    sentAt: str  # ISO-8601 timestamp
    reaction: "MsgReaction"

class CIReactionCount(TypedDict):
    reaction: "MsgReaction"
    userReacted: bool
    totalReacted: int  # int

class CIStatus_sndNew(TypedDict):
    type: Literal["sndNew"]

class CIStatus_sndSent(TypedDict):
    type: Literal["sndSent"]
    sndProgress: "SndCIStatusProgress"

class CIStatus_sndRcvd(TypedDict):
    type: Literal["sndRcvd"]
    msgRcptStatus: "MsgReceiptStatus"
    sndProgress: "SndCIStatusProgress"

class CIStatus_sndErrorAuth(TypedDict):
    type: Literal["sndErrorAuth"]

class CIStatus_sndError(TypedDict):
    type: Literal["sndError"]
    agentError: "SndError"

class CIStatus_sndWarning(TypedDict):
    type: Literal["sndWarning"]
    agentError: "SndError"

class CIStatus_rcvNew(TypedDict):
    type: Literal["rcvNew"]

class CIStatus_rcvRead(TypedDict):
    type: Literal["rcvRead"]

class CIStatus_invalid(TypedDict):
    type: Literal["invalid"]
    text: str

CIStatus = (
    CIStatus_sndNew
    | CIStatus_sndSent
    | CIStatus_sndRcvd
    | CIStatus_sndErrorAuth
    | CIStatus_sndError
    | CIStatus_sndWarning
    | CIStatus_rcvNew
    | CIStatus_rcvRead
    | CIStatus_invalid
)

CIStatus_Tag = Literal["sndNew", "sndSent", "sndRcvd", "sndErrorAuth", "sndError", "sndWarning", "rcvNew", "rcvRead", "invalid"]

class CITimed(TypedDict):
    ttl: int  # int
    deleteAt: NotRequired[str]  # ISO-8601 timestamp

class ChatBotCommand_command(TypedDict):
    type: Literal["command"]
    keyword: str
    label: str
    params: NotRequired[str]

class ChatBotCommand_menu(TypedDict):
    type: Literal["menu"]
    label: str
    commands: list["ChatBotCommand"]

ChatBotCommand = ChatBotCommand_command | ChatBotCommand_menu

ChatBotCommand_Tag = Literal["command", "menu"]

class ChatDeleteMode_full(TypedDict):
    type: Literal["full"]
    notify: bool

class ChatDeleteMode_entity(TypedDict):
    type: Literal["entity"]
    notify: bool

class ChatDeleteMode_messages(TypedDict):
    type: Literal["messages"]

ChatDeleteMode = ChatDeleteMode_full | ChatDeleteMode_entity | ChatDeleteMode_messages

ChatDeleteMode_Tag = Literal["full", "entity", "messages"]


def ChatDeleteMode_cmd_string(self: ChatDeleteMode) -> str:
    return str(self['type']) + ('' if str(self['type']) == 'messages' else (' notify=off' if not self['notify'] else ''))  # type: ignore[typeddict-item]

class ChatError_error(TypedDict):
    type: Literal["error"]
    errorType: "ChatErrorType"

class ChatError_errorAgent(TypedDict):
    type: Literal["errorAgent"]
    agentError: "AgentErrorType"
    agentConnId: str
    connectionEntity_: NotRequired["ConnectionEntity"]

class ChatError_errorStore(TypedDict):
    type: Literal["errorStore"]
    storeError: "StoreError"

ChatError = ChatError_error | ChatError_errorAgent | ChatError_errorStore

ChatError_Tag = Literal["error", "errorAgent", "errorStore"]

class ChatErrorType_noActiveUser(TypedDict):
    type: Literal["noActiveUser"]

class ChatErrorType_noConnectionUser(TypedDict):
    type: Literal["noConnectionUser"]
    agentConnId: str

class ChatErrorType_noSndFileUser(TypedDict):
    type: Literal["noSndFileUser"]
    agentSndFileId: str

class ChatErrorType_noRcvFileUser(TypedDict):
    type: Literal["noRcvFileUser"]
    agentRcvFileId: str

class ChatErrorType_userUnknown(TypedDict):
    type: Literal["userUnknown"]

class ChatErrorType_userExists(TypedDict):
    type: Literal["userExists"]
    contactName: str

class ChatErrorType_chatRelayExists(TypedDict):
    type: Literal["chatRelayExists"]

class ChatErrorType_differentActiveUser(TypedDict):
    type: Literal["differentActiveUser"]
    commandUserId: int  # int64
    activeUserId: int  # int64

class ChatErrorType_cantDeleteActiveUser(TypedDict):
    type: Literal["cantDeleteActiveUser"]
    userId: int  # int64

class ChatErrorType_cantDeleteLastUser(TypedDict):
    type: Literal["cantDeleteLastUser"]
    userId: int  # int64

class ChatErrorType_cantHideLastUser(TypedDict):
    type: Literal["cantHideLastUser"]
    userId: int  # int64

class ChatErrorType_hiddenUserAlwaysMuted(TypedDict):
    type: Literal["hiddenUserAlwaysMuted"]
    userId: int  # int64

class ChatErrorType_emptyUserPassword(TypedDict):
    type: Literal["emptyUserPassword"]
    userId: int  # int64

class ChatErrorType_userAlreadyHidden(TypedDict):
    type: Literal["userAlreadyHidden"]
    userId: int  # int64

class ChatErrorType_userNotHidden(TypedDict):
    type: Literal["userNotHidden"]
    userId: int  # int64

class ChatErrorType_invalidDisplayName(TypedDict):
    type: Literal["invalidDisplayName"]
    displayName: str
    validName: str

class ChatErrorType_chatNotStarted(TypedDict):
    type: Literal["chatNotStarted"]

class ChatErrorType_chatNotStopped(TypedDict):
    type: Literal["chatNotStopped"]

class ChatErrorType_chatStoreChanged(TypedDict):
    type: Literal["chatStoreChanged"]

class ChatErrorType_invalidConnReq(TypedDict):
    type: Literal["invalidConnReq"]

class ChatErrorType_simplexDomainNotReady(TypedDict):
    type: Literal["simplexDomainNotReady"]
    simplexDomain: "SimplexDomain"
    simplexDomainError: "SimplexDomainError"

class ChatErrorType_notResolvedLocally(TypedDict):
    type: Literal["notResolvedLocally"]

class ChatErrorType_unsupportedConnReq(TypedDict):
    type: Literal["unsupportedConnReq"]

class ChatErrorType_connReqMessageProhibited(TypedDict):
    type: Literal["connReqMessageProhibited"]

class ChatErrorType_contactNotReady(TypedDict):
    type: Literal["contactNotReady"]
    contact: "Contact"

class ChatErrorType_contactNotActive(TypedDict):
    type: Literal["contactNotActive"]
    contact: "Contact"

class ChatErrorType_contactDisabled(TypedDict):
    type: Literal["contactDisabled"]
    contact: "Contact"

class ChatErrorType_connectionDisabled(TypedDict):
    type: Literal["connectionDisabled"]
    connection: "Connection"

class ChatErrorType_groupUserRole(TypedDict):
    type: Literal["groupUserRole"]
    groupInfo: "GroupInfo"
    requiredRole: "GroupMemberRole"

class ChatErrorType_groupMemberInitialRole(TypedDict):
    type: Literal["groupMemberInitialRole"]
    groupInfo: "GroupInfo"
    initialRole: "GroupMemberRole"

class ChatErrorType_contactIncognitoCantInvite(TypedDict):
    type: Literal["contactIncognitoCantInvite"]

class ChatErrorType_groupIncognitoCantInvite(TypedDict):
    type: Literal["groupIncognitoCantInvite"]

class ChatErrorType_groupContactRole(TypedDict):
    type: Literal["groupContactRole"]
    contactName: str

class ChatErrorType_groupDuplicateMember(TypedDict):
    type: Literal["groupDuplicateMember"]
    contactName: str

class ChatErrorType_groupDuplicateMemberId(TypedDict):
    type: Literal["groupDuplicateMemberId"]

class ChatErrorType_groupNotJoined(TypedDict):
    type: Literal["groupNotJoined"]
    groupInfo: "GroupInfo"

class ChatErrorType_groupMemberNotActive(TypedDict):
    type: Literal["groupMemberNotActive"]

class ChatErrorType_cantBlockMemberForSelf(TypedDict):
    type: Literal["cantBlockMemberForSelf"]
    groupInfo: "GroupInfo"
    member: "GroupMember"
    setShowMessages: bool

class ChatErrorType_groupMemberUserRemoved(TypedDict):
    type: Literal["groupMemberUserRemoved"]

class ChatErrorType_groupMemberNotFound(TypedDict):
    type: Literal["groupMemberNotFound"]

class ChatErrorType_groupCantResendInvitation(TypedDict):
    type: Literal["groupCantResendInvitation"]
    groupInfo: "GroupInfo"
    contactName: str

class ChatErrorType_groupInternal(TypedDict):
    type: Literal["groupInternal"]
    message: str

class ChatErrorType_fileNotFound(TypedDict):
    type: Literal["fileNotFound"]
    message: str

class ChatErrorType_fileSize(TypedDict):
    type: Literal["fileSize"]
    filePath: str

class ChatErrorType_fileAlreadyReceiving(TypedDict):
    type: Literal["fileAlreadyReceiving"]
    message: str

class ChatErrorType_fileCancelled(TypedDict):
    type: Literal["fileCancelled"]
    message: str

class ChatErrorType_fileCancel(TypedDict):
    type: Literal["fileCancel"]
    fileId: int  # int64
    message: str

class ChatErrorType_fileAlreadyExists(TypedDict):
    type: Literal["fileAlreadyExists"]
    filePath: str

class ChatErrorType_fileWrite(TypedDict):
    type: Literal["fileWrite"]
    filePath: str
    message: str

class ChatErrorType_fileSend(TypedDict):
    type: Literal["fileSend"]
    fileId: int  # int64
    agentError: "AgentErrorType"

class ChatErrorType_fileRcvChunk(TypedDict):
    type: Literal["fileRcvChunk"]
    message: str

class ChatErrorType_fileInternal(TypedDict):
    type: Literal["fileInternal"]
    message: str

class ChatErrorType_fileImageType(TypedDict):
    type: Literal["fileImageType"]
    filePath: str

class ChatErrorType_fileImageSize(TypedDict):
    type: Literal["fileImageSize"]
    filePath: str

class ChatErrorType_fileNotReceived(TypedDict):
    type: Literal["fileNotReceived"]
    fileId: int  # int64

class ChatErrorType_fileNotApproved(TypedDict):
    type: Literal["fileNotApproved"]
    fileId: int  # int64
    unknownServers: list[str]

class ChatErrorType_fallbackToSMPProhibited(TypedDict):
    type: Literal["fallbackToSMPProhibited"]
    fileId: int  # int64

class ChatErrorType_inlineFileProhibited(TypedDict):
    type: Literal["inlineFileProhibited"]
    fileId: int  # int64

class ChatErrorType_invalidForward(TypedDict):
    type: Literal["invalidForward"]

class ChatErrorType_invalidChatItemUpdate(TypedDict):
    type: Literal["invalidChatItemUpdate"]

class ChatErrorType_invalidChatItemDelete(TypedDict):
    type: Literal["invalidChatItemDelete"]

class ChatErrorType_hasCurrentCall(TypedDict):
    type: Literal["hasCurrentCall"]

class ChatErrorType_noCurrentCall(TypedDict):
    type: Literal["noCurrentCall"]

class ChatErrorType_callContact(TypedDict):
    type: Literal["callContact"]
    contactId: int  # int64

class ChatErrorType_directMessagesProhibited(TypedDict):
    type: Literal["directMessagesProhibited"]
    direction: "MsgDirection"
    contact: "Contact"

class ChatErrorType_agentVersion(TypedDict):
    type: Literal["agentVersion"]

class ChatErrorType_agentNoSubResult(TypedDict):
    type: Literal["agentNoSubResult"]
    agentConnId: str

class ChatErrorType_commandError(TypedDict):
    type: Literal["commandError"]
    message: str

class ChatErrorType_agentCommandError(TypedDict):
    type: Literal["agentCommandError"]
    message: str

class ChatErrorType_invalidFileDescription(TypedDict):
    type: Literal["invalidFileDescription"]
    message: str

class ChatErrorType_connectionIncognitoChangeProhibited(TypedDict):
    type: Literal["connectionIncognitoChangeProhibited"]

class ChatErrorType_connectionUserChangeProhibited(TypedDict):
    type: Literal["connectionUserChangeProhibited"]

class ChatErrorType_peerChatVRangeIncompatible(TypedDict):
    type: Literal["peerChatVRangeIncompatible"]

class ChatErrorType_relayTestError(TypedDict):
    type: Literal["relayTestError"]
    message: str

class ChatErrorType_internalError(TypedDict):
    type: Literal["internalError"]
    message: str

class ChatErrorType_exception(TypedDict):
    type: Literal["exception"]
    message: str

ChatErrorType = (
    ChatErrorType_noActiveUser
    | ChatErrorType_noConnectionUser
    | ChatErrorType_noSndFileUser
    | ChatErrorType_noRcvFileUser
    | ChatErrorType_userUnknown
    | ChatErrorType_userExists
    | ChatErrorType_chatRelayExists
    | ChatErrorType_differentActiveUser
    | ChatErrorType_cantDeleteActiveUser
    | ChatErrorType_cantDeleteLastUser
    | ChatErrorType_cantHideLastUser
    | ChatErrorType_hiddenUserAlwaysMuted
    | ChatErrorType_emptyUserPassword
    | ChatErrorType_userAlreadyHidden
    | ChatErrorType_userNotHidden
    | ChatErrorType_invalidDisplayName
    | ChatErrorType_chatNotStarted
    | ChatErrorType_chatNotStopped
    | ChatErrorType_chatStoreChanged
    | ChatErrorType_invalidConnReq
    | ChatErrorType_simplexDomainNotReady
    | ChatErrorType_notResolvedLocally
    | ChatErrorType_unsupportedConnReq
    | ChatErrorType_connReqMessageProhibited
    | ChatErrorType_contactNotReady
    | ChatErrorType_contactNotActive
    | ChatErrorType_contactDisabled
    | ChatErrorType_connectionDisabled
    | ChatErrorType_groupUserRole
    | ChatErrorType_groupMemberInitialRole
    | ChatErrorType_contactIncognitoCantInvite
    | ChatErrorType_groupIncognitoCantInvite
    | ChatErrorType_groupContactRole
    | ChatErrorType_groupDuplicateMember
    | ChatErrorType_groupDuplicateMemberId
    | ChatErrorType_groupNotJoined
    | ChatErrorType_groupMemberNotActive
    | ChatErrorType_cantBlockMemberForSelf
    | ChatErrorType_groupMemberUserRemoved
    | ChatErrorType_groupMemberNotFound
    | ChatErrorType_groupCantResendInvitation
    | ChatErrorType_groupInternal
    | ChatErrorType_fileNotFound
    | ChatErrorType_fileSize
    | ChatErrorType_fileAlreadyReceiving
    | ChatErrorType_fileCancelled
    | ChatErrorType_fileCancel
    | ChatErrorType_fileAlreadyExists
    | ChatErrorType_fileWrite
    | ChatErrorType_fileSend
    | ChatErrorType_fileRcvChunk
    | ChatErrorType_fileInternal
    | ChatErrorType_fileImageType
    | ChatErrorType_fileImageSize
    | ChatErrorType_fileNotReceived
    | ChatErrorType_fileNotApproved
    | ChatErrorType_fallbackToSMPProhibited
    | ChatErrorType_inlineFileProhibited
    | ChatErrorType_invalidForward
    | ChatErrorType_invalidChatItemUpdate
    | ChatErrorType_invalidChatItemDelete
    | ChatErrorType_hasCurrentCall
    | ChatErrorType_noCurrentCall
    | ChatErrorType_callContact
    | ChatErrorType_directMessagesProhibited
    | ChatErrorType_agentVersion
    | ChatErrorType_agentNoSubResult
    | ChatErrorType_commandError
    | ChatErrorType_agentCommandError
    | ChatErrorType_invalidFileDescription
    | ChatErrorType_connectionIncognitoChangeProhibited
    | ChatErrorType_connectionUserChangeProhibited
    | ChatErrorType_peerChatVRangeIncompatible
    | ChatErrorType_relayTestError
    | ChatErrorType_internalError
    | ChatErrorType_exception
)

ChatErrorType_Tag = Literal["noActiveUser", "noConnectionUser", "noSndFileUser", "noRcvFileUser", "userUnknown", "userExists", "chatRelayExists", "differentActiveUser", "cantDeleteActiveUser", "cantDeleteLastUser", "cantHideLastUser", "hiddenUserAlwaysMuted", "emptyUserPassword", "userAlreadyHidden", "userNotHidden", "invalidDisplayName", "chatNotStarted", "chatNotStopped", "chatStoreChanged", "invalidConnReq", "simplexDomainNotReady", "notResolvedLocally", "unsupportedConnReq", "connReqMessageProhibited", "contactNotReady", "contactNotActive", "contactDisabled", "connectionDisabled", "groupUserRole", "groupMemberInitialRole", "contactIncognitoCantInvite", "groupIncognitoCantInvite", "groupContactRole", "groupDuplicateMember", "groupDuplicateMemberId", "groupNotJoined", "groupMemberNotActive", "cantBlockMemberForSelf", "groupMemberUserRemoved", "groupMemberNotFound", "groupCantResendInvitation", "groupInternal", "fileNotFound", "fileSize", "fileAlreadyReceiving", "fileCancelled", "fileCancel", "fileAlreadyExists", "fileWrite", "fileSend", "fileRcvChunk", "fileInternal", "fileImageType", "fileImageSize", "fileNotReceived", "fileNotApproved", "fallbackToSMPProhibited", "inlineFileProhibited", "invalidForward", "invalidChatItemUpdate", "invalidChatItemDelete", "hasCurrentCall", "noCurrentCall", "callContact", "directMessagesProhibited", "agentVersion", "agentNoSubResult", "commandError", "agentCommandError", "invalidFileDescription", "connectionIncognitoChangeProhibited", "connectionUserChangeProhibited", "peerChatVRangeIncompatible", "relayTestError", "internalError", "exception"]

ChatFeature = Literal["timedMessages", "fullDelete", "reactions", "voice", "files", "calls", "sessions"]

class ChatInfo_direct(TypedDict):
    type: Literal["direct"]
    contact: "Contact"

class ChatInfo_group(TypedDict):
    type: Literal["group"]
    groupInfo: "GroupInfo"
    groupChatScope: NotRequired["GroupChatScopeInfo"]

class ChatInfo_local(TypedDict):
    type: Literal["local"]
    noteFolder: "NoteFolder"

class ChatInfo_contactRequest(TypedDict):
    type: Literal["contactRequest"]
    contactRequest: "UserContactRequest"

class ChatInfo_contactConnection(TypedDict):
    type: Literal["contactConnection"]
    contactConnection: "PendingContactConnection"

ChatInfo = (
    ChatInfo_direct
    | ChatInfo_group
    | ChatInfo_local
    | ChatInfo_contactRequest
    | ChatInfo_contactConnection
)

ChatInfo_Tag = Literal["direct", "group", "local", "contactRequest", "contactConnection"]

class ChatItem(TypedDict):
    chatDir: "CIDirection"
    meta: "CIMeta"
    content: "CIContent"
    mentions: dict[str, "CIMention"]
    formattedText: NotRequired[list["FormattedText"]]
    quotedItem: NotRequired["CIQuote"]
    reactions: list["CIReactionCount"]
    file: NotRequired["CIFile"]

# Message deletion result.

class ChatItemDeletion(TypedDict):
    deletedChatItem: "AChatItem"
    toChatItem: NotRequired["AChatItem"]

class ChatListQuery_filters(TypedDict):
    type: Literal["filters"]
    favorite: bool
    unread: bool

class ChatListQuery_search(TypedDict):
    type: Literal["search"]
    search: str

ChatListQuery = ChatListQuery_filters | ChatListQuery_search

ChatListQuery_Tag = Literal["filters", "search"]

ChatPeerType = Literal["human", "bot"]

# Used in API commands. Chat scope can only be passed with groups.

class ChatRef(TypedDict):
    chatType: "ChatType"
    chatId: int  # int64
    chatScope: NotRequired["GroupChatScope"]


def ChatRef_cmd_string(self: ChatRef) -> str:
    return ChatType_cmd_string(self['chatType']) + str(self['chatId']) + ((GroupChatScope_cmd_string(self.get('chatScope'))) if self.get('chatScope') is not None else '')

class ChatSettings(TypedDict):
    enableNtfs: "MsgFilter"
    sendRcpts: NotRequired[bool]
    favorite: bool

class ChatStats(TypedDict):
    unreadCount: int  # int
    unreadMentions: int  # int
    reportsCount: int  # int
    minUnreadItemId: int  # int64
    unreadChat: bool

ChatType = Literal["direct", "group", "local"]


def ChatType_cmd_string(self: ChatType) -> str:
    return '@' if str(self) == 'direct' else '#' if str(self) == 'group' else '*' if str(self) == 'local' else ''

class ChatWallpaper(TypedDict):
    preset: NotRequired[str]
    imageFile: NotRequired[str]
    background: NotRequired[str]
    tint: NotRequired[str]
    scaleType: NotRequired["ChatWallpaperScale"]
    scale: NotRequired[float]  # double

ChatWallpaperScale = Literal["fill", "fit", "repeat"]

class ClientNotice(TypedDict):
    ttl: NotRequired[int]  # int64

Color = Literal["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]

class CommandError_UNKNOWN(TypedDict):
    type: Literal["UNKNOWN"]

class CommandError_SYNTAX(TypedDict):
    type: Literal["SYNTAX"]

class CommandError_PROHIBITED(TypedDict):
    type: Literal["PROHIBITED"]

class CommandError_NO_AUTH(TypedDict):
    type: Literal["NO_AUTH"]

class CommandError_HAS_AUTH(TypedDict):
    type: Literal["HAS_AUTH"]

class CommandError_NO_ENTITY(TypedDict):
    type: Literal["NO_ENTITY"]

CommandError = (
    CommandError_UNKNOWN
    | CommandError_SYNTAX
    | CommandError_PROHIBITED
    | CommandError_NO_AUTH
    | CommandError_HAS_AUTH
    | CommandError_NO_ENTITY
)

CommandError_Tag = Literal["UNKNOWN", "SYNTAX", "PROHIBITED", "NO_AUTH", "HAS_AUTH", "NO_ENTITY"]

class CommandErrorType_PROHIBITED(TypedDict):
    type: Literal["PROHIBITED"]

class CommandErrorType_SYNTAX(TypedDict):
    type: Literal["SYNTAX"]

class CommandErrorType_NO_CONN(TypedDict):
    type: Literal["NO_CONN"]

class CommandErrorType_SIZE(TypedDict):
    type: Literal["SIZE"]

class CommandErrorType_LARGE(TypedDict):
    type: Literal["LARGE"]

CommandErrorType = (
    CommandErrorType_PROHIBITED
    | CommandErrorType_SYNTAX
    | CommandErrorType_NO_CONN
    | CommandErrorType_SIZE
    | CommandErrorType_LARGE
)

CommandErrorType_Tag = Literal["PROHIBITED", "SYNTAX", "NO_CONN", "SIZE", "LARGE"]

class CommentsGroupPreference(TypedDict):
    enable: "GroupFeatureEnabled"
    duration: NotRequired[int]  # int

class ComposedMessage(TypedDict):
    fileSource: NotRequired["CryptoFile"]
    quotedItemId: NotRequired[int]  # int64
    msgContent: "MsgContent"
    mentions: dict[str, int]  # str : int64

class ConnStatus_new(TypedDict):
    type: Literal["new"]

class ConnStatus_prepared(TypedDict):
    type: Literal["prepared"]

class ConnStatus_joined(TypedDict):
    type: Literal["joined"]

class ConnStatus_requested(TypedDict):
    type: Literal["requested"]

class ConnStatus_accepted(TypedDict):
    type: Literal["accepted"]

class ConnStatus_sndReady(TypedDict):
    type: Literal["sndReady"]

class ConnStatus_ready(TypedDict):
    type: Literal["ready"]

class ConnStatus_deleted(TypedDict):
    type: Literal["deleted"]

class ConnStatus_failed(TypedDict):
    type: Literal["failed"]
    connError: str

ConnStatus = (
    ConnStatus_new
    | ConnStatus_prepared
    | ConnStatus_joined
    | ConnStatus_requested
    | ConnStatus_accepted
    | ConnStatus_sndReady
    | ConnStatus_ready
    | ConnStatus_deleted
    | ConnStatus_failed
)

ConnStatus_Tag = Literal["new", "prepared", "joined", "requested", "accepted", "sndReady", "ready", "deleted", "failed"]

ConnType = Literal["contact", "member", "user_contact"]

class Connection(TypedDict):
    connId: int  # int64
    agentConnId: str
    connChatVersion: int  # int
    peerChatVRange: "VersionRange"
    connLevel: int  # int
    viaContact: NotRequired[int]  # int64
    viaUserContactLink: NotRequired[int]  # int64
    viaGroupLink: bool
    groupLinkId: NotRequired[str]
    xContactId: NotRequired[str]
    customUserProfileId: NotRequired[int]  # int64
    connType: "ConnType"
    connStatus: "ConnStatus"
    contactConnInitiated: bool
    localAlias: str
    entityId: NotRequired[int]  # int64
    connectionCode: NotRequired["SecurityCode"]
    pqSupport: bool
    pqEncryption: bool
    pqSndEnabled: NotRequired[bool]
    pqRcvEnabled: NotRequired[bool]
    authErrCounter: int  # int
    quotaErrCounter: int  # int
    createdAt: str  # ISO-8601 timestamp

class ConnectionEntity_rcvDirectMsgConnection(TypedDict):
    type: Literal["rcvDirectMsgConnection"]
    entityConnection: "Connection"
    contact: NotRequired["Contact"]

class ConnectionEntity_rcvGroupMsgConnection(TypedDict):
    type: Literal["rcvGroupMsgConnection"]
    entityConnection: "Connection"
    groupInfo: "GroupInfo"
    groupMember: "GroupMember"

class ConnectionEntity_userContactConnection(TypedDict):
    type: Literal["userContactConnection"]
    entityConnection: "Connection"
    userContact: "UserContact"

ConnectionEntity = (
    ConnectionEntity_rcvDirectMsgConnection
    | ConnectionEntity_rcvGroupMsgConnection
    | ConnectionEntity_userContactConnection
)

ConnectionEntity_Tag = Literal["rcvDirectMsgConnection", "rcvGroupMsgConnection", "userContactConnection"]

class ConnectionErrorType_NOT_FOUND(TypedDict):
    type: Literal["NOT_FOUND"]

class ConnectionErrorType_DUPLICATE(TypedDict):
    type: Literal["DUPLICATE"]

class ConnectionErrorType_SIMPLEX(TypedDict):
    type: Literal["SIMPLEX"]

class ConnectionErrorType_NOT_ACCEPTED(TypedDict):
    type: Literal["NOT_ACCEPTED"]

class ConnectionErrorType_NOT_AVAILABLE(TypedDict):
    type: Literal["NOT_AVAILABLE"]

ConnectionErrorType = (
    ConnectionErrorType_NOT_FOUND
    | ConnectionErrorType_DUPLICATE
    | ConnectionErrorType_SIMPLEX
    | ConnectionErrorType_NOT_ACCEPTED
    | ConnectionErrorType_NOT_AVAILABLE
)

ConnectionErrorType_Tag = Literal["NOT_FOUND", "DUPLICATE", "SIMPLEX", "NOT_ACCEPTED", "NOT_AVAILABLE"]

ConnectionMode = Literal["INV", "CON"]

class ConnectionPlan_invitationLink(TypedDict):
    type: Literal["invitationLink"]
    invitationLinkPlan: "InvitationLinkPlan"

class ConnectionPlan_contactAddress(TypedDict):
    type: Literal["contactAddress"]
    contactAddressPlan: "ContactAddressPlan"

class ConnectionPlan_groupLink(TypedDict):
    type: Literal["groupLink"]
    groupLinkPlan: "GroupLinkPlan"

class ConnectionPlan_error(TypedDict):
    type: Literal["error"]
    chatError: "ChatError"

ConnectionPlan = (
    ConnectionPlan_invitationLink
    | ConnectionPlan_contactAddress
    | ConnectionPlan_groupLink
    | ConnectionPlan_error
)

ConnectionPlan_Tag = Literal["invitationLink", "contactAddress", "groupLink", "error"]

class Contact(TypedDict):
    contactId: int  # int64
    localDisplayName: str
    profile: "LocalProfile"
    activeConn: NotRequired["Connection"]
    contactUsed: bool
    contactStatus: "ContactStatus"
    chatSettings: "ChatSettings"
    userPreferences: "Preferences"
    mergedPreferences: "ContactUserPreferences"
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp
    chatTs: NotRequired[str]  # ISO-8601 timestamp
    preparedContact: NotRequired["PreparedContact"]
    contactRequestId: NotRequired[int]  # int64
    contactRequest: NotRequired["UserContactRequestRef"]
    contactGroupMemberId: NotRequired[int]  # int64
    contactGrpInvSent: bool
    groupDirectInv: NotRequired["GroupDirectInvitation"]
    chatTags: list[int]  # int64
    chatItemTTL: NotRequired[int]  # int64
    uiThemes: NotRequired["UIThemeEntityOverrides"]
    chatDeleted: bool
    customData: NotRequired[dict[str, object]]

class ContactAddressPlan_ok(TypedDict):
    type: Literal["ok"]
    contactSLinkData_: NotRequired["ContactShortLinkData"]
    ownerVerification: NotRequired["OwnerVerification"]

class ContactAddressPlan_ownLink(TypedDict):
    type: Literal["ownLink"]

class ContactAddressPlan_connectingConfirmReconnect(TypedDict):
    type: Literal["connectingConfirmReconnect"]

class ContactAddressPlan_connectingProhibit(TypedDict):
    type: Literal["connectingProhibit"]
    contact: "Contact"

class ContactAddressPlan_known(TypedDict):
    type: Literal["known"]
    contact: "Contact"

class ContactAddressPlan_contactViaAddress(TypedDict):
    type: Literal["contactViaAddress"]
    contact: "Contact"

ContactAddressPlan = (
    ContactAddressPlan_ok
    | ContactAddressPlan_ownLink
    | ContactAddressPlan_connectingConfirmReconnect
    | ContactAddressPlan_connectingProhibit
    | ContactAddressPlan_known
    | ContactAddressPlan_contactViaAddress
)

ContactAddressPlan_Tag = Literal["ok", "ownLink", "connectingConfirmReconnect", "connectingProhibit", "known", "contactViaAddress"]

class ContactShortLinkData(TypedDict):
    profile: "Profile"
    message: NotRequired["MsgContent"]
    business: bool
    localBadge: NotRequired["LocalBadge"]

ContactStatus = Literal["active", "deleted", "deletedByUser", "rejected"]

class ContactUserPref_contact(TypedDict):
    type: Literal["contact"]
    preference: "SimplePreference"

class ContactUserPref_user(TypedDict):
    type: Literal["user"]
    preference: "SimplePreference"

ContactUserPref = ContactUserPref_contact | ContactUserPref_user

ContactUserPref_Tag = Literal["contact", "user"]

class ContactUserPreference(TypedDict):
    enabled: "PrefEnabled"
    userPreference: "ContactUserPref"
    contactPreference: "SimplePreference"

class ContactUserPreferences(TypedDict):
    timedMessages: "ContactUserPreference"
    fullDelete: "ContactUserPreference"
    reactions: "ContactUserPreference"
    voice: "ContactUserPreference"
    files: "ContactUserPreference"
    calls: "ContactUserPreference"
    sessions: "ContactUserPreference"
    commands: NotRequired[list["ChatBotCommand"]]

class CreatedConnLink(TypedDict):
    connFullLink: str
    connShortLink: NotRequired[str]


def CreatedConnLink_cmd_string(self: CreatedConnLink) -> str:
    return self['connFullLink'] + ((' ' + self.get('connShortLink')) if self.get('connShortLink') is not None else '')

class CryptoFile(TypedDict):
    filePath: str
    cryptoArgs: NotRequired["CryptoFileArgs"]

class CryptoFileArgs(TypedDict):
    fileKey: str
    fileNonce: str

class DroppedMsg(TypedDict):
    brokerTs: str  # ISO-8601 timestamp
    attempts: int  # int

class E2EInfo(TypedDict):
    public: NotRequired[bool]
    pqEnabled: NotRequired[bool]

class ErrorType_BLOCK(TypedDict):
    type: Literal["BLOCK"]

class ErrorType_SESSION(TypedDict):
    type: Literal["SESSION"]

class ErrorType_CMD(TypedDict):
    type: Literal["CMD"]
    cmdErr: "CommandError"

class ErrorType_PROXY(TypedDict):
    type: Literal["PROXY"]
    proxyErr: "ProxyError"

class ErrorType_AUTH(TypedDict):
    type: Literal["AUTH"]

class ErrorType_BLOCKED(TypedDict):
    type: Literal["BLOCKED"]
    blockInfo: "BlockingInfo"

class ErrorType_SERVICE(TypedDict):
    type: Literal["SERVICE"]

class ErrorType_CRYPTO(TypedDict):
    type: Literal["CRYPTO"]

class ErrorType_QUOTA(TypedDict):
    type: Literal["QUOTA"]

class ErrorType_STORE(TypedDict):
    type: Literal["STORE"]
    storeErr: str

class ErrorType_NO_MSG(TypedDict):
    type: Literal["NO_MSG"]

class ErrorType_LARGE_MSG(TypedDict):
    type: Literal["LARGE_MSG"]

class ErrorType_EXPIRED(TypedDict):
    type: Literal["EXPIRED"]

class ErrorType_INTERNAL(TypedDict):
    type: Literal["INTERNAL"]

class ErrorType_NAME(TypedDict):
    type: Literal["NAME"]
    nameErr: "NameErrorType"

class ErrorType_DUPLICATE_(TypedDict):
    type: Literal["DUPLICATE_"]

ErrorType = (
    ErrorType_BLOCK
    | ErrorType_SESSION
    | ErrorType_CMD
    | ErrorType_PROXY
    | ErrorType_AUTH
    | ErrorType_BLOCKED
    | ErrorType_SERVICE
    | ErrorType_CRYPTO
    | ErrorType_QUOTA
    | ErrorType_STORE
    | ErrorType_NO_MSG
    | ErrorType_LARGE_MSG
    | ErrorType_EXPIRED
    | ErrorType_INTERNAL
    | ErrorType_NAME
    | ErrorType_DUPLICATE_
)

ErrorType_Tag = Literal["BLOCK", "SESSION", "CMD", "PROXY", "AUTH", "BLOCKED", "SERVICE", "CRYPTO", "QUOTA", "STORE", "NO_MSG", "LARGE_MSG", "EXPIRED", "INTERNAL", "NAME", "DUPLICATE_"]

FeatureAllowed = Literal["always", "yes", "no"]

class FileDescr(TypedDict):
    fileDescrText: str
    fileDescrPartNo: int  # int
    fileDescrComplete: bool

class FileError_auth(TypedDict):
    type: Literal["auth"]

class FileError_blocked(TypedDict):
    type: Literal["blocked"]
    server: str
    blockInfo: "BlockingInfo"

class FileError_noFile(TypedDict):
    type: Literal["noFile"]

class FileError_relay(TypedDict):
    type: Literal["relay"]
    srvError: "SrvError"

class FileError_other(TypedDict):
    type: Literal["other"]
    fileError: str

FileError = (
    FileError_auth
    | FileError_blocked
    | FileError_noFile
    | FileError_relay
    | FileError_other
)

FileError_Tag = Literal["auth", "blocked", "noFile", "relay", "other"]

class FileErrorType_NOT_APPROVED(TypedDict):
    type: Literal["NOT_APPROVED"]

class FileErrorType_SIZE(TypedDict):
    type: Literal["SIZE"]

class FileErrorType_REDIRECT(TypedDict):
    type: Literal["REDIRECT"]
    redirectError: str

class FileErrorType_FILE_IO(TypedDict):
    type: Literal["FILE_IO"]
    fileIOError: str

class FileErrorType_NO_FILE(TypedDict):
    type: Literal["NO_FILE"]

FileErrorType = (
    FileErrorType_NOT_APPROVED
    | FileErrorType_SIZE
    | FileErrorType_REDIRECT
    | FileErrorType_FILE_IO
    | FileErrorType_NO_FILE
)

FileErrorType_Tag = Literal["NOT_APPROVED", "SIZE", "REDIRECT", "FILE_IO", "NO_FILE"]

class FileInvitation(TypedDict):
    fileName: str
    fileSize: int  # int64
    fileDigest: NotRequired[str]
    fileConnReq: NotRequired[str]
    fileInline: NotRequired["InlineFileMode"]
    fileDescr: NotRequired["FileDescr"]

FileProtocol = Literal["SMP", "XFTP", "LOCAL"]

FileStatus = Literal["new", "accepted", "connected", "complete", "cancelled"]

class FileTransferMeta(TypedDict):
    fileId: int  # int64
    xftpSndFile: NotRequired["XFTPSndFile"]
    xftpRedirectFor: NotRequired[int]  # int64
    fileName: str
    filePath: str
    fileSize: int  # int64
    fileInline: NotRequired["InlineFileMode"]
    chunkSize: int  # int64
    cancelled: bool

FileType = Literal["normal", "roster"]

class Format_bold(TypedDict):
    type: Literal["bold"]

class Format_italic(TypedDict):
    type: Literal["italic"]

class Format_strikeThrough(TypedDict):
    type: Literal["strikeThrough"]

class Format_snippet(TypedDict):
    type: Literal["snippet"]

class Format_secret(TypedDict):
    type: Literal["secret"]

class Format_small(TypedDict):
    type: Literal["small"]

class Format_colored(TypedDict):
    type: Literal["colored"]
    color: "Color"

class Format_uri(TypedDict):
    type: Literal["uri"]

class Format_hyperLink(TypedDict):
    type: Literal["hyperLink"]
    showText: NotRequired[str]
    linkUri: str

class Format_simplexLink(TypedDict):
    type: Literal["simplexLink"]
    showText: NotRequired[str]
    linkType: "SimplexLinkType"
    simplexUri: str
    smpHosts: list[str]  # non-empty

class Format_simplexName(TypedDict):
    type: Literal["simplexName"]
    nameInfo: "SimplexNameInfo"

class Format_command(TypedDict):
    type: Literal["command"]
    commandStr: str

class Format_mention(TypedDict):
    type: Literal["mention"]
    memberName: str

class Format_email(TypedDict):
    type: Literal["email"]

class Format_phone(TypedDict):
    type: Literal["phone"]

Format = (
    Format_bold
    | Format_italic
    | Format_strikeThrough
    | Format_snippet
    | Format_secret
    | Format_small
    | Format_colored
    | Format_uri
    | Format_hyperLink
    | Format_simplexLink
    | Format_simplexName
    | Format_command
    | Format_mention
    | Format_email
    | Format_phone
)

Format_Tag = Literal["bold", "italic", "strikeThrough", "snippet", "secret", "small", "colored", "uri", "hyperLink", "simplexLink", "simplexName", "command", "mention", "email", "phone"]

class FormattedText(TypedDict):
    format: NotRequired["Format"]
    text: str

class FullGroupPreferences(TypedDict):
    timedMessages: "TimedMessagesGroupPreference"
    directMessages: "RoleGroupPreference"
    fullDelete: "GroupPreference"
    reactions: "GroupPreference"
    voice: "RoleGroupPreference"
    files: "RoleGroupPreference"
    simplexLinks: "RoleGroupPreference"
    reports: "GroupPreference"
    history: "GroupPreference"
    support: "SupportGroupPreference"
    sessions: "RoleGroupPreference"
    comments: "CommentsGroupPreference"
    signMessages: "GroupPreference"
    commands: list["ChatBotCommand"]

class FullPreferences(TypedDict):
    timedMessages: "TimedMessagesPreference"
    fullDelete: "SimplePreference"
    reactions: "SimplePreference"
    voice: "SimplePreference"
    files: "SimplePreference"
    calls: "SimplePreference"
    sessions: "SimplePreference"
    commands: list["ChatBotCommand"]

class Group(TypedDict):
    groupInfo: "GroupInfo"
    members: list["GroupMember"]

class GroupChatScope_memberSupport(TypedDict):
    type: Literal["memberSupport"]
    groupMemberId_: NotRequired[int]  # int64

GroupChatScope = GroupChatScope_memberSupport

GroupChatScope_Tag = Literal["memberSupport"]


def GroupChatScope_cmd_string(self: GroupChatScope) -> str:
    return '(_support' + ((':' + str(self.get('groupMemberId_'))) if self.get('groupMemberId_') is not None else '') + ')'  # type: ignore[typeddict-item]

class GroupChatScopeInfo_memberSupport(TypedDict):
    type: Literal["memberSupport"]
    groupMember_: NotRequired["GroupMember"]

GroupChatScopeInfo = GroupChatScopeInfo_memberSupport

GroupChatScopeInfo_Tag = Literal["memberSupport"]

class GroupDirectInvitation(TypedDict):
    groupDirectInvLink: str
    fromGroupId_: NotRequired[int]  # int64
    fromGroupMemberId_: NotRequired[int]  # int64
    fromGroupMemberConnId_: NotRequired[int]  # int64
    groupDirectInvStartedConnection: bool

GroupFeature = Literal["timedMessages", "directMessages", "fullDelete", "reactions", "voice", "files", "simplexLinks", "reports", "history", "support", "sessions", "comments", "signMessages"]

GroupFeatureEnabled = Literal["on", "off"]

class GroupInfo(TypedDict):
    groupId: int  # int64
    useRelays: bool
    relayOwnStatus: NotRequired["RelayStatus"]
    localDisplayName: str
    groupProfile: "GroupProfile"
    localAlias: str
    businessChat: NotRequired["BusinessChatInfo"]
    fullGroupPreferences: "FullGroupPreferences"
    membership: "GroupMember"
    chatSettings: "ChatSettings"
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp
    chatTs: NotRequired[str]  # ISO-8601 timestamp
    userMemberProfileSentAt: NotRequired[str]  # ISO-8601 timestamp
    preparedGroup: NotRequired["PreparedGroup"]
    chatTags: list[int]  # int64
    chatItemTTL: NotRequired[int]  # int64
    uiThemes: NotRequired["UIThemeEntityOverrides"]
    customData: NotRequired[dict[str, object]]
    groupSummary: "GroupSummary"
    rosterVersion: NotRequired[int]  # int64
    membersRequireAttention: int  # int
    viaGroupLinkUri: NotRequired[str]
    groupKeys: NotRequired["GroupKeys"]
    groupDomainVerified: NotRequired[bool]

class GroupKeys(TypedDict):
    publicGroupId: str
    groupRootKey: "GroupRootKey"
    memberPrivKey: str

class GroupLink(TypedDict):
    userContactLinkId: int  # int64
    connLinkContact: "CreatedConnLink"
    shortLinkDataSet: bool
    shortLinkLargeDataSet: bool
    groupLinkId: str
    acceptMemberRole: "GroupMemberRole"

class GroupLinkOwner(TypedDict):
    memberId: str
    memberKey: str

class GroupLinkPlan_ok(TypedDict):
    type: Literal["ok"]
    groupSLinkInfo_: NotRequired["GroupShortLinkInfo"]
    groupSLinkData_: NotRequired["GroupShortLinkData"]
    ownerVerification: NotRequired["OwnerVerification"]

class GroupLinkPlan_ownLink(TypedDict):
    type: Literal["ownLink"]
    groupInfo: "GroupInfo"

class GroupLinkPlan_connectingConfirmReconnect(TypedDict):
    type: Literal["connectingConfirmReconnect"]

class GroupLinkPlan_connectingProhibit(TypedDict):
    type: Literal["connectingProhibit"]
    groupInfo_: NotRequired["GroupInfo"]

class GroupLinkPlan_known(TypedDict):
    type: Literal["known"]
    groupInfo: "GroupInfo"
    groupUpdated: bool
    ownerVerification: NotRequired["OwnerVerification"]
    linkOwners: list["GroupLinkOwner"]

class GroupLinkPlan_noRelays(TypedDict):
    type: Literal["noRelays"]
    groupSLinkData_: NotRequired["GroupShortLinkData"]

class GroupLinkPlan_updateRequired(TypedDict):
    type: Literal["updateRequired"]
    groupSLinkData_: NotRequired["GroupShortLinkData"]

GroupLinkPlan = (
    GroupLinkPlan_ok
    | GroupLinkPlan_ownLink
    | GroupLinkPlan_connectingConfirmReconnect
    | GroupLinkPlan_connectingProhibit
    | GroupLinkPlan_known
    | GroupLinkPlan_noRelays
    | GroupLinkPlan_updateRequired
)

GroupLinkPlan_Tag = Literal["ok", "ownLink", "connectingConfirmReconnect", "connectingProhibit", "known", "noRelays", "updateRequired"]

class GroupMember(TypedDict):
    groupMemberId: int  # int64
    groupId: int  # int64
    indexInGroup: int  # int64
    memberId: str
    memberRole: "GroupMemberRole"
    memberCategory: "GroupMemberCategory"
    memberStatus: "GroupMemberStatus"
    memberSettings: "GroupMemberSettings"
    blockedByAdmin: bool
    invitedBy: "InvitedBy"
    invitedByGroupMemberId: NotRequired[int]  # int64
    localDisplayName: str
    memberProfile: "LocalProfile"
    memberContactId: NotRequired[int]  # int64
    memberContactProfileId: int  # int64
    activeConn: NotRequired["Connection"]
    memberChatVRange: "VersionRange"
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp
    supportChat: NotRequired["GroupSupportChat"]
    memberPubKey: NotRequired[str]
    relayLink: NotRequired[str]
    memberVerifiedCode: NotRequired["SecurityCode"]

class GroupMemberAdmission(TypedDict):
    review: NotRequired["MemberCriteria"]

GroupMemberCategory = Literal["user", "invitee", "host", "pre", "post"]

class GroupMemberRef(TypedDict):
    groupMemberId: int  # int64
    profile: "Profile"

GroupMemberRole = Literal["relay", "observer", "author", "member", "moderator", "admin", "owner"]

class GroupMemberSettings(TypedDict):
    showMessages: bool

GroupMemberStatus = Literal["rejected", "removed", "left", "deleted", "unknown", "invited", "pending_approval", "pending_review", "introduced", "intro-inv", "accepted", "announced", "connected", "complete", "creator"]

class GroupPreference(TypedDict):
    enable: "GroupFeatureEnabled"

class GroupPreferences(TypedDict):
    timedMessages: NotRequired["TimedMessagesGroupPreference"]
    directMessages: NotRequired["RoleGroupPreference"]
    fullDelete: NotRequired["GroupPreference"]
    reactions: NotRequired["GroupPreference"]
    voice: NotRequired["RoleGroupPreference"]
    files: NotRequired["RoleGroupPreference"]
    simplexLinks: NotRequired["RoleGroupPreference"]
    reports: NotRequired["GroupPreference"]
    history: NotRequired["GroupPreference"]
    support: NotRequired["SupportGroupPreference"]
    sessions: NotRequired["RoleGroupPreference"]
    comments: NotRequired["CommentsGroupPreference"]
    signMessages: NotRequired["GroupPreference"]
    commands: NotRequired[list["ChatBotCommand"]]

class GroupProfile(TypedDict):
    displayName: str
    fullName: str
    shortDescr: NotRequired[str]
    description: NotRequired[str]
    image: NotRequired[str]
    publicGroup: NotRequired["PublicGroupProfile"]
    groupPreferences: NotRequired["GroupPreferences"]
    memberAdmission: NotRequired["GroupMemberAdmission"]

class GroupRelay(TypedDict):
    groupRelayId: int  # int64
    groupMemberId: int  # int64
    userChatRelay: "UserChatRelay"
    relayStatus: "RelayStatus"
    relayLink: NotRequired[str]
    relayCap: "RelayCapabilities"

class GroupRootKey_private(TypedDict):
    type: Literal["private"]
    rootPrivKey: str

class GroupRootKey_public(TypedDict):
    type: Literal["public"]
    rootPubKey: str

GroupRootKey = GroupRootKey_private | GroupRootKey_public

GroupRootKey_Tag = Literal["private", "public"]

class GroupShortLinkData(TypedDict):
    groupProfile: "GroupProfile"
    publicGroupData: NotRequired["PublicGroupData"]

class GroupShortLinkInfo(TypedDict):
    direct: bool
    groupRelays: list[str]
    publicGroupId: NotRequired[str]

class GroupSummary(TypedDict):
    currentMembers: int  # int64
    publicMemberCount: NotRequired[int]  # int64

class GroupSupportChat(TypedDict):
    chatTs: str  # ISO-8601 timestamp
    unread: int  # int64
    memberAttention: int  # int64
    mentions: int  # int64
    lastMsgFromMemberTs: NotRequired[str]  # ISO-8601 timestamp

GroupType = Literal["channel", "group"]

HandshakeError = Literal["PARSE", "IDENTITY", "BAD_AUTH", "BAD_SERVICE"]

InlineFileMode = Literal["offer", "sent"]

class InvitationLinkPlan_ok(TypedDict):
    type: Literal["ok"]
    contactSLinkData_: NotRequired["ContactShortLinkData"]
    ownerVerification: NotRequired["OwnerVerification"]

class InvitationLinkPlan_ownLink(TypedDict):
    type: Literal["ownLink"]

class InvitationLinkPlan_connecting(TypedDict):
    type: Literal["connecting"]
    contact_: NotRequired["Contact"]

class InvitationLinkPlan_known(TypedDict):
    type: Literal["known"]
    contact: "Contact"

InvitationLinkPlan = (
    InvitationLinkPlan_ok
    | InvitationLinkPlan_ownLink
    | InvitationLinkPlan_connecting
    | InvitationLinkPlan_known
)

InvitationLinkPlan_Tag = Literal["ok", "ownLink", "connecting", "known"]

class InvitedBy_contact(TypedDict):
    type: Literal["contact"]
    byContactId: int  # int64

class InvitedBy_user(TypedDict):
    type: Literal["user"]

class InvitedBy_unknown(TypedDict):
    type: Literal["unknown"]

InvitedBy = InvitedBy_contact | InvitedBy_user | InvitedBy_unknown

InvitedBy_Tag = Literal["contact", "user", "unknown"]

class LinkContent_page(TypedDict):
    type: Literal["page"]

class LinkContent_image(TypedDict):
    type: Literal["image"]

class LinkContent_video(TypedDict):
    type: Literal["video"]
    duration: NotRequired[int]  # int

class LinkContent_unknown(TypedDict):
    type: Literal["unknown"]
    tag: str
    json: dict[str, object]

LinkContent = LinkContent_page | LinkContent_image | LinkContent_video | LinkContent_unknown

LinkContent_Tag = Literal["page", "image", "video", "unknown"]

class LinkOwnerSig(TypedDict):
    ownerId: NotRequired[str]
    chatBinding: str
    ownerSig: str

class LinkPreview(TypedDict):
    uri: str
    title: str
    description: str
    image: str
    content: NotRequired["LinkContent"]

class LocalBadge(TypedDict):
    badge: "BadgeInfo"
    status: "BadgeStatus"

class LocalProfile(TypedDict):
    profileId: int  # int64
    displayName: str
    fullName: str
    shortDescr: NotRequired[str]
    description: NotRequired[str]
    image: NotRequired[str]
    contactLink: NotRequired[str]
    preferences: NotRequired["Preferences"]
    peerType: NotRequired["ChatPeerType"]
    localBadge: NotRequired["LocalBadge"]
    localAlias: str
    contactDomain: NotRequired["SimplexDomainClaim"]
    contactDomainVerified: NotRequired[bool]

MemberCriteria = Literal["all"]

# Connection link sent in a message - only short links are allowed.

class MsgChatLink_contact(TypedDict):
    type: Literal["contact"]
    connLink: str
    profile: "Profile"
    business: bool

class MsgChatLink_invitation(TypedDict):
    type: Literal["invitation"]
    invLink: str
    profile: "Profile"

class MsgChatLink_group(TypedDict):
    type: Literal["group"]
    connLink: str
    groupProfile: "GroupProfile"

MsgChatLink = MsgChatLink_contact | MsgChatLink_invitation | MsgChatLink_group

MsgChatLink_Tag = Literal["contact", "invitation", "group"]

class MsgContent_text(TypedDict):
    type: Literal["text"]
    text: str

class MsgContent_link(TypedDict):
    type: Literal["link"]
    text: str
    preview: "LinkPreview"

class MsgContent_image(TypedDict):
    type: Literal["image"]
    text: str
    image: str

class MsgContent_video(TypedDict):
    type: Literal["video"]
    text: str
    image: str
    duration: int  # int

class MsgContent_voice(TypedDict):
    type: Literal["voice"]
    text: str
    duration: int  # int

class MsgContent_file(TypedDict):
    type: Literal["file"]
    text: str

class MsgContent_report(TypedDict):
    type: Literal["report"]
    text: str
    reason: "ReportReason"

class MsgContent_chat(TypedDict):
    type: Literal["chat"]
    text: str
    chatLink: "MsgChatLink"
    ownerSig: NotRequired["LinkOwnerSig"]

class MsgContent_unknown(TypedDict):
    type: Literal["unknown"]
    tag: str
    text: str
    json: dict[str, object]

MsgContent = (
    MsgContent_text
    | MsgContent_link
    | MsgContent_image
    | MsgContent_video
    | MsgContent_voice
    | MsgContent_file
    | MsgContent_report
    | MsgContent_chat
    | MsgContent_unknown
)

MsgContent_Tag = Literal["text", "link", "image", "video", "voice", "file", "report", "chat", "unknown"]

MsgDecryptError = Literal["ratchetHeader", "tooManySkipped", "ratchetEarlier", "other", "ratchetSync"]

MsgDirection = Literal["rcv", "snd"]

class MsgErrorType_msgSkipped(TypedDict):
    type: Literal["msgSkipped"]
    fromMsgId: int  # int64
    toMsgId: int  # int64

class MsgErrorType_msgBadId(TypedDict):
    type: Literal["msgBadId"]
    msgId: int  # int64

class MsgErrorType_msgBadHash(TypedDict):
    type: Literal["msgBadHash"]

class MsgErrorType_msgDuplicate(TypedDict):
    type: Literal["msgDuplicate"]

MsgErrorType = (
    MsgErrorType_msgSkipped
    | MsgErrorType_msgBadId
    | MsgErrorType_msgBadHash
    | MsgErrorType_msgDuplicate
)

MsgErrorType_Tag = Literal["msgSkipped", "msgBadId", "msgBadHash", "msgDuplicate"]

MsgFilter = Literal["none", "all", "mentions"]

class MsgReaction_emoji(TypedDict):
    type: Literal["emoji"]
    emoji: str

class MsgReaction_unknown(TypedDict):
    type: Literal["unknown"]
    tag: str
    json: dict[str, object]

MsgReaction = MsgReaction_emoji | MsgReaction_unknown

MsgReaction_Tag = Literal["emoji", "unknown"]

MsgReceiptStatus = Literal["ok", "badMsgHash"]

MsgSigStatus = Literal["verified", "signedNoKey"]

class MsgVerified_signed(TypedDict):
    type: Literal["signed"]
    sigStatus: "MsgSigStatus"

class MsgVerified_sigMissing(TypedDict):
    type: Literal["sigMissing"]

MsgVerified = MsgVerified_signed | MsgVerified_sigMissing

MsgVerified_Tag = Literal["signed", "sigMissing"]

class NameErrorType_NO_RESOLVER(TypedDict):
    type: Literal["NO_RESOLVER"]

class NameErrorType_NOT_FOUND(TypedDict):
    type: Literal["NOT_FOUND"]

class NameErrorType_RESOLVER(TypedDict):
    type: Literal["RESOLVER"]
    resolverErr: str

NameErrorType = NameErrorType_NO_RESOLVER | NameErrorType_NOT_FOUND | NameErrorType_RESOLVER

NameErrorType_Tag = Literal["NO_RESOLVER", "NOT_FOUND", "RESOLVER"]

class NetworkError_connectError(TypedDict):
    type: Literal["connectError"]
    connectError: str

class NetworkError_tLSError(TypedDict):
    type: Literal["tLSError"]
    tlsError: str

class NetworkError_unknownCAError(TypedDict):
    type: Literal["unknownCAError"]

class NetworkError_failedError(TypedDict):
    type: Literal["failedError"]

class NetworkError_timeoutError(TypedDict):
    type: Literal["timeoutError"]

class NetworkError_subscribeError(TypedDict):
    type: Literal["subscribeError"]
    subscribeError: str

NetworkError = (
    NetworkError_connectError
    | NetworkError_tLSError
    | NetworkError_unknownCAError
    | NetworkError_failedError
    | NetworkError_timeoutError
    | NetworkError_subscribeError
)

NetworkError_Tag = Literal["connectError", "tLSError", "unknownCAError", "failedError", "timeoutError", "subscribeError"]

class NewUser(TypedDict):
    profile: NotRequired["Profile"]
    pastTimestamp: bool
    userChatRelay: bool
    clientService: bool

class NoteFolder(TypedDict):
    noteFolderId: int  # int64
    userId: int  # int64
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp
    chatTs: str  # ISO-8601 timestamp
    favorite: bool
    unread: bool

class OwnerVerification_verified(TypedDict):
    type: Literal["verified"]

class OwnerVerification_failed(TypedDict):
    type: Literal["failed"]
    reason: str

OwnerVerification = OwnerVerification_verified | OwnerVerification_failed

OwnerVerification_Tag = Literal["verified", "failed"]

class PaginationByTime_last(TypedDict):
    type: Literal["last"]
    count: int  # int

PaginationByTime = PaginationByTime_last

PaginationByTime_Tag = Literal["last"]


def PaginationByTime_cmd_string(self: PaginationByTime) -> str:
    return 'count=' + str(self['count'])  # type: ignore[typeddict-item]

class PendingContactConnection(TypedDict):
    pccConnId: int  # int64
    pccAgentConnId: str
    pccConnStatus: "ConnStatus"
    viaContactUri: bool
    viaUserContactLink: NotRequired[int]  # int64
    groupLinkId: NotRequired[str]
    customUserProfileId: NotRequired[int]  # int64
    connLinkInv: NotRequired["CreatedConnLink"]
    localAlias: str
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp

PlanResolveMode = Literal["allGroups", "unknown", "never"]

class PrefEnabled(TypedDict):
    forUser: bool
    forContact: bool

class Preferences(TypedDict):
    timedMessages: NotRequired["TimedMessagesPreference"]
    fullDelete: NotRequired["SimplePreference"]
    reactions: NotRequired["SimplePreference"]
    voice: NotRequired["SimplePreference"]
    files: NotRequired["SimplePreference"]
    calls: NotRequired["SimplePreference"]
    sessions: NotRequired["SimplePreference"]
    commands: NotRequired[list["ChatBotCommand"]]

class PreparedContact(TypedDict):
    connLinkToConnect: "CreatedConnLink"
    uiConnLinkType: "ConnectionMode"
    welcomeSharedMsgId: NotRequired[str]
    requestSharedMsgId: NotRequired[str]

class PreparedGroup(TypedDict):
    connLinkToConnect: "CreatedConnLink"
    connLinkPreparedConnection: bool
    connLinkStartedConnection: bool
    welcomeSharedMsgId: NotRequired[str]
    requestSharedMsgId: NotRequired[str]

class Profile(TypedDict):
    displayName: str
    fullName: str
    shortDescr: NotRequired[str]
    description: NotRequired[str]
    image: NotRequired[str]
    contactLink: NotRequired[str]
    preferences: NotRequired["Preferences"]
    peerType: NotRequired["ChatPeerType"]
    badge: NotRequired["BadgeProof"]
    contactDomain: NotRequired["SimplexDomainClaim"]

class ProxyClientError_protocolError(TypedDict):
    type: Literal["protocolError"]
    protocolErr: "ErrorType"

class ProxyClientError_unexpectedResponse(TypedDict):
    type: Literal["unexpectedResponse"]
    responseStr: str

class ProxyClientError_responseError(TypedDict):
    type: Literal["responseError"]
    responseErr: "ErrorType"

ProxyClientError = (
    ProxyClientError_protocolError
    | ProxyClientError_unexpectedResponse
    | ProxyClientError_responseError
)

ProxyClientError_Tag = Literal["protocolError", "unexpectedResponse", "responseError"]

class ProxyError_PROTOCOL(TypedDict):
    type: Literal["PROTOCOL"]
    protocolErr: "ErrorType"

class ProxyError_BROKER(TypedDict):
    type: Literal["BROKER"]
    brokerErr: "BrokerErrorType"

class ProxyError_BASIC_AUTH(TypedDict):
    type: Literal["BASIC_AUTH"]

class ProxyError_NO_SESSION(TypedDict):
    type: Literal["NO_SESSION"]

ProxyError = ProxyError_PROTOCOL | ProxyError_BROKER | ProxyError_BASIC_AUTH | ProxyError_NO_SESSION

ProxyError_Tag = Literal["PROTOCOL", "BROKER", "BASIC_AUTH", "NO_SESSION"]

class PublicGroupAccess(TypedDict):
    groupWebPage: NotRequired[str]
    groupDomainClaim: NotRequired["SimplexDomainClaim"]
    domainWebPage: bool
    allowEmbedding: bool

class PublicGroupData(TypedDict):
    publicMemberCount: int  # int64

class PublicGroupProfile(TypedDict):
    groupType: "GroupType"
    groupLink: str
    publicGroupId: str
    publicGroupAccess: NotRequired["PublicGroupAccess"]

class RCErrorType_internal(TypedDict):
    type: Literal["internal"]
    internalErr: str

class RCErrorType_identity(TypedDict):
    type: Literal["identity"]

class RCErrorType_noLocalAddress(TypedDict):
    type: Literal["noLocalAddress"]

class RCErrorType_newController(TypedDict):
    type: Literal["newController"]

class RCErrorType_notDiscovered(TypedDict):
    type: Literal["notDiscovered"]

class RCErrorType_tLSStartFailed(TypedDict):
    type: Literal["tLSStartFailed"]

class RCErrorType_exception(TypedDict):
    type: Literal["exception"]
    exception: str

class RCErrorType_ctrlAuth(TypedDict):
    type: Literal["ctrlAuth"]

class RCErrorType_ctrlNotFound(TypedDict):
    type: Literal["ctrlNotFound"]

class RCErrorType_ctrlError(TypedDict):
    type: Literal["ctrlError"]
    ctrlErr: str

class RCErrorType_invitation(TypedDict):
    type: Literal["invitation"]

class RCErrorType_version(TypedDict):
    type: Literal["version"]

class RCErrorType_encrypt(TypedDict):
    type: Literal["encrypt"]

class RCErrorType_decrypt(TypedDict):
    type: Literal["decrypt"]

class RCErrorType_blockSize(TypedDict):
    type: Literal["blockSize"]

class RCErrorType_syntax(TypedDict):
    type: Literal["syntax"]
    syntaxErr: str

RCErrorType = (
    RCErrorType_internal
    | RCErrorType_identity
    | RCErrorType_noLocalAddress
    | RCErrorType_newController
    | RCErrorType_notDiscovered
    | RCErrorType_tLSStartFailed
    | RCErrorType_exception
    | RCErrorType_ctrlAuth
    | RCErrorType_ctrlNotFound
    | RCErrorType_ctrlError
    | RCErrorType_invitation
    | RCErrorType_version
    | RCErrorType_encrypt
    | RCErrorType_decrypt
    | RCErrorType_blockSize
    | RCErrorType_syntax
)

RCErrorType_Tag = Literal["internal", "identity", "noLocalAddress", "newController", "notDiscovered", "tLSStartFailed", "exception", "ctrlAuth", "ctrlNotFound", "ctrlError", "invitation", "version", "encrypt", "decrypt", "blockSize", "syntax"]

RatchetSyncState = Literal["ok", "allowed", "required", "started", "agreed"]

class RcvConnEvent_switchQueue(TypedDict):
    type: Literal["switchQueue"]
    phase: "SwitchPhase"

class RcvConnEvent_ratchetSync(TypedDict):
    type: Literal["ratchetSync"]
    syncStatus: "RatchetSyncState"

class RcvConnEvent_verificationCodeReset(TypedDict):
    type: Literal["verificationCodeReset"]

class RcvConnEvent_pqEnabled(TypedDict):
    type: Literal["pqEnabled"]
    enabled: bool

RcvConnEvent = (
    RcvConnEvent_switchQueue
    | RcvConnEvent_ratchetSync
    | RcvConnEvent_verificationCodeReset
    | RcvConnEvent_pqEnabled
)

RcvConnEvent_Tag = Literal["switchQueue", "ratchetSync", "verificationCodeReset", "pqEnabled"]

class RcvDirectEvent_contactDeleted(TypedDict):
    type: Literal["contactDeleted"]

class RcvDirectEvent_profileUpdated(TypedDict):
    type: Literal["profileUpdated"]
    fromProfile: "Profile"
    toProfile: "Profile"

class RcvDirectEvent_groupInvLinkReceived(TypedDict):
    type: Literal["groupInvLinkReceived"]
    groupProfile: "GroupProfile"

RcvDirectEvent = (
    RcvDirectEvent_contactDeleted
    | RcvDirectEvent_profileUpdated
    | RcvDirectEvent_groupInvLinkReceived
)

RcvDirectEvent_Tag = Literal["contactDeleted", "profileUpdated", "groupInvLinkReceived"]

class RcvFileDescr(TypedDict):
    fileDescrId: int  # int64
    fileDescrText: str
    fileDescrPartNo: int  # int
    fileDescrComplete: bool

class RcvFileStatus_new(TypedDict):
    type: Literal["new"]

class RcvFileStatus_accepted(TypedDict):
    type: Literal["accepted"]
    filePath: str

class RcvFileStatus_connected(TypedDict):
    type: Literal["connected"]
    filePath: str

class RcvFileStatus_complete(TypedDict):
    type: Literal["complete"]
    filePath: str

class RcvFileStatus_cancelled(TypedDict):
    type: Literal["cancelled"]
    filePath_: NotRequired[str]

RcvFileStatus = (
    RcvFileStatus_new
    | RcvFileStatus_accepted
    | RcvFileStatus_connected
    | RcvFileStatus_complete
    | RcvFileStatus_cancelled
)

RcvFileStatus_Tag = Literal["new", "accepted", "connected", "complete", "cancelled"]

class RcvFileTransfer(TypedDict):
    fileId: int  # int64
    xftpRcvFile: NotRequired["XFTPRcvFile"]
    fileInvitation: "FileInvitation"
    fileStatus: "RcvFileStatus"
    fileType: "FileType"
    rcvFileInline: NotRequired["InlineFileMode"]
    senderDisplayName: str
    chunkSize: int  # int64
    cancelled: bool
    grpMemberId: NotRequired[int]  # int64
    cryptoArgs: NotRequired["CryptoFileArgs"]

class RcvGroupEvent_memberAdded(TypedDict):
    type: Literal["memberAdded"]
    groupMemberId: int  # int64
    profile: "Profile"

class RcvGroupEvent_memberConnected(TypedDict):
    type: Literal["memberConnected"]

class RcvGroupEvent_memberAccepted(TypedDict):
    type: Literal["memberAccepted"]
    groupMemberId: int  # int64
    profile: "Profile"

class RcvGroupEvent_userAccepted(TypedDict):
    type: Literal["userAccepted"]

class RcvGroupEvent_memberLeft(TypedDict):
    type: Literal["memberLeft"]

class RcvGroupEvent_memberRole(TypedDict):
    type: Literal["memberRole"]
    groupMemberId: int  # int64
    profile: "Profile"
    role: "GroupMemberRole"

class RcvGroupEvent_memberBlocked(TypedDict):
    type: Literal["memberBlocked"]
    groupMemberId: int  # int64
    profile: "Profile"
    blocked: bool

class RcvGroupEvent_userRole(TypedDict):
    type: Literal["userRole"]
    role: "GroupMemberRole"

class RcvGroupEvent_memberDeleted(TypedDict):
    type: Literal["memberDeleted"]
    groupMemberId: int  # int64
    profile: "Profile"

class RcvGroupEvent_userDeleted(TypedDict):
    type: Literal["userDeleted"]

class RcvGroupEvent_groupDeleted(TypedDict):
    type: Literal["groupDeleted"]

class RcvGroupEvent_groupUpdated(TypedDict):
    type: Literal["groupUpdated"]
    groupProfile: "GroupProfile"

class RcvGroupEvent_invitedViaGroupLink(TypedDict):
    type: Literal["invitedViaGroupLink"]

class RcvGroupEvent_memberCreatedContact(TypedDict):
    type: Literal["memberCreatedContact"]

class RcvGroupEvent_memberProfileUpdated(TypedDict):
    type: Literal["memberProfileUpdated"]
    fromProfile: "Profile"
    toProfile: "Profile"

class RcvGroupEvent_newMemberPendingReview(TypedDict):
    type: Literal["newMemberPendingReview"]

class RcvGroupEvent_msgBadSignature(TypedDict):
    type: Literal["msgBadSignature"]

RcvGroupEvent = (
    RcvGroupEvent_memberAdded
    | RcvGroupEvent_memberConnected
    | RcvGroupEvent_memberAccepted
    | RcvGroupEvent_userAccepted
    | RcvGroupEvent_memberLeft
    | RcvGroupEvent_memberRole
    | RcvGroupEvent_memberBlocked
    | RcvGroupEvent_userRole
    | RcvGroupEvent_memberDeleted
    | RcvGroupEvent_userDeleted
    | RcvGroupEvent_groupDeleted
    | RcvGroupEvent_groupUpdated
    | RcvGroupEvent_invitedViaGroupLink
    | RcvGroupEvent_memberCreatedContact
    | RcvGroupEvent_memberProfileUpdated
    | RcvGroupEvent_newMemberPendingReview
    | RcvGroupEvent_msgBadSignature
)

RcvGroupEvent_Tag = Literal["memberAdded", "memberConnected", "memberAccepted", "userAccepted", "memberLeft", "memberRole", "memberBlocked", "userRole", "memberDeleted", "userDeleted", "groupDeleted", "groupUpdated", "invitedViaGroupLink", "memberCreatedContact", "memberProfileUpdated", "newMemberPendingReview", "msgBadSignature"]

class RcvMsgError_dropped(TypedDict):
    type: Literal["dropped"]
    attempts: int  # int

class RcvMsgError_parseError(TypedDict):
    type: Literal["parseError"]
    parseError: str

RcvMsgError = RcvMsgError_dropped | RcvMsgError_parseError

RcvMsgError_Tag = Literal["dropped", "parseError"]

class RelayCapabilities(TypedDict):
    webDomain: NotRequired[str]

class RelayProfile(TypedDict):
    displayName: str
    fullName: str
    shortDescr: NotRequired[str]
    image: NotRequired[str]

RelayStatus = Literal["new", "invited", "accepted", "acknowledgedRoster", "active", "inactive", "rejected"]

ReportReason = Literal["spam", "content", "community", "profile", "other"]

class RoleGroupPreference(TypedDict):
    enable: "GroupFeatureEnabled"
    role: NotRequired["GroupMemberRole"]

class SMPAgentError_A_MESSAGE(TypedDict):
    type: Literal["A_MESSAGE"]

class SMPAgentError_A_PROHIBITED(TypedDict):
    type: Literal["A_PROHIBITED"]
    prohibitedErr: str

class SMPAgentError_A_VERSION(TypedDict):
    type: Literal["A_VERSION"]

class SMPAgentError_A_LINK(TypedDict):
    type: Literal["A_LINK"]
    linkErr: str

class SMPAgentError_A_CRYPTO(TypedDict):
    type: Literal["A_CRYPTO"]
    cryptoErr: "AgentCryptoError"

class SMPAgentError_A_DUPLICATE(TypedDict):
    type: Literal["A_DUPLICATE"]
    droppedMsg_: NotRequired["DroppedMsg"]

class SMPAgentError_A_QUEUE(TypedDict):
    type: Literal["A_QUEUE"]
    queueErr: str

class SMPAgentError_A_SERVICE(TypedDict):
    type: Literal["A_SERVICE"]
    serviceError: "AgentServiceError"

SMPAgentError = (
    SMPAgentError_A_MESSAGE
    | SMPAgentError_A_PROHIBITED
    | SMPAgentError_A_VERSION
    | SMPAgentError_A_LINK
    | SMPAgentError_A_CRYPTO
    | SMPAgentError_A_DUPLICATE
    | SMPAgentError_A_QUEUE
    | SMPAgentError_A_SERVICE
)

SMPAgentError_Tag = Literal["A_MESSAGE", "A_PROHIBITED", "A_VERSION", "A_LINK", "A_CRYPTO", "A_DUPLICATE", "A_QUEUE", "A_SERVICE"]

class SecurityCode(TypedDict):
    securityCode: str
    verifiedAt: str  # ISO-8601 timestamp

class SimplePreference(TypedDict):
    allow: "FeatureAllowed"

class SimplexDomain(TypedDict):
    nameTLD: "SimplexTLD"
    domain: str
    subDomain: list[str]

class SimplexDomainClaim(TypedDict):
    domain: str
    proof: NotRequired["SimplexDomainProof"]

class SimplexDomainError_noValidLink(TypedDict):
    type: Literal["noValidLink"]

class SimplexDomainError_unknownDomain(TypedDict):
    type: Literal["unknownDomain"]

SimplexDomainError = SimplexDomainError_noValidLink | SimplexDomainError_unknownDomain

SimplexDomainError_Tag = Literal["noValidLink", "unknownDomain"]

class SimplexDomainProof(TypedDict):
    linkOwnerId: NotRequired[str]
    presHeader: str
    signature: str

SimplexLinkType = Literal["contact", "invitation", "group", "channel", "relay"]

class SimplexNameInfo(TypedDict):
    nameType: "SimplexNameType"
    nameDomain: "SimplexDomain"

SimplexNameType = Literal["publicGroup", "contact"]

SimplexTLD = Literal["simplex", "testing", "web"]

SndCIStatusProgress = Literal["partial", "complete"]

class SndConnEvent_switchQueue(TypedDict):
    type: Literal["switchQueue"]
    phase: "SwitchPhase"
    member: NotRequired["GroupMemberRef"]

class SndConnEvent_ratchetSync(TypedDict):
    type: Literal["ratchetSync"]
    syncStatus: "RatchetSyncState"
    member: NotRequired["GroupMemberRef"]

class SndConnEvent_pqEnabled(TypedDict):
    type: Literal["pqEnabled"]
    enabled: bool

SndConnEvent = SndConnEvent_switchQueue | SndConnEvent_ratchetSync | SndConnEvent_pqEnabled

SndConnEvent_Tag = Literal["switchQueue", "ratchetSync", "pqEnabled"]

class SndError_auth(TypedDict):
    type: Literal["auth"]

class SndError_quota(TypedDict):
    type: Literal["quota"]

class SndError_expired(TypedDict):
    type: Literal["expired"]

class SndError_relay(TypedDict):
    type: Literal["relay"]
    srvError: "SrvError"

class SndError_proxy(TypedDict):
    type: Literal["proxy"]
    proxyServer: str
    srvError: "SrvError"

class SndError_proxyRelay(TypedDict):
    type: Literal["proxyRelay"]
    proxyServer: str
    srvError: "SrvError"

class SndError_other(TypedDict):
    type: Literal["other"]
    sndError: str

SndError = (
    SndError_auth
    | SndError_quota
    | SndError_expired
    | SndError_relay
    | SndError_proxy
    | SndError_proxyRelay
    | SndError_other
)

SndError_Tag = Literal["auth", "quota", "expired", "relay", "proxy", "proxyRelay", "other"]

class SndFileTransfer(TypedDict):
    fileId: int  # int64
    fileName: str
    filePath: str
    fileSize: int  # int64
    chunkSize: int  # int64
    recipientDisplayName: str
    connId: int  # int64
    agentConnId: str
    groupMemberId: NotRequired[int]  # int64
    fileStatus: "FileStatus"
    fileDescrId: NotRequired[int]  # int64
    fileInline: NotRequired["InlineFileMode"]

class SndGroupEvent_memberRole(TypedDict):
    type: Literal["memberRole"]
    groupMemberId: int  # int64
    profile: "Profile"
    role: "GroupMemberRole"

class SndGroupEvent_memberBlocked(TypedDict):
    type: Literal["memberBlocked"]
    groupMemberId: int  # int64
    profile: "Profile"
    blocked: bool

class SndGroupEvent_userRole(TypedDict):
    type: Literal["userRole"]
    role: "GroupMemberRole"

class SndGroupEvent_memberDeleted(TypedDict):
    type: Literal["memberDeleted"]
    groupMemberId: int  # int64
    profile: "Profile"

class SndGroupEvent_userLeft(TypedDict):
    type: Literal["userLeft"]

class SndGroupEvent_groupUpdated(TypedDict):
    type: Literal["groupUpdated"]
    groupProfile: "GroupProfile"

class SndGroupEvent_memberAccepted(TypedDict):
    type: Literal["memberAccepted"]
    groupMemberId: int  # int64
    profile: "Profile"

class SndGroupEvent_userPendingReview(TypedDict):
    type: Literal["userPendingReview"]

SndGroupEvent = (
    SndGroupEvent_memberRole
    | SndGroupEvent_memberBlocked
    | SndGroupEvent_userRole
    | SndGroupEvent_memberDeleted
    | SndGroupEvent_userLeft
    | SndGroupEvent_groupUpdated
    | SndGroupEvent_memberAccepted
    | SndGroupEvent_userPendingReview
)

SndGroupEvent_Tag = Literal["memberRole", "memberBlocked", "userRole", "memberDeleted", "userLeft", "groupUpdated", "memberAccepted", "userPendingReview"]

class SrvError_host(TypedDict):
    type: Literal["host"]

class SrvError_version(TypedDict):
    type: Literal["version"]

class SrvError_other(TypedDict):
    type: Literal["other"]
    srvError: str

SrvError = SrvError_host | SrvError_version | SrvError_other

SrvError_Tag = Literal["host", "version", "other"]

class StoreError_duplicateName(TypedDict):
    type: Literal["duplicateName"]

class StoreError_userNotFound(TypedDict):
    type: Literal["userNotFound"]
    userId: int  # int64

class StoreError_relayUserNotFound(TypedDict):
    type: Literal["relayUserNotFound"]

class StoreError_userNotFoundByName(TypedDict):
    type: Literal["userNotFoundByName"]
    contactName: str

class StoreError_userNotFoundByContactId(TypedDict):
    type: Literal["userNotFoundByContactId"]
    contactId: int  # int64

class StoreError_userNotFoundByGroupId(TypedDict):
    type: Literal["userNotFoundByGroupId"]
    groupId: int  # int64

class StoreError_userNotFoundByFileId(TypedDict):
    type: Literal["userNotFoundByFileId"]
    fileId: int  # int64

class StoreError_userNotFoundByContactRequestId(TypedDict):
    type: Literal["userNotFoundByContactRequestId"]
    contactRequestId: int  # int64

class StoreError_contactNotFound(TypedDict):
    type: Literal["contactNotFound"]
    contactId: int  # int64

class StoreError_contactNotFoundByName(TypedDict):
    type: Literal["contactNotFoundByName"]
    contactName: str

class StoreError_contactNotFoundByMemberId(TypedDict):
    type: Literal["contactNotFoundByMemberId"]
    groupMemberId: int  # int64

class StoreError_contactNotReady(TypedDict):
    type: Literal["contactNotReady"]
    contactName: str

class StoreError_duplicateContactLink(TypedDict):
    type: Literal["duplicateContactLink"]

class StoreError_userContactLinkNotFound(TypedDict):
    type: Literal["userContactLinkNotFound"]

class StoreError_contactRequestNotFound(TypedDict):
    type: Literal["contactRequestNotFound"]
    contactRequestId: int  # int64

class StoreError_contactRequestNotFoundByName(TypedDict):
    type: Literal["contactRequestNotFoundByName"]
    contactName: str

class StoreError_invalidContactRequestEntity(TypedDict):
    type: Literal["invalidContactRequestEntity"]
    contactRequestId: int  # int64

class StoreError_invalidBusinessChatContactRequest(TypedDict):
    type: Literal["invalidBusinessChatContactRequest"]

class StoreError_groupNotFound(TypedDict):
    type: Literal["groupNotFound"]
    groupId: int  # int64

class StoreError_groupNotFoundByName(TypedDict):
    type: Literal["groupNotFoundByName"]
    groupName: str

class StoreError_groupMemberNameNotFound(TypedDict):
    type: Literal["groupMemberNameNotFound"]
    groupId: int  # int64
    groupMemberName: str

class StoreError_groupMemberNotFound(TypedDict):
    type: Literal["groupMemberNotFound"]
    groupMemberId: int  # int64

class StoreError_groupMemberNotFoundByIndex(TypedDict):
    type: Literal["groupMemberNotFoundByIndex"]
    groupMemberIndex: int  # int64

class StoreError_memberRelationsVectorNotFound(TypedDict):
    type: Literal["memberRelationsVectorNotFound"]
    groupMemberId: int  # int64

class StoreError_groupHostMemberNotFound(TypedDict):
    type: Literal["groupHostMemberNotFound"]
    groupId: int  # int64

class StoreError_groupMemberNotFoundByMemberId(TypedDict):
    type: Literal["groupMemberNotFoundByMemberId"]
    memberId: str

class StoreError_memberContactGroupMemberNotFound(TypedDict):
    type: Literal["memberContactGroupMemberNotFound"]
    contactId: int  # int64

class StoreError_invalidMemberRelationUpdate(TypedDict):
    type: Literal["invalidMemberRelationUpdate"]

class StoreError_groupWithoutUser(TypedDict):
    type: Literal["groupWithoutUser"]

class StoreError_duplicateGroupMember(TypedDict):
    type: Literal["duplicateGroupMember"]

class StoreError_duplicateMemberId(TypedDict):
    type: Literal["duplicateMemberId"]

class StoreError_groupAlreadyJoined(TypedDict):
    type: Literal["groupAlreadyJoined"]

class StoreError_groupInvitationNotFound(TypedDict):
    type: Literal["groupInvitationNotFound"]

class StoreError_noteFolderAlreadyExists(TypedDict):
    type: Literal["noteFolderAlreadyExists"]
    noteFolderId: int  # int64

class StoreError_noteFolderNotFound(TypedDict):
    type: Literal["noteFolderNotFound"]
    noteFolderId: int  # int64

class StoreError_userNoteFolderNotFound(TypedDict):
    type: Literal["userNoteFolderNotFound"]

class StoreError_sndFileNotFound(TypedDict):
    type: Literal["sndFileNotFound"]
    fileId: int  # int64

class StoreError_sndFileInvalid(TypedDict):
    type: Literal["sndFileInvalid"]
    fileId: int  # int64

class StoreError_rcvFileNotFound(TypedDict):
    type: Literal["rcvFileNotFound"]
    fileId: int  # int64

class StoreError_rcvFileDescrNotFound(TypedDict):
    type: Literal["rcvFileDescrNotFound"]
    fileId: int  # int64

class StoreError_fileNotFound(TypedDict):
    type: Literal["fileNotFound"]
    fileId: int  # int64

class StoreError_rcvFileInvalid(TypedDict):
    type: Literal["rcvFileInvalid"]
    fileId: int  # int64

class StoreError_rcvFileInvalidDescrPart(TypedDict):
    type: Literal["rcvFileInvalidDescrPart"]

class StoreError_localFileNoTransfer(TypedDict):
    type: Literal["localFileNoTransfer"]
    fileId: int  # int64

class StoreError_sharedMsgIdNotFoundByFileId(TypedDict):
    type: Literal["sharedMsgIdNotFoundByFileId"]
    fileId: int  # int64

class StoreError_fileIdNotFoundBySharedMsgId(TypedDict):
    type: Literal["fileIdNotFoundBySharedMsgId"]
    sharedMsgId: str

class StoreError_sndFileNotFoundXFTP(TypedDict):
    type: Literal["sndFileNotFoundXFTP"]
    agentSndFileId: str

class StoreError_rcvFileNotFoundXFTP(TypedDict):
    type: Literal["rcvFileNotFoundXFTP"]
    agentRcvFileId: str

class StoreError_connectionNotFound(TypedDict):
    type: Literal["connectionNotFound"]
    agentConnId: str

class StoreError_connectionNotFoundById(TypedDict):
    type: Literal["connectionNotFoundById"]
    connId: int  # int64

class StoreError_connectionNotFoundByMemberId(TypedDict):
    type: Literal["connectionNotFoundByMemberId"]
    groupMemberId: int  # int64

class StoreError_pendingConnectionNotFound(TypedDict):
    type: Literal["pendingConnectionNotFound"]
    connId: int  # int64

class StoreError_uniqueID(TypedDict):
    type: Literal["uniqueID"]

class StoreError_largeMsg(TypedDict):
    type: Literal["largeMsg"]

class StoreError_internalError(TypedDict):
    type: Literal["internalError"]
    message: str

class StoreError_dBException(TypedDict):
    type: Literal["dBException"]
    message: str

class StoreError_dBBusyError(TypedDict):
    type: Literal["dBBusyError"]
    message: str

class StoreError_badChatItem(TypedDict):
    type: Literal["badChatItem"]
    itemId: int  # int64
    itemTs: NotRequired[str]  # ISO-8601 timestamp

class StoreError_chatItemNotFound(TypedDict):
    type: Literal["chatItemNotFound"]
    itemId: int  # int64

class StoreError_chatItemNotFoundByText(TypedDict):
    type: Literal["chatItemNotFoundByText"]
    text: str

class StoreError_chatItemSharedMsgIdNotFound(TypedDict):
    type: Literal["chatItemSharedMsgIdNotFound"]
    sharedMsgId: str

class StoreError_chatItemNotFoundByFileId(TypedDict):
    type: Literal["chatItemNotFoundByFileId"]
    fileId: int  # int64

class StoreError_chatItemNotFoundByContactId(TypedDict):
    type: Literal["chatItemNotFoundByContactId"]
    contactId: int  # int64

class StoreError_chatItemNotFoundByGroupId(TypedDict):
    type: Literal["chatItemNotFoundByGroupId"]
    groupId: int  # int64

class StoreError_profileNotFound(TypedDict):
    type: Literal["profileNotFound"]
    profileId: int  # int64

class StoreError_duplicateGroupLink(TypedDict):
    type: Literal["duplicateGroupLink"]
    groupInfo: "GroupInfo"

class StoreError_groupLinkNotFound(TypedDict):
    type: Literal["groupLinkNotFound"]
    groupInfo: "GroupInfo"

class StoreError_hostMemberIdNotFound(TypedDict):
    type: Literal["hostMemberIdNotFound"]
    groupId: int  # int64

class StoreError_contactNotFoundByFileId(TypedDict):
    type: Literal["contactNotFoundByFileId"]
    fileId: int  # int64

class StoreError_noGroupSndStatus(TypedDict):
    type: Literal["noGroupSndStatus"]
    itemId: int  # int64
    groupMemberId: int  # int64

class StoreError_duplicateGroupMessage(TypedDict):
    type: Literal["duplicateGroupMessage"]
    groupId: int  # int64
    sharedMsgId: str
    authorGroupMemberId: NotRequired[int]  # int64
    forwardedByGroupMemberId: NotRequired[int]  # int64

class StoreError_remoteHostNotFound(TypedDict):
    type: Literal["remoteHostNotFound"]
    remoteHostId: int  # int64

class StoreError_remoteHostUnknown(TypedDict):
    type: Literal["remoteHostUnknown"]

class StoreError_remoteHostDuplicateCA(TypedDict):
    type: Literal["remoteHostDuplicateCA"]

class StoreError_remoteCtrlNotFound(TypedDict):
    type: Literal["remoteCtrlNotFound"]
    remoteCtrlId: int  # int64

class StoreError_remoteCtrlDuplicateCA(TypedDict):
    type: Literal["remoteCtrlDuplicateCA"]

class StoreError_prohibitedDeleteUser(TypedDict):
    type: Literal["prohibitedDeleteUser"]
    userId: int  # int64
    contactId: int  # int64

class StoreError_operatorNotFound(TypedDict):
    type: Literal["operatorNotFound"]
    serverOperatorId: int  # int64

class StoreError_usageConditionsNotFound(TypedDict):
    type: Literal["usageConditionsNotFound"]

class StoreError_userChatRelayNotFound(TypedDict):
    type: Literal["userChatRelayNotFound"]
    chatRelayId: int  # int64

class StoreError_groupRelayNotFound(TypedDict):
    type: Literal["groupRelayNotFound"]
    groupRelayId: int  # int64

class StoreError_groupRelayNotFoundByMemberId(TypedDict):
    type: Literal["groupRelayNotFoundByMemberId"]
    groupMemberId: int  # int64

class StoreError_invalidQuote(TypedDict):
    type: Literal["invalidQuote"]

class StoreError_invalidMention(TypedDict):
    type: Literal["invalidMention"]

class StoreError_invalidDeliveryTask(TypedDict):
    type: Literal["invalidDeliveryTask"]
    taskId: int  # int64

class StoreError_deliveryTaskNotFound(TypedDict):
    type: Literal["deliveryTaskNotFound"]
    taskId: int  # int64

class StoreError_invalidDeliveryJob(TypedDict):
    type: Literal["invalidDeliveryJob"]
    jobId: int  # int64

class StoreError_deliveryJobNotFound(TypedDict):
    type: Literal["deliveryJobNotFound"]
    jobId: int  # int64

class StoreError_workItemError(TypedDict):
    type: Literal["workItemError"]
    errContext: str

StoreError = (
    StoreError_duplicateName
    | StoreError_userNotFound
    | StoreError_relayUserNotFound
    | StoreError_userNotFoundByName
    | StoreError_userNotFoundByContactId
    | StoreError_userNotFoundByGroupId
    | StoreError_userNotFoundByFileId
    | StoreError_userNotFoundByContactRequestId
    | StoreError_contactNotFound
    | StoreError_contactNotFoundByName
    | StoreError_contactNotFoundByMemberId
    | StoreError_contactNotReady
    | StoreError_duplicateContactLink
    | StoreError_userContactLinkNotFound
    | StoreError_contactRequestNotFound
    | StoreError_contactRequestNotFoundByName
    | StoreError_invalidContactRequestEntity
    | StoreError_invalidBusinessChatContactRequest
    | StoreError_groupNotFound
    | StoreError_groupNotFoundByName
    | StoreError_groupMemberNameNotFound
    | StoreError_groupMemberNotFound
    | StoreError_groupMemberNotFoundByIndex
    | StoreError_memberRelationsVectorNotFound
    | StoreError_groupHostMemberNotFound
    | StoreError_groupMemberNotFoundByMemberId
    | StoreError_memberContactGroupMemberNotFound
    | StoreError_invalidMemberRelationUpdate
    | StoreError_groupWithoutUser
    | StoreError_duplicateGroupMember
    | StoreError_duplicateMemberId
    | StoreError_groupAlreadyJoined
    | StoreError_groupInvitationNotFound
    | StoreError_noteFolderAlreadyExists
    | StoreError_noteFolderNotFound
    | StoreError_userNoteFolderNotFound
    | StoreError_sndFileNotFound
    | StoreError_sndFileInvalid
    | StoreError_rcvFileNotFound
    | StoreError_rcvFileDescrNotFound
    | StoreError_fileNotFound
    | StoreError_rcvFileInvalid
    | StoreError_rcvFileInvalidDescrPart
    | StoreError_localFileNoTransfer
    | StoreError_sharedMsgIdNotFoundByFileId
    | StoreError_fileIdNotFoundBySharedMsgId
    | StoreError_sndFileNotFoundXFTP
    | StoreError_rcvFileNotFoundXFTP
    | StoreError_connectionNotFound
    | StoreError_connectionNotFoundById
    | StoreError_connectionNotFoundByMemberId
    | StoreError_pendingConnectionNotFound
    | StoreError_uniqueID
    | StoreError_largeMsg
    | StoreError_internalError
    | StoreError_dBException
    | StoreError_dBBusyError
    | StoreError_badChatItem
    | StoreError_chatItemNotFound
    | StoreError_chatItemNotFoundByText
    | StoreError_chatItemSharedMsgIdNotFound
    | StoreError_chatItemNotFoundByFileId
    | StoreError_chatItemNotFoundByContactId
    | StoreError_chatItemNotFoundByGroupId
    | StoreError_profileNotFound
    | StoreError_duplicateGroupLink
    | StoreError_groupLinkNotFound
    | StoreError_hostMemberIdNotFound
    | StoreError_contactNotFoundByFileId
    | StoreError_noGroupSndStatus
    | StoreError_duplicateGroupMessage
    | StoreError_remoteHostNotFound
    | StoreError_remoteHostUnknown
    | StoreError_remoteHostDuplicateCA
    | StoreError_remoteCtrlNotFound
    | StoreError_remoteCtrlDuplicateCA
    | StoreError_prohibitedDeleteUser
    | StoreError_operatorNotFound
    | StoreError_usageConditionsNotFound
    | StoreError_userChatRelayNotFound
    | StoreError_groupRelayNotFound
    | StoreError_groupRelayNotFoundByMemberId
    | StoreError_invalidQuote
    | StoreError_invalidMention
    | StoreError_invalidDeliveryTask
    | StoreError_deliveryTaskNotFound
    | StoreError_invalidDeliveryJob
    | StoreError_deliveryJobNotFound
    | StoreError_workItemError
)

StoreError_Tag = Literal["duplicateName", "userNotFound", "relayUserNotFound", "userNotFoundByName", "userNotFoundByContactId", "userNotFoundByGroupId", "userNotFoundByFileId", "userNotFoundByContactRequestId", "contactNotFound", "contactNotFoundByName", "contactNotFoundByMemberId", "contactNotReady", "duplicateContactLink", "userContactLinkNotFound", "contactRequestNotFound", "contactRequestNotFoundByName", "invalidContactRequestEntity", "invalidBusinessChatContactRequest", "groupNotFound", "groupNotFoundByName", "groupMemberNameNotFound", "groupMemberNotFound", "groupMemberNotFoundByIndex", "memberRelationsVectorNotFound", "groupHostMemberNotFound", "groupMemberNotFoundByMemberId", "memberContactGroupMemberNotFound", "invalidMemberRelationUpdate", "groupWithoutUser", "duplicateGroupMember", "duplicateMemberId", "groupAlreadyJoined", "groupInvitationNotFound", "noteFolderAlreadyExists", "noteFolderNotFound", "userNoteFolderNotFound", "sndFileNotFound", "sndFileInvalid", "rcvFileNotFound", "rcvFileDescrNotFound", "fileNotFound", "rcvFileInvalid", "rcvFileInvalidDescrPart", "localFileNoTransfer", "sharedMsgIdNotFoundByFileId", "fileIdNotFoundBySharedMsgId", "sndFileNotFoundXFTP", "rcvFileNotFoundXFTP", "connectionNotFound", "connectionNotFoundById", "connectionNotFoundByMemberId", "pendingConnectionNotFound", "uniqueID", "largeMsg", "internalError", "dBException", "dBBusyError", "badChatItem", "chatItemNotFound", "chatItemNotFoundByText", "chatItemSharedMsgIdNotFound", "chatItemNotFoundByFileId", "chatItemNotFoundByContactId", "chatItemNotFoundByGroupId", "profileNotFound", "duplicateGroupLink", "groupLinkNotFound", "hostMemberIdNotFound", "contactNotFoundByFileId", "noGroupSndStatus", "duplicateGroupMessage", "remoteHostNotFound", "remoteHostUnknown", "remoteHostDuplicateCA", "remoteCtrlNotFound", "remoteCtrlDuplicateCA", "prohibitedDeleteUser", "operatorNotFound", "usageConditionsNotFound", "userChatRelayNotFound", "groupRelayNotFound", "groupRelayNotFoundByMemberId", "invalidQuote", "invalidMention", "invalidDeliveryTask", "deliveryTaskNotFound", "invalidDeliveryJob", "deliveryJobNotFound", "workItemError"]

class SubscriptionStatus_active(TypedDict):
    type: Literal["active"]

class SubscriptionStatus_pending(TypedDict):
    type: Literal["pending"]

class SubscriptionStatus_removed(TypedDict):
    type: Literal["removed"]
    subError: str

class SubscriptionStatus_noSub(TypedDict):
    type: Literal["noSub"]

SubscriptionStatus = (
    SubscriptionStatus_active
    | SubscriptionStatus_pending
    | SubscriptionStatus_removed
    | SubscriptionStatus_noSub
)

SubscriptionStatus_Tag = Literal["active", "pending", "removed", "noSub"]

class SupportGroupPreference(TypedDict):
    enable: "GroupFeatureEnabled"

SwitchPhase = Literal["started", "confirmed", "secured", "completed"]

class TimedMessagesGroupPreference(TypedDict):
    enable: "GroupFeatureEnabled"
    ttl: NotRequired[int]  # int

class TimedMessagesPreference(TypedDict):
    allow: "FeatureAllowed"
    ttl: NotRequired[int]  # int

class TransportError_badBlock(TypedDict):
    type: Literal["badBlock"]

class TransportError_version(TypedDict):
    type: Literal["version"]

class TransportError_largeMsg(TypedDict):
    type: Literal["largeMsg"]

class TransportError_badSession(TypedDict):
    type: Literal["badSession"]

class TransportError_noServerAuth(TypedDict):
    type: Literal["noServerAuth"]

class TransportError_handshake(TypedDict):
    type: Literal["handshake"]
    handshakeErr: "HandshakeError"

TransportError = (
    TransportError_badBlock
    | TransportError_version
    | TransportError_largeMsg
    | TransportError_badSession
    | TransportError_noServerAuth
    | TransportError_handshake
)

TransportError_Tag = Literal["badBlock", "version", "largeMsg", "badSession", "noServerAuth", "handshake"]

UIColorMode = Literal["light", "dark"]

class UIColors(TypedDict):
    accent: NotRequired[str]
    accentVariant: NotRequired[str]
    secondary: NotRequired[str]
    secondaryVariant: NotRequired[str]
    background: NotRequired[str]
    menus: NotRequired[str]
    title: NotRequired[str]
    accentVariant2: NotRequired[str]
    sentMessage: NotRequired[str]
    sentReply: NotRequired[str]
    receivedMessage: NotRequired[str]
    receivedReply: NotRequired[str]

class UIThemeEntityOverride(TypedDict):
    mode: "UIColorMode"
    wallpaper: NotRequired["ChatWallpaper"]
    colors: "UIColors"

class UIThemeEntityOverrides(TypedDict):
    light: NotRequired["UIThemeEntityOverride"]
    dark: NotRequired["UIThemeEntityOverride"]

class UpdatedMessage(TypedDict):
    msgContent: "MsgContent"
    mentions: dict[str, int]  # str : int64

class User(TypedDict):
    userId: int  # int64
    agentUserId: int  # int64
    userContactId: int  # int64
    localDisplayName: str
    profile: "LocalProfile"
    fullPreferences: "FullPreferences"
    activeUser: bool
    activeOrder: int  # int64
    viewPwdHash: NotRequired["UserPwdHash"]
    showNtfs: bool
    sendRcptsContacts: bool
    sendRcptsSmallGroups: bool
    autoAcceptMemberContacts: bool
    userMemberProfileUpdatedAt: NotRequired[str]  # ISO-8601 timestamp
    userChatRelay: bool
    clientService: bool
    uiThemes: NotRequired["UIThemeEntityOverrides"]

class UserChatRelay(TypedDict):
    chatRelayId: int  # int64
    address: str
    relayProfile: "RelayProfile"
    domains: list[str]
    preset: bool
    tested: NotRequired[bool]
    enabled: bool
    deleted: bool

class UserContact(TypedDict):
    userContactLinkId: int  # int64
    connReqContact: str
    groupId: NotRequired[int]  # int64

class UserContactLink(TypedDict):
    userContactLinkId: int  # int64
    connLinkContact: "CreatedConnLink"
    shortLinkDataSet: bool
    shortLinkLargeDataSet: bool
    addressSettings: "AddressSettings"

class UserContactRequest(TypedDict):
    contactRequestId: int  # int64
    agentInvitationId: str
    contactId_: NotRequired[int]  # int64
    businessGroupId_: NotRequired[int]  # int64
    userContactLinkId_: NotRequired[int]  # int64
    cReqChatVRange: "VersionRange"
    localDisplayName: str
    profileId: int  # int64
    profile: "LocalProfile"
    createdAt: str  # ISO-8601 timestamp
    updatedAt: str  # ISO-8601 timestamp
    xContactId: NotRequired[str]
    pqSupport: bool
    welcomeSharedMsgId: NotRequired[str]
    requestSharedMsgId: NotRequired[str]
    rejectionSupported: bool

class UserContactRequestRef(TypedDict):
    contactRequestId: int  # int64
    rejectionSupported: bool

class UserInfo(TypedDict):
    user: "User"
    unreadCount: int  # int

class UserProfileUpdateSummary(TypedDict):
    updateSuccesses: int  # int
    updateFailures: int  # int
    changedContacts: list["Contact"]

class UserPwdHash(TypedDict):
    hash: str
    salt: str

class VersionRange(TypedDict):
    minVersion: int  # int
    maxVersion: int  # int

class XFTPErrorType_BLOCK(TypedDict):
    type: Literal["BLOCK"]

class XFTPErrorType_SESSION(TypedDict):
    type: Literal["SESSION"]

class XFTPErrorType_HANDSHAKE(TypedDict):
    type: Literal["HANDSHAKE"]

class XFTPErrorType_CMD(TypedDict):
    type: Literal["CMD"]
    cmdErr: "CommandError"

class XFTPErrorType_AUTH(TypedDict):
    type: Literal["AUTH"]

class XFTPErrorType_BLOCKED(TypedDict):
    type: Literal["BLOCKED"]
    blockInfo: "BlockingInfo"

class XFTPErrorType_SIZE(TypedDict):
    type: Literal["SIZE"]

class XFTPErrorType_QUOTA(TypedDict):
    type: Literal["QUOTA"]

class XFTPErrorType_DIGEST(TypedDict):
    type: Literal["DIGEST"]

class XFTPErrorType_CRYPTO(TypedDict):
    type: Literal["CRYPTO"]

class XFTPErrorType_NO_FILE(TypedDict):
    type: Literal["NO_FILE"]

class XFTPErrorType_HAS_FILE(TypedDict):
    type: Literal["HAS_FILE"]

class XFTPErrorType_FILE_IO(TypedDict):
    type: Literal["FILE_IO"]

class XFTPErrorType_TIMEOUT(TypedDict):
    type: Literal["TIMEOUT"]

class XFTPErrorType_INTERNAL(TypedDict):
    type: Literal["INTERNAL"]

class XFTPErrorType_DUPLICATE_(TypedDict):
    type: Literal["DUPLICATE_"]

XFTPErrorType = (
    XFTPErrorType_BLOCK
    | XFTPErrorType_SESSION
    | XFTPErrorType_HANDSHAKE
    | XFTPErrorType_CMD
    | XFTPErrorType_AUTH
    | XFTPErrorType_BLOCKED
    | XFTPErrorType_SIZE
    | XFTPErrorType_QUOTA
    | XFTPErrorType_DIGEST
    | XFTPErrorType_CRYPTO
    | XFTPErrorType_NO_FILE
    | XFTPErrorType_HAS_FILE
    | XFTPErrorType_FILE_IO
    | XFTPErrorType_TIMEOUT
    | XFTPErrorType_INTERNAL
    | XFTPErrorType_DUPLICATE_
)

XFTPErrorType_Tag = Literal["BLOCK", "SESSION", "HANDSHAKE", "CMD", "AUTH", "BLOCKED", "SIZE", "QUOTA", "DIGEST", "CRYPTO", "NO_FILE", "HAS_FILE", "FILE_IO", "TIMEOUT", "INTERNAL", "DUPLICATE_"]

class XFTPRcvFile(TypedDict):
    rcvFileDescription: "RcvFileDescr"
    agentRcvFileId: NotRequired[str]
    agentRcvFileDeleted: bool
    userApprovedRelays: bool

class XFTPSndFile(TypedDict):
    agentSndFileId: str
    privateSndFileDescr: NotRequired[str]
    agentSndFileDeleted: bool
    cryptoArgs: NotRequired["CryptoFileArgs"]
