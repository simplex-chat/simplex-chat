# API Events

This file is generated automatically.

[Contact connection events](#contact-connection-events)
- Main event
  - [ContactConnected](#contactconnected)
- Other events
  - [ContactUpdated](#contactupdated)
  - [ContactDeletedByContact](#contactdeletedbycontact)
  - [ReceivedContactRequest](#receivedcontactrequest)
  - [NewMemberContactReceivedInv](#newmembercontactreceivedinv)
  - [ContactSndReady](#contactsndready)

[Message events](#message-events)
- Main event
  - [NewChatItems](#newchatitems)
- Other events
  - [ChatItemReaction](#chatitemreaction)
  - [ChatItemsDeleted](#chatitemsdeleted)
  - [ChatItemUpdated](#chatitemupdated)
  - [GroupChatItemsDeleted](#groupchatitemsdeleted)
  - [ChatItemsStatusesUpdated](#chatitemsstatusesupdated)

[Group events](#group-events)
- Main events
  - [ReceivedGroupInvitation](#receivedgroupinvitation)
  - [UserJoinedGroup](#userjoinedgroup)
  - [GroupUpdated](#groupupdated)
  - [JoinedGroupMember](#joinedgroupmember)
  - [MemberRole](#memberrole)
  - [DeletedMember](#deletedmember)
  - [LeftMember](#leftmember)
  - [DeletedMemberUser](#deletedmemberuser)
  - [GroupDeleted](#groupdeleted)
- Other events
  - [ConnectedToGroupMember](#connectedtogroupmember)
  - [MemberAcceptedByOther](#memberacceptedbyother)
  - [MemberBlockedForAll](#memberblockedforall)
  - [GroupMemberUpdated](#groupmemberupdated)

[File events](#file-events)
- Main events
  - [RcvFileDescrReady](#rcvfiledescrready)
  - [RcvFileComplete](#rcvfilecomplete)
  - [SndFileCompleteXFTP](#sndfilecompletexftp)
- Other events
  - [RcvFileStart](#rcvfilestart)
  - [RcvFileSndCancelled](#rcvfilesndcancelled)
  - [RcvFileAccepted](#rcvfileaccepted)
  - [RcvFileError](#rcvfileerror)
  - [RcvFileWarning](#rcvfilewarning)
  - [SndFileError](#sndfileerror)
  - [SndFileWarning](#sndfilewarning)

[Connection progress events](#connection-progress-events)
- [AcceptingContactRequest](#acceptingcontactrequest)
- [AcceptingBusinessRequest](#acceptingbusinessrequest)
- [ContactConnecting](#contactconnecting)
- [BusinessLinkConnecting](#businesslinkconnecting)
- [JoinedGroupMemberConnecting](#joinedgroupmemberconnecting)
- [SentGroupInvitation](#sentgroupinvitation)
- [GroupLinkConnecting](#grouplinkconnecting)

[Network connection events](#network-connection-events)
- [HostConnected](#hostconnected)
- [HostDisconnected](#hostdisconnected)
- [SubscriptionStatus](#subscriptionstatus)

[Error events](#error-events)
- [MessageError](#messageerror)
- [ChatError](#chaterror)
- [ChatErrors](#chaterrors)

---


## Contact connection events

Bots must use these events to process connecting users.

Most bots enable auto-accept and don't need to accept connections via commands.

You may create bot SimpleX address manually via CLI or desktop app or from bot code with these commands:
- [APIShowMyAddress](./COMMANDS.md#apishowmyaddress) to check if address exists,
- [APICreateMyAddress](./COMMANDS.md#apicreatemyaddress) to create address,
- [APISetAddressSettings](./COMMANDS.md#apisetaddresssettings) to enable auto-access.


### ContactConnected

This event is sent after a user connects via bot SimpleX address (not a business address).

**Record type**:
- type: "contactConnected"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)
- userCustomProfile: [Profile](./TYPES.md#profile)?

---


### ContactUpdated

Contact profile of another user is updated.

**Record type**:
- type: "contactUpdated"
- user: [User](./TYPES.md#user)
- fromContact: [Contact](./TYPES.md#contact)
- toContact: [Contact](./TYPES.md#contact)

---


### ContactDeletedByContact

Bot user's connection with another contact is deleted (conversation is kept).

**Record type**:
- type: "contactDeletedByContact"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### ReceivedContactRequest

Contact request received.

This event is only sent when auto-accept is disabled.

The request needs to be accepted using [APIAcceptContact](./COMMANDS.md#apiacceptcontact) command

**Record type**:
- type: "receivedContactRequest"
- user: [User](./TYPES.md#user)
- contactRequest: [UserContactRequest](./TYPES.md#usercontactrequest)
- chat_: [AChat](./TYPES.md#achat)?

---


### NewMemberContactReceivedInv

Received invitation to connect directly with a group member.

This event only needs to be processed to associate contact with group, the connection will proceed automatically.

**Record type**:
- type: "newMemberContactReceivedInv"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### ContactSndReady

Connecting via 1-time invitation or after accepting contact request.

After this event bot can send messages to this contact.

**Record type**:
- type: "contactSndReady"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


## Message events

Bots must use these events to process received messages.


### NewChatItems

Received message(s).

**Record type**:
- type: "newChatItems"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]

---


### ChatItemReaction

Received message reaction.

**Record type**:
- type: "chatItemReaction"
- user: [User](./TYPES.md#user)
- added: bool
- reaction: [ACIReaction](./TYPES.md#acireaction)

---


### ChatItemsDeleted

Message was deleted by another user.

**Record type**:
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: bool
- timed: bool

---


### ChatItemUpdated

Message was updated by another user.

**Record type**:
- type: "chatItemUpdated"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### GroupChatItemsDeleted

Group messages are deleted or moderated.

**Record type**:
- type: "groupChatItemsDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- chatItemIDs: [int64]
- byUser: bool
- member_: [GroupMember](./TYPES.md#groupmember)?

---


### ChatItemsStatusesUpdated

Message delivery status updates.

**Record type**:
- type: "chatItemsStatusesUpdated"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]

---


## Group events

Bots may use these events to manage users' groups and business address groups.

*Please note*: programming groups is more complex than programming direct connections


### ReceivedGroupInvitation

Received group invitation.

**Record type**:
- type: "receivedGroupInvitation"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- contact: [Contact](./TYPES.md#contact)
- fromMemberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

---


### UserJoinedGroup

Bot user joined group. Received when connection via group link completes.

**Record type**:
- type: "userJoinedGroup"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)

---


### GroupUpdated

Group profile or preferences updated.

**Record type**:
- type: "groupUpdated"
- user: [User](./TYPES.md#user)
- fromGroup: [GroupInfo](./TYPES.md#groupinfo)
- toGroup: [GroupInfo](./TYPES.md#groupinfo)
- member_: [GroupMember](./TYPES.md#groupmember)?

---


### JoinedGroupMember

Another member joined group.

**Record type**:
- type: "joinedGroupMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### MemberRole

Member (or bot user's) group role changed.

**Record type**:
- type: "memberRole"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- byMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)
- fromRole: [GroupMemberRole](./TYPES.md#groupmemberrole)
- toRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

---


### DeletedMember

Another member is removed from the group.

**Record type**:
- type: "deletedMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- byMember: [GroupMember](./TYPES.md#groupmember)
- deletedMember: [GroupMember](./TYPES.md#groupmember)
- withMessages: bool

---


### LeftMember

Another member left the group.

**Record type**:
- type: "leftMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### DeletedMemberUser

Bot user was removed from the group.

**Record type**:
- type: "deletedMemberUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)
- withMessages: bool

---


### GroupDeleted

Group was deleted by the owner (not bot user).

**Record type**:
- type: "groupDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### ConnectedToGroupMember

Connected to another group member.

**Record type**:
- type: "connectedToGroupMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)
- memberContact: [Contact](./TYPES.md#contact)?

---


### MemberAcceptedByOther

Another group owner, admin or moderator accepted member to the group after review ("knocking").

**Record type**:
- type: "memberAcceptedByOther"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- acceptingMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)

---


### MemberBlockedForAll

Another member blocked for all members.

**Record type**:
- type: "memberBlockedForAll"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- byMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)
- blocked: bool

---


### GroupMemberUpdated

Another group member profile updated.

**Record type**:
- type: "groupMemberUpdated"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- fromMember: [GroupMember](./TYPES.md#groupmember)
- toMember: [GroupMember](./TYPES.md#groupmember)

---


## File events

Bots that send or receive files may process these events to track delivery status and to process completion.

Bots that need to receive or moderate files (e.g., based on name, size or extension), can use relevant commands (e.g., [ReceiveFile](./COMMANDS.md#receivefile) or [APIDeleteMemberChatItem](./COMMANDS.md#apideletememberchatitem)) when processing [NewChatItems](#newchatitems) event.

Bots that need to send files should use [APISendMessages](./COMMANDS.md#apisendmessages) command.


### RcvFileDescrReady

File is ready to be received.

This event is useful for processing sender file servers and monitoring file reception progress.

[ReceiveFile](./COMMANDS.md#receivefile) command can be used before this event.

**Record type**:
- type: "rcvFileDescrReady"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)
- rcvFileDescr: [RcvFileDescr](./TYPES.md#rcvfiledescr)

---


### RcvFileComplete

File reception is competed.

**Record type**:
- type: "rcvFileComplete"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### SndFileCompleteXFTP

File upload is competed.

**Record type**:
- type: "sndFileCompleteXFTP"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)

---


### RcvFileStart

File reception started. This event will be sent after [CEvtRcvFileDescrReady](#rcvfiledescrready) event.

**Record type**:
- type: "rcvFileStart"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### RcvFileSndCancelled

File was cancelled by the sender. This event may be sent instead of [CEvtRcvFileDescrReady](#rcvfiledescrready) event.

**Record type**:
- type: "rcvFileSndCancelled"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### RcvFileAccepted

This event will be sent when file is automatically accepted because of CLI option.

**Record type**:
- type: "rcvFileAccepted"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### RcvFileError

Error receiving file.

**Record type**:
- type: "rcvFileError"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- agentError: [AgentErrorType](./TYPES.md#agenterrortype)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### RcvFileWarning

Warning when receiving file. It can happen when CLI settings do not allow to connect to file server(s).

**Record type**:
- type: "rcvFileWarning"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- agentError: [AgentErrorType](./TYPES.md#agenterrortype)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### SndFileError

Error sending file.

**Record type**:
- type: "sndFileError"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)
- errorMessage: string

---


### SndFileWarning

Warning when sending file.

**Record type**:
- type: "sndFileWarning"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)
- errorMessage: string

---


## Connection progress events

Bots may use these events to track progress of connections for monitoring or debugging.


### AcceptingContactRequest

Automatically accepting contact request via bot's SimpleX address with auto-accept enabled.

**Record type**:
- type: "acceptingContactRequest"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### AcceptingBusinessRequest

Automatically accepting contact request via bot's business address.

**Record type**:
- type: "acceptingBusinessRequest"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


### ContactConnecting

Contact confirmed connection.

Sent when contact started connecting via bot's 1-time invitation link or when bot connects to another SimpleX address.

**Record type**:
- type: "contactConnecting"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### BusinessLinkConnecting

Contact confirmed connection.

Sent when bot connects to another business address.

**Record type**:
- type: "businessLinkConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)
- fromContact: [Contact](./TYPES.md#contact)

---


### JoinedGroupMemberConnecting

Group member is announced to the group and will be connecting to bot.

**Record type**:
- type: "joinedGroupMemberConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)

---


### SentGroupInvitation

Sent when another user joins group via bot's link.

**Record type**:
- type: "sentGroupInvitation"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- contact: [Contact](./TYPES.md#contact)
- member: [GroupMember](./TYPES.md#groupmember)

---


### GroupLinkConnecting

Sent when bot joins group via another user link.

**Record type**:
- type: "groupLinkConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)

---


## Network connection events




### HostConnected

Messaging or file server connected

**Record type**:
- type: "hostConnected"
- protocol: string
- transportHost: string

---


### HostDisconnected

Messaging or file server disconnected

**Record type**:
- type: "hostDisconnected"
- protocol: string
- transportHost: string

---


### SubscriptionStatus

Messaging subscription status changed

**Record type**:
- type: "subscriptionStatus"
- server: string
- subscriptionStatus: [SubscriptionStatus](./TYPES.md#subscriptionstatus)
- connections: [string]

---


## Error events

Bots may log these events for debugging. There will be many error events - this does NOT indicate a malfunction - e.g., they may happen because of bad network connectivity, or because messages may be delivered to deleted chats for a short period of time (they will be ignored).


### MessageError

Message error.

**Record type**:
- type: "messageError"
- user: [User](./TYPES.md#user)
- severity: string
- errorMessage: string

---


### ChatError

Chat error (only used in WebSockets API).

**Record type**:
- type: "chatError"
- chatError: [ChatError](./TYPES.md#chaterror)

---


### ChatErrors

Chat errors.

**Record type**:
- type: "chatErrors"
- chatErrors: [[ChatError](./TYPES.md#chaterror)]

---
