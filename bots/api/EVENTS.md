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

[Error events](#error-events)
- [MessageError](#messageerror)
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

Contact updated.

**Record type**:
- type: "contactUpdated"
- user: [User](./TYPES.md#user)
- fromContact: [Contact](./TYPES.md#contact)
- toContact: [Contact](./TYPES.md#contact)

---


### ContactDeletedByContact

Contact deleted by contact.

**Record type**:
- type: "contactDeletedByContact"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### ReceivedContactRequest

received - needs to be accepted

**Record type**:
- type: "receivedContactRequest"
- user: [User](./TYPES.md#user)
- contactRequest: [UserContactRequest](./TYPES.md#usercontactrequest)
- chat_: [AChat](./TYPES.md#achat)?

---


### NewMemberContactReceivedInv

only needs to be processed to associate contact with group

**Record type**:
- type: "newMemberContactReceivedInv"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### ContactSndReady

Contact snd ready.

**Record type**:
- type: "contactSndReady"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


## Message events

Bots must use these events to process received messages.


### NewChatItems

New chat items.

**Record type**:
- type: "newChatItems"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]

---


### ChatItemReaction

Chat item reaction.

**Record type**:
- type: "chatItemReaction"
- user: [User](./TYPES.md#user)
- added: bool
- reaction: [ACIReaction](./TYPES.md#acireaction)

---


### ChatItemsDeleted

Chat items deleted.

**Record type**:
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: bool
- timed: bool

---


### ChatItemUpdated

Chat item updated.

**Record type**:
- type: "chatItemUpdated"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### GroupChatItemsDeleted

Group chat items deleted.

**Record type**:
- type: "groupChatItemsDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- chatItemIDs: [int64]
- byUser: bool
- member_: [GroupMember](./TYPES.md#groupmember)?

---


### ChatItemsStatusesUpdated

Chat items statuses updated.

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

User joined group.

**Record type**:
- type: "userJoinedGroup"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)

---


### GroupUpdated

Group updated.

**Record type**:
- type: "groupUpdated"
- user: [User](./TYPES.md#user)
- fromGroup: [GroupInfo](./TYPES.md#groupinfo)
- toGroup: [GroupInfo](./TYPES.md#groupinfo)
- member_: [GroupMember](./TYPES.md#groupmember)?

---


### JoinedGroupMember

Joined group member.

**Record type**:
- type: "joinedGroupMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### MemberRole

Member role.

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

Deleted member.

**Record type**:
- type: "deletedMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- byMember: [GroupMember](./TYPES.md#groupmember)
- deletedMember: [GroupMember](./TYPES.md#groupmember)
- withMessages: bool

---


### LeftMember

Left member.

**Record type**:
- type: "leftMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### DeletedMemberUser

Deleted member user.

**Record type**:
- type: "deletedMemberUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)
- withMessages: bool

---


### GroupDeleted

Group deleted.

**Record type**:
- type: "groupDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### ConnectedToGroupMember

Connected to group member.

**Record type**:
- type: "connectedToGroupMember"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)
- memberContact: [Contact](./TYPES.md#contact)?

---


### MemberAcceptedByOther

Member accepted by other.

**Record type**:
- type: "memberAcceptedByOther"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- acceptingMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)

---


### MemberBlockedForAll

Member blocked for all.

**Record type**:
- type: "memberBlockedForAll"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- byMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)
- blocked: bool

---


### GroupMemberUpdated

Group member updated.

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

Rcv file descr ready.

**Record type**:
- type: "rcvFileDescrReady"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)
- rcvFileDescr: [RcvFileDescr](./TYPES.md#rcvfiledescr)

---


### RcvFileComplete

Rcv file complete.

**Record type**:
- type: "rcvFileComplete"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### SndFileCompleteXFTP

Snd file complete x f t p.

**Record type**:
- type: "sndFileCompleteXFTP"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)

---


### RcvFileStart

file reception started (happens when FD is received)

**Record type**:
- type: "rcvFileStart"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### RcvFileSndCancelled

sender cancelled sending file

**Record type**:
- type: "rcvFileSndCancelled"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### RcvFileAccepted

when file auto-accepted - not recommended

**Record type**:
- type: "rcvFileAccepted"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### RcvFileError

Rcv file error.

**Record type**:
- type: "rcvFileError"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- agentError: [AgentErrorType](./TYPES.md#agenterrortype)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### RcvFileWarning

Rcv file warning.

**Record type**:
- type: "rcvFileWarning"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- agentError: [AgentErrorType](./TYPES.md#agenterrortype)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### SndFileError

Snd file error.

**Record type**:
- type: "sndFileError"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)
- errorMessage: string

---


### SndFileWarning

Snd file warning.

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

Accepting contact request.

**Record type**:
- type: "acceptingContactRequest"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### AcceptingBusinessRequest

Accepting business request.

**Record type**:
- type: "acceptingBusinessRequest"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


### ContactConnecting

Contact connecting.

**Record type**:
- type: "contactConnecting"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### BusinessLinkConnecting

Business link connecting.

**Record type**:
- type: "businessLinkConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)
- fromContact: [Contact](./TYPES.md#contact)

---


### JoinedGroupMemberConnecting

Joined group member connecting.

**Record type**:
- type: "joinedGroupMemberConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)
- member: [GroupMember](./TYPES.md#groupmember)

---


### SentGroupInvitation

Sent group invitation.

**Record type**:
- type: "sentGroupInvitation"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- contact: [Contact](./TYPES.md#contact)
- member: [GroupMember](./TYPES.md#groupmember)

---


### GroupLinkConnecting

Group link connecting.

**Record type**:
- type: "groupLinkConnecting"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostMember: [GroupMember](./TYPES.md#groupmember)

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


### ChatErrors

Chat errors.

**Record type**:
- type: "chatErrors"
- chatErrors: [[ChatError](./TYPES.md#chaterror)]

---
