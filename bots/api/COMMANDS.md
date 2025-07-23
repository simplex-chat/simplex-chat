# API Commands and Responses

This file is generated automatically.

[Address commands](#address-commands)
- [APICreateMyAddress](#apicreatemyaddress)
- [APIDeleteMyAddress](#apideletemyaddress)
- [APIShowMyAddress](#apishowmyaddress)
- [APISetProfileAddress](#apisetprofileaddress)
- [APISetAddressSettings](#apisetaddresssettings)

[Message commands](#message-commands)
- [APISendMessages](#apisendmessages)
- [APIUpdateChatItem](#apiupdatechatitem)
- [APIDeleteChatItem](#apideletechatitem)
- [APIDeleteMemberChatItem](#apideletememberchatitem)
- [APIChatItemReaction](#apichatitemreaction)

[File commands](#file-commands)
- [ReceiveFile](#receivefile)
- [CancelFile](#cancelfile)

[Group commands](#group-commands)
- [APIAddMember](#apiaddmember)
- [APIJoinGroup](#apijoingroup)
- [APIAcceptMember](#apiacceptmember)
- [APIMembersRole](#apimembersrole)
- [APIBlockMembersForAll](#apiblockmembersforall)
- [APIRemoveMembers](#apiremovemembers)
- [APILeaveGroup](#apileavegroup)

[Group link commands](#group-link-commands)
- [APICreateGroupLink](#apicreategrouplink)
- [APIGroupLinkMemberRole](#apigrouplinkmemberrole)
- [APIDeleteGroupLink](#apideletegrouplink)
- [APIGetGroupLink](#apigetgrouplink)

[Connection commands](#connection-commands)
- [APIAddContact](#apiaddcontact)
- [APIConnectPlan](#apiconnectplan)
- [APIConnect](#apiconnect)
- [APIAcceptContact](#apiacceptcontact)
- [APIRejectContact](#apirejectcontact)

[Chat commands](#chat-commands)
- [APIListContacts](#apilistcontacts)
- [APIListGroups](#apilistgroups)
- [APIDeleteChat](#apideletechat)

[User profile commands](#user-profile-commands)
- [ShowActiveUser](#showactiveuser)
- [CreateActiveUser](#createactiveuser)
- [ListUsers](#listusers)
- [APISetActiveUser](#apisetactiveuser)
- [APIDeleteUser](#apideleteuser)
- [APIUpdateProfile](#apiupdateprofile)

---


## Address commands

Bots can use these commands to automatically check and create address when initialized


### APICreateMyAddress

Create bot address.

**Parameters**:
- userId: int64

**Syntax**:

```
/_address <userId>
```

```javascript
'/_address ' + userId // JavaScript
```

**Response**:

UserContactLinkCreated: User contact address created.
- type: "userContactLinkCreated"
- user: [User](./TYPES.md#user)
- connLinkContact: [CreatedConnLink](./TYPES.md#createdconnlink)

---


### APIDeleteMyAddress

Delete bot address.

**Parameters**:
- userId: int64

**Syntax**:

```
/_delete_address <userId>
```

```javascript
'/_delete_address ' + userId // JavaScript
```

**Response**:

UserContactLinkDeleted: User contact address deleted.
- type: "userContactLinkDeleted"
- user: [User](./TYPES.md#user)

---


### APIShowMyAddress

Get bot address and settings.

**Parameters**:
- userId: int64

**Syntax**:

```
/_show_address <userId>
```

```javascript
'/_show_address ' + userId // JavaScript
```

**Response**:

UserContactLink: User contact address.
- type: "userContactLink"
- user: [User](./TYPES.md#user)
- contactLink: [UserContactLink](./TYPES.md#usercontactlink)

---


### APISetProfileAddress

Add address to bot profile.

**Parameters**:
- userId: int64
- enable: bool

**Syntax**:

```
/_profile_address <userId> on|off
```

```javascript
'/_profile_address ' + userId + ' ' + (enable ? 'on' : 'off') // JavaScript
```

**Response**:

UserProfileUpdated: User profile updated.
- type: "userProfileUpdated"
- user: [User](./TYPES.md#user)
- fromProfile: [Profile](./TYPES.md#profile)
- toProfile: [Profile](./TYPES.md#profile)
- updateSummary: [UserProfileUpdateSummary](./TYPES.md#userprofileupdatesummary)

---


### APISetAddressSettings

Set bot address settings.

**Parameters**:
- userId: int64
- settings: [AddressSettings](./TYPES.md#addresssettings)

**Syntax**:

```
/_address_settings <userId> <json(settings)>
```

```javascript
'/_address_settings ' + userId + ' ' + JSON.stringify(settings) // JavaScript
```

**Response**:

UserContactLinkUpdated: User contact address updated.
- type: "userContactLinkUpdated"
- user: [User](./TYPES.md#user)
- contactLink: [UserContactLink](./TYPES.md#usercontactlink)

---


## Message commands

Commands to send, update, delete, moderate messages and set message reactions


### APISendMessages

Send messages.

**Parameters**:
- sendRef: [ChatRef](./TYPES.md#chatref)
- liveMessage: bool
- ttl: int?
- composedMessages: [[ComposedMessage](./TYPES.md#composedmessage)]

**Syntax**:

```
/_send <str(sendRef)>[ live=on][ ttl=<ttl>] json <json(composedMessages)>
```

```javascript
'/_send ' + sendRef.toString() + (liveMessage ? ' live=on' : '') + (ttl ? ' ttl=' + ttl : '') + ' json ' + JSON.stringify(composedMessages) // JavaScript
```

**Response**:

NewChatItems: New messages.
- type: "newChatItems"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]

---


### APIUpdateChatItem

Update message.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: int64
- liveMessage: bool
- updatedMessage: [UpdatedMessage](./TYPES.md#updatedmessage)

**Syntax**:

```
/_update item <str(chatRef)> <chatItemId>[ live=on] json <json(updatedMessage)>
```

```javascript
'/_update item ' + chatRef.toString() + ' ' + chatItemId + (liveMessage ? ' live=on' : '') + ' json ' + JSON.stringify(updatedMessage) // JavaScript
```

**Responses**:

ChatItemUpdated: Message updated.
- type: "chatItemUpdated"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

ChatItemNotChanged: Message not changed.
- type: "chatItemNotChanged"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

ChatCmdError: Command error.
- type: "chatCmdError"
- chatError: [ChatError](./TYPES.md#chaterror)

---


### APIDeleteChatItem

Delete message.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemIds: [int64]
- deleteMode: [CIDeleteMode](./TYPES.md#cideletemode)

**Syntax**:

```
/_delete item <str(chatRef)> <chatItemIds[0]>[,<chatItemIds[1]>...] broadcast|internal|internalMark
```

```javascript
'/_delete item ' + chatRef.toString() + ' ' + chatItemIds.join(',') + ' ' + deleteMode // JavaScript
```

**Response**:

ChatItemsDeleted: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: bool
- timed: bool

---


### APIDeleteMemberChatItem

Moderate message. Requires Moderator role (and higher than message author's).

**Parameters**:
- groupId: int64
- chatItemIds: [int64]

**Syntax**:

```
/_delete member item #<groupId> <chatItemIds[0]>[,<chatItemIds[1]>...]
```

```javascript
'/_delete member item #' + groupId + ' ' + chatItemIds.join(',') // JavaScript
```

**Response**:

ChatItemsDeleted: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: bool
- timed: bool

---


### APIChatItemReaction

Add/remove message reaction.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: int64
- add: bool
- reaction: [MsgReaction](./TYPES.md#msgreaction)

**Syntax**:

```
/_reaction <str(chatRef)> <chatItemId> on|off <json(reaction)>
```

```javascript
'/_reaction ' + chatRef.toString() + ' ' + chatItemId + ' ' + (add ? 'on' : 'off') + ' ' + JSON.stringify(reaction) // JavaScript
```

**Response**:

ChatItemReaction: Message reaction.
- type: "chatItemReaction"
- user: [User](./TYPES.md#user)
- added: bool
- reaction: [ACIReaction](./TYPES.md#acireaction)

---


## File commands

Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.


### ReceiveFile

Receive file.

**Parameters**:
- fileId: int64
- userApprovedRelays: bool
- storeEncrypted: bool?
- fileInline: bool?
- filePath: string?

**Syntax**:

```
/freceive <fileId>[ approved_relays=on][ encrypt=on|off][ inline=on|off][ <filePath>]
```

```javascript
'/freceive ' + fileId + (userApprovedRelays ? ' approved_relays=on' : '') + (typeof storeEncrypted == 'boolean' ? ' encrypt=' + (storeEncrypted ? 'on' : 'off') : '') + (typeof fileInline == 'boolean' ? ' inline=' + (fileInline ? 'on' : 'off') : '') + (filePath ? ' ' + filePath : '') // JavaScript
```

**Responses**:

RcvFileAccepted: File accepted to be received.
- type: "rcvFileAccepted"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

RcvFileAcceptedSndCancelled: File accepted, but no longer sent.
- type: "rcvFileAcceptedSndCancelled"
- user: [User](./TYPES.md#user)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

---


### CancelFile

Cancel file.

**Parameters**:
- fileId: int64

**Syntax**:

```
/fcancel <fileId>
```

```javascript
'/fcancel ' + fileId // JavaScript
```

**Responses**:

SndFileCancelled: Cancelled sending file.
- type: "sndFileCancelled"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)
- sndFileTransfers: [[SndFileTransfer](./TYPES.md#sndfiletransfer)]

RcvFileCancelled: Cancelled receiving file.
- type: "rcvFileCancelled"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

ChatCmdError: Command error.
- type: "chatCmdError"
- chatError: [ChatError](./TYPES.md#chaterror)

---


## Group commands

Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.


### APIAddMember

Add contact to group. Requires bot to have Admin role.

**Parameters**:
- groupId: int64
- contactId: int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Syntax**:

```
/_add #<groupId> <contactId> observer|author|member|moderator|admin|owner
```

```javascript
'/_add #' + groupId + ' ' + contactId + ' ' + memberRole // JavaScript
```

**Response**:

SentGroupInvitation: Group invitation sent.
- type: "sentGroupInvitation"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- contact: [Contact](./TYPES.md#contact)
- member: [GroupMember](./TYPES.md#groupmember)

---


### APIJoinGroup

Join group.

**Parameters**:
- groupId: int64
- enableNtfs: [MsgFilter](./TYPES.md#msgfilter)

**Syntax**:

```
/_join #<groupId>
```

```javascript
'/_join #' + groupId // JavaScript
```

**Response**:

UserAcceptedGroupSent: User accepted group invitation.
- type: "userAcceptedGroupSent"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostContact: [Contact](./TYPES.md#contact)?

---


### APIAcceptMember

Accept group member. Requires Admin role.

**Parameters**:
- groupId: int64
- groupMemberId: int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Syntax**:

```
/_accept member #<groupId> <groupMemberId> observer|author|member|moderator|admin|owner
```

```javascript
'/_accept member #' + groupId + ' ' + groupMemberId + ' ' + memberRole // JavaScript
```

**Responses**:

MemberAccepted: Member accepted to group.
- type: "memberAccepted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

ChatCmdError: Command error.
- type: "chatCmdError"
- chatError: [ChatError](./TYPES.md#chaterror)

---


### APIMembersRole

Set members role. Requires Admin role.

**Parameters**:
- groupId: int64
- groupMemberIds: [int64]
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Syntax**:

```
/_member role #<groupId> <groupMemberIds[0]>[,<groupMemberIds[1]>...] observer|author|member|moderator|admin|owner
```

```javascript
'/_member role #' + groupId + ' ' + groupMemberIds.join(',') + ' ' + memberRole // JavaScript
```

**Response**:

MembersRoleUser: Members role changed by user.
- type: "membersRoleUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- toRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

---


### APIBlockMembersForAll

Block members. Requires Moderator role.

**Parameters**:
- groupId: int64
- groupMemberIds: [int64]
- blocked: bool

**Syntax**:

```
/_block #<groupId> <groupMemberIds[0]>[,<groupMemberIds[1]>...] blocked=on|off
```

```javascript
'/_block #' + groupId + ' ' + groupMemberIds.join(',') + ' blocked=' + (blocked ? 'on' : 'off') // JavaScript
```

**Response**:

MembersBlockedForAllUser: Members blocked for all by admin.
- type: "membersBlockedForAllUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- blocked: bool

---


### APIRemoveMembers

Remove members. Requires Admin role.

**Parameters**:
- groupId: int64
- groupMemberIds: [int64]
- withMessages: bool

**Syntax**:

```
/_remove #<groupId> <groupMemberIds[0]>[,<groupMemberIds[1]>...][ messages=on]
```

```javascript
'/_remove #' + groupId + ' ' + groupMemberIds.join(',') + (withMessages ? ' messages=on' : '') // JavaScript
```

**Responses**:

UserDeletedMembers: Members deleted.
- type: "userDeletedMembers"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- withMessages: bool

ChatCmdError: Command error.
- type: "chatCmdError"
- chatError: [ChatError](./TYPES.md#chaterror)

---


### APILeaveGroup

Leave group.

**Parameters**:
- groupId: int64

**Syntax**:

```
/_leave #<groupId>
```

```javascript
'/_leave #' + groupId // JavaScript
```

**Response**:

LeftMemberUser: User left group.
- type: "leftMemberUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


## Group link commands

These commands can be used by bots that manage multiple public groups


### APICreateGroupLink

Create group link.

**Parameters**:
- groupId: int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Syntax**:

```
/_create link #<groupId> observer|author|member|moderator|admin|owner
```

```javascript
'/_create link #' + groupId + ' ' + memberRole // JavaScript
```

**Response**:

GroupLinkCreated: Group link created.
- type: "groupLinkCreated"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)

---


### APIGroupLinkMemberRole

Set member role for group link.

**Parameters**:
- groupId: int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Syntax**:

```
/_set link role #<groupId> observer|author|member|moderator|admin|owner
```

```javascript
'/_set link role #' + groupId + ' ' + memberRole // JavaScript
```

**Response**:

GroupLink: Group link.
- type: "groupLink"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)

---


### APIDeleteGroupLink

Delete group link.

**Parameters**:
- groupId: int64

**Syntax**:

```
/_delete link #<groupId>
```

```javascript
'/_delete link #' + groupId // JavaScript
```

**Response**:

GroupLinkDeleted: Group link deleted.
- type: "groupLinkDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


### APIGetGroupLink

Get group link.

**Parameters**:
- groupId: int64

**Syntax**:

```
/_get link #<groupId>
```

```javascript
'/_get link #' + groupId // JavaScript
```

**Response**:

GroupLink: Group link.
- type: "groupLink"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)

---


## Connection commands

These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.


### APIAddContact

Create 1-time invitation link.

**Parameters**:
- userId: int64
- incognito: bool

**Syntax**:

```
/_connect <userId>[ incognito=on]
```

```javascript
'/_connect ' + userId + (incognito ? ' incognito=on' : '') // JavaScript
```

**Response**:

Invitation: One-time invitation.
- type: "invitation"
- user: [User](./TYPES.md#user)
- connLinkInvitation: [CreatedConnLink](./TYPES.md#createdconnlink)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)

---


### APIConnectPlan

Determine SimpleX link type and if the bot is already connected via this link.

**Parameters**:
- userId: int64
- connectionLink: string?

**Syntax**:

```
/_connect plan <userId> <connectionLink>
```

```javascript
'/_connect plan ' + userId + ' ' + connectionLink // JavaScript
```

**Response**:

ConnectionPlan: Connection link information.
- type: "connectionPlan"
- user: [User](./TYPES.md#user)
- connLink: [CreatedConnLink](./TYPES.md#createdconnlink)
- connectionPlan: [ConnectionPlan](./TYPES.md#connectionplan)

---


### APIConnect

Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link

**Parameters**:
- userId: int64
- incognito: bool
- connLink_: [CreatedConnLink](./TYPES.md#createdconnlink)?

**Syntax**:

```
/_connect <userId> <str(connLink_)>
```

```javascript
'/_connect ' + userId + ' ' + connLink_.toString() // JavaScript
```

**Responses**:

SentConfirmation: Confirmation sent to one-time invitation.
- type: "sentConfirmation"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)
- customUserProfile: [Profile](./TYPES.md#profile)?

ContactAlreadyExists: Contact already exists.
- type: "contactAlreadyExists"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

SentInvitation: Invitation sent to contact address.
- type: "sentInvitation"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)
- customUserProfile: [Profile](./TYPES.md#profile)?

---


### APIAcceptContact

Accept contact request.

**Parameters**:
- incognito: bool
- contactReqId: int64

**Syntax**:

```
/_accept <contactReqId>
```

```javascript
'/_accept ' + contactReqId // JavaScript
```

**Response**:

AcceptingContactRequest: Contact request accepted.
- type: "acceptingContactRequest"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### APIRejectContact

Reject contact request. The user who sent the request is **not notified**.

**Parameters**:
- contactReqId: int64

**Syntax**:

```
/_reject <contactReqId>
```

```javascript
'/_reject ' + contactReqId // JavaScript
```

**Response**:

ContactRequestRejected: Contact request rejected.
- type: "contactRequestRejected"
- user: [User](./TYPES.md#user)
- contactRequest: [UserContactRequest](./TYPES.md#usercontactrequest)
- contact_: [Contact](./TYPES.md#contact)?

---


## Chat commands

Commands to list and delete coversations.


### APIListContacts

Get contacts.

**Parameters**:
- userId: int64

**Syntax**:

```
/_contacts <userId>
```

```javascript
'/_contacts ' + userId // JavaScript
```

**Response**:

ContactsList: Contacts.
- type: "contactsList"
- user: [User](./TYPES.md#user)
- contacts: [[Contact](./TYPES.md#contact)]

---


### APIListGroups

Get groups.

**Parameters**:
- userId: int64
- contactId_: int64?
- search: string?

**Syntax**:

```
/_groups <userId>[ @<contactId_>][ <search>]
```

```javascript
'/_groups ' + userId + (contactId_ ? ' @' + contactId_ : '') + (search ? ' ' + search : '') // JavaScript
```

**Response**:

GroupsList: Groups.
- type: "groupsList"
- user: [User](./TYPES.md#user)
- groups: [[GroupInfoSummary](./TYPES.md#groupinfosummary)]

---


### APIDeleteChat

Delete chat.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatDeleteMode: [ChatDeleteMode](./TYPES.md#chatdeletemode)

**Syntax**:

```
/_delete <str(chatRef)> <str(chatDeleteMode)>
```

```javascript
'/_delete ' + chatRef.toString() + ' ' + chatDeleteMode.toString() // JavaScript
```

**Responses**:

ContactDeleted: Contact deleted.
- type: "contactDeleted"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

ContactConnectionDeleted: Connection deleted.
- type: "contactConnectionDeleted"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)

GroupDeletedUser: User deleted group.
- type: "groupDeletedUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


## User profile commands

Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).


### ShowActiveUser

Get active user profile

**Syntax**:

```
/user
```

**Response**:

ActiveUser: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)

---


### CreateActiveUser

Create new user profile

**Parameters**:
- newUser: [NewUser](./TYPES.md#newuser)

**Syntax**:

```
/_create user <json(newUser)>
```

```javascript
'/_create user ' + JSON.stringify(newUser) // JavaScript
```

**Response**:

ActiveUser: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)

---


### ListUsers

Get all user profiles

**Syntax**:

```
/users
```

**Response**:

UsersList: Users.
- type: "usersList"
- users: [[UserInfo](./TYPES.md#userinfo)]

---


### APISetActiveUser

Set active user profile

**Parameters**:
- userId: int64
- viewPwd: string?

**Syntax**:

```
/_user <userId>[ <json(viewPwd)>]
```

```javascript
'/_user ' + userId + (viewPwd ? ' ' + JSON.stringify(viewPwd) : '') // JavaScript
```

**Response**:

ActiveUser: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)

---


### APIDeleteUser

Delete user profile.

**Parameters**:
- userId: int64
- delSMPQueues: bool
- viewPwd: string?

**Syntax**:

```
/_delete user <userId> del_smp=on|off[ <json(viewPwd)>]
```

```javascript
'/_delete user ' + userId + ' del_smp=' + (delSMPQueues ? 'on' : 'off') + (viewPwd ? ' ' + JSON.stringify(viewPwd) : '') // JavaScript
```

**Response**:

CmdOk: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?

---


### APIUpdateProfile

Update user profile.

**Parameters**:
- userId: int64
- profile: [Profile](./TYPES.md#profile)

**Syntax**:

```
/_profile <userId> <json(profile)>
```

```javascript
'/_profile ' + userId + ' ' + JSON.stringify(profile) // JavaScript
```

**Response**:

UserProfileUpdated: User profile updated.
- type: "userProfileUpdated"
- user: [User](./TYPES.md#user)
- fromProfile: [Profile](./TYPES.md#profile)
- toProfile: [Profile](./TYPES.md#profile)
- updateSummary: [UserProfileUpdateSummary](./TYPES.md#userprofileupdatesummary)

---
