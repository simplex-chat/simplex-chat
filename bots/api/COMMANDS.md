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
- userId: Int64

**Syntax**:

```
/_address <userId>
```

```javascript
'/_address  + userId // JavaScript
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
- userId: Int64

**Syntax**:

```
/_delete_address <userId>
```

```javascript
'/_delete_address  + userId // JavaScript
```


**Response**:

UserContactLinkDeleted: User contact address deleted.
- type: "userContactLinkDeleted"
- user: [User](./TYPES.md#user)

---


### APIShowMyAddress

Get bot address and settings.

**Parameters**:
- userId: Int64

**Syntax**:

```
/_show_address <userId>
```

```javascript
'/_show_address  + userId // JavaScript
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
- userId: Int64
- enable: Bool

**Syntax**:

```
/_profile_address <userId> on|off
```

```javascript
'/_profile_address  + userId + '  + (enable ? 'on' : 'off') // JavaScript
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
- userId: Int64
- settings: [AddressSettings](./TYPES.md#addresssettings)

**Syntax**:

```
/_address_settings <userId> <json(addressSettings)>
```

```javascript
'/_address_settings  + userId + '  + JSON.stringify(addressSettings) // JavaScript
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
- sendRef: [SendRef](./TYPES.md#sendref)
- liveMessage: Bool
- ttl: Int?
- composedMessages: [[ComposedMessage](./TYPES.md#composedmessage)]

**Syntax**:

```
/_send @<sendRef.contactId>|#<sendRef.groupId>[(_support[:sendRef.groupMemberId])] live=<liveMessage> ttl=default|<ttl> json <json(composedMessages)>
```

```javascript
'/_send  + (sendRef.contactId ? '@' + sendRef.contactId : '#' + sendRef.groupId + (sendRef.scope ? '(_support' + (n.scope.groupMemberId ? ':' + n.scope.groupMemberId : '') + ')' : '')) + ' live= + liveMessage + ' ttl= + (!ttl ? 'default : ttl) + ' json  + JSON.stringify(composedMessages) // JavaScript
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
- chatItemId: Int64
- liveMessage: Bool
- updatedMessage: [UpdatedMessage](./TYPES.md#updatedmessage)

**Responses**:

ChatItemUpdated: Message updated.
- type: "chatItemUpdated"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

ChatItemNotChanged: Message not changed.
- type: "chatItemNotChanged"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

---


### APIDeleteChatItem

Delete message.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemIds: [Int64]
- deleteMode: [CIDeleteMode](./TYPES.md#cideletemode)

**Response**:

ChatItemsDeleted: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: Bool
- timed: Bool

---


### APIDeleteMemberChatItem

Moderate message. Requires Moderator role (and higher than message author's).

**Parameters**:
- groupId: Int64
- chatItemIds: [Int64]

**Response**:

ChatItemsDeleted: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: Bool
- timed: Bool

---


### APIChatItemReaction

Add/remove message reaction.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: Int64
- add: Bool
- reaction: [MsgReaction](./TYPES.md#msgreaction)

**Response**:

ChatItemReaction: Message reaction.
- type: "chatItemReaction"
- user: [User](./TYPES.md#user)
- added: Bool
- reaction: [ACIReaction](./TYPES.md#acireaction)

---


## File commands

Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.


### ReceiveFile

Receive file.

**Parameters**:
- fileId: Int64
- userApprovedRelays: Bool
- storeEncrypted: Bool?
- fileInline: Bool?
- filePath: String?

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
- fileId: Int64

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

---


## Group commands

Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.


### APIAddMember

Add contact to group. Requires bot to have Admin role.

**Parameters**:
- groupId: Int64
- contactId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

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
- groupId: Int64
- enableNtfs: [MsgFilter](./TYPES.md#msgfilter)

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
- groupId: Int64
- groupMemberId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**:

MemberAccepted: Member accepted to group.
- type: "memberAccepted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)

---


### APIMembersRole

Set members role. Requires Admin role.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64]
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

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
- groupId: Int64
- groupMemberIds: [Int64]
- blocked: Bool

**Response**:

MembersBlockedForAllUser: Members blocked for all by admin.
- type: "membersBlockedForAllUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- blocked: Bool

---


### APIRemoveMembers

Remove members. Requires Admin role.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64]
- withMessages: Bool

**Response**:

UserDeletedMembers: Members deleted.
- type: "userDeletedMembers"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- withMessages: Bool

---


### APILeaveGroup

Leave group.

**Parameters**:
- groupId: Int64

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
- groupId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

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
- groupId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

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
- groupId: Int64

**Response**:

GroupLinkDeleted: Group link deleted.
- type: "groupLinkDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)

---


### APIGetGroupLink

Get group link.

**Parameters**:
- groupId: Int64

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
- userId: Int64
- incognito: Bool

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
- userId: Int64
- connectionLink: String?

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
- userId: Int64
- incognito: Bool
- connLink_: [CreatedConnLink](./TYPES.md#createdconnlink)?

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
- incognito: Bool
- contactReqId: Int64

**Response**:

AcceptingContactRequest: Contact request accepted.
- type: "acceptingContactRequest"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

---


### APIRejectContact

Reject contact request. The user who sent the request is **not notified**.

**Parameters**:
- contactReqId: Int64

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
- userId: Int64

**Response**:

ContactsList: Contacts.
- type: "contactsList"
- user: [User](./TYPES.md#user)
- contacts: [[Contact](./TYPES.md#contact)]

---


### APIListGroups

Get groups.

**Parameters**:
- userId: Int64
- contactId_: Int64?
- search: String?

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
'/_create user  + JSON.stringify(newUser) // JavaScript
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
- userId: Int64
- viewPwd: String?

**Syntax**:

```
/_user <userId>[ <json(viewPwd)>]
```

```javascript
'/_user  + userId + (!viewPwd || viewPwd == '' ? '' : '  + JSON.stringify(viewPwd)) // JavaScript
```


**Response**:

ActiveUser: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)

---


### APIDeleteUser

Delete user profile.

**Parameters**:
- userId: Int64
- delSMPQueues: Bool
- viewPwd: String?

**Response**:

CmdOk: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?

---


### APIUpdateProfile

Update user profile.

**Parameters**:
- userId: Int64
- profile: [Profile](./TYPES.md#profile)

**Response**:

UserProfileUpdated: User profile updated.
- type: "userProfileUpdated"
- user: [User](./TYPES.md#user)
- fromProfile: [Profile](./TYPES.md#profile)
- toProfile: [Profile](./TYPES.md#profile)
- updateSummary: [UserProfileUpdateSummary](./TYPES.md#userprofileupdatesummary)

---
