# API Commands and Responses

This file is generated automatically.

## Command categories

- [Address commands](#address-commands)
- [Message commands](#message-commands)
- [File commands](#file-commands)
- [Group commands](#group-commands)
- [Group link commands](#group-link-commands)
- [Connection commands](#connection-commands)
- [User profile commands](#user-profile-commands)
- [Chat commands](#chat-commands)


## Address commands

Bots can use these commands to automatically check and create address when initialized


### APICreateMyAddress

Create bot address.

**Parameters**:
- userId: Int64

**Response**: User contact address created.
- type: "userContactLinkCreated"
- user: [User](./TYPES.md#user)
- connLinkContact: [CreatedConnLink](./TYPES.md#createdconnlink)


### APIDeleteMyAddress

Delete bot address.

**Parameters**:
- userId: Int64

**Response**: User contact address deleted.
- type: "userContactLinkDeleted"
- user: [User](./TYPES.md#user)


### APIShowMyAddress

Get bot address and settings.

**Parameters**:
- userId: Int64

**Response**: User contact address.
- type: "userContactLink"
- user: [User](./TYPES.md#user)
- contactLink: [UserContactLink](./TYPES.md#usercontactlink)


### APISetProfileAddress

Add address to bot profile.

**Parameters**:
- userId: Int64
- enable: Bool

**Response**: User profile updated.
- type: "userProfileUpdated"
- user: [User](./TYPES.md#user)
- fromProfile: [Profile](./TYPES.md#profile)
- toProfile: [Profile](./TYPES.md#profile)
- updateSummary: [UserProfileUpdateSummary](./TYPES.md#userprofileupdatesummary)


### APISetAddressSettings

Set bot address settings.

**Parameters**:
- userId: Int64
- settings: [AddressSettings](./TYPES.md#addresssettings)

**Response**: User contact address updated.
- type: "userContactLinkUpdated"
- user: [User](./TYPES.md#user)
- contactLink: [UserContactLink](./TYPES.md#usercontactlink)

## Message commands

Commands to send, update, delete, moderate messages and set message reactions


### APISendMessages

Send messages.

**Parameters**:
- sendRef: [SendRef](./TYPES.md#sendref)
- liveMessage: Bool
- ttl: Int?
- composedMessages: [[ComposedMessage](./TYPES.md#composedmessage)]

**Response**: New messages.
- type: "newChatItems"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]


### APIReportMessage

Report message.

**Parameters**:
- groupId: Int64
- chatItemId: Int64
- reportReason: [ReportReason](./TYPES.md#reportreason)
- reportText: String

**Response**: New messages.
- type: "newChatItems"
- user: [User](./TYPES.md#user)
- chatItems: [[AChatItem](./TYPES.md#achatitem)]


### APIUpdateChatItem

Update message.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: Int64
- liveMessage: Bool
- updatedMessage: [UpdatedMessage](./TYPES.md#updatedmessage)

**Responses**:

Message updated.
- type: "chatItemUpdated"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

Message not changed.
- type: "chatItemNotChanged"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)


### APIDeleteChatItem

Delete message.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemIds: [Int64]
- deleteMode: [CIDeleteMode](./TYPES.md#cideletemode)

**Response**: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: Bool
- timed: Bool


### APIDeleteMemberChatItem

Moderate message.

**Parameters**:
- groupId: Int64
- chatItemIds: [Int64]

**Response**: Messages deleted.
- type: "chatItemsDeleted"
- user: [User](./TYPES.md#user)
- chatItemDeletions: [[ChatItemDeletion](./TYPES.md#chatitemdeletion)]
- byUser: Bool
- timed: Bool


### APIChatItemReaction

Add/remove message reaction.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: Int64
- add: Bool
- reaction: [MsgReaction](./TYPES.md#msgreaction)

**Response**: Message reaction.
- type: "chatItemReaction"
- user: [User](./TYPES.md#user)
- added: Bool
- reaction: [ACIReaction](./TYPES.md#acireaction)


### APIGetReactionMembers

Get reaction members.

**Parameters**:
- userId: Int64
- groupId: Int64
- chatItemId: Int64
- reaction: [MsgReaction](./TYPES.md#msgreaction)

**Response**: Members who set reaction on the message.
- type: "reactionMembers"
- user: [User](./TYPES.md#user)
- memberReactions: [[MemberReaction](./TYPES.md#memberreaction)]

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

File accepted to be received.
- type: "rcvFileAccepted"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)

File accepted, but no longer sent.
- type: "rcvFileAcceptedSndCancelled"
- user: [User](./TYPES.md#user)
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)


### CancelFile

Cancel file.

**Parameters**:
- fileId: Int64

**Responses**:

Cancelled sending file.
- type: "sndFileCancelled"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- fileTransferMeta: [FileTransferMeta](./TYPES.md#filetransfermeta)
- sndFileTransfers: [[SndFileTransfer](./TYPES.md#sndfiletransfer)]

Cancelled receiving file.
- type: "rcvFileCancelled"
- user: [User](./TYPES.md#user)
- chatItem_: [AChatItem](./TYPES.md#achatitem)?
- rcvFileTransfer: [RcvFileTransfer](./TYPES.md#rcvfiletransfer)

## Group commands

Commands to create and manage groups. These commands have to be used to manage business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.


### APINewGroup

Create group.

**Parameters**:
- userId: Int64
- incognito: Bool
- groupProfile: [GroupProfile](./TYPES.md#groupprofile)

**Response**: Group created.
- type: "groupCreated"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)


### APIAddMember

Add contact to group.

**Parameters**:
- groupId: Int64
- contactId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**: Group invitation sent.
- type: "sentGroupInvitation"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- contact: [Contact](./TYPES.md#contact)
- member: [GroupMember](./TYPES.md#groupmember)


### APIJoinGroup

Join group.

**Parameters**:
- groupId: Int64
- enableNtfs: [MsgFilter](./TYPES.md#msgfilter)

**Response**: User accepted group invitation.
- type: "userAcceptedGroupSent"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- hostContact: [Contact](./TYPES.md#contact)?


### APIAcceptMember

Accept group member.

**Parameters**:
- groupId: Int64
- groupMemberId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**: Member accepted to group.
- type: "memberAccepted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)


### APIMembersRole

Set members role.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64]
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**: Members role changed by user.
- type: "membersRoleUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- toRole: [GroupMemberRole](./TYPES.md#groupmemberrole)


### APIBlockMembersForAll

Block members.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64]
- blocked: Bool

**Response**: Members blocked for all by admin.
- type: "membersBlockedForAllUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- blocked: Bool


### APIRemoveMembers

Remove members.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64]
- withMessages: Bool

**Response**: Members deleted.
- type: "userDeletedMembers"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- members: [[GroupMember](./TYPES.md#groupmember)]
- withMessages: Bool


### APILeaveGroup

Leave group.

**Parameters**:
- groupId: Int64

**Response**: User left group.
- type: "leftMemberUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)


### APIListMembers

Get group members.

**Parameters**:
- groupId: Int64

**Response**: Group members.
- type: "groupMembers"
- user: [User](./TYPES.md#user)
- group: [Group](./TYPES.md#group)


### APIUpdateGroupProfile

Update group profile.

**Parameters**:
- groupId: Int64
- groupProfile: [GroupProfile](./TYPES.md#groupprofile)

**Response**: Group updated.
- type: "groupUpdated"
- user: [User](./TYPES.md#user)
- fromGroup: [GroupInfo](./TYPES.md#groupinfo)
- toGroup: [GroupInfo](./TYPES.md#groupinfo)
- member_: [GroupMember](./TYPES.md#groupmember)?

## Group link commands

These commands can be used by bots that manage multiple public groups


### APICreateGroupLink

Create group link.

**Parameters**:
- groupId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**: Group link created.
- type: "groupLinkCreated"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)


### APIGroupLinkMemberRole

Set member role for group link.

**Parameters**:
- groupId: Int64
- memberRole: [GroupMemberRole](./TYPES.md#groupmemberrole)

**Response**: Group link.
- type: "groupLink"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)


### APIDeleteGroupLink

Delete group link.

**Parameters**:
- groupId: Int64

**Response**: Group link deleted.
- type: "groupLinkDeleted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)


### APIGetGroupLink

Get group link.

**Parameters**:
- groupId: Int64

**Response**: Group link.
- type: "groupLink"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- groupLink: [GroupLink](./TYPES.md#grouplink)

## Connection commands

These commands may be used to establish connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.


### APIAddContact

Create 1-time invitation link.

**Parameters**:
- userId: Int64
- incognito: Bool

**Response**: One-time invitation.
- type: "invitation"
- user: [User](./TYPES.md#user)
- connLinkInvitation: [CreatedConnLink](./TYPES.md#createdconnlink)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)


### APIConnectPlan

Determine SimpleX link type and if the bot is already connected via this link.

**Parameters**:
- userId: Int64
- connectionLink: String?

**Response**: Connection link information.
- type: "connectionPlan"
- user: [User](./TYPES.md#user)
- connLink: [ACreatedConnLink](./TYPES.md#acreatedconnlink)
- connectionPlan: [ConnectionPlan](./TYPES.md#connectionplan)


### APIConnect

Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link

**Parameters**:
- userId: Int64
- incognito: Bool
- connLink_: [ACreatedConnLink](./TYPES.md#acreatedconnlink)?

**Responses**:

Confirmation sent to one-time invitation.
- type: "sentConfirmation"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)
- customUserProfile: [Profile](./TYPES.md#profile)?

Contact already exists.
- type: "contactAlreadyExists"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

Invitation sent to contact address.
- type: "sentInvitation"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)
- customUserProfile: [Profile](./TYPES.md#profile)?


### APIAcceptContact

Accept contact request.

**Parameters**:
- incognito: Bool
- contactReqId: Int64

**Response**: Contact request accepted.
- type: "acceptingContactRequest"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)


### APIRejectContact

Reject contact request. The user who sent the request is **not notified**.

**Parameters**:
- contactReqId: Int64

**Response**: Contact request rejected.
- type: "contactRequestRejected"
- user: [User](./TYPES.md#user)
- contactRequest: [UserContactRequest](./TYPES.md#usercontactrequest)
- contact_: [Contact](./TYPES.md#contact)?

## User profile commands

Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).


### ShowActiveUser

Get active user profile

**Response**: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)


### CreateActiveUser

Create new user profile

**Parameters**:
- newUser: [NewUser](./TYPES.md#newuser)

**Response**: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)


### ListUsers

Get all user profiles

**Response**: Users.
- type: "usersList"
- users: [[UserInfo](./TYPES.md#userinfo)]


### APISetActiveUser

Set active user profile

**Parameters**:
- userId: Int64
- viewPwd: String?

**Response**: Active user profile.
- type: "activeUser"
- user: [User](./TYPES.md#user)


### APIDeleteUser

Delete user profile.

**Parameters**:
- userId: Int64
- delSMPQueues: Bool
- viewPwd: String?

**Response**: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?


### APIUpdateProfile

Update user profile.

**Parameters**:
- userId: Int64
- profile: [Profile](./TYPES.md#profile)

**Response**: User profile updated.
- type: "userProfileUpdated"
- user: [User](./TYPES.md#user)
- fromProfile: [Profile](./TYPES.md#profile)
- toProfile: [Profile](./TYPES.md#profile)
- updateSummary: [UserProfileUpdateSummary](./TYPES.md#userprofileupdatesummary)

## Chat commands

Commands to get and to manage coversations.


### APIGetChats

Get chats.

**Parameters**:
- userId: Int64
- pendingConnections: Bool
- pagination: [PaginationByTime](./TYPES.md#paginationbytime)
- query: [ChatListQuery](./TYPES.md#chatlistquery)

**Response**: Chats with the most recent messages.
- type: "apiChats"
- user: [User](./TYPES.md#user)
- chats: [[AChat](./TYPES.md#achat)]


### APIGetChat

Get chat.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- contentTag: [MsgContentTag](./TYPES.md#msgcontenttag)?
- chatPagination: [ChatPagination](./TYPES.md#chatpagination)
- search: String?

**Response**: Chat and messages.
- type: "apiChat"
- user: [User](./TYPES.md#user)
- chat: [AChat](./TYPES.md#achat)
- navInfo: [NavigationInfo](./TYPES.md#navigationinfo)?


### APIGetChatItems

Get the most recent messages from all chats.

**Parameters**:
- chatPagination: [ChatPagination](./TYPES.md#chatpagination)
- search: String?

**Response**: The most recent messages.
- type: "chatItems"
- user: [User](./TYPES.md#user)
- chatName_: [ChatName](./TYPES.md#chatname)?
- chatItems: [[AChatItem](./TYPES.md#achatitem)]


### APIGetChatItemInfo

Get message information.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemId: Int64

**Response**: Message information.
- type: "chatItemInfo"
- user: [User](./TYPES.md#user)
- chatItem: [AChatItem](./TYPES.md#achatitem)
- chatItemInfo: [ChatItemInfo](./TYPES.md#chatiteminfo)


### APIChatRead

Mark chat as read.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)

**Response**: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?


### APIChatItemsRead

Mark items as read.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatItemIds: [Int64]

**Response**: Messages marked as read.
- type: "itemsReadForChat"
- user: [User](./TYPES.md#user)
- chatInfo: [AChatInfo](./TYPES.md#achatinfo)


### APIChatUnread

Mark chat as unread.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- unreadChat: Bool

**Response**: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?


### APIDeleteChat

Delete chat.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatDeleteMode: [ChatDeleteMode](./TYPES.md#chatdeletemode)

**Responses**:

Contact deleted.
- type: "contactDeleted"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)

Connection deleted.
- type: "contactConnectionDeleted"
- user: [User](./TYPES.md#user)
- connection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)

User deleted group.
- type: "groupDeletedUser"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)


### APIClearChat

Clear chat.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)

**Response**: Chat cleared.
- type: "chatCleared"
- user: [User](./TYPES.md#user)
- chatInfo: [AChatInfo](./TYPES.md#achatinfo)


### APISetContactPrefs

Set contact preferences.

**Parameters**:
- contactId: Int64
- preferences: [Preferences](./TYPES.md#preferences)

**Response**: Contact preferences updated.
- type: "contactPrefsUpdated"
- user: [User](./TYPES.md#user)
- fromContact: [Contact](./TYPES.md#contact)
- toContact: [Contact](./TYPES.md#contact)


### APISetContactAlias

Set contact alias.

**Parameters**:
- contactId: Int64
- localAlias: String

**Response**: Contact alias updated.
- type: "contactAliasUpdated"
- user: [User](./TYPES.md#user)
- toContact: [Contact](./TYPES.md#contact)


### APISetGroupAlias

Set group alias.

**Parameters**:
- groupId: Int64
- localAlias: String

**Response**: Group alias updated.
- type: "groupAliasUpdated"
- user: [User](./TYPES.md#user)
- toGroup: [GroupInfo](./TYPES.md#groupinfo)


### APISetConnectionAlias

Set connection alias.

**Parameters**:
- connectionId: Int64
- localAlias: String

**Response**: Connection alias updated.
- type: "connectionAliasUpdated"
- user: [User](./TYPES.md#user)
- toConnection: [PendingContactConnection](./TYPES.md#pendingcontactconnection)


### APISetChatTTL

Set TTL for chat messages.

**Parameters**:
- userId: Int64
- chatRef: [ChatRef](./TYPES.md#chatref)
- seconds: Int64?

**Response**: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?


### APISetChatSettings

Set chat settings.

**Parameters**:
- chatRef: [ChatRef](./TYPES.md#chatref)
- chatSettings: [ChatSettings](./TYPES.md#chatsettings)

**Response**: Ok.
- type: "cmdOk"
- user_: [User](./TYPES.md#user)?


### APISyncContactRatchet

Synchronize encryption with contact.

**Parameters**:
- contactId: Int64
- force: Bool

**Response**: Contact encryption synchronization started.
- type: "contactRatchetSyncStarted"
- user: [User](./TYPES.md#user)
- contact: [Contact](./TYPES.md#contact)
- connectionStats: [ConnectionStats](./TYPES.md#connectionstats)


### APISyncGroupMemberRatchet

Synchronize encryption with member.

**Parameters**:
- groupId: Int64
- groupMemberId: Int64
- force: Bool

**Response**: Member encryption synchronization started.
- type: "groupMemberRatchetSyncStarted"
- user: [User](./TYPES.md#user)
- groupInfo: [GroupInfo](./TYPES.md#groupinfo)
- member: [GroupMember](./TYPES.md#groupmember)
- connectionStats: [ConnectionStats](./TYPES.md#connectionstats)


### APIListContacts

Get contacts.

**Parameters**:
- userId: Int64

**Response**: Contacts.
- type: "contactsList"
- user: [User](./TYPES.md#user)
- contacts: [[Contact](./TYPES.md#contact)]


### APIListGroups

Get groups.

**Parameters**:
- userId: Int64
- contactId_: Int64?
- search: String?

**Response**: Group.
- type: "groupsList"
- user: [User](./TYPES.md#user)
- groups: [[GroupInfoSummary](./TYPES.md#groupinfosummary)]
