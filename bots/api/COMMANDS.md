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
- user: User
- connLinkContact: CreatedConnLink 'CMContact


### APIDeleteMyAddress

Delete bot address.

**Parameters**:
- userId: Int64

**Response**: User contact address deleted.
- type: "userContactLinkDeleted"
- user: User


### APIShowMyAddress

Get bot address and settings.

**Parameters**:
- userId: Int64

**Response**: User contact address.
- type: "userContactLink"
- user: User
- contactLink: UserContactLink


### APISetProfileAddress

Add address to bot profile.

**Parameters**:
- userId: Int64
- enable: Bool

**Response**: User profile updated.
- type: "userProfileUpdated"
- user: User
- fromProfile: Profile
- toProfile: Profile
- updateSummary: UserProfileUpdateSummary


### APISetAddressSettings

Set bot address settings.

**Parameters**:
- userId: Int64
- settings: AddressSettings

**Response**: User contact address updated.
- type: "userContactLinkUpdated"
- user: User
- contactLink: UserContactLink

## Message commands

Commands to send, update, delete, moderate messages and set message reactions


### APISendMessages

Send messages.

**Parameters**:
- sendRef: SendRef
- liveMessage: Bool
- ttl: Int?
- composedMessages: [ComposedMessage] // (non-empty)

**Response**: New messages.
- type: "newChatItems"
- user: User
- chatItems: [AChatItem]


### APIReportMessage

Report message.

**Parameters**:
- groupId: Int64
- chatItemId: Int64
- reportReason: ReportReason
- reportText: String

**Response**: New messages.
- type: "newChatItems"
- user: User
- chatItems: [AChatItem]


### APIUpdateChatItem

Update message.

**Parameters**:
- chatRef: ChatRef
- chatItemId: Int64
- liveMessage: Bool
- updatedMessage: UpdatedMessage

**Responses**:

Message updated.
- type: "chatItemUpdated"
- user: User
- chatItem: AChatItem

Message not changed.
- type: "chatItemNotChanged"
- user: User
- chatItem: AChatItem


### APIDeleteChatItem

Delete message.

**Parameters**:
- chatRef: ChatRef
- chatItemIds: [Int64] // (non-empty)
- deleteMode: CIDeleteMode

**Response**: Messages deleted.
- type: "chatItemsDeleted"
- user: User
- chatItemDeletions: [ChatItemDeletion]
- byUser: Bool
- timed: Bool


### APIDeleteMemberChatItem

Moderate message.

**Parameters**:
- groupId: Int64
- chatItemIds: [Int64] // (non-empty)

**Response**: Messages deleted.
- type: "chatItemsDeleted"
- user: User
- chatItemDeletions: [ChatItemDeletion]
- byUser: Bool
- timed: Bool


### APIChatItemReaction

Add/remove message reaction.

**Parameters**:
- chatRef: ChatRef
- chatItemId: Int64
- add: Bool
- reaction: MsgReaction

**Response**: Message reaction.
- type: "chatItemReaction"
- user: User
- added: Bool
- reaction: ACIReaction


### APIGetReactionMembers

Get reaction members.

**Parameters**:
- userId: Int64
- groupId: Int64
- chatItemId: Int64
- reaction: MsgReaction

**Response**: Members who set reaction on the message.
- type: "reactionMembers"
- user: User
- memberReactions: [MemberReaction]

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
- user: User
- chatItem: AChatItem

File accepted, but no longer sent.
- type: "rcvFileAcceptedSndCancelled"
- user: User
- rcvFileTransfer: RcvFileTransfer


### CancelFile

Cancel file.

**Parameters**:
- fileId: Int64

**Responses**:

Cancelled sending file.
- type: "sndFileCancelled"
- user: User
- chatItem_: AChatItem?
- fileTransferMeta: FileTransferMeta
- sndFileTransfers: [SndFileTransfer]

Cancelled receiving file.
- type: "rcvFileCancelled"
- user: User
- chatItem_: AChatItem?
- rcvFileTransfer: RcvFileTransfer

## Group commands

Commands to create and manage groups. These commands have to be used to manage business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.


### APINewGroup

Create group.

**Parameters**:
- userId: Int64
- incognito: Bool
- groupProfile: GroupProfile

**Response**: Group created.
- type: "groupCreated"
- user: User
- groupInfo: GroupInfo


### APIAddMember

Add contact to group.

**Parameters**:
- groupId: Int64
- contactId: Int64
- memberRole: GroupMemberRole

**Response**: Group invitation sent.
- type: "sentGroupInvitation"
- user: User
- groupInfo: GroupInfo
- contact: Contact
- member: GroupMember


### APIJoinGroup

Join group.

**Parameters**:
- groupId: Int64
- enableNtfs: MsgFilter

**Response**: User accepted group invitation.
- type: "userAcceptedGroupSent"
- user: User
- groupInfo: GroupInfo
- hostContact: Contact?


### APIAcceptMember

Accept group member.

**Parameters**:
- groupId: Int64
- groupMemberId: Int64
- memberRole: GroupMemberRole

**Response**: Member accepted to group.
- type: "memberAccepted"
- user: User
- groupInfo: GroupInfo
- member: GroupMember


### APIMembersRole

Set members role.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64] // (non-empty)
- memberRole: GroupMemberRole

**Response**: Members role changed by user.
- type: "membersRoleUser"
- user: User
- groupInfo: GroupInfo
- members: [GroupMember]
- toRole: GroupMemberRole


### APIBlockMembersForAll

Block members.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64] // (non-empty)
- blocked: Bool

**Response**: Members blocked for all by admin.
- type: "membersBlockedForAllUser"
- user: User
- groupInfo: GroupInfo
- members: [GroupMember]
- blocked: Bool


### APIRemoveMembers

Remove members.

**Parameters**:
- groupId: Int64
- groupMemberIds: [Int64] // (non-empty)
- withMessages: Bool

**Response**: Members deleted.
- type: "userDeletedMembers"
- user: User
- groupInfo: GroupInfo
- members: [GroupMember]
- withMessages: Bool


### APILeaveGroup

Leave group.

**Parameters**:
- groupId: Int64

**Response**: User left group.
- type: "leftMemberUser"
- user: User
- groupInfo: GroupInfo


### APIListMembers

Get group members.

**Parameters**:
- groupId: Int64

**Response**: Group members.
- type: "groupMembers"
- user: User
- group: Group


### APIUpdateGroupProfile

Update group profile.

**Parameters**:
- groupId: Int64
- groupProfile: GroupProfile

**Response**: Group updated.
- type: "groupUpdated"
- user: User
- fromGroup: GroupInfo
- toGroup: GroupInfo
- member_: GroupMember?

## Group link commands

These commands can be used by bots that manage multiple public groups


### APICreateGroupLink

Create group link.

**Parameters**:
- groupId: Int64
- memberRole: GroupMemberRole

**Response**: Group link created.
- type: "groupLinkCreated"
- user: User
- groupInfo: GroupInfo
- groupLink: GroupLink


### APIGroupLinkMemberRole

Set member role for group link.

**Parameters**:
- groupId: Int64
- memberRole: GroupMemberRole

**Response**: Group link.
- type: "groupLink"
- user: User
- groupInfo: GroupInfo
- groupLink: GroupLink


### APIDeleteGroupLink

Delete group link.

**Parameters**:
- groupId: Int64

**Response**: Group link deleted.
- type: "groupLinkDeleted"
- user: User
- groupInfo: GroupInfo


### APIGetGroupLink

Get group link.

**Parameters**:
- groupId: Int64

**Response**: Group link.
- type: "groupLink"
- user: User
- groupInfo: GroupInfo
- groupLink: GroupLink

## Connection commands

These commands may be used to establish connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.


### APIAddContact

Create 1-time invitation link.

**Parameters**:
- userId: Int64
- incognito: Bool

**Response**: One-time invitation.
- type: "invitation"
- user: User
- connLinkInvitation: CreatedConnLink 'CMInvitation
- connection: PendingContactConnection


### APIConnectPlan

Determine SimpleX link type and if the bot is already connected via this link.

**Parameters**:
- userId: Int64
- connectionLink: AConnectionLink?

**Response**: Connection link information.
- type: "connectionPlan"
- user: User
- connLink: ACreatedConnLink
- connectionPlan: ConnectionPlan


### APIConnect

Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link

**Parameters**:
- userId: Int64
- incognito: Bool
- connLink_: ACreatedConnLink?

**Responses**:

Confirmation sent to one-time invitation.
- type: "sentConfirmation"
- user: User
- connection: PendingContactConnection
- customUserProfile: Profile?

Contact already exists.
- type: "contactAlreadyExists"
- user: User
- contact: Contact

Invitation sent to contact address.
- type: "sentInvitation"
- user: User
- connection: PendingContactConnection
- customUserProfile: Profile?


### APIAcceptContact

Accept contact request.

**Parameters**:
- incognito: Bool
- contactReqId: Int64

**Response**: Contact request accepted.
- type: "acceptingContactRequest"
- user: User
- contact: Contact


### APIRejectContact

Reject contact request. The user who sent the request is **not notified**.

**Parameters**:
- contactReqId: Int64

**Response**: Contact request rejected.
- type: "contactRequestRejected"
- user: User
- contactRequest: UserContactRequest
- contact_: Contact?

## User profile commands

Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).


### ShowActiveUser

Get active user profile

**Response**: Active user profile.
- type: "activeUser"
- user: User


### CreateActiveUser

Create new user profile

**Parameters**:
- newUser: NewUser

**Response**: Active user profile.
- type: "activeUser"
- user: User


### ListUsers

Get all user profiles

**Response**: Users.
- type: "usersList"
- users: [UserInfo]


### APISetActiveUser

Set active user profile

**Parameters**:
- userId: Int64
- viewPwd: String?

**Response**: Active user profile.
- type: "activeUser"
- user: User


### APIDeleteUser

Delete user profile.

**Parameters**:
- userId: Int64
- delSMPQueues: Bool
- viewPwd: String?

**Response**: Ok.
- type: "cmdOk"
- user_: User?


### APIUpdateProfile

Update user profile.

**Parameters**:
- userId: Int64
- profile: Profile

**Response**: User profile updated.
- type: "userProfileUpdated"
- user: User
- fromProfile: Profile
- toProfile: Profile
- updateSummary: UserProfileUpdateSummary

## Chat commands

Commands to get and to manage coversations.


### APIGetChats

Get chats.

**Parameters**:
- userId: Int64
- pendingConnections: Bool
- pagination: PaginationByTime
- query: ChatListQuery

**Response**: Chats with the most recent messages.
- type: "apiChats"
- user: User
- chats: [AChat]


### APIGetChat

Get chat.

**Parameters**:
- chatRef: ChatRef
- contentTag: MsgContentTag?
- chatPagination: ChatPagination
- search: String?

**Response**: Chat and messages.
- type: "apiChat"
- user: User
- chat: AChat
- navInfo: NavigationInfo?


### APIGetChatItems

Get the most recent messages from all chats.

**Parameters**:
- chatPagination: ChatPagination
- search: String?

**Response**: The most recent messages.
- type: "chatItems"
- user: User
- chatName_: ChatName?
- chatItems: [AChatItem]


### APIGetChatItemInfo

Get message information.

**Parameters**:
- chatRef: ChatRef
- chatItemId: Int64

**Response**: Message information.
- type: "chatItemInfo"
- user: User
- chatItem: AChatItem
- chatItemInfo: ChatItemInfo


### APIChatRead

Mark chat as read.

**Parameters**:
- chatRef: ChatRef

**Response**: Ok.
- type: "cmdOk"
- user_: User?


### APIChatItemsRead

Mark items as read.

**Parameters**:
- chatRef: ChatRef
- chatItemIds: [Int64] // (non-empty)

**Response**: Messages marked as read.
- type: "itemsReadForChat"
- user: User
- chatInfo: AChatInfo


### APIChatUnread

Mark chat as unread.

**Parameters**:
- chatRef: ChatRef
- unreadChat: Bool

**Response**: Ok.
- type: "cmdOk"
- user_: User?


### APIDeleteChat

Delete chat.

**Parameters**:
- chatRef: ChatRef
- chatDeleteMode: ChatDeleteMode

**Responses**:

Contact deleted.
- type: "contactDeleted"
- user: User
- contact: Contact

Connection deleted.
- type: "contactConnectionDeleted"
- user: User
- connection: PendingContactConnection

User deleted group.
- type: "groupDeletedUser"
- user: User
- groupInfo: GroupInfo


### APIClearChat

Clear chat.

**Parameters**:
- chatRef: ChatRef

**Response**: Chat cleared.
- type: "chatCleared"
- user: User
- chatInfo: AChatInfo


### APISetContactPrefs

Set contact preferences.

**Parameters**:
- contactId: Int64
- preferences: Preferences

**Response**: Contact preferences updated.
- type: "contactPrefsUpdated"
- user: User
- fromContact: Contact
- toContact: Contact


### APISetContactAlias

Set contact alias.

**Parameters**:
- contactId: Int64
- localAlias: String

**Response**: Contact alias updated.
- type: "contactAliasUpdated"
- user: User
- toContact: Contact


### APISetGroupAlias

Set group alias.

**Parameters**:
- groupId: Int64
- localAlias: String

**Response**: Group alias updated.
- type: "groupAliasUpdated"
- user: User
- toGroup: GroupInfo


### APISetConnectionAlias

Set connection alias.

**Parameters**:
- connectionId: Int64
- localAlias: String

**Response**: Connection alias updated.
- type: "connectionAliasUpdated"
- user: User
- toConnection: PendingContactConnection


### APISetChatTTL

Set TTL for chat messages.

**Parameters**:
- userId: Int64
- chatRef: ChatRef
- seconds: Int64?

**Response**: Ok.
- type: "cmdOk"
- user_: User?


### APISetChatSettings

Set chat settings.

**Parameters**:
- chatRef: ChatRef
- chatSettings: ChatSettings

**Response**: Ok.
- type: "cmdOk"
- user_: User?


### APISyncContactRatchet

Synchronize encryption with contact.

**Parameters**:
- contactId: Int64
- force: Bool

**Response**: Contact encryption synchronization started.
- type: "contactRatchetSyncStarted"
- user: User
- contact: Contact
- connectionStats: ConnectionStats


### APISyncGroupMemberRatchet

Synchronize encryption with member.

**Parameters**:
- groupId: Int64
- groupMemberId: Int64
- force: Bool

**Response**: Member encryption synchronization started.
- type: "groupMemberRatchetSyncStarted"
- user: User
- groupInfo: GroupInfo
- member: GroupMember
- connectionStats: ConnectionStats


### APIListContacts

Get contacts.

**Parameters**:
- userId: Int64

**Response**: Contacts.
- type: "contactsList"
- user: User
- contacts: [Contact]


### APIListGroups

Get groups.

**Parameters**:
- userId: Int64
- contactId_: Int64?
- search: String?

**Response**: Group.
- type: "groupsList"
- user: User
- groups: [GroupInfoSummary]
