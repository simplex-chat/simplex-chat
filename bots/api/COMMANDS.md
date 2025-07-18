# API Commands and Responses

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

### APIDeleteMyAddress

Delete bot address.

### APIShowMyAddress

Get bot address and settings.

### APISetProfileAddress

Add address to bot profile.

### APISetAddressSettings

Set bot address settings.

## Message commands

Commands to send, update, delete, moderate messages and set message reactions

### APISendMessages

Send messages.

### APIReportMessage

Report message.

### APIUpdateChatItem

Update message.

### APIDeleteChatItem

Delete message.

### APIDeleteMemberChatItem

Moderate message.

### APIChatItemReaction

Add/remove message reaction.

### APIGetReactionMembers

Get reaction members.

## File commands

Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.

### ReceiveFile

Receive file.

### CancelFile

Cancel file.

## Group commands

Commands to create and manage groups. These commands have to be used to manage business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.

### APINewGroup

Create group.

### APIAddMember

Add contact to group.

### APIJoinGroup

Join group.

### APIAcceptMember

Accept group member.

### APIMembersRole

Set members role.

### APIBlockMembersForAll

Block members.

### APIRemoveMembers

Remove members.

### APILeaveGroup

Leave group.

### APIListMembers

Get group members.

### APIUpdateGroupProfile

Update group profile.

## Group link commands

These commands can be used by bots that manage multiple public groups

### APICreateGroupLink

Create group link.

### APIGroupLinkMemberRole

Set member role for group link.

### APIDeleteGroupLink

Delete group link.

### APIGetGroupLink

Get group link.

## Connection commands

These commands may be used to establish connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.

### APIAddContact

Create 1-time invitation link.

### APIConnectPlan

Determine SimpleX link type and if the bot is already connected via this link.

### APIConnect

Connect via SimpleX link. The link can be 1-time invitation link, contact address or group link

### APIAcceptContact

Accept contact request.

### APIRejectContact

Reject contact request.

## User profile commands

Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).

### ShowActiveUser

Get active user profile

### CreateActiveUser

Create new user profile

### ListUsers

Get all user profiles

### APISetActiveUser

Set active user profile

### APIDeleteUser

Delete user profile.

### APIUpdateProfile

Update user profile.

## Chat commands

Commands to get and to manage coversations.

### APIGetChats

Get chats.

### APIGetChat

Get chat.

### APIGetChatItems

Get messages.

### APIGetChatItemInfo

Get message information.

### APIChatRead

Mark chat as read.

### APIChatItemsRead

Mark items as read.

### APIChatUnread

Mark chat as unread.

### APIDeleteChat

Delete chat.

### APIClearChat

Clear chat.

### APISetContactPrefs

Set contact preferences.

### APISetContactAlias

Set contact alias.

### APISetGroupAlias

Set group alias.

### APISetConnectionAlias

Set connection alias.

### APISetChatTTL

Set TTL for chat messages.

### APISetChatSettings

Set chat settings.

### APISyncContactRatchet

Synchronize encryption with contact.

### APISyncGroupMemberRatchet

Synchronize encryption with member.

### APIListContacts

Get contacts.

### APIListGroups

Get groups.
