// API Commands
// This file is generated automatically.

import * as T from "./types"

import {CR} from "./responses"

// Address commands
// Bots can use these commands to automatically check and create address when initialized

// Create bot address.
// Network usage: interactive.
export interface APICreateMyAddress {
  userId: number // int64
}

export namespace APICreateMyAddress {
  export type Response = CR.UserContactLinkCreated | CR.ChatCmdError

  export function cmdString(self: APICreateMyAddress): string {
    return '/_address ' + self.userId
  }
}

// Delete bot address.
// Network usage: background.
export interface APIDeleteMyAddress {
  userId: number // int64
}

export namespace APIDeleteMyAddress {
  export type Response = CR.UserContactLinkDeleted | CR.ChatCmdError

  export function cmdString(self: APIDeleteMyAddress): string {
    return '/_delete_address ' + self.userId
  }
}

// Get bot address and settings.
// Network usage: no.
export interface APIShowMyAddress {
  userId: number // int64
}

export namespace APIShowMyAddress {
  export type Response = CR.UserContactLink | CR.ChatCmdError

  export function cmdString(self: APIShowMyAddress): string {
    return '/_show_address ' + self.userId
  }
}

// Add address to bot profile.
// Network usage: interactive.
export interface APISetProfileAddress {
  userId: number // int64
  enable: boolean
}

export namespace APISetProfileAddress {
  export type Response = CR.UserProfileUpdated | CR.ChatCmdError

  export function cmdString(self: APISetProfileAddress): string {
    return '/_profile_address ' + self.userId + ' ' + (self.enable ? 'on' : 'off')
  }
}

// Set bot address settings.
// Network usage: interactive.
export interface APISetAddressSettings {
  userId: number // int64
  settings: T.AddressSettings
}

export namespace APISetAddressSettings {
  export type Response = CR.UserContactLinkUpdated | CR.ChatCmdError

  export function cmdString(self: APISetAddressSettings): string {
    return '/_address_settings ' + self.userId + ' ' + JSON.stringify(self.settings)
  }
}

// Message commands
// Commands to send, update, delete, moderate messages and set message reactions

// Send messages.
// Network usage: background.
export interface APISendMessages {
  sendRef: T.ChatRef
  liveMessage: boolean
  ttl?: number // int
  composedMessages: T.ComposedMessage[] // non-empty
}

export namespace APISendMessages {
  export type Response = CR.NewChatItems | CR.ChatCmdError

  export function cmdString(self: APISendMessages): string {
    return '/_send ' + T.ChatRef.cmdString(self.sendRef) + (self.liveMessage ? ' live=on' : '') + (self.ttl ? ' ttl=' + self.ttl : '') + ' json ' + JSON.stringify(self.composedMessages)
  }
}

// Update message.
// Network usage: background.
export interface APIUpdateChatItem {
  chatRef: T.ChatRef
  chatItemId: number // int64
  liveMessage: boolean
  updatedMessage: T.UpdatedMessage
}

export namespace APIUpdateChatItem {
  export type Response = CR.ChatItemUpdated | CR.ChatItemNotChanged | CR.ChatCmdError

  export function cmdString(self: APIUpdateChatItem): string {
    return '/_update item ' + T.ChatRef.cmdString(self.chatRef) + ' ' + self.chatItemId + (self.liveMessage ? ' live=on' : '') + ' json ' + JSON.stringify(self.updatedMessage)
  }
}

// Delete message.
// Network usage: background.
export interface APIDeleteChatItem {
  chatRef: T.ChatRef
  chatItemIds: number[] // int64, non-empty
  deleteMode: T.CIDeleteMode
}

export namespace APIDeleteChatItem {
  export type Response = CR.ChatItemsDeleted | CR.ChatCmdError

  export function cmdString(self: APIDeleteChatItem): string {
    return '/_delete item ' + T.ChatRef.cmdString(self.chatRef) + ' ' + self.chatItemIds.join(',') + ' ' + self.deleteMode
  }
}

// Moderate message. Requires Moderator role (and higher than message author's).
// Network usage: background.
export interface APIDeleteMemberChatItem {
  groupId: number // int64
  chatItemIds: number[] // int64, non-empty
}

export namespace APIDeleteMemberChatItem {
  export type Response = CR.ChatItemsDeleted | CR.ChatCmdError

  export function cmdString(self: APIDeleteMemberChatItem): string {
    return '/_delete member item #' + self.groupId + ' ' + self.chatItemIds.join(',')
  }
}

// Add/remove message reaction.
// Network usage: background.
export interface APIChatItemReaction {
  chatRef: T.ChatRef
  chatItemId: number // int64
  add: boolean
  reaction: T.MsgReaction
}

export namespace APIChatItemReaction {
  export type Response = CR.ChatItemReaction | CR.ChatCmdError

  export function cmdString(self: APIChatItemReaction): string {
    return '/_reaction ' + T.ChatRef.cmdString(self.chatRef) + ' ' + self.chatItemId + ' ' + (self.add ? 'on' : 'off') + ' ' + JSON.stringify(self.reaction)
  }
}

// File commands
// Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.

// Receive file.
// Network usage: no.
export interface ReceiveFile {
  fileId: number // int64
  userApprovedRelays: boolean
  storeEncrypted?: boolean
  fileInline?: boolean
  filePath?: string
}

export namespace ReceiveFile {
  export type Response = CR.RcvFileAccepted | CR.RcvFileAcceptedSndCancelled | CR.ChatCmdError

  export function cmdString(self: ReceiveFile): string {
    return '/freceive ' + self.fileId + (self.userApprovedRelays ? ' approved_relays=on' : '') + (typeof self.storeEncrypted == 'boolean' ? ' encrypt=' + (self.storeEncrypted ? 'on' : 'off') : '') + (typeof self.fileInline == 'boolean' ? ' inline=' + (self.fileInline ? 'on' : 'off') : '') + (self.filePath ? ' ' + self.filePath : '')
  }
}

// Cancel file.
// Network usage: background.
export interface CancelFile {
  fileId: number // int64
}

export namespace CancelFile {
  export type Response = CR.SndFileCancelled | CR.RcvFileCancelled | CR.ChatCmdError

  export function cmdString(self: CancelFile): string {
    return '/fcancel ' + self.fileId
  }
}

// Group commands
// Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.

// Add contact to group. Requires bot to have Admin role.
// Network usage: interactive.
export interface APIAddMember {
  groupId: number // int64
  contactId: number // int64
  memberRole: T.GroupMemberRole
}

export namespace APIAddMember {
  export type Response = CR.SentGroupInvitation | CR.ChatCmdError

  export function cmdString(self: APIAddMember): string {
    return '/_add #' + self.groupId + ' ' + self.contactId + ' ' + self.memberRole
  }
}

// Join group.
// Network usage: interactive.
export interface APIJoinGroup {
  groupId: number // int64
}

export namespace APIJoinGroup {
  export type Response = CR.UserAcceptedGroupSent | CR.ChatCmdError

  export function cmdString(self: APIJoinGroup): string {
    return '/_join #' + self.groupId
  }
}

// Accept group member. Requires Admin role.
// Network usage: background.
export interface APIAcceptMember {
  groupId: number // int64
  groupMemberId: number // int64
  memberRole: T.GroupMemberRole
}

export namespace APIAcceptMember {
  export type Response = CR.MemberAccepted | CR.ChatCmdError

  export function cmdString(self: APIAcceptMember): string {
    return '/_accept member #' + self.groupId + ' ' + self.groupMemberId + ' ' + self.memberRole
  }
}

// Set members role. Requires Admin role.
// Network usage: background.
export interface APIMembersRole {
  groupId: number // int64
  groupMemberIds: number[] // int64, non-empty
  memberRole: T.GroupMemberRole
}

export namespace APIMembersRole {
  export type Response = CR.MembersRoleUser | CR.ChatCmdError

  export function cmdString(self: APIMembersRole): string {
    return '/_member role #' + self.groupId + ' ' + self.groupMemberIds.join(',') + ' ' + self.memberRole
  }
}

// Block members. Requires Moderator role.
// Network usage: background.
export interface APIBlockMembersForAll {
  groupId: number // int64
  groupMemberIds: number[] // int64, non-empty
  blocked: boolean
}

export namespace APIBlockMembersForAll {
  export type Response = CR.MembersBlockedForAllUser | CR.ChatCmdError

  export function cmdString(self: APIBlockMembersForAll): string {
    return '/_block #' + self.groupId + ' ' + self.groupMemberIds.join(',') + ' blocked=' + (self.blocked ? 'on' : 'off')
  }
}

// Remove members. Requires Admin role.
// Network usage: background.
export interface APIRemoveMembers {
  groupId: number // int64
  groupMemberIds: number[] // int64, non-empty
  withMessages: boolean
}

export namespace APIRemoveMembers {
  export type Response = CR.UserDeletedMembers | CR.ChatCmdError

  export function cmdString(self: APIRemoveMembers): string {
    return '/_remove #' + self.groupId + ' ' + self.groupMemberIds.join(',') + (self.withMessages ? ' messages=on' : '')
  }
}

// Leave group.
// Network usage: background.
export interface APILeaveGroup {
  groupId: number // int64
}

export namespace APILeaveGroup {
  export type Response = CR.LeftMemberUser | CR.ChatCmdError

  export function cmdString(self: APILeaveGroup): string {
    return '/_leave #' + self.groupId
  }
}

// Get group members.
// Network usage: no.
export interface APIListMembers {
  groupId: number // int64
}

export namespace APIListMembers {
  export type Response = CR.GroupMembers | CR.ChatCmdError

  export function cmdString(self: APIListMembers): string {
    return '/_members #' + self.groupId
  }
}

// Create group.
// Network usage: no.
export interface APINewGroup {
  userId: number // int64
  incognito: boolean
  groupProfile: T.GroupProfile
}

export namespace APINewGroup {
  export type Response = CR.GroupCreated | CR.ChatCmdError

  export function cmdString(self: APINewGroup): string {
    return '/_group ' + self.userId + (self.incognito ? ' incognito=on' : '') + ' ' + JSON.stringify(self.groupProfile)
  }
}

// Create public group.
// Network usage: interactive.
export interface APINewPublicGroup {
  userId: number // int64
  incognito: boolean
  relayIds: number[] // int64, non-empty
  groupProfile: T.GroupProfile
}

export namespace APINewPublicGroup {
  export type Response = CR.PublicGroupCreated | CR.ChatCmdError

  export function cmdString(self: APINewPublicGroup): string {
    return '/_public group ' + self.userId + (self.incognito ? ' incognito=on' : '') + ' ' + self.relayIds + ' ' + JSON.stringify(self.groupProfile)
  }
}

// Update group profile.
// Network usage: background.
export interface APIUpdateGroupProfile {
  groupId: number // int64
  groupProfile: T.GroupProfile
}

export namespace APIUpdateGroupProfile {
  export type Response = CR.GroupUpdated | CR.ChatCmdError

  export function cmdString(self: APIUpdateGroupProfile): string {
    return '/_group_profile #' + self.groupId + ' ' + JSON.stringify(self.groupProfile)
  }
}

// Group link commands
// These commands can be used by bots that manage multiple public groups

// Create group link.
// Network usage: interactive.
export interface APICreateGroupLink {
  groupId: number // int64
  memberRole: T.GroupMemberRole
}

export namespace APICreateGroupLink {
  export type Response = CR.GroupLinkCreated | CR.ChatCmdError

  export function cmdString(self: APICreateGroupLink): string {
    return '/_create link #' + self.groupId + ' ' + self.memberRole
  }
}

// Set member role for group link.
// Network usage: no.
export interface APIGroupLinkMemberRole {
  groupId: number // int64
  memberRole: T.GroupMemberRole
}

export namespace APIGroupLinkMemberRole {
  export type Response = CR.GroupLink | CR.ChatCmdError

  export function cmdString(self: APIGroupLinkMemberRole): string {
    return '/_set link role #' + self.groupId + ' ' + self.memberRole
  }
}

// Delete group link.
// Network usage: background.
export interface APIDeleteGroupLink {
  groupId: number // int64
}

export namespace APIDeleteGroupLink {
  export type Response = CR.GroupLinkDeleted | CR.ChatCmdError

  export function cmdString(self: APIDeleteGroupLink): string {
    return '/_delete link #' + self.groupId
  }
}

// Get group link.
// Network usage: no.
export interface APIGetGroupLink {
  groupId: number // int64
}

export namespace APIGetGroupLink {
  export type Response = CR.GroupLink | CR.ChatCmdError

  export function cmdString(self: APIGetGroupLink): string {
    return '/_get link #' + self.groupId
  }
}

// Connection commands
// These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.

// Create 1-time invitation link.
// Network usage: interactive.
export interface APIAddContact {
  userId: number // int64
  incognito: boolean
}

export namespace APIAddContact {
  export type Response = CR.Invitation | CR.ChatCmdError

  export function cmdString(self: APIAddContact): string {
    return '/_connect ' + self.userId + (self.incognito ? ' incognito=on' : '')
  }
}

// Determine SimpleX link type and if the bot is already connected via this link.
// Network usage: interactive.
export interface APIConnectPlan {
  userId: number // int64
  connectionLink?: string
}

export namespace APIConnectPlan {
  export type Response = CR.ConnectionPlan | CR.ChatCmdError

  export function cmdString(self: APIConnectPlan): string {
    return '/_connect plan ' + self.userId + ' ' + self.connectionLink
  }
}

// Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link.
// Network usage: interactive.
export interface APIConnect {
  userId: number // int64
  incognito: boolean
  preparedLink_?: T.CreatedConnLink
}

export namespace APIConnect {
  export type Response = CR.SentConfirmation | CR.ContactAlreadyExists | CR.SentInvitation | CR.ChatCmdError

  export function cmdString(self: APIConnect): string {
    return '/_connect ' + self.userId + (self.preparedLink_ ? ' ' + T.CreatedConnLink.cmdString(self.preparedLink_) : '')
  }
}

// Connect via SimpleX link as string in the active user profile.
// Network usage: interactive.
export interface Connect {
  incognito: boolean
  connLink_?: string
}

export namespace Connect {
  export type Response = CR.SentConfirmation | CR.ContactAlreadyExists | CR.SentInvitation | CR.ChatCmdError

  export function cmdString(self: Connect): string {
    return '/connect' + (self.connLink_ ? ' ' + self.connLink_ : '')
  }
}

// Accept contact request.
// Network usage: interactive.
export interface APIAcceptContact {
  contactReqId: number // int64
}

export namespace APIAcceptContact {
  export type Response = CR.AcceptingContactRequest | CR.ChatCmdError

  export function cmdString(self: APIAcceptContact): string {
    return '/_accept ' + self.contactReqId
  }
}

// Reject contact request. The user who sent the request is **not notified**.
// Network usage: no.
export interface APIRejectContact {
  contactReqId: number // int64
}

export namespace APIRejectContact {
  export type Response = CR.ContactRequestRejected | CR.ChatCmdError

  export function cmdString(self: APIRejectContact): string {
    return '/_reject ' + self.contactReqId
  }
}

// Chat commands
// Commands to list and delete conversations.

// Get contacts.
// Network usage: no.
export interface APIListContacts {
  userId: number // int64
}

export namespace APIListContacts {
  export type Response = CR.ContactsList | CR.ChatCmdError

  export function cmdString(self: APIListContacts): string {
    return '/_contacts ' + self.userId
  }
}

// Get groups.
// Network usage: no.
export interface APIListGroups {
  userId: number // int64
  contactId_?: number // int64
  search?: string
}

export namespace APIListGroups {
  export type Response = CR.GroupsList | CR.ChatCmdError

  export function cmdString(self: APIListGroups): string {
    return '/_groups ' + self.userId + (self.contactId_ ? ' @' + self.contactId_ : '') + (self.search ? ' ' + self.search : '')
  }
}

// Delete chat.
// Network usage: background.
export interface APIDeleteChat {
  chatRef: T.ChatRef
  chatDeleteMode: T.ChatDeleteMode
}

export namespace APIDeleteChat {
  export type Response = CR.ContactDeleted | CR.ContactConnectionDeleted | CR.GroupDeletedUser | CR.ChatCmdError

  export function cmdString(self: APIDeleteChat): string {
    return '/_delete ' + T.ChatRef.cmdString(self.chatRef) + ' ' + T.ChatDeleteMode.cmdString(self.chatDeleteMode)
  }
}

// User profile commands
// Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).

// Get active user profile.
// Network usage: no.
export interface ShowActiveUser {
}

export namespace ShowActiveUser {
  export type Response = CR.ActiveUser | CR.ChatCmdError

  export function cmdString(_self: ShowActiveUser): string {
    return '/user'
  }
}

// Create new user profile.
// Network usage: no.
export interface CreateActiveUser {
  newUser: T.NewUser
}

export namespace CreateActiveUser {
  export type Response = CR.ActiveUser | CR.ChatCmdError

  export function cmdString(self: CreateActiveUser): string {
    return '/_create user ' + JSON.stringify(self.newUser)
  }
}

// Get all user profiles.
// Network usage: no.
export interface ListUsers {
}

export namespace ListUsers {
  export type Response = CR.UsersList | CR.ChatCmdError

  export function cmdString(_self: ListUsers): string {
    return '/users'
  }
}

// Set active user profile.
// Network usage: no.
export interface APISetActiveUser {
  userId: number // int64
  viewPwd?: string
}

export namespace APISetActiveUser {
  export type Response = CR.ActiveUser | CR.ChatCmdError

  export function cmdString(self: APISetActiveUser): string {
    return '/_user ' + self.userId + (self.viewPwd ? ' ' + JSON.stringify(self.viewPwd) : '')
  }
}

// Delete user profile.
// Network usage: background.
export interface APIDeleteUser {
  userId: number // int64
  delSMPQueues: boolean
  viewPwd?: string
}

export namespace APIDeleteUser {
  export type Response = CR.CmdOk | CR.ChatCmdError

  export function cmdString(self: APIDeleteUser): string {
    return '/_delete user ' + self.userId + ' del_smp=' + (self.delSMPQueues ? 'on' : 'off') + (self.viewPwd ? ' ' + JSON.stringify(self.viewPwd) : '')
  }
}

// Update user profile.
// Network usage: background.
export interface APIUpdateProfile {
  userId: number // int64
  profile: T.Profile
}

export namespace APIUpdateProfile {
  export type Response = CR.UserProfileUpdated | CR.UserProfileNoChange | CR.ChatCmdError

  export function cmdString(self: APIUpdateProfile): string {
    return '/_profile ' + self.userId + ' ' + JSON.stringify(self.profile)
  }
}

// Configure chat preference overrides for the contact.
// Network usage: background.
export interface APISetContactPrefs {
  contactId: number // int64
  preferences: T.Preferences
}

export namespace APISetContactPrefs {
  export type Response = CR.ContactPrefsUpdated | CR.ChatCmdError

  export function cmdString(self: APISetContactPrefs): string {
    return '/_set prefs @' + self.contactId + ' ' + JSON.stringify(self.preferences)
  }
}

// Chat management
// These commands should not be used with CLI-based bots

// Start chat controller.
// Network usage: no.
export interface StartChat {
  mainApp: boolean
  enableSndFiles: boolean
}

export namespace StartChat {
  export type Response = CR.ChatStarted | CR.ChatRunning

  export function cmdString(_self: StartChat): string {
    return '/_start'
  }
}

// Stop chat controller.
// Network usage: no.
export interface APIStopChat {
}

export namespace APIStopChat {
  export type Response = CR.ChatStopped

  export function cmdString(_self: APIStopChat): string {
    return '/_stop'
  }
}
