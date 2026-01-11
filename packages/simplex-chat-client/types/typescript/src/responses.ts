// API Responses
// This file is generated automatically.

import * as T from "./types"

export type ChatResponse = 
  | CR.AcceptingContactRequest
  | CR.ActiveUser
  | CR.ChatItemNotChanged
  | CR.ChatItemReaction
  | CR.ChatItemUpdated
  | CR.ChatItemsDeleted
  | CR.ChatRunning
  | CR.ChatStarted
  | CR.ChatStopped
  | CR.CmdOk
  | CR.ChatCmdError
  | CR.ConnectionPlan
  | CR.ContactAlreadyExists
  | CR.ContactConnectionDeleted
  | CR.ContactDeleted
  | CR.ContactPrefsUpdated
  | CR.ContactRequestRejected
  | CR.ContactsList
  | CR.GroupDeletedUser
  | CR.GroupLink
  | CR.GroupLinkCreated
  | CR.GroupLinkDeleted
  | CR.GroupCreated
  | CR.GroupMembers
  | CR.GroupUpdated
  | CR.GroupsList
  | CR.Invitation
  | CR.LeftMemberUser
  | CR.MemberAccepted
  | CR.MembersBlockedForAllUser
  | CR.MembersRoleUser
  | CR.NewChatItems
  | CR.RcvFileAccepted
  | CR.RcvFileAcceptedSndCancelled
  | CR.RcvFileCancelled
  | CR.SentConfirmation
  | CR.SentGroupInvitation
  | CR.SentInvitation
  | CR.SndFileCancelled
  | CR.UserAcceptedGroupSent
  | CR.UserContactLink
  | CR.UserContactLinkCreated
  | CR.UserContactLinkDeleted
  | CR.UserContactLinkUpdated
  | CR.UserDeletedMembers
  | CR.UserProfileUpdated
  | CR.UserProfileNoChange
  | CR.UsersList
  | CR.Interface

export namespace CR {
  export type Tag = 
    | "acceptingContactRequest"
    | "activeUser"
    | "chatItemNotChanged"
    | "chatItemReaction"
    | "chatItemUpdated"
    | "chatItemsDeleted"
    | "chatRunning"
    | "chatStarted"
    | "chatStopped"
    | "cmdOk"
    | "chatCmdError"
    | "connectionPlan"
    | "contactAlreadyExists"
    | "contactConnectionDeleted"
    | "contactDeleted"
    | "contactPrefsUpdated"
    | "contactRequestRejected"
    | "contactsList"
    | "groupDeletedUser"
    | "groupLink"
    | "groupLinkCreated"
    | "groupLinkDeleted"
    | "groupCreated"
    | "groupMembers"
    | "groupUpdated"
    | "groupsList"
    | "invitation"
    | "leftMemberUser"
    | "memberAccepted"
    | "membersBlockedForAllUser"
    | "membersRoleUser"
    | "newChatItems"
    | "rcvFileAccepted"
    | "rcvFileAcceptedSndCancelled"
    | "rcvFileCancelled"
    | "sentConfirmation"
    | "sentGroupInvitation"
    | "sentInvitation"
    | "sndFileCancelled"
    | "userAcceptedGroupSent"
    | "userContactLink"
    | "userContactLinkCreated"
    | "userContactLinkDeleted"
    | "userContactLinkUpdated"
    | "userDeletedMembers"
    | "userProfileUpdated"
    | "userProfileNoChange"
    | "usersList"
    | string

  export interface Interface {
    type: Tag
  }

  export interface AcceptingContactRequest extends Interface {
    type: "acceptingContactRequest"
    user: T.User
    contact: T.Contact
  }

  export interface ActiveUser extends Interface {
    type: "activeUser"
    user: T.User
  }

  export interface ChatItemNotChanged extends Interface {
    type: "chatItemNotChanged"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface ChatItemReaction extends Interface {
    type: "chatItemReaction"
    user: T.User
    added: boolean
    reaction: T.ACIReaction
  }

  export interface ChatItemUpdated extends Interface {
    type: "chatItemUpdated"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface ChatItemsDeleted extends Interface {
    type: "chatItemsDeleted"
    user: T.User
    chatItemDeletions: T.ChatItemDeletion[]
    byUser: boolean
    timed: boolean
  }

  export interface ChatRunning extends Interface {
    type: "chatRunning"
  }

  export interface ChatStarted extends Interface {
    type: "chatStarted"
  }

  export interface ChatStopped extends Interface {
    type: "chatStopped"
  }

  export interface CmdOk extends Interface {
    type: "cmdOk"
    user_?: T.User
  }

  export interface ChatCmdError extends Interface {
    type: "chatCmdError"
    chatError: T.ChatError
  }

  export interface ConnectionPlan extends Interface {
    type: "connectionPlan"
    user: T.User
    connLink: T.CreatedConnLink
    connectionPlan: T.ConnectionPlan
  }

  export interface ContactAlreadyExists extends Interface {
    type: "contactAlreadyExists"
    user: T.User
    contact: T.Contact
  }

  export interface ContactConnectionDeleted extends Interface {
    type: "contactConnectionDeleted"
    user: T.User
    connection: T.PendingContactConnection
  }

  export interface ContactDeleted extends Interface {
    type: "contactDeleted"
    user: T.User
    contact: T.Contact
  }

  export interface ContactPrefsUpdated extends Interface {
    type: "contactPrefsUpdated"
    user: T.User
    fromContact: T.Contact
    toContact: T.Contact
  }

  export interface ContactRequestRejected extends Interface {
    type: "contactRequestRejected"
    user: T.User
    contactRequest: T.UserContactRequest
    contact_?: T.Contact
  }

  export interface ContactsList extends Interface {
    type: "contactsList"
    user: T.User
    contacts: T.Contact[]
  }

  export interface GroupDeletedUser extends Interface {
    type: "groupDeletedUser"
    user: T.User
    groupInfo: T.GroupInfo
  }

  export interface GroupLink extends Interface {
    type: "groupLink"
    user: T.User
    groupInfo: T.GroupInfo
    groupLink: T.GroupLink
  }

  export interface GroupLinkCreated extends Interface {
    type: "groupLinkCreated"
    user: T.User
    groupInfo: T.GroupInfo
    groupLink: T.GroupLink
  }

  export interface GroupLinkDeleted extends Interface {
    type: "groupLinkDeleted"
    user: T.User
    groupInfo: T.GroupInfo
  }

  export interface GroupCreated extends Interface {
    type: "groupCreated"
    user: T.User
    groupInfo: T.GroupInfo
  }

  export interface GroupMembers extends Interface {
    type: "groupMembers"
    user: T.User
    group: T.Group
  }

  export interface GroupUpdated extends Interface {
    type: "groupUpdated"
    user: T.User
    fromGroup: T.GroupInfo
    toGroup: T.GroupInfo
    member_?: T.GroupMember
  }

  export interface GroupsList extends Interface {
    type: "groupsList"
    user: T.User
    groups: T.GroupInfo[]
  }

  export interface Invitation extends Interface {
    type: "invitation"
    user: T.User
    connLinkInvitation: T.CreatedConnLink
    connection: T.PendingContactConnection
  }

  export interface LeftMemberUser extends Interface {
    type: "leftMemberUser"
    user: T.User
    groupInfo: T.GroupInfo
  }

  export interface MemberAccepted extends Interface {
    type: "memberAccepted"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
  }

  export interface MembersBlockedForAllUser extends Interface {
    type: "membersBlockedForAllUser"
    user: T.User
    groupInfo: T.GroupInfo
    members: T.GroupMember[]
    blocked: boolean
  }

  export interface MembersRoleUser extends Interface {
    type: "membersRoleUser"
    user: T.User
    groupInfo: T.GroupInfo
    members: T.GroupMember[]
    toRole: T.GroupMemberRole
  }

  export interface NewChatItems extends Interface {
    type: "newChatItems"
    user: T.User
    chatItems: T.AChatItem[]
  }

  export interface RcvFileAccepted extends Interface {
    type: "rcvFileAccepted"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface RcvFileAcceptedSndCancelled extends Interface {
    type: "rcvFileAcceptedSndCancelled"
    user: T.User
    rcvFileTransfer: T.RcvFileTransfer
  }

  export interface RcvFileCancelled extends Interface {
    type: "rcvFileCancelled"
    user: T.User
    chatItem_?: T.AChatItem
    rcvFileTransfer: T.RcvFileTransfer
  }

  export interface SentConfirmation extends Interface {
    type: "sentConfirmation"
    user: T.User
    connection: T.PendingContactConnection
    customUserProfile?: T.Profile
  }

  export interface SentGroupInvitation extends Interface {
    type: "sentGroupInvitation"
    user: T.User
    groupInfo: T.GroupInfo
    contact: T.Contact
    member: T.GroupMember
  }

  export interface SentInvitation extends Interface {
    type: "sentInvitation"
    user: T.User
    connection: T.PendingContactConnection
    customUserProfile?: T.Profile
  }

  export interface SndFileCancelled extends Interface {
    type: "sndFileCancelled"
    user: T.User
    chatItem_?: T.AChatItem
    fileTransferMeta: T.FileTransferMeta
    sndFileTransfers: T.SndFileTransfer[]
  }

  export interface UserAcceptedGroupSent extends Interface {
    type: "userAcceptedGroupSent"
    user: T.User
    groupInfo: T.GroupInfo
    hostContact?: T.Contact
  }

  export interface UserContactLink extends Interface {
    type: "userContactLink"
    user: T.User
    contactLink: T.UserContactLink
  }

  export interface UserContactLinkCreated extends Interface {
    type: "userContactLinkCreated"
    user: T.User
    connLinkContact: T.CreatedConnLink
  }

  export interface UserContactLinkDeleted extends Interface {
    type: "userContactLinkDeleted"
    user: T.User
  }

  export interface UserContactLinkUpdated extends Interface {
    type: "userContactLinkUpdated"
    user: T.User
    contactLink: T.UserContactLink
  }

  export interface UserDeletedMembers extends Interface {
    type: "userDeletedMembers"
    user: T.User
    groupInfo: T.GroupInfo
    members: T.GroupMember[]
    withMessages: boolean
  }

  export interface UserProfileUpdated extends Interface {
    type: "userProfileUpdated"
    user: T.User
    fromProfile: T.Profile
    toProfile: T.Profile
    updateSummary: T.UserProfileUpdateSummary
  }

  export interface UserProfileNoChange extends Interface {
    type: "userProfileNoChange"
    user: T.User
  }

  export interface UsersList extends Interface {
    type: "usersList"
    users: T.UserInfo[]
  }
}
