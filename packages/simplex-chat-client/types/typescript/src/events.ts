// API Events
// This file is generated automatically.

import * as T from "./types"

export type ChatEvent = 
  | CEvt.ContactConnected
  | CEvt.ContactUpdated
  | CEvt.ContactDeletedByContact
  | CEvt.ReceivedContactRequest
  | CEvt.NewMemberContactReceivedInv
  | CEvt.ContactSndReady
  | CEvt.NewChatItems
  | CEvt.ChatItemReaction
  | CEvt.ChatItemsDeleted
  | CEvt.ChatItemUpdated
  | CEvt.GroupChatItemsDeleted
  | CEvt.ChatItemsStatusesUpdated
  | CEvt.ReceivedGroupInvitation
  | CEvt.UserJoinedGroup
  | CEvt.GroupUpdated
  | CEvt.JoinedGroupMember
  | CEvt.MemberRole
  | CEvt.DeletedMember
  | CEvt.LeftMember
  | CEvt.DeletedMemberUser
  | CEvt.GroupDeleted
  | CEvt.ConnectedToGroupMember
  | CEvt.MemberAcceptedByOther
  | CEvt.MemberBlockedForAll
  | CEvt.GroupMemberUpdated
  | CEvt.RcvFileDescrReady
  | CEvt.RcvFileComplete
  | CEvt.SndFileCompleteXFTP
  | CEvt.RcvFileStart
  | CEvt.RcvFileSndCancelled
  | CEvt.RcvFileAccepted
  | CEvt.RcvFileError
  | CEvt.RcvFileWarning
  | CEvt.SndFileError
  | CEvt.SndFileWarning
  | CEvt.AcceptingContactRequest
  | CEvt.AcceptingBusinessRequest
  | CEvt.ContactConnecting
  | CEvt.BusinessLinkConnecting
  | CEvt.JoinedGroupMemberConnecting
  | CEvt.SentGroupInvitation
  | CEvt.GroupLinkConnecting
  | CEvt.MessageError
  | CEvt.ChatError
  | CEvt.ChatErrors
  | CEvt.Interface

export namespace CEvt {
  export type Tag = 
    | "contactConnected"
    | "contactUpdated"
    | "contactDeletedByContact"
    | "receivedContactRequest"
    | "newMemberContactReceivedInv"
    | "contactSndReady"
    | "newChatItems"
    | "chatItemReaction"
    | "chatItemsDeleted"
    | "chatItemUpdated"
    | "groupChatItemsDeleted"
    | "chatItemsStatusesUpdated"
    | "receivedGroupInvitation"
    | "userJoinedGroup"
    | "groupUpdated"
    | "joinedGroupMember"
    | "memberRole"
    | "deletedMember"
    | "leftMember"
    | "deletedMemberUser"
    | "groupDeleted"
    | "connectedToGroupMember"
    | "memberAcceptedByOther"
    | "memberBlockedForAll"
    | "groupMemberUpdated"
    | "rcvFileDescrReady"
    | "rcvFileComplete"
    | "sndFileCompleteXFTP"
    | "rcvFileStart"
    | "rcvFileSndCancelled"
    | "rcvFileAccepted"
    | "rcvFileError"
    | "rcvFileWarning"
    | "sndFileError"
    | "sndFileWarning"
    | "acceptingContactRequest"
    | "acceptingBusinessRequest"
    | "contactConnecting"
    | "businessLinkConnecting"
    | "joinedGroupMemberConnecting"
    | "sentGroupInvitation"
    | "groupLinkConnecting"
    | "messageError"
    | "chatError"
    | "chatErrors"
    | string

  export interface Interface {
    type: Tag
  }

  export interface ContactConnected extends Interface {
    type: "contactConnected"
    user: T.User
    contact: T.Contact
    userCustomProfile?: T.Profile
  }

  export interface ContactUpdated extends Interface {
    type: "contactUpdated"
    user: T.User
    fromContact: T.Contact
    toContact: T.Contact
  }

  export interface ContactDeletedByContact extends Interface {
    type: "contactDeletedByContact"
    user: T.User
    contact: T.Contact
  }

  export interface ReceivedContactRequest extends Interface {
    type: "receivedContactRequest"
    user: T.User
    contactRequest: T.UserContactRequest
    chat_?: T.AChat
  }

  export interface NewMemberContactReceivedInv extends Interface {
    type: "newMemberContactReceivedInv"
    user: T.User
    contact: T.Contact
    groupInfo: T.GroupInfo
    member: T.GroupMember
  }

  export interface ContactSndReady extends Interface {
    type: "contactSndReady"
    user: T.User
    contact: T.Contact
  }

  export interface NewChatItems extends Interface {
    type: "newChatItems"
    user: T.User
    chatItems: T.AChatItem[]
  }

  export interface ChatItemReaction extends Interface {
    type: "chatItemReaction"
    user: T.User
    added: boolean
    reaction: T.ACIReaction
  }

  export interface ChatItemsDeleted extends Interface {
    type: "chatItemsDeleted"
    user: T.User
    chatItemDeletions: T.ChatItemDeletion[]
    byUser: boolean
    timed: boolean
  }

  export interface ChatItemUpdated extends Interface {
    type: "chatItemUpdated"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface GroupChatItemsDeleted extends Interface {
    type: "groupChatItemsDeleted"
    user: T.User
    groupInfo: T.GroupInfo
    chatItemIDs: number[] // int64
    byUser: boolean
    member_?: T.GroupMember
  }

  export interface ChatItemsStatusesUpdated extends Interface {
    type: "chatItemsStatusesUpdated"
    user: T.User
    chatItems: T.AChatItem[]
  }

  export interface ReceivedGroupInvitation extends Interface {
    type: "receivedGroupInvitation"
    user: T.User
    groupInfo: T.GroupInfo
    contact: T.Contact
    fromMemberRole: T.GroupMemberRole
    memberRole: T.GroupMemberRole
  }

  export interface UserJoinedGroup extends Interface {
    type: "userJoinedGroup"
    user: T.User
    groupInfo: T.GroupInfo
    hostMember: T.GroupMember
  }

  export interface GroupUpdated extends Interface {
    type: "groupUpdated"
    user: T.User
    fromGroup: T.GroupInfo
    toGroup: T.GroupInfo
    member_?: T.GroupMember
  }

  export interface JoinedGroupMember extends Interface {
    type: "joinedGroupMember"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
  }

  export interface MemberRole extends Interface {
    type: "memberRole"
    user: T.User
    groupInfo: T.GroupInfo
    byMember: T.GroupMember
    member: T.GroupMember
    fromRole: T.GroupMemberRole
    toRole: T.GroupMemberRole
  }

  export interface DeletedMember extends Interface {
    type: "deletedMember"
    user: T.User
    groupInfo: T.GroupInfo
    byMember: T.GroupMember
    deletedMember: T.GroupMember
    withMessages: boolean
  }

  export interface LeftMember extends Interface {
    type: "leftMember"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
  }

  export interface DeletedMemberUser extends Interface {
    type: "deletedMemberUser"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
    withMessages: boolean
  }

  export interface GroupDeleted extends Interface {
    type: "groupDeleted"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
  }

  export interface ConnectedToGroupMember extends Interface {
    type: "connectedToGroupMember"
    user: T.User
    groupInfo: T.GroupInfo
    member: T.GroupMember
    memberContact?: T.Contact
  }

  export interface MemberAcceptedByOther extends Interface {
    type: "memberAcceptedByOther"
    user: T.User
    groupInfo: T.GroupInfo
    acceptingMember: T.GroupMember
    member: T.GroupMember
  }

  export interface MemberBlockedForAll extends Interface {
    type: "memberBlockedForAll"
    user: T.User
    groupInfo: T.GroupInfo
    byMember: T.GroupMember
    member: T.GroupMember
    blocked: boolean
  }

  export interface GroupMemberUpdated extends Interface {
    type: "groupMemberUpdated"
    user: T.User
    groupInfo: T.GroupInfo
    fromMember: T.GroupMember
    toMember: T.GroupMember
  }

  export interface RcvFileDescrReady extends Interface {
    type: "rcvFileDescrReady"
    user: T.User
    chatItem: T.AChatItem
    rcvFileTransfer: T.RcvFileTransfer
    rcvFileDescr: T.RcvFileDescr
  }

  export interface RcvFileComplete extends Interface {
    type: "rcvFileComplete"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface SndFileCompleteXFTP extends Interface {
    type: "sndFileCompleteXFTP"
    user: T.User
    chatItem: T.AChatItem
    fileTransferMeta: T.FileTransferMeta
  }

  export interface RcvFileStart extends Interface {
    type: "rcvFileStart"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface RcvFileSndCancelled extends Interface {
    type: "rcvFileSndCancelled"
    user: T.User
    chatItem: T.AChatItem
    rcvFileTransfer: T.RcvFileTransfer
  }

  export interface RcvFileAccepted extends Interface {
    type: "rcvFileAccepted"
    user: T.User
    chatItem: T.AChatItem
  }

  export interface RcvFileError extends Interface {
    type: "rcvFileError"
    user: T.User
    chatItem_?: T.AChatItem
    agentError: T.AgentErrorType
    rcvFileTransfer: T.RcvFileTransfer
  }

  export interface RcvFileWarning extends Interface {
    type: "rcvFileWarning"
    user: T.User
    chatItem_?: T.AChatItem
    agentError: T.AgentErrorType
    rcvFileTransfer: T.RcvFileTransfer
  }

  export interface SndFileError extends Interface {
    type: "sndFileError"
    user: T.User
    chatItem_?: T.AChatItem
    fileTransferMeta: T.FileTransferMeta
    errorMessage: string
  }

  export interface SndFileWarning extends Interface {
    type: "sndFileWarning"
    user: T.User
    chatItem_?: T.AChatItem
    fileTransferMeta: T.FileTransferMeta
    errorMessage: string
  }

  export interface AcceptingContactRequest extends Interface {
    type: "acceptingContactRequest"
    user: T.User
    contact: T.Contact
  }

  export interface AcceptingBusinessRequest extends Interface {
    type: "acceptingBusinessRequest"
    user: T.User
    groupInfo: T.GroupInfo
  }

  export interface ContactConnecting extends Interface {
    type: "contactConnecting"
    user: T.User
    contact: T.Contact
  }

  export interface BusinessLinkConnecting extends Interface {
    type: "businessLinkConnecting"
    user: T.User
    groupInfo: T.GroupInfo
    hostMember: T.GroupMember
    fromContact: T.Contact
  }

  export interface JoinedGroupMemberConnecting extends Interface {
    type: "joinedGroupMemberConnecting"
    user: T.User
    groupInfo: T.GroupInfo
    hostMember: T.GroupMember
    member: T.GroupMember
  }

  export interface SentGroupInvitation extends Interface {
    type: "sentGroupInvitation"
    user: T.User
    groupInfo: T.GroupInfo
    contact: T.Contact
    member: T.GroupMember
  }

  export interface GroupLinkConnecting extends Interface {
    type: "groupLinkConnecting"
    user: T.User
    groupInfo: T.GroupInfo
    hostMember: T.GroupMember
  }

  export interface MessageError extends Interface {
    type: "messageError"
    user: T.User
    severity: string
    errorMessage: string
  }

  export interface ChatError extends Interface {
    type: "chatError"
    chatError: T.ChatError
  }

  export interface ChatErrors extends Interface {
    type: "chatErrors"
    chatErrors: T.ChatError[]
  }
}
