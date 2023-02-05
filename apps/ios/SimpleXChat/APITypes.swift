//
//  SimpleXAPI.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

let jsonDecoder = getJSONDecoder()
let jsonEncoder = getJSONEncoder()

public enum ChatCommand {
    case showActiveUser
    case createActiveUser(profile: Profile)
    case listUsers
    case apiSetActiveUser(userId: Int64)
    case apiDeleteUser(userId: Int64, delSMPQueues: Bool)
    case startChat(subscribe: Bool, expire: Bool)
    case apiStopChat
    case apiActivateChat
    case apiSuspendChat(timeoutMicroseconds: Int)
    case setFilesFolder(filesFolder: String)
    case setIncognito(incognito: Bool)
    case apiExportArchive(config: ArchiveConfig)
    case apiImportArchive(config: ArchiveConfig)
    case apiDeleteStorage
    case apiStorageEncryption(config: DBEncryptionConfig)
    case apiGetChats(userId: Int64)
    case apiGetChat(type: ChatType, id: Int64, pagination: ChatPagination, search: String)
    case apiSendMessage(type: ChatType, id: Int64, file: String?, quotedItemId: Int64?, msg: MsgContent, live: Bool)
    case apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent, live: Bool)
    case apiDeleteChatItem(type: ChatType, id: Int64, itemId: Int64, mode: CIDeleteMode)
    case apiGetNtfToken
    case apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode)
    case apiVerifyToken(token: DeviceToken, nonce: String, code: String)
    case apiDeleteToken(token: DeviceToken)
    case apiGetNtfMessage(nonce: String, encNtfInfo: String)
    case apiNewGroup(userId: Int64, groupProfile: GroupProfile)
    case apiAddMember(groupId: Int64, contactId: Int64, memberRole: GroupMemberRole)
    case apiJoinGroup(groupId: Int64)
    case apiMemberRole(groupId: Int64, memberId: Int64, memberRole: GroupMemberRole)
    case apiRemoveMember(groupId: Int64, memberId: Int64)
    case apiLeaveGroup(groupId: Int64)
    case apiListMembers(groupId: Int64)
    case apiUpdateGroupProfile(groupId: Int64, groupProfile: GroupProfile)
    case apiCreateGroupLink(groupId: Int64)
    case apiDeleteGroupLink(groupId: Int64)
    case apiGetGroupLink(groupId: Int64)
    case apiGetUserSMPServers(userId: Int64)
    case apiSetUserSMPServers(userId: Int64, smpServers: [ServerCfg])
    case testSMPServer(userId: Int64, smpServer: String)
    case apiSetChatItemTTL(userId: Int64, seconds: Int64?)
    case apiGetChatItemTTL(userId: Int64)
    case apiSetNetworkConfig(networkConfig: NetCfg)
    case apiGetNetworkConfig
    case apiSetChatSettings(type: ChatType, id: Int64, chatSettings: ChatSettings)
    case apiContactInfo(contactId: Int64)
    case apiGroupMemberInfo(groupId: Int64, groupMemberId: Int64)
    case apiSwitchContact(contactId: Int64)
    case apiSwitchGroupMember(groupId: Int64, groupMemberId: Int64)
    case apiGetContactCode(contactId: Int64)
    case apiGetGroupMemberCode(groupId: Int64, groupMemberId: Int64)
    case apiVerifyContact(contactId: Int64, connectionCode: String?)
    case apiVerifyGroupMember(groupId: Int64, groupMemberId: Int64, connectionCode: String?)
    case apiAddContact(userId: Int64)
    case apiConnect(userId: Int64, connReq: String)
    case apiDeleteChat(type: ChatType, id: Int64)
    case apiClearChat(type: ChatType, id: Int64)
    case apiListContacts(userId: Int64)
    case apiUpdateProfile(userId: Int64, profile: Profile)
    case apiSetContactPrefs(contactId: Int64, preferences: Preferences)
    case apiSetContactAlias(contactId: Int64, localAlias: String)
    case apiSetConnectionAlias(connId: Int64, localAlias: String)
    case apiCreateMyAddress(userId: Int64)
    case apiDeleteMyAddress(userId: Int64)
    case apiShowMyAddress(userId: Int64)
    case apiAddressAutoAccept(userId: Int64, autoAccept: AutoAccept?)
    case apiAcceptContact(contactReqId: Int64)
    case apiRejectContact(contactReqId: Int64)
    // WebRTC calls
    case apiSendCallInvitation(contact: Contact, callType: CallType)
    case apiRejectCall(contact: Contact)
    case apiSendCallOffer(contact: Contact, callOffer: WebRTCCallOffer)
    case apiSendCallAnswer(contact: Contact, answer: WebRTCSession)
    case apiSendCallExtraInfo(contact: Contact, extraInfo: WebRTCExtraInfo)
    case apiEndCall(contact: Contact)
    case apiGetCallInvitations
    case apiCallStatus(contact: Contact, callStatus: WebRTCCallStatus)
    case apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64))
    case apiChatUnread(type: ChatType, id: Int64, unreadChat: Bool)
    case receiveFile(fileId: Int64, inline: Bool?)
    case showVersion
    case string(String)

    public var cmdString: String {
        get {
            switch self {
            case .showActiveUser: return "/u"
            case let .createActiveUser(profile): return "/create user \(profile.displayName) \(profile.fullName)"
            case .listUsers: return "/users"
            case let .apiSetActiveUser(userId): return "/_user \(userId)"
            case let .apiDeleteUser(userId, delSMPQueues): return "/_delete user \(userId) del_smp=\(onOff(delSMPQueues))"
            case let .startChat(subscribe, expire): return "/_start subscribe=\(onOff(subscribe)) expire=\(onOff(expire))"
            case .apiStopChat: return "/_stop"
            case .apiActivateChat: return "/_app activate"
            case let .apiSuspendChat(timeoutMicroseconds): return "/_app suspend \(timeoutMicroseconds)"
            case let .setFilesFolder(filesFolder): return "/_files_folder \(filesFolder)"
            case let .setIncognito(incognito): return "/incognito \(onOff(incognito))"
            case let .apiExportArchive(cfg): return "/_db export \(encodeJSON(cfg))"
            case let .apiImportArchive(cfg): return "/_db import \(encodeJSON(cfg))"
            case .apiDeleteStorage: return "/_db delete"
            case let .apiStorageEncryption(cfg): return "/_db encryption \(encodeJSON(cfg))"
            case let .apiGetChats(userId): return "/_get chats \(userId) pcc=on"
            case let .apiGetChat(type, id, pagination, search): return "/_get chat \(ref(type, id)) \(pagination.cmdString)" +
                (search == "" ? "" : " search=\(search)")
            case let .apiSendMessage(type, id, file, quotedItemId, mc, live):
                let msg = encodeJSON(ComposedMessage(filePath: file, quotedItemId: quotedItemId, msgContent: mc))
                return "/_send \(ref(type, id)) live=\(onOff(live)) json \(msg)"
            case let .apiUpdateChatItem(type, id, itemId, mc, live): return "/_update item \(ref(type, id)) \(itemId) live=\(onOff(live)) \(mc.cmdString)"
            case let .apiDeleteChatItem(type, id, itemId, mode): return "/_delete item \(ref(type, id)) \(itemId) \(mode.rawValue)"
            case .apiGetNtfToken: return "/_ntf get "
            case let .apiRegisterToken(token, notificationMode): return "/_ntf register \(token.cmdString) \(notificationMode.rawValue)"
            case let .apiVerifyToken(token, nonce, code): return "/_ntf verify \(token.cmdString) \(nonce) \(code)"
            case let .apiDeleteToken(token): return "/_ntf delete \(token.cmdString)"
            case let .apiGetNtfMessage(nonce, encNtfInfo): return "/_ntf message \(nonce) \(encNtfInfo)"
            case let .apiNewGroup(userId, groupProfile): return "/_group \(userId) \(encodeJSON(groupProfile))"
            case let .apiAddMember(groupId, contactId, memberRole): return "/_add #\(groupId) \(contactId) \(memberRole)"
            case let .apiJoinGroup(groupId): return "/_join #\(groupId)"
            case let .apiMemberRole(groupId, memberId, memberRole): return "/_member role #\(groupId) \(memberId) \(memberRole.rawValue)"
            case let .apiRemoveMember(groupId, memberId): return "/_remove #\(groupId) \(memberId)"
            case let .apiLeaveGroup(groupId): return "/_leave #\(groupId)"
            case let .apiListMembers(groupId): return "/_members #\(groupId)"
            case let .apiUpdateGroupProfile(groupId, groupProfile): return "/_group_profile #\(groupId) \(encodeJSON(groupProfile))"
            case let .apiCreateGroupLink(groupId): return "/_create link #\(groupId)"
            case let .apiDeleteGroupLink(groupId): return "/_delete link #\(groupId)"
            case let .apiGetGroupLink(groupId): return "/_get link #\(groupId)"
            case let .apiGetUserSMPServers(userId): return "/_smp \(userId)"
            case let .apiSetUserSMPServers(userId, smpServers): return "/_smp \(userId) \(smpServersStr(smpServers: smpServers))"
            case let .testSMPServer(userId, smpServer): return "/smp test \(userId) \(smpServer)"
            case let .apiSetChatItemTTL(userId, seconds): return "/_ttl \(userId) \(chatItemTTLStr(seconds: seconds))"
            case let .apiGetChatItemTTL(userId): return "/_ttl \(userId)"
            case let .apiSetNetworkConfig(networkConfig): return "/_network \(encodeJSON(networkConfig))"
            case .apiGetNetworkConfig: return "/network"
            case let .apiSetChatSettings(type, id, chatSettings): return "/_settings \(ref(type, id)) \(encodeJSON(chatSettings))"
            case let .apiContactInfo(contactId): return "/_info @\(contactId)"
            case let .apiGroupMemberInfo(groupId, groupMemberId): return "/_info #\(groupId) \(groupMemberId)"
            case let .apiSwitchContact(contactId): return "/_switch @\(contactId)"
            case let .apiSwitchGroupMember(groupId, groupMemberId): return "/_switch #\(groupId) \(groupMemberId)"
            case let .apiGetContactCode(contactId): return "/_get code @\(contactId)"
            case let .apiGetGroupMemberCode(groupId, groupMemberId): return "/_get code #\(groupId) \(groupMemberId)"
            case let .apiVerifyContact(contactId, .some(connectionCode)): return "/_verify code @\(contactId) \(connectionCode)"
            case let .apiVerifyContact(contactId, .none): return "/_verify code @\(contactId)"
            case let .apiVerifyGroupMember(groupId, groupMemberId, .some(connectionCode)): return "/_verify code #\(groupId) \(groupMemberId) \(connectionCode)"
            case let .apiVerifyGroupMember(groupId, groupMemberId, .none): return "/_verify code #\(groupId) \(groupMemberId)"
            case let .apiAddContact(userId): return "/_connect \(userId)"
            case let .apiConnect(userId, connReq): return "/_connect \(userId) \(connReq)"
            case let .apiDeleteChat(type, id): return "/_delete \(ref(type, id))"
            case let .apiClearChat(type, id): return "/_clear chat \(ref(type, id))"
            case let .apiListContacts(userId): return "/_contacts \(userId)"
            case let .apiUpdateProfile(userId, profile): return "/_profile \(userId) \(encodeJSON(profile))"
            case let .apiSetContactPrefs(contactId, preferences): return "/_set prefs @\(contactId) \(encodeJSON(preferences))"
            case let .apiSetContactAlias(contactId, localAlias): return "/_set alias @\(contactId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiSetConnectionAlias(connId, localAlias): return "/_set alias :\(connId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiCreateMyAddress(userId): return "/_address \(userId)"
            case let .apiDeleteMyAddress(userId): return "/_delete_address \(userId)"
            case let .apiShowMyAddress(userId): return "/_show_address \(userId)"
            case let .apiAddressAutoAccept(userId, autoAccept): return "/_auto_accept \(userId) \(AutoAccept.cmdString(autoAccept))"
            case let .apiAcceptContact(contactReqId): return "/_accept \(contactReqId)"
            case let .apiRejectContact(contactReqId): return "/_reject \(contactReqId)"
            case let .apiSendCallInvitation(contact, callType): return "/_call invite @\(contact.apiId) \(encodeJSON(callType))"
            case let .apiRejectCall(contact): return "/_call reject @\(contact.apiId)"
            case let .apiSendCallOffer(contact, callOffer): return "/_call offer @\(contact.apiId) \(encodeJSON(callOffer))"
            case let .apiSendCallAnswer(contact, answer): return "/_call answer @\(contact.apiId) \(encodeJSON(answer))"
            case let .apiSendCallExtraInfo(contact, extraInfo): return "/_call extra @\(contact.apiId) \(encodeJSON(extraInfo))"
            case let .apiEndCall(contact): return "/_call end @\(contact.apiId)"
            case .apiGetCallInvitations: return "/_call get"
            case let .apiCallStatus(contact, callStatus): return "/_call status @\(contact.apiId) \(callStatus.rawValue)"
            case let .apiChatRead(type, id, itemRange: (from, to)): return "/_read chat \(ref(type, id)) from=\(from) to=\(to)"
            case let .apiChatUnread(type, id, unreadChat): return "/_unread chat \(ref(type, id)) \(onOff(unreadChat))"
            case let .receiveFile(fileId, inline):
                if let inline = inline {
                    return "/freceive \(fileId) inline=\(onOff(inline))"
                }
                return "/freceive \(fileId)"
            case .showVersion: return "/version"
            case let .string(str): return str
            }
        }
    }

    public var cmdType: String {
        get {
            switch self {
            case .showActiveUser: return "showActiveUser"
            case .createActiveUser: return "createActiveUser"
            case .listUsers: return "listUsers"
            case .apiSetActiveUser: return "apiSetActiveUser"
            case .apiDeleteUser: return "apiDeleteUser"
            case .startChat: return "startChat"
            case .apiStopChat: return "apiStopChat"
            case .apiActivateChat: return "apiActivateChat"
            case .apiSuspendChat: return "apiSuspendChat"
            case .setFilesFolder: return "setFilesFolder"
            case .setIncognito: return "setIncognito"
            case .apiExportArchive: return "apiExportArchive"
            case .apiImportArchive: return "apiImportArchive"
            case .apiDeleteStorage: return "apiDeleteStorage"
            case .apiStorageEncryption: return "apiStorageEncryption"
            case .apiGetChats: return "apiGetChats"
            case .apiGetChat: return "apiGetChat"
            case .apiSendMessage: return "apiSendMessage"
            case .apiUpdateChatItem: return "apiUpdateChatItem"
            case .apiDeleteChatItem: return "apiDeleteChatItem"
            case .apiGetNtfToken: return "apiGetNtfToken"
            case .apiRegisterToken: return "apiRegisterToken"
            case .apiVerifyToken: return "apiVerifyToken"
            case .apiDeleteToken: return "apiDeleteToken"
            case .apiGetNtfMessage: return "apiGetNtfMessage"
            case .apiNewGroup: return "apiNewGroup"
            case .apiAddMember: return "apiAddMember"
            case .apiJoinGroup: return "apiJoinGroup"
            case .apiMemberRole: return "apiMemberRole"
            case .apiRemoveMember: return "apiRemoveMember"
            case .apiLeaveGroup: return "apiLeaveGroup"
            case .apiListMembers: return "apiListMembers"
            case .apiUpdateGroupProfile: return "apiUpdateGroupProfile"
            case .apiCreateGroupLink: return "apiCreateGroupLink"
            case .apiDeleteGroupLink: return "apiDeleteGroupLink"
            case .apiGetGroupLink: return "apiGetGroupLink"
            case .apiGetUserSMPServers: return "apiGetUserSMPServers"
            case .apiSetUserSMPServers: return "apiSetUserSMPServers"
            case .testSMPServer: return "testSMPServer"
            case .apiSetChatItemTTL: return "apiSetChatItemTTL"
            case .apiGetChatItemTTL: return "apiGetChatItemTTL"
            case .apiSetNetworkConfig: return "apiSetNetworkConfig"
            case .apiGetNetworkConfig: return "apiGetNetworkConfig"
            case .apiSetChatSettings: return "apiSetChatSettings"
            case .apiContactInfo: return "apiContactInfo"
            case .apiGroupMemberInfo: return "apiGroupMemberInfo"
            case .apiSwitchContact: return "apiSwitchContact"
            case .apiSwitchGroupMember: return "apiSwitchGroupMember"
            case .apiGetContactCode: return "apiGetContactCode"
            case .apiGetGroupMemberCode: return "apiGetGroupMemberCode"
            case .apiVerifyContact: return "apiVerifyContact"
            case .apiVerifyGroupMember: return "apiVerifyGroupMember"
            case .apiAddContact: return "apiAddContact"
            case .apiConnect: return "apiConnect"
            case .apiDeleteChat: return "apiDeleteChat"
            case .apiClearChat: return "apiClearChat"
            case .apiListContacts: return "apiListContacts"
            case .apiUpdateProfile: return "apiUpdateProfile"
            case .apiSetContactPrefs: return "apiSetContactPrefs"
            case .apiSetContactAlias: return "apiSetContactAlias"
            case .apiSetConnectionAlias: return "apiSetConnectionAlias"
            case .apiCreateMyAddress: return "apiCreateMyAddress"
            case .apiDeleteMyAddress: return "apiDeleteMyAddress"
            case .apiShowMyAddress: return "apiShowMyAddress"
            case .apiAddressAutoAccept: return "apiAddressAutoAccept"
            case .apiAcceptContact: return "apiAcceptContact"
            case .apiRejectContact: return "apiRejectContact"
            case .apiSendCallInvitation: return "apiSendCallInvitation"
            case .apiRejectCall: return "apiRejectCall"
            case .apiSendCallOffer: return "apiSendCallOffer"
            case .apiSendCallAnswer: return "apiSendCallAnswer"
            case .apiSendCallExtraInfo: return "apiSendCallExtraInfo"
            case .apiEndCall: return "apiEndCall"
            case .apiGetCallInvitations: return "apiGetCallInvitations"
            case .apiCallStatus: return "apiCallStatus"
            case .apiChatRead: return "apiChatRead"
            case .apiChatUnread: return "apiChatUnread"
            case .receiveFile: return "receiveFile"
            case .showVersion: return "showVersion"
            case .string: return "console command"
            }
        }
    }

    func ref(_ type: ChatType, _ id: Int64) -> String {
        "\(type.rawValue)\(id)"
    }

    func smpServersStr(smpServers: [ServerCfg]) -> String {
        smpServers.isEmpty ? "default" : encodeJSON(SMPServersConfig(smpServers: smpServers))
    }

    func chatItemTTLStr(seconds: Int64?) -> String {
        if let seconds = seconds {
            return String(seconds)
        } else {
            return "none"
        }
    }

    public var obfuscated: ChatCommand {
        switch self {
        case let .apiStorageEncryption(cfg):
            return .apiStorageEncryption(config: DBEncryptionConfig(currentKey: obfuscate(cfg.currentKey), newKey: obfuscate(cfg.newKey)))
        default: return self
        }
    }

    private func obfuscate(_ s: String) -> String {
        s == "" ? "" : "***"
    }

    private func onOff(_ b: Bool) -> String {
        b ? "on" : "off"
    }
}

struct APIResponse: Decodable {
    var resp: ChatResponse
}

public enum ChatResponse: Decodable, Error {
    case response(type: String, json: String)
    case activeUser(user: User)
    case usersList(users: [UserInfo])
    case chatStarted
    case chatRunning
    case chatStopped
    case chatSuspended
    case apiChats(user: User, chats: [ChatData])
    case apiChat(user: User, chat: ChatData)
    case userSMPServers(user: User, smpServers: [ServerCfg], presetSMPServers: [String])
    case smpTestResult(user: User, smpTestFailure: SMPTestFailure?)
    case chatItemTTL(user: User, chatItemTTL: Int64?)
    case networkConfig(networkConfig: NetCfg)
    case contactInfo(user: User, contact: Contact, connectionStats: ConnectionStats, customUserProfile: Profile?)
    case groupMemberInfo(user: User, groupInfo: GroupInfo, member: GroupMember, connectionStats_: ConnectionStats?)
    case contactCode(user: User, contact: Contact, connectionCode: String)
    case groupMemberCode(user: User, groupInfo: GroupInfo, member: GroupMember, connectionCode: String)
    case connectionVerified(user: User, verified: Bool, expectedCode: String)
    case invitation(user: User, connReqInvitation: String)
    case sentConfirmation(user: User)
    case sentInvitation(user: User)
    case contactAlreadyExists(user: User, contact: Contact)
    case contactDeleted(user: User, contact: Contact)
    case chatCleared(user: User, chatInfo: ChatInfo)
    case userProfileNoChange(user: User)
    case userProfileUpdated(user: User, fromProfile: Profile, toProfile: Profile)
    case contactAliasUpdated(user: User, toContact: Contact)
    case connectionAliasUpdated(user: User, toConnection: PendingContactConnection)
    case contactPrefsUpdated(user: User, fromContact: Contact, toContact: Contact)
    case userContactLink(user: User, contactLink: UserContactLink)
    case userContactLinkUpdated(user: User, contactLink: UserContactLink)
    case userContactLinkCreated(user: User, connReqContact: String)
    case userContactLinkDeleted(user: User)
    case contactConnected(user: User, contact: Contact, userCustomProfile: Profile?)
    case contactConnecting(user: User, contact: Contact)
    case receivedContactRequest(user: User, contactRequest: UserContactRequest)
    case acceptingContactRequest(user: User, contact: Contact)
    case contactRequestRejected(user: User)
    case contactUpdated(user: User, toContact: Contact)
    case contactsSubscribed(server: String, contactRefs: [ContactRef])
    case contactsDisconnected(server: String, contactRefs: [ContactRef])
    case contactSubError(user: User, contact: Contact, chatError: ChatError)
    case contactSubSummary(user: User, contactSubscriptions: [ContactSubStatus])
    case groupSubscribed(user: User, groupInfo: GroupInfo)
    case memberSubErrors(user: User, memberSubErrors: [MemberSubError])
    case groupEmpty(user: User, groupInfo: GroupInfo)
    case userContactLinkSubscribed
    case newChatItem(user: User, chatItem: AChatItem)
    case chatItemStatusUpdated(user: User, chatItem: AChatItem)
    case chatItemUpdated(user: User, chatItem: AChatItem)
    case chatItemDeleted(user: User, deletedChatItem: AChatItem, toChatItem: AChatItem?, byUser: Bool)
    case contactsList(user: User, contacts: [Contact])
    // group events
    case groupCreated(user: User, groupInfo: GroupInfo)
    case sentGroupInvitation(user: User, groupInfo: GroupInfo, contact: Contact, member: GroupMember)
    case userAcceptedGroupSent(user: User, groupInfo: GroupInfo, hostContact: Contact?)
    case userDeletedMember(user: User, groupInfo: GroupInfo, member: GroupMember)
    case leftMemberUser(user: User, groupInfo: GroupInfo)
    case groupMembers(user: User, group: Group)
    case receivedGroupInvitation(user: User, groupInfo: GroupInfo, contact: Contact, memberRole: GroupMemberRole)
    case groupDeletedUser(user: User, groupInfo: GroupInfo)
    case joinedGroupMemberConnecting(user: User, groupInfo: GroupInfo, hostMember: GroupMember, member: GroupMember)
    case memberRole(user: User, groupInfo: GroupInfo, byMember: GroupMember, member: GroupMember, fromRole: GroupMemberRole, toRole: GroupMemberRole)
    case memberRoleUser(user: User, groupInfo: GroupInfo, member: GroupMember, fromRole: GroupMemberRole, toRole: GroupMemberRole)
    case deletedMemberUser(user: User, groupInfo: GroupInfo, member: GroupMember)
    case deletedMember(user: User, groupInfo: GroupInfo, byMember: GroupMember, deletedMember: GroupMember)
    case leftMember(user: User, groupInfo: GroupInfo, member: GroupMember)
    case groupDeleted(user: User, groupInfo: GroupInfo, member: GroupMember)
    case contactsMerged(user: User, intoContact: Contact, mergedContact: Contact)
    case groupInvitation(user: User, groupInfo: GroupInfo) // unused
    case userJoinedGroup(user: User, groupInfo: GroupInfo)
    case joinedGroupMember(user: User, groupInfo: GroupInfo, member: GroupMember)
    case connectedToGroupMember(user: User, groupInfo: GroupInfo, member: GroupMember)
    case groupRemoved(user: User, groupInfo: GroupInfo) // unused
    case groupUpdated(user: User, toGroup: GroupInfo)
    case groupLinkCreated(user: User, groupInfo: GroupInfo, connReqContact: String)
    case groupLink(user: User, groupInfo: GroupInfo, connReqContact: String)
    case groupLinkDeleted(user: User, groupInfo: GroupInfo)
    // receiving file events
    case rcvFileAccepted(user: User, chatItem: AChatItem)
    case rcvFileAcceptedSndCancelled(user: User, rcvFileTransfer: RcvFileTransfer)
    case rcvFileStart(user: User, chatItem: AChatItem)
    case rcvFileComplete(user: User, chatItem: AChatItem)
    // sending file events
    case sndFileStart(user: User, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileComplete(user: User, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileCancelled(chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileRcvCancelled(user: User, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndGroupFileCancelled(user: User, chatItem: AChatItem, fileTransferMeta: FileTransferMeta, sndFileTransfers: [SndFileTransfer])
    case callInvitation(callInvitation: RcvCallInvitation)
    case callOffer(user: User, contact: Contact, callType: CallType, offer: WebRTCSession, sharedKey: String?, askConfirmation: Bool)
    case callAnswer(user: User, contact: Contact, answer: WebRTCSession)
    case callExtraInfo(user: User, contact: Contact, extraInfo: WebRTCExtraInfo)
    case callEnded(user: User, contact: Contact)
    case callInvitations(callInvitations: [RcvCallInvitation])
    case ntfTokenStatus(status: NtfTknStatus)
    case ntfToken(token: DeviceToken, status: NtfTknStatus, ntfMode: NotificationsMode)
    case ntfMessages(user_: User?, connEntity: ConnectionEntity?, msgTs: Date?, ntfMessages: [NtfMsgInfo])
    case newContactConnection(user: User, connection: PendingContactConnection)
    case contactConnectionDeleted(user: User, connection: PendingContactConnection)
    case versionInfo(versionInfo: CoreVersionInfo)
    case cmdOk(user: User?)
    case chatCmdError(user: User?, chatError: ChatError)
    case chatError(user: User?, chatError: ChatError)

    public var responseType: String {
        get {
            switch self {
            case let .response(type, _): return "* \(type)"
            case .activeUser: return "activeUser"
            case .usersList: return "usersList"
            case .chatStarted: return "chatStarted"
            case .chatRunning: return "chatRunning"
            case .chatStopped: return "chatStopped"
            case .chatSuspended: return "chatSuspended"
            case .apiChats: return "apiChats"
            case .apiChat: return "apiChat"
            case .userSMPServers: return "userSMPServers"
            case .smpTestResult: return "smpTestResult"
            case .chatItemTTL: return "chatItemTTL"
            case .networkConfig: return "networkConfig"
            case .contactInfo: return "contactInfo"
            case .groupMemberInfo: return "groupMemberInfo"
            case .contactCode: return "contactCode"
            case .groupMemberCode: return "groupMemberCode"
            case .connectionVerified: return "connectionVerified"
            case .invitation: return "invitation"
            case .sentConfirmation: return "sentConfirmation"
            case .sentInvitation: return "sentInvitation"
            case .contactAlreadyExists: return "contactAlreadyExists"
            case .contactDeleted: return "contactDeleted"
            case .chatCleared: return "chatCleared"
            case .userProfileNoChange: return "userProfileNoChange"
            case .userProfileUpdated: return "userProfileUpdated"
            case .contactAliasUpdated: return "contactAliasUpdated"
            case .connectionAliasUpdated: return "connectionAliasUpdated"
            case .contactPrefsUpdated: return "contactPrefsUpdated"
            case .userContactLink: return "userContactLink"
            case .userContactLinkUpdated: return "userContactLinkUpdated"
            case .userContactLinkCreated: return "userContactLinkCreated"
            case .userContactLinkDeleted: return "userContactLinkDeleted"
            case .contactConnected: return "contactConnected"
            case .contactConnecting: return "contactConnecting"
            case .receivedContactRequest: return "receivedContactRequest"
            case .acceptingContactRequest: return "acceptingContactRequest"
            case .contactRequestRejected: return "contactRequestRejected"
            case .contactUpdated: return "contactUpdated"
            case .contactsSubscribed: return "contactsSubscribed"
            case .contactsDisconnected: return "contactsDisconnected"
            case .contactSubError: return "contactSubError"
            case .contactSubSummary: return "contactSubSummary"
            case .groupSubscribed: return "groupSubscribed"
            case .memberSubErrors: return "memberSubErrors"
            case .groupEmpty: return "groupEmpty"
            case .userContactLinkSubscribed: return "userContactLinkSubscribed"
            case .newChatItem: return "newChatItem"
            case .chatItemStatusUpdated: return "chatItemStatusUpdated"
            case .chatItemUpdated: return "chatItemUpdated"
            case .chatItemDeleted: return "chatItemDeleted"
            case .contactsList: return "contactsList"
            case .groupCreated: return "groupCreated"
            case .sentGroupInvitation: return "sentGroupInvitation"
            case .userAcceptedGroupSent: return "userAcceptedGroupSent"
            case .userDeletedMember: return "userDeletedMember"
            case .leftMemberUser: return "leftMemberUser"
            case .groupMembers: return "groupMembers"
            case .receivedGroupInvitation: return "receivedGroupInvitation"
            case .groupDeletedUser: return "groupDeletedUser"
            case .joinedGroupMemberConnecting: return "joinedGroupMemberConnecting"
            case .memberRole: return "memberRole"
            case .memberRoleUser: return "memberRoleUser"
            case .deletedMemberUser: return "deletedMemberUser"
            case .deletedMember: return "deletedMember"
            case .leftMember: return "leftMember"
            case .groupDeleted: return "groupDeleted"
            case .contactsMerged: return "contactsMerged"
            case .groupInvitation: return "groupInvitation"
            case .userJoinedGroup: return "userJoinedGroup"
            case .joinedGroupMember: return "joinedGroupMember"
            case .connectedToGroupMember: return "connectedToGroupMember"
            case .groupRemoved: return "groupRemoved"
            case .groupUpdated: return "groupUpdated"
            case .groupLinkCreated: return "groupLinkCreated"
            case .groupLink: return "groupLink"
            case .groupLinkDeleted: return "groupLinkDeleted"
            case .rcvFileAccepted: return "rcvFileAccepted"
            case .rcvFileAcceptedSndCancelled: return "rcvFileAcceptedSndCancelled"
            case .rcvFileStart: return "rcvFileStart"
            case .rcvFileComplete: return "rcvFileComplete"
            case .sndFileStart: return "sndFileStart"
            case .sndFileComplete: return "sndFileComplete"
            case .sndFileCancelled: return "sndFileCancelled"
            case .sndFileRcvCancelled: return "sndFileRcvCancelled"
            case .sndGroupFileCancelled: return "sndGroupFileCancelled"
            case .callInvitation: return "callInvitation"
            case .callOffer: return "callOffer"
            case .callAnswer: return "callAnswer"
            case .callExtraInfo: return "callExtraInfo"
            case .callEnded: return "callEnded"
            case .callInvitations: return "callInvitations"
            case .ntfTokenStatus: return "ntfTokenStatus"
            case .ntfToken: return "ntfToken"
            case .ntfMessages: return "ntfMessages"
            case .newContactConnection: return "newContactConnection"
            case .contactConnectionDeleted: return "contactConnectionDeleted"
            case .versionInfo: return "versionInfo"
            case .cmdOk: return "cmdOk"
            case .chatCmdError: return "chatCmdError"
            case .chatError: return "chatError"
            }
        }
    }

    public var details: String {
        get {
            switch self {
            case let .response(_, json): return json
            case let .activeUser(user): return String(describing: user)
            case let .usersList(users): return String(describing: users)
            case .chatStarted: return noDetails
            case .chatRunning: return noDetails
            case .chatStopped: return noDetails
            case .chatSuspended: return noDetails
            case let .apiChats(u, chats): return withUser(u, String(describing: chats))
            case let .apiChat(u, chat): return withUser(u, String(describing: chat))
            case let .userSMPServers(u, smpServers, presetServers): return withUser(u, "smpServers: \(String(describing: smpServers))\npresetServers: \(String(describing: presetServers))")
            case let .smpTestResult(u, smpTestFailure): return withUser(u, String(describing: smpTestFailure))
            case let .chatItemTTL(u, chatItemTTL): return withUser(u, String(describing: chatItemTTL))
            case let .networkConfig(networkConfig): return String(describing: networkConfig)
            case let .contactInfo(u, contact, connectionStats, customUserProfile): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))\ncustomUserProfile: \(String(describing: customUserProfile))")
            case let .groupMemberInfo(u, groupInfo, member, connectionStats_): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats_: \(String(describing: connectionStats_)))")
            case let .contactCode(u, contact, connectionCode): return withUser(u, "contact: \(String(describing: contact))\nconnectionCode: \(connectionCode)")
            case let .groupMemberCode(u, groupInfo, member, connectionCode): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionCode: \(connectionCode)")
            case let .connectionVerified(u, verified, expectedCode): return withUser(u, "verified: \(verified)\nconnectionCode: \(expectedCode)")
            case let .invitation(u, connReqInvitation): return withUser(u, connReqInvitation)
            case .sentConfirmation: return noDetails
            case .sentInvitation: return noDetails
            case let .contactAlreadyExists(u, contact): return withUser(u, String(describing: contact))
            case let .contactDeleted(u, contact): return withUser(u, String(describing: contact))
            case let .chatCleared(u, chatInfo): return withUser(u, String(describing: chatInfo))
            case .userProfileNoChange: return noDetails
            case let .userProfileUpdated(u, _, toProfile): return withUser(u, String(describing: toProfile))
            case let .contactAliasUpdated(u, toContact): return withUser(u, String(describing: toContact))
            case let .connectionAliasUpdated(u, toConnection): return withUser(u, String(describing: toConnection))
            case let .contactPrefsUpdated(u, fromContact, toContact): return withUser(u, "fromContact: \(String(describing: fromContact))\ntoContact: \(String(describing: toContact))")
            case let .userContactLink(u, contactLink): return withUser(u, contactLink.responseDetails)
            case let .userContactLinkUpdated(u, contactLink): return withUser(u, contactLink.responseDetails)
            case let .userContactLinkCreated(u, connReq): return withUser(u, connReq)
            case .userContactLinkDeleted: return noDetails
            case let .contactConnected(u, contact, _): return withUser(u, String(describing: contact))
            case let .contactConnecting(u, contact): return withUser(u, String(describing: contact))
            case let .receivedContactRequest(u, contactRequest): return withUser(u, String(describing: contactRequest))
            case let .acceptingContactRequest(u, contact): return withUser(u, String(describing: contact))
            case .contactRequestRejected: return noDetails
            case let .contactUpdated(u, toContact): return withUser(u, String(describing: toContact))
            case let .contactsSubscribed(server, contactRefs): return "server: \(server)\ncontacts:\n\(String(describing: contactRefs))"
            case let .contactsDisconnected(server, contactRefs): return "server: \(server)\ncontacts:\n\(String(describing: contactRefs))"
            case let .contactSubError(u, contact, chatError): return withUser(u, "contact:\n\(String(describing: contact))\nerror:\n\(String(describing: chatError))")
            case let .contactSubSummary(u, contactSubscriptions): return withUser(u, String(describing: contactSubscriptions))
            case let .groupSubscribed(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .memberSubErrors(u, memberSubErrors): return withUser(u, String(describing: memberSubErrors))
            case let .groupEmpty(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case .userContactLinkSubscribed: return noDetails
            case let .newChatItem(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemStatusUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemDeleted(u, deletedChatItem, toChatItem, byUser): return withUser(u, "deletedChatItem:\n\(String(describing: deletedChatItem))\ntoChatItem:\n\(String(describing: toChatItem))\nbyUser: \(byUser)")
            case let .contactsList(u, contacts): return withUser(u, String(describing: contacts))
            case let .groupCreated(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .sentGroupInvitation(u, groupInfo, contact, member): return withUser(u, "groupInfo: \(groupInfo)\ncontact: \(contact)\nmember: \(member)")
            case let .userAcceptedGroupSent(u, groupInfo, hostContact): return withUser(u, "groupInfo: \(groupInfo)\nhostContact: \(String(describing: hostContact))")
            case let .userDeletedMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .leftMemberUser(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .groupMembers(u, group): return withUser(u, String(describing: group))
            case let .receivedGroupInvitation(u, groupInfo, contact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\ncontact: \(contact)\nmemberRole: \(memberRole)")
            case let .groupDeletedUser(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .joinedGroupMemberConnecting(u, groupInfo, hostMember, member): return withUser(u, "groupInfo: \(groupInfo)\nhostMember: \(hostMember)\nmember: \(member)")
            case let .memberRole(u, groupInfo, byMember, member, fromRole, toRole): return withUser(u, "groupInfo: \(groupInfo)\nbyMember: \(byMember)\nmember: \(member)\nfromRole: \(fromRole)\ntoRole: \(toRole)")
            case let .memberRoleUser(u, groupInfo, member, fromRole, toRole): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)\nfromRole: \(fromRole)\ntoRole: \(toRole)")
            case let .deletedMemberUser(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .deletedMember(u, groupInfo, byMember, deletedMember): return withUser(u, "groupInfo: \(groupInfo)\nbyMember: \(byMember)\ndeletedMember: \(deletedMember)")
            case let .leftMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .groupDeleted(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .contactsMerged(u, intoContact, mergedContact): return withUser(u, "intoContact: \(intoContact)\nmergedContact: \(mergedContact)")
            case let .groupInvitation(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .userJoinedGroup(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .joinedGroupMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .connectedToGroupMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
            case let .groupRemoved(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .groupUpdated(u, toGroup): return withUser(u, String(describing: toGroup))
            case let .groupLinkCreated(u, groupInfo, connReqContact): return withUser(u, "groupInfo: \(groupInfo)\nconnReqContact: \(connReqContact)")
            case let .groupLink(u, groupInfo, connReqContact): return withUser(u, "groupInfo: \(groupInfo)\nconnReqContact: \(connReqContact)")
            case let .groupLinkDeleted(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .rcvFileAccepted(u, chatItem): return withUser(u, String(describing: chatItem))
            case .rcvFileAcceptedSndCancelled: return noDetails
            case let .rcvFileStart(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .rcvFileComplete(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .sndFileStart(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileComplete(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileCancelled(chatItem, _): return String(describing: chatItem)
            case let .sndFileRcvCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndGroupFileCancelled(u, chatItem, _, _): return withUser(u, String(describing: chatItem))
            case let .callInvitation(inv): return String(describing: inv)
            case let .callOffer(u, contact, callType, offer, sharedKey, askConfirmation): return withUser(u, "contact: \(contact.id)\ncallType: \(String(describing: callType))\nsharedKey: \(sharedKey ?? "")\naskConfirmation: \(askConfirmation)\noffer: \(String(describing: offer))")
            case let .callAnswer(u, contact, answer): return withUser(u, "contact: \(contact.id)\nanswer: \(String(describing: answer))")
            case let .callExtraInfo(u, contact, extraInfo): return withUser(u, "contact: \(contact.id)\nextraInfo: \(String(describing: extraInfo))")
            case let .callEnded(u, contact): return withUser(u, "contact: \(contact.id)")
            case let .callInvitations(invs): return String(describing: invs)
            case let .ntfTokenStatus(status): return String(describing: status)
            case let .ntfToken(token, status, ntfMode): return "token: \(token)\nstatus: \(status.rawValue)\nntfMode: \(ntfMode.rawValue)"
            case let .ntfMessages(u, connEntity, msgTs, ntfMessages): return withUser(u, "connEntity: \(String(describing: connEntity))\nmsgTs: \(String(describing: msgTs))\nntfMessages: \(String(describing: ntfMessages))")
            case let .newContactConnection(u, connection): return withUser(u, String(describing: connection))
            case let .contactConnectionDeleted(u, connection): return withUser(u, String(describing: connection))
            case let .versionInfo(versionInfo): return String(describing: versionInfo)
            case .cmdOk: return noDetails
            case let .chatCmdError(u, chatError): return withUser(u, String(describing: chatError))
            case let .chatError(u, chatError): return withUser(u, String(describing: chatError))
            }
        }
    }

    private var noDetails: String { get { "\(responseType): no details" } }

    private func withUser(_ u: User?, _ s: String) -> String {
        if let id = u?.userId {
            return "userId: \(id)\n\(s)"
        }
        return s
    }
}

public enum ChatPagination {
    case last(count: Int)
    case after(chatItemId: Int64, count: Int)
    case before(chatItemId: Int64, count: Int)

    var cmdString: String {
        switch self {
        case let .last(count): return "count=\(count)"
        case let .after(chatItemId, count): return "after=\(chatItemId) count=\(count)"
        case let .before(chatItemId, count): return "before=\(chatItemId) count=\(count)"
        }
    }
}

struct ComposedMessage: Encodable {
    var filePath: String?
    var quotedItemId: Int64?
    var msgContent: MsgContent
}

public struct ArchiveConfig: Encodable {
    var archivePath: String
    var disableCompression: Bool?

    public init(archivePath: String, disableCompression: Bool? = nil) {
        self.archivePath = archivePath
        self.disableCompression = disableCompression
    }
}

public struct DBEncryptionConfig: Codable {
    public init(currentKey: String, newKey: String) {
        self.currentKey = currentKey
        self.newKey = newKey
    }

    public var currentKey: String
    public var newKey: String
}

struct SMPServersConfig: Encodable {
    var smpServers: [ServerCfg]
}

public struct ServerCfg: Identifiable, Equatable, Codable {
    public var server: String
    public var preset: Bool
    public var tested: Bool?
    public var enabled: Bool
    var createdAt = Date()
//    public var sendEnabled: Bool // can we potentially want to prevent sending on the servers we use to receive?
// Even if we don't see the use case, it's probably better to allow it in the model
// In any case, "trusted/known" servers are out of scope of this change

    public init(server: String, preset: Bool, tested: Bool?, enabled: Bool) {
        self.server = server
        self.preset = preset
        self.tested = tested
        self.enabled = enabled
    }

    public static func == (l: ServerCfg, r: ServerCfg) -> Bool {
        l.server == r.server && l.preset == r.preset && l.tested == r.tested && l.enabled == r.enabled
    }

    public var id: String { "\(server) \(createdAt)" }

    public static var empty = ServerCfg(server: "", preset: false, tested: nil, enabled: true)

    public var isEmpty: Bool {
        server.trimmingCharacters(in: .whitespaces) == ""
    }

    public struct SampleData {
        public var preset: ServerCfg
        public var custom: ServerCfg
        public var untested: ServerCfg
    }

    public static var sampleData = SampleData(
        preset: ServerCfg(
            server: "smp://abcd@smp8.simplex.im",
            preset: true,
            tested: true,
            enabled: true
        ),
        custom: ServerCfg(
            server: "smp://abcd@smp9.simplex.im",
            preset: false,
            tested: false,
            enabled: false
        ),
        untested: ServerCfg(
            server: "smp://abcd@smp10.simplex.im",
            preset: false,
            tested: nil,
            enabled: true
        )
    )

    enum CodingKeys: CodingKey {
        case server
        case preset
        case tested
        case enabled
    }
}

public enum SMPTestStep: String, Decodable, Equatable {
    case connect
    case createQueue
    case secureQueue
    case deleteQueue
    case disconnect

    var text: String {
        switch self {
        case .connect: return NSLocalizedString("Connect", comment: "server test step")
        case .createQueue: return NSLocalizedString("Create queue", comment: "server test step")
        case .secureQueue: return NSLocalizedString("Secure queue", comment: "server test step")
        case .deleteQueue: return NSLocalizedString("Delete queue", comment: "server test step")
        case .disconnect: return NSLocalizedString("Disconnect", comment: "server test step")
        }
    }
}

public struct SMPTestFailure: Decodable, Error, Equatable {
    var testStep: SMPTestStep
    var testError: AgentErrorType

    public static func == (l: SMPTestFailure, r: SMPTestFailure) -> Bool {
        l.testStep == r.testStep
    }

    public var localizedDescription: String {
        let err = String.localizedStringWithFormat(NSLocalizedString("Test failed at step %@.", comment: "server test failure"), testStep.text)
        switch testError {
        case .SMP(.AUTH):
            return err + " " + NSLocalizedString("Server requires authorization to create queues, check password", comment: "server test error")
        case .BROKER(_, .NETWORK):
            return err + " " + NSLocalizedString("Possibly, certificate fingerprint in server address is incorrect", comment: "server test error")
        default:
            return err
        }
    }
}

public struct ServerAddress: Decodable {
    public var hostnames: [String]
    public var port: String
    public var keyHash: String
    public var basicAuth: String

    public init(hostnames: [String], port: String, keyHash: String, basicAuth: String = "") {
        self.hostnames = hostnames
        self.port = port
        self.keyHash = keyHash
        self.basicAuth = basicAuth
    }

    public var uri: String {
        "smp://\(keyHash)\(basicAuth == "" ? "" : ":" + basicAuth)@\(hostnames.joined(separator: ","))"
    }

    public var valid: Bool {
        hostnames.count > 0 && Set(hostnames).count == hostnames.count
    }

    static public var empty = ServerAddress(
        hostnames: [],
        port: "",
        keyHash: "",
        basicAuth: ""
    )

    static public var sampleData = ServerAddress(
        hostnames: ["smp.simplex.im", "1234.onion"],
        port: "",
        keyHash: "LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=",
        basicAuth: "server_password"
    )
}

public struct NetCfg: Codable, Equatable {
    public var socksProxy: String? = nil
    public var hostMode: HostMode = .publicHost
    public var requiredHostMode = true
    public var sessionMode: TransportSessionMode
    public var tcpConnectTimeout: Int // microseconds
    public var tcpTimeout: Int // microseconds
    public var tcpKeepAlive: KeepAliveOpts?
    public var smpPingInterval: Int // microseconds
    public var smpPingCount: Int // times
    public var logTLSErrors: Bool

    public static let defaults: NetCfg = NetCfg(
        socksProxy: nil,
        sessionMode: TransportSessionMode.user,
        tcpConnectTimeout: 10_000_000,
        tcpTimeout: 7_000_000,
        tcpKeepAlive: KeepAliveOpts.defaults,
        smpPingInterval: 1200_000_000,
        smpPingCount: 3,
        logTLSErrors: false
    )

    public static let proxyDefaults: NetCfg = NetCfg(
        socksProxy: nil,
        sessionMode: TransportSessionMode.user,
        tcpConnectTimeout: 20_000_000,
        tcpTimeout: 15_000_000,
        tcpKeepAlive: KeepAliveOpts.defaults,
        smpPingInterval: 1200_000_000,
        smpPingCount: 3,
        logTLSErrors: false
    )

    public var enableKeepAlive: Bool { tcpKeepAlive != nil }
}

public enum HostMode: String, Codable {
    case onionViaSocks
    case onionHost = "onion"
    case publicHost = "public"
}

public enum OnionHosts: String, Identifiable {
    case no
    case prefer
    case require

    public var text: LocalizedStringKey {
        switch self {
        case .no: return "No"
        case .prefer: return "When available"
        case .require: return "Required"
        }
    }

    public var hostMode: (HostMode, Bool) {
        switch self {
        case .no: return (.publicHost, true)
        case .prefer: return (.onionHost, false)
        case .require: return (.onionHost, true)
        }
    }

    public init(netCfg: NetCfg) {
        switch netCfg.hostMode {
        case .onionViaSocks: self = .no
        case .onionHost: self = netCfg.requiredHostMode ? .require : .prefer
        case .publicHost: self = .no
        }
    }

    public var id: OnionHosts { self }

    public static let values: [OnionHosts] = [.no, .prefer, .require]
}

public enum TransportSessionMode: String, Codable, Identifiable {
    case user
    case entity

    public var text: LocalizedStringKey {
        switch self {
        case .user: return "User profile"
        case .entity: return "Connection"
        }
    }

    public var id: TransportSessionMode { self }

    public static let values: [TransportSessionMode] = [.user, .entity]
}

public struct KeepAliveOpts: Codable, Equatable {
    public var keepIdle: Int // seconds
    public var keepIntvl: Int // seconds
    public var keepCnt: Int // times

    public static let defaults: KeepAliveOpts = KeepAliveOpts(keepIdle: 30, keepIntvl: 15, keepCnt: 4)
}

public struct ChatSettings: Codable {
    public var enableNtfs: Bool

    public init(enableNtfs: Bool) {
        self.enableNtfs = enableNtfs
    }

    public static let defaults: ChatSettings = ChatSettings(enableNtfs: true)
}

public struct ConnectionStats: Codable {
    public var rcvServers: [String]?
    public var sndServers: [String]?
}

public struct UserContactLink: Decodable {
    public var connReqContact: String
    public var autoAccept: AutoAccept?

    public init(connReqContact: String, autoAccept: AutoAccept? = nil) {
        self.connReqContact = connReqContact
        self.autoAccept = autoAccept
    }

    var responseDetails: String {
        "connReqContact: \(connReqContact)\nautoAccept: \(AutoAccept.cmdString(autoAccept))"
    }
}

public struct AutoAccept: Codable {
    public var acceptIncognito: Bool
    public var autoReply: MsgContent?

    public init(acceptIncognito: Bool, autoReply: MsgContent? = nil) {
        self.acceptIncognito = acceptIncognito
        self.autoReply = autoReply
    }

    static func cmdString(_ autoAccept: AutoAccept?) -> String {
        guard let autoAccept = autoAccept else { return "off" }
        let s = "on" + (autoAccept.acceptIncognito ? " incognito=on" : "")
        guard let msg = autoAccept.autoReply else { return s }
        return s + " " + msg.cmdString
    }
}

public protocol SelectableItem: Hashable, Identifiable {
    var label: LocalizedStringKey { get }
    static var values: [Self] { get }
}

public struct DeviceToken: Decodable {
    var pushProvider: PushProvider
    var token: String

    public init(pushProvider: PushProvider, token: String) {
        self.pushProvider = pushProvider
        self.token = token
    }

    public var cmdString: String {
        "\(pushProvider) \(token)"
    }
}

public enum PushEnvironment: String {
    case development
    case production
}

public enum PushProvider: String, Decodable {
    case apns_dev
    case apns_prod

    public init(env: PushEnvironment) {
        switch env {
        case .development: self = .apns_dev
        case .production: self = .apns_prod
        }
    }
}

public enum NotificationsMode: String, Decodable, SelectableItem {
    case off = "OFF"
    case periodic = "PERIODIC"
    case instant = "INSTANT"

    public var label: LocalizedStringKey {
        switch self {
        case .off: return "Off (Local)"
        case .periodic: return "Periodically"
        case .instant: return "Instantly"
        }
    }

    public var id: String { self.rawValue }

    public static var values: [NotificationsMode] = [.instant, .periodic, .off]
}

public enum NotificationPreviewMode: String, SelectableItem {
    case hidden
    case contact
    case message

    public var label: LocalizedStringKey {
        switch self {
        case .hidden: return "Hidden"
        case .contact: return "Contact name"
        case .message: return "Message text"
        }
    }

    public var id: String { self.rawValue }

    public static var values: [NotificationPreviewMode] = [.message, .contact, .hidden]
}

public struct CoreVersionInfo: Decodable {
    public var version: String
    public var buildTimestamp: String
    public var simplexmqVersion: String
    public var simplexmqCommit: String
}

public func decodeJSON<T: Decodable>(_ json: String) -> T? {
    if let data = json.data(using: .utf8) {
        return try? jsonDecoder.decode(T.self, from: data)
    }
    return nil
}

public func encodeJSON<T: Encodable>(_ value: T) -> String {
    let data = try! jsonEncoder.encode(value)
    return String(decoding: data, as: UTF8.self)
}

private func encodeCJSON<T: Encodable>(_ value: T) -> [CChar] {
    encodeJSON(value).cString(using: .utf8)!
}

public enum ChatError: Decodable {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
    case errorDatabase(databaseError: DatabaseError)
}

public enum ChatErrorType: Decodable {
    case noActiveUser
    case activeUserExists
    case userExists
    case differentActiveUser
    case chatNotStarted
    case invalidConnReq
    case invalidChatMessage(message: String)
    case contactNotReady(contact: Contact)
    case groupUserRole
    case groupContactRole(contactName: ContactName)
    case groupDuplicateMember(contactName: ContactName)
    case groupDuplicateMemberId
    case groupNotJoined(groupInfo: GroupInfo)
    case groupMemberNotActive
    case groupMemberUserRemoved
    case groupMemberNotFound
    case groupMemberIntroNotFound(contactName: ContactName)
    case groupCantResendInvitation(groupInfo: GroupInfo, contactName: ContactName)
    case groupInternal(message: String)
    case fileNotFound(message: String)
    case fileAlreadyReceiving(message: String)
    case fileAlreadyExists(filePath: String)
    case fileRead(filePath: String, message: String)
    case fileWrite(filePath: String, message: String)
    case fileSend(fileId: Int64, agentError: String)
    case fileRcvChunk(message: String)
    case fileInternal(message: String)
    case invalidQuote
    case invalidChatItemUpdate
    case invalidChatItemDelete
    case agentVersion
    case commandError(message: String)
}

public enum StoreError: Decodable {
    case duplicateName
    case contactNotFound(contactId: Int64)
    case contactNotFoundByName(contactName: ContactName)
    case contactNotReady(contactName: ContactName)
    case duplicateContactLink
    case userContactLinkNotFound
    case contactRequestNotFound(contactRequestId: Int64)
    case contactRequestNotFoundByName(contactName: ContactName)
    case groupNotFound(groupId: Int64)
    case groupNotFoundByName(groupName: GroupName)
    case groupWithoutUser
    case duplicateGroupMember
    case groupAlreadyJoined
    case groupInvitationNotFound
    case sndFileNotFound(fileId: Int64)
    case sndFileInvalid(fileId: Int64)
    case rcvFileNotFound(fileId: Int64)
    case fileNotFound(fileId: Int64)
    case rcvFileInvalid(fileId: Int64)
    case connectionNotFound(agentConnId: String)
    case pendingConnectionNotFound(connId: Int64)
    case introNotFound
    case uniqueID
    case internalError(message: String)
    case noMsgDelivery(connId: Int64, agentMsgId: String)
    case badChatItem(itemId: Int64)
    case chatItemNotFound(itemId: Int64)
    case quotedChatItemNotFound
    case chatItemSharedMsgIdNotFound(sharedMsgId: String)
    case chatItemNotFoundByFileId(fileId: Int64)
    case duplicateGroupLink(groupInfo: GroupInfo)
    case groupLinkNotFound(groupInfo: GroupInfo)
}

public enum DatabaseError: Decodable {
    case errorEncrypted
    case errorPlaintext
    case errorNoFile(dbFile: String)
    case errorExport(sqliteError: SQLiteError)
    case errorOpen(sqliteError: SQLiteError)
}

public enum SQLiteError: Decodable {
    case errorNotADatabase
    case error(String)
}

public enum AgentErrorType: Decodable {
    case CMD(cmdErr: CommandErrorType)
    case CONN(connErr: ConnectionErrorType)
    case SMP(smpErr: ProtocolErrorType)
    case NTF(ntfErr: ProtocolErrorType)
    case BROKER(brokerAddress: String, brokerErr: BrokerErrorType)
    case AGENT(agentErr: SMPAgentError)
    case INTERNAL(internalErr: String)
}

public enum CommandErrorType: Decodable {
    case PROHIBITED
    case SYNTAX
    case NO_CONN
    case SIZE
    case LARGE
}

public enum ConnectionErrorType: Decodable {
    case NOT_FOUND
    case DUPLICATE
    case SIMPLEX
    case NOT_ACCEPTED
    case NOT_AVAILABLE
}

public enum BrokerErrorType: Decodable {
    case RESPONSE(smpErr: ProtocolErrorType)
    case UNEXPECTED
    case NETWORK
    case TRANSPORT(transportErr: ProtocolTransportError)
    case TIMEOUT
}

public enum ProtocolErrorType: Decodable {
    case BLOCK
    case SESSION
    case CMD(cmdErr: ProtocolCommandError)
    case AUTH
    case QUOTA
    case NO_MSG
    case LARGE_MSG
    case INTERNAL
}

public enum ProtocolCommandError: Decodable {
    case UNKNOWN
    case SYNTAX
    case NO_AUTH
    case HAS_AUTH
    case NO_ENTITY
}

public enum ProtocolTransportError: Decodable {
    case badBlock
    case largeMsg
    case badSession
    case handshake(handshakeErr: SMPHandshakeError)
}

public enum SMPHandshakeError: Decodable {
    case PARSE
    case VERSION
    case IDENTITY
}

public enum SMPAgentError: Decodable {
    case A_MESSAGE
    case A_PROHIBITED
    case A_VERSION
    case A_ENCRYPTION
}
