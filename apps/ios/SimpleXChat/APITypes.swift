//
//  SimpleXAPI.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI

public let jsonDecoder = getJSONDecoder()
let jsonEncoder = getJSONEncoder()

public enum ChatCommand {
    case showActiveUser
    case createActiveUser(profile: Profile?, sameServers: Bool, pastTimestamp: Bool)
    case listUsers
    case apiSetActiveUser(userId: Int64, viewPwd: String?)
    case setAllContactReceipts(enable: Bool)
    case apiSetUserContactReceipts(userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings)
    case apiSetUserGroupReceipts(userId: Int64, userMsgReceiptSettings: UserMsgReceiptSettings)
    case apiHideUser(userId: Int64, viewPwd: String)
    case apiUnhideUser(userId: Int64, viewPwd: String)
    case apiMuteUser(userId: Int64)
    case apiUnmuteUser(userId: Int64)
    case apiDeleteUser(userId: Int64, delSMPQueues: Bool, viewPwd: String?)
    case startChat(subscribe: Bool, expire: Bool, xftp: Bool)
    case apiStopChat
    case apiActivateChat(restoreChat: Bool)
    case apiSuspendChat(timeoutMicroseconds: Int)
    case setTempFolder(tempFolder: String)
    case setFilesFolder(filesFolder: String)
    case apiSetXFTPConfig(config: XFTPFileConfig?)
    case apiSetEncryptLocalFiles(enable: Bool)
    case apiExportArchive(config: ArchiveConfig)
    case apiImportArchive(config: ArchiveConfig)
    case apiDeleteStorage
    case apiStorageEncryption(config: DBEncryptionConfig)
    case apiGetChats(userId: Int64)
    case apiGetChat(type: ChatType, id: Int64, pagination: ChatPagination, search: String)
    case apiGetChatItemInfo(type: ChatType, id: Int64, itemId: Int64)
    case apiSendMessage(type: ChatType, id: Int64, file: CryptoFile?, quotedItemId: Int64?, msg: MsgContent, live: Bool, ttl: Int?)
    case apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, msg: MsgContent, live: Bool)
    case apiDeleteChatItem(type: ChatType, id: Int64, itemId: Int64, mode: CIDeleteMode)
    case apiDeleteMemberChatItem(groupId: Int64, groupMemberId: Int64, itemId: Int64)
    case apiChatItemReaction(type: ChatType, id: Int64, itemId: Int64, add: Bool, reaction: MsgReaction)
    case apiGetNtfToken
    case apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode)
    case apiVerifyToken(token: DeviceToken, nonce: String, code: String)
    case apiDeleteToken(token: DeviceToken)
    case apiGetNtfMessage(nonce: String, encNtfInfo: String)
    case apiNewGroup(userId: Int64, incognito: Bool, groupProfile: GroupProfile)
    case apiAddMember(groupId: Int64, contactId: Int64, memberRole: GroupMemberRole)
    case apiJoinGroup(groupId: Int64)
    case apiMemberRole(groupId: Int64, memberId: Int64, memberRole: GroupMemberRole)
    case apiRemoveMember(groupId: Int64, memberId: Int64)
    case apiLeaveGroup(groupId: Int64)
    case apiListMembers(groupId: Int64)
    case apiUpdateGroupProfile(groupId: Int64, groupProfile: GroupProfile)
    case apiCreateGroupLink(groupId: Int64, memberRole: GroupMemberRole)
    case apiGroupLinkMemberRole(groupId: Int64, memberRole: GroupMemberRole)
    case apiDeleteGroupLink(groupId: Int64)
    case apiGetGroupLink(groupId: Int64)
    case apiCreateMemberContact(groupId: Int64, groupMemberId: Int64)
    case apiSendMemberContactInvitation(contactId: Int64, msg: MsgContent)
    case apiGetUserProtoServers(userId: Int64, serverProtocol: ServerProtocol)
    case apiSetUserProtoServers(userId: Int64, serverProtocol: ServerProtocol, servers: [ServerCfg])
    case apiTestProtoServer(userId: Int64, server: String)
    case apiSetChatItemTTL(userId: Int64, seconds: Int64?)
    case apiGetChatItemTTL(userId: Int64)
    case apiSetNetworkConfig(networkConfig: NetCfg)
    case apiGetNetworkConfig
    case reconnectAllServers
    case apiSetChatSettings(type: ChatType, id: Int64, chatSettings: ChatSettings)
    case apiSetMemberSettings(groupId: Int64, groupMemberId: Int64, memberSettings: GroupMemberSettings)
    case apiContactInfo(contactId: Int64)
    case apiGroupMemberInfo(groupId: Int64, groupMemberId: Int64)
    case apiSwitchContact(contactId: Int64)
    case apiSwitchGroupMember(groupId: Int64, groupMemberId: Int64)
    case apiAbortSwitchContact(contactId: Int64)
    case apiAbortSwitchGroupMember(groupId: Int64, groupMemberId: Int64)
    case apiSyncContactRatchet(contactId: Int64, force: Bool)
    case apiSyncGroupMemberRatchet(groupId: Int64, groupMemberId: Int64, force: Bool)
    case apiGetContactCode(contactId: Int64)
    case apiGetGroupMemberCode(groupId: Int64, groupMemberId: Int64)
    case apiVerifyContact(contactId: Int64, connectionCode: String?)
    case apiVerifyGroupMember(groupId: Int64, groupMemberId: Int64, connectionCode: String?)
    case apiAddContact(userId: Int64, incognito: Bool)
    case apiSetConnectionIncognito(connId: Int64, incognito: Bool)
    case apiConnectPlan(userId: Int64, connReq: String)
    case apiConnect(userId: Int64, incognito: Bool, connReq: String)
    case apiConnectContactViaAddress(userId: Int64, incognito: Bool, contactId: Int64)
    case apiDeleteChat(type: ChatType, id: Int64, notify: Bool?)
    case apiClearChat(type: ChatType, id: Int64)
    case apiListContacts(userId: Int64)
    case apiUpdateProfile(userId: Int64, profile: Profile)
    case apiSetContactPrefs(contactId: Int64, preferences: Preferences)
    case apiSetContactAlias(contactId: Int64, localAlias: String)
    case apiSetConnectionAlias(connId: Int64, localAlias: String)
    case apiCreateMyAddress(userId: Int64)
    case apiDeleteMyAddress(userId: Int64)
    case apiShowMyAddress(userId: Int64)
    case apiSetProfileAddress(userId: Int64, on: Bool)
    case apiAddressAutoAccept(userId: Int64, autoAccept: AutoAccept?)
    case apiAcceptContact(incognito: Bool, contactReqId: Int64)
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
    case apiGetNetworkStatuses
    case apiChatRead(type: ChatType, id: Int64, itemRange: (Int64, Int64))
    case apiChatUnread(type: ChatType, id: Int64, unreadChat: Bool)
    case receiveFile(fileId: Int64, encrypted: Bool?, inline: Bool?)
    case setFileToReceive(fileId: Int64, encrypted: Bool?)
    case cancelFile(fileId: Int64)
    // remote desktop commands
    case setLocalDeviceName(displayName: String)
    case connectRemoteCtrl(xrcpInvitation: String)
    case findKnownRemoteCtrl
    case confirmRemoteCtrl(remoteCtrlId: Int64)
    case verifyRemoteCtrlSession(sessionCode: String)
    case listRemoteCtrls
    case stopRemoteCtrl
    case deleteRemoteCtrl(remoteCtrlId: Int64)
    // misc
    case showVersion
    case string(String)

    public var cmdString: String {
        get {
            switch self {
            case .showActiveUser: return "/u"
            case let .createActiveUser(profile, sameServers, pastTimestamp):
                let user = NewUser(profile: profile, sameServers: sameServers, pastTimestamp: pastTimestamp)
                return "/_create user \(encodeJSON(user))"
            case .listUsers: return "/users"
            case let .apiSetActiveUser(userId, viewPwd): return "/_user \(userId)\(maybePwd(viewPwd))"
            case let .setAllContactReceipts(enable): return "/set receipts all \(onOff(enable))"
            case let .apiSetUserContactReceipts(userId, userMsgReceiptSettings):
                let umrs = userMsgReceiptSettings
                return "/_set receipts contacts \(userId) \(onOff(umrs.enable)) clear_overrides=\(onOff(umrs.clearOverrides))"
            case let .apiSetUserGroupReceipts(userId, userMsgReceiptSettings):
                let umrs = userMsgReceiptSettings
                return "/_set receipts groups \(userId) \(onOff(umrs.enable)) clear_overrides=\(onOff(umrs.clearOverrides))"
            case let .apiHideUser(userId, viewPwd): return "/_hide user \(userId) \(encodeJSON(viewPwd))"
            case let .apiUnhideUser(userId, viewPwd): return "/_unhide user \(userId) \(encodeJSON(viewPwd))"
            case let .apiMuteUser(userId): return "/_mute user \(userId)"
            case let .apiUnmuteUser(userId): return "/_unmute user \(userId)"
            case let .apiDeleteUser(userId, delSMPQueues, viewPwd): return "/_delete user \(userId) del_smp=\(onOff(delSMPQueues))\(maybePwd(viewPwd))"
            case let .startChat(subscribe, expire, xftp): return "/_start subscribe=\(onOff(subscribe)) expire=\(onOff(expire)) xftp=\(onOff(xftp))"
            case .apiStopChat: return "/_stop"
            case let .apiActivateChat(restore): return "/_app activate restore=\(onOff(restore))"
            case let .apiSuspendChat(timeoutMicroseconds): return "/_app suspend \(timeoutMicroseconds)"
            case let .setTempFolder(tempFolder): return "/_temp_folder \(tempFolder)"
            case let .setFilesFolder(filesFolder): return "/_files_folder \(filesFolder)"
            case let .apiSetXFTPConfig(cfg): if let cfg = cfg {
                return "/_xftp on \(encodeJSON(cfg))"
            } else {
                return "/_xftp off"
            }
            case let .apiSetEncryptLocalFiles(enable): return "/_files_encrypt \(onOff(enable))"
            case let .apiExportArchive(cfg): return "/_db export \(encodeJSON(cfg))"
            case let .apiImportArchive(cfg): return "/_db import \(encodeJSON(cfg))"
            case .apiDeleteStorage: return "/_db delete"
            case let .apiStorageEncryption(cfg): return "/_db encryption \(encodeJSON(cfg))"
            case let .apiGetChats(userId): return "/_get chats \(userId) pcc=on"
            case let .apiGetChat(type, id, pagination, search): return "/_get chat \(ref(type, id)) \(pagination.cmdString)" +
                (search == "" ? "" : " search=\(search)")
            case let .apiGetChatItemInfo(type, id, itemId): return "/_get item info \(ref(type, id)) \(itemId)"
            case let .apiSendMessage(type, id, file, quotedItemId, mc, live, ttl):
                let msg = encodeJSON(ComposedMessage(fileSource: file, quotedItemId: quotedItemId, msgContent: mc))
                let ttlStr = ttl != nil ? "\(ttl!)" : "default"
                return "/_send \(ref(type, id)) live=\(onOff(live)) ttl=\(ttlStr) json \(msg)"
            case let .apiUpdateChatItem(type, id, itemId, mc, live): return "/_update item \(ref(type, id)) \(itemId) live=\(onOff(live)) \(mc.cmdString)"
            case let .apiDeleteChatItem(type, id, itemId, mode): return "/_delete item \(ref(type, id)) \(itemId) \(mode.rawValue)"
            case let .apiDeleteMemberChatItem(groupId, groupMemberId, itemId): return "/_delete member item #\(groupId) \(groupMemberId) \(itemId)"
            case let .apiChatItemReaction(type, id, itemId, add, reaction): return "/_reaction \(ref(type, id)) \(itemId) \(onOff(add)) \(encodeJSON(reaction))"
            case .apiGetNtfToken: return "/_ntf get "
            case let .apiRegisterToken(token, notificationMode): return "/_ntf register \(token.cmdString) \(notificationMode.rawValue)"
            case let .apiVerifyToken(token, nonce, code): return "/_ntf verify \(token.cmdString) \(nonce) \(code)"
            case let .apiDeleteToken(token): return "/_ntf delete \(token.cmdString)"
            case let .apiGetNtfMessage(nonce, encNtfInfo): return "/_ntf message \(nonce) \(encNtfInfo)"
            case let .apiNewGroup(userId, incognito, groupProfile): return "/_group \(userId) incognito=\(onOff(incognito)) \(encodeJSON(groupProfile))"
            case let .apiAddMember(groupId, contactId, memberRole): return "/_add #\(groupId) \(contactId) \(memberRole)"
            case let .apiJoinGroup(groupId): return "/_join #\(groupId)"
            case let .apiMemberRole(groupId, memberId, memberRole): return "/_member role #\(groupId) \(memberId) \(memberRole.rawValue)"
            case let .apiRemoveMember(groupId, memberId): return "/_remove #\(groupId) \(memberId)"
            case let .apiLeaveGroup(groupId): return "/_leave #\(groupId)"
            case let .apiListMembers(groupId): return "/_members #\(groupId)"
            case let .apiUpdateGroupProfile(groupId, groupProfile): return "/_group_profile #\(groupId) \(encodeJSON(groupProfile))"
            case let .apiCreateGroupLink(groupId, memberRole): return "/_create link #\(groupId) \(memberRole)"
            case let .apiGroupLinkMemberRole(groupId, memberRole): return "/_set link role #\(groupId) \(memberRole)"
            case let .apiDeleteGroupLink(groupId): return "/_delete link #\(groupId)"
            case let .apiGetGroupLink(groupId): return "/_get link #\(groupId)"
            case let .apiCreateMemberContact(groupId, groupMemberId): return "/_create member contact #\(groupId) \(groupMemberId)"
            case let .apiSendMemberContactInvitation(contactId, mc): return "/_invite member contact @\(contactId) \(mc.cmdString)"
            case let .apiGetUserProtoServers(userId, serverProtocol): return "/_servers \(userId) \(serverProtocol)"
            case let .apiSetUserProtoServers(userId, serverProtocol, servers): return "/_servers \(userId) \(serverProtocol) \(protoServersStr(servers))"
            case let .apiTestProtoServer(userId, server): return "/_server test \(userId) \(server)"
            case let .apiSetChatItemTTL(userId, seconds): return "/_ttl \(userId) \(chatItemTTLStr(seconds: seconds))"
            case let .apiGetChatItemTTL(userId): return "/_ttl \(userId)"
            case let .apiSetNetworkConfig(networkConfig): return "/_network \(encodeJSON(networkConfig))"
            case .apiGetNetworkConfig: return "/network"
            case .reconnectAllServers: return "/reconnect"
            case let .apiSetChatSettings(type, id, chatSettings): return "/_settings \(ref(type, id)) \(encodeJSON(chatSettings))"
            case let .apiSetMemberSettings(groupId, groupMemberId, memberSettings): return "/_member settings #\(groupId) \(groupMemberId) \(encodeJSON(memberSettings))"
            case let .apiContactInfo(contactId): return "/_info @\(contactId)"
            case let .apiGroupMemberInfo(groupId, groupMemberId): return "/_info #\(groupId) \(groupMemberId)"
            case let .apiSwitchContact(contactId): return "/_switch @\(contactId)"
            case let .apiSwitchGroupMember(groupId, groupMemberId): return "/_switch #\(groupId) \(groupMemberId)"
            case let .apiAbortSwitchContact(contactId): return "/_abort switch @\(contactId)"
            case let .apiAbortSwitchGroupMember(groupId, groupMemberId): return "/_abort switch #\(groupId) \(groupMemberId)"
            case let .apiSyncContactRatchet(contactId, force): if force {
                return "/_sync @\(contactId) force=on"
            } else {
                return "/_sync @\(contactId)"
            }
            case let .apiSyncGroupMemberRatchet(groupId, groupMemberId, force): if force {
                return "/_sync #\(groupId) \(groupMemberId) force=on"
            } else {
                return "/_sync #\(groupId) \(groupMemberId)"
            }
            case let .apiGetContactCode(contactId): return "/_get code @\(contactId)"
            case let .apiGetGroupMemberCode(groupId, groupMemberId): return "/_get code #\(groupId) \(groupMemberId)"
            case let .apiVerifyContact(contactId, .some(connectionCode)): return "/_verify code @\(contactId) \(connectionCode)"
            case let .apiVerifyContact(contactId, .none): return "/_verify code @\(contactId)"
            case let .apiVerifyGroupMember(groupId, groupMemberId, .some(connectionCode)): return "/_verify code #\(groupId) \(groupMemberId) \(connectionCode)"
            case let .apiVerifyGroupMember(groupId, groupMemberId, .none): return "/_verify code #\(groupId) \(groupMemberId)"
            case let .apiAddContact(userId, incognito): return "/_connect \(userId) incognito=\(onOff(incognito))"
            case let .apiSetConnectionIncognito(connId, incognito): return "/_set incognito :\(connId) \(onOff(incognito))"
            case let .apiConnectPlan(userId, connReq): return "/_connect plan \(userId) \(connReq)"
            case let .apiConnect(userId, incognito, connReq): return "/_connect \(userId) incognito=\(onOff(incognito)) \(connReq)"
            case let .apiConnectContactViaAddress(userId, incognito, contactId): return "/_connect contact \(userId) incognito=\(onOff(incognito)) \(contactId)"
            case let .apiDeleteChat(type, id, notify): if let notify = notify {
                return "/_delete \(ref(type, id)) notify=\(onOff(notify))"
            } else {
                return "/_delete \(ref(type, id))"
            }
            case let .apiClearChat(type, id): return "/_clear chat \(ref(type, id))"
            case let .apiListContacts(userId): return "/_contacts \(userId)"
            case let .apiUpdateProfile(userId, profile): return "/_profile \(userId) \(encodeJSON(profile))"
            case let .apiSetContactPrefs(contactId, preferences): return "/_set prefs @\(contactId) \(encodeJSON(preferences))"
            case let .apiSetContactAlias(contactId, localAlias): return "/_set alias @\(contactId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiSetConnectionAlias(connId, localAlias): return "/_set alias :\(connId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiCreateMyAddress(userId): return "/_address \(userId)"
            case let .apiDeleteMyAddress(userId): return "/_delete_address \(userId)"
            case let .apiShowMyAddress(userId): return "/_show_address \(userId)"
            case let .apiSetProfileAddress(userId, on): return "/_profile_address \(userId) \(onOff(on))"
            case let .apiAddressAutoAccept(userId, autoAccept): return "/_auto_accept \(userId) \(AutoAccept.cmdString(autoAccept))"
            case let .apiAcceptContact(incognito, contactReqId): return "/_accept incognito=\(onOff(incognito)) \(contactReqId)"
            case let .apiRejectContact(contactReqId): return "/_reject \(contactReqId)"
            case let .apiSendCallInvitation(contact, callType): return "/_call invite @\(contact.apiId) \(encodeJSON(callType))"
            case let .apiRejectCall(contact): return "/_call reject @\(contact.apiId)"
            case let .apiSendCallOffer(contact, callOffer): return "/_call offer @\(contact.apiId) \(encodeJSON(callOffer))"
            case let .apiSendCallAnswer(contact, answer): return "/_call answer @\(contact.apiId) \(encodeJSON(answer))"
            case let .apiSendCallExtraInfo(contact, extraInfo): return "/_call extra @\(contact.apiId) \(encodeJSON(extraInfo))"
            case let .apiEndCall(contact): return "/_call end @\(contact.apiId)"
            case .apiGetCallInvitations: return "/_call get"
            case let .apiCallStatus(contact, callStatus): return "/_call status @\(contact.apiId) \(callStatus.rawValue)"
            case .apiGetNetworkStatuses: return "/_network_statuses"
            case let .apiChatRead(type, id, itemRange: (from, to)): return "/_read chat \(ref(type, id)) from=\(from) to=\(to)"
            case let .apiChatUnread(type, id, unreadChat): return "/_unread chat \(ref(type, id)) \(onOff(unreadChat))"
            case let .receiveFile(fileId, encrypt, inline): return "/freceive \(fileId)\(onOffParam("encrypt", encrypt))\(onOffParam("inline", inline))"
            case let .setFileToReceive(fileId, encrypt): return "/_set_file_to_receive \(fileId)\(onOffParam("encrypt", encrypt))"
            case let .cancelFile(fileId): return "/fcancel \(fileId)"
            case let .setLocalDeviceName(displayName): return "/set device name \(displayName)"
            case let .connectRemoteCtrl(xrcpInv): return "/connect remote ctrl \(xrcpInv)"
            case .findKnownRemoteCtrl: return "/find remote ctrl"
            case let .confirmRemoteCtrl(rcId): return "/confirm remote ctrl \(rcId)"
            case let .verifyRemoteCtrlSession(sessCode): return "/verify remote ctrl \(sessCode)"
            case .listRemoteCtrls: return "/list remote ctrls"
            case .stopRemoteCtrl: return "/stop remote ctrl"
            case let .deleteRemoteCtrl(rcId): return "/delete remote ctrl \(rcId)"
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
            case .setAllContactReceipts: return "setAllContactReceipts"
            case .apiSetUserContactReceipts: return "apiSetUserContactReceipts"
            case .apiSetUserGroupReceipts: return "apiSetUserGroupReceipts"
            case .apiHideUser: return "apiHideUser"
            case .apiUnhideUser: return "apiUnhideUser"
            case .apiMuteUser: return "apiMuteUser"
            case .apiUnmuteUser: return "apiUnmuteUser"
            case .apiDeleteUser: return "apiDeleteUser"
            case .startChat: return "startChat"
            case .apiStopChat: return "apiStopChat"
            case .apiActivateChat: return "apiActivateChat"
            case .apiSuspendChat: return "apiSuspendChat"
            case .setTempFolder: return "setTempFolder"
            case .setFilesFolder: return "setFilesFolder"
            case .apiSetXFTPConfig: return "apiSetXFTPConfig"
            case .apiSetEncryptLocalFiles: return "apiSetEncryptLocalFiles"
            case .apiExportArchive: return "apiExportArchive"
            case .apiImportArchive: return "apiImportArchive"
            case .apiDeleteStorage: return "apiDeleteStorage"
            case .apiStorageEncryption: return "apiStorageEncryption"
            case .apiGetChats: return "apiGetChats"
            case .apiGetChat: return "apiGetChat"
            case .apiGetChatItemInfo: return "apiGetChatItemInfo"
            case .apiSendMessage: return "apiSendMessage"
            case .apiUpdateChatItem: return "apiUpdateChatItem"
            case .apiDeleteChatItem: return "apiDeleteChatItem"
            case .apiConnectContactViaAddress: return "apiConnectContactViaAddress"
            case .apiDeleteMemberChatItem: return "apiDeleteMemberChatItem"
            case .apiChatItemReaction: return "apiChatItemReaction"
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
            case .apiGroupLinkMemberRole: return "apiGroupLinkMemberRole"
            case .apiDeleteGroupLink: return "apiDeleteGroupLink"
            case .apiGetGroupLink: return "apiGetGroupLink"
            case .apiCreateMemberContact: return "apiCreateMemberContact"
            case .apiSendMemberContactInvitation: return "apiSendMemberContactInvitation"
            case .apiGetUserProtoServers: return "apiGetUserProtoServers"
            case .apiSetUserProtoServers: return "apiSetUserProtoServers"
            case .apiTestProtoServer: return "apiTestProtoServer"
            case .apiSetChatItemTTL: return "apiSetChatItemTTL"
            case .apiGetChatItemTTL: return "apiGetChatItemTTL"
            case .apiSetNetworkConfig: return "apiSetNetworkConfig"
            case .apiGetNetworkConfig: return "apiGetNetworkConfig"
            case .reconnectAllServers: return "reconnectAllServers"
            case .apiSetChatSettings: return "apiSetChatSettings"
            case .apiSetMemberSettings: return "apiSetMemberSettings"
            case .apiContactInfo: return "apiContactInfo"
            case .apiGroupMemberInfo: return "apiGroupMemberInfo"
            case .apiSwitchContact: return "apiSwitchContact"
            case .apiSwitchGroupMember: return "apiSwitchGroupMember"
            case .apiAbortSwitchContact: return "apiAbortSwitchContact"
            case .apiAbortSwitchGroupMember: return "apiAbortSwitchGroupMember"
            case .apiSyncContactRatchet: return "apiSyncContactRatchet"
            case .apiSyncGroupMemberRatchet: return "apiSyncGroupMemberRatchet"
            case .apiGetContactCode: return "apiGetContactCode"
            case .apiGetGroupMemberCode: return "apiGetGroupMemberCode"
            case .apiVerifyContact: return "apiVerifyContact"
            case .apiVerifyGroupMember: return "apiVerifyGroupMember"
            case .apiAddContact: return "apiAddContact"
            case .apiSetConnectionIncognito: return "apiSetConnectionIncognito"
            case .apiConnectPlan: return "apiConnectPlan"
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
            case .apiSetProfileAddress: return "apiSetProfileAddress"
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
            case .apiGetNetworkStatuses: return "apiGetNetworkStatuses"
            case .apiChatRead: return "apiChatRead"
            case .apiChatUnread: return "apiChatUnread"
            case .receiveFile: return "receiveFile"
            case .setFileToReceive: return "setFileToReceive"
            case .cancelFile: return "cancelFile"
            case .setLocalDeviceName: return "setLocalDeviceName"
            case .connectRemoteCtrl: return "connectRemoteCtrl"
            case .findKnownRemoteCtrl: return "findKnownRemoteCtrl"
            case .confirmRemoteCtrl: return "confirmRemoteCtrl"
            case .verifyRemoteCtrlSession: return "verifyRemoteCtrlSession"
            case .listRemoteCtrls: return "listRemoteCtrls"
            case .stopRemoteCtrl: return "stopRemoteCtrl"
            case .deleteRemoteCtrl: return "deleteRemoteCtrl"
            case .showVersion: return "showVersion"
            case .string: return "console command"
            }
        }
    }

    func ref(_ type: ChatType, _ id: Int64) -> String {
        "\(type.rawValue)\(id)"
    }

    func protoServersStr(_ servers: [ServerCfg]) -> String {
        encodeJSON(ProtoServersConfig(servers: servers))
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
        case let .apiSetActiveUser(userId, viewPwd):
            return .apiSetActiveUser(userId: userId, viewPwd: obfuscate(viewPwd))
        case let .apiHideUser(userId, viewPwd):
            return .apiHideUser(userId: userId, viewPwd: obfuscate(viewPwd))
        case let .apiUnhideUser(userId, viewPwd):
            return .apiUnhideUser(userId: userId, viewPwd: obfuscate(viewPwd))
        case let .apiDeleteUser(userId, delSMPQueues, viewPwd):
            return .apiDeleteUser(userId: userId, delSMPQueues: delSMPQueues, viewPwd: obfuscate(viewPwd))
        default: return self
        }
    }

    private func obfuscate(_ s: String) -> String {
        s == "" ? "" : "***"
    }

    private func obfuscate(_ s: String?) -> String? {
        if let s = s {
            return obfuscate(s)
        }
        return nil
    }

    private func onOff(_ b: Bool) -> String {
        b ? "on" : "off"
    }

    private func onOffParam(_ param: String, _ b: Bool?) -> String {
        if let b = b {
            return " \(param)=\(onOff(b))"
        }
        return ""
    }

    private func maybePwd(_ pwd: String?) -> String {
        pwd == "" || pwd == nil ? "" : " " + encodeJSON(pwd)
    }
}

public struct APIResponse: Decodable {
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
    case apiChats(user: UserRef, chats: [ChatData])
    case apiChat(user: UserRef, chat: ChatData)
    case chatItemInfo(user: UserRef, chatItem: AChatItem, chatItemInfo: ChatItemInfo)
    case userProtoServers(user: UserRef, servers: UserProtoServers)
    case serverTestResult(user: UserRef, testServer: String, testFailure: ProtocolTestFailure?)
    case chatItemTTL(user: UserRef, chatItemTTL: Int64?)
    case networkConfig(networkConfig: NetCfg)
    case contactInfo(user: UserRef, contact: Contact, connectionStats_: ConnectionStats?, customUserProfile: Profile?)
    case groupMemberInfo(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats_: ConnectionStats?)
    case contactSwitchStarted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberSwitchStarted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactSwitchAborted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberSwitchAborted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactSwitch(user: UserRef, contact: Contact, switchProgress: SwitchProgress)
    case groupMemberSwitch(user: UserRef, groupInfo: GroupInfo, member: GroupMember, switchProgress: SwitchProgress)
    case contactRatchetSyncStarted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberRatchetSyncStarted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactRatchetSync(user: UserRef, contact: Contact, ratchetSyncProgress: RatchetSyncProgress)
    case groupMemberRatchetSync(user: UserRef, groupInfo: GroupInfo, member: GroupMember, ratchetSyncProgress: RatchetSyncProgress)
    case contactVerificationReset(user: UserRef, contact: Contact)
    case groupMemberVerificationReset(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case contactCode(user: UserRef, contact: Contact, connectionCode: String)
    case groupMemberCode(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionCode: String)
    case connectionVerified(user: UserRef, verified: Bool, expectedCode: String)
    case invitation(user: UserRef, connReqInvitation: String, connection: PendingContactConnection)
    case connectionIncognitoUpdated(user: UserRef, toConnection: PendingContactConnection)
    case connectionPlan(user: UserRef, connectionPlan: ConnectionPlan)
    case sentConfirmation(user: UserRef, connection: PendingContactConnection)
    case sentInvitation(user: UserRef, connection: PendingContactConnection)
    case sentInvitationToContact(user: UserRef, contact: Contact, customUserProfile: Profile?)
    case contactAlreadyExists(user: UserRef, contact: Contact)
    case contactRequestAlreadyAccepted(user: UserRef, contact: Contact)
    case contactDeleted(user: UserRef, contact: Contact)
    case contactDeletedByContact(user: UserRef, contact: Contact)
    case chatCleared(user: UserRef, chatInfo: ChatInfo)
    case userProfileNoChange(user: User)
    case userProfileUpdated(user: User, fromProfile: Profile, toProfile: Profile, updateSummary: UserProfileUpdateSummary)
    case userPrivacy(user: User, updatedUser: User)
    case contactAliasUpdated(user: UserRef, toContact: Contact)
    case connectionAliasUpdated(user: UserRef, toConnection: PendingContactConnection)
    case contactPrefsUpdated(user: User, fromContact: Contact, toContact: Contact)
    case userContactLink(user: User, contactLink: UserContactLink)
    case userContactLinkUpdated(user: User, contactLink: UserContactLink)
    case userContactLinkCreated(user: User, connReqContact: String)
    case userContactLinkDeleted(user: User)
    case contactConnected(user: UserRef, contact: Contact, userCustomProfile: Profile?)
    case contactConnecting(user: UserRef, contact: Contact)
    case receivedContactRequest(user: UserRef, contactRequest: UserContactRequest)
    case acceptingContactRequest(user: UserRef, contact: Contact)
    case contactRequestRejected(user: UserRef)
    case contactUpdated(user: UserRef, toContact: Contact)
    case groupMemberUpdated(user: UserRef, groupInfo: GroupInfo, fromMember: GroupMember, toMember: GroupMember)
    // TODO remove events below
    case contactsSubscribed(server: String, contactRefs: [ContactRef])
    case contactsDisconnected(server: String, contactRefs: [ContactRef])
    case contactSubSummary(user: UserRef, contactSubscriptions: [ContactSubStatus])
    // TODO remove events above
    case networkStatus(networkStatus: NetworkStatus, connections: [String])
    case networkStatuses(user_: UserRef?, networkStatuses: [ConnNetworkStatus])
    case groupSubscribed(user: UserRef, groupInfo: GroupRef)
    case memberSubErrors(user: UserRef, memberSubErrors: [MemberSubError])
    case groupEmpty(user: UserRef, groupInfo: GroupInfo)
    case userContactLinkSubscribed
    case newChatItem(user: UserRef, chatItem: AChatItem)
    case chatItemStatusUpdated(user: UserRef, chatItem: AChatItem)
    case chatItemUpdated(user: UserRef, chatItem: AChatItem)
    case chatItemNotChanged(user: UserRef, chatItem: AChatItem)
    case chatItemReaction(user: UserRef, added: Bool, reaction: ACIReaction)
    case chatItemDeleted(user: UserRef, deletedChatItem: AChatItem, toChatItem: AChatItem?, byUser: Bool)
    case contactsList(user: UserRef, contacts: [Contact])
    // group events
    case groupCreated(user: UserRef, groupInfo: GroupInfo)
    case sentGroupInvitation(user: UserRef, groupInfo: GroupInfo, contact: Contact, member: GroupMember)
    case userAcceptedGroupSent(user: UserRef, groupInfo: GroupInfo, hostContact: Contact?)
    case groupLinkConnecting(user: UserRef, groupInfo: GroupInfo, hostMember: GroupMember)
    case userDeletedMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case leftMemberUser(user: UserRef, groupInfo: GroupInfo)
    case groupMembers(user: UserRef, group: Group)
    case receivedGroupInvitation(user: UserRef, groupInfo: GroupInfo, contact: Contact, memberRole: GroupMemberRole)
    case groupDeletedUser(user: UserRef, groupInfo: GroupInfo)
    case joinedGroupMemberConnecting(user: UserRef, groupInfo: GroupInfo, hostMember: GroupMember, member: GroupMember)
    case memberRole(user: UserRef, groupInfo: GroupInfo, byMember: GroupMember, member: GroupMember, fromRole: GroupMemberRole, toRole: GroupMemberRole)
    case memberRoleUser(user: UserRef, groupInfo: GroupInfo, member: GroupMember, fromRole: GroupMemberRole, toRole: GroupMemberRole)
    case deletedMemberUser(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case deletedMember(user: UserRef, groupInfo: GroupInfo, byMember: GroupMember, deletedMember: GroupMember)
    case leftMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case groupDeleted(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case contactsMerged(user: UserRef, intoContact: Contact, mergedContact: Contact)
    case groupInvitation(user: UserRef, groupInfo: GroupInfo) // unused
    case userJoinedGroup(user: UserRef, groupInfo: GroupInfo)
    case joinedGroupMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case connectedToGroupMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember, memberContact: Contact?)
    case groupRemoved(user: UserRef, groupInfo: GroupInfo) // unused
    case groupUpdated(user: UserRef, toGroup: GroupInfo)
    case groupLinkCreated(user: UserRef, groupInfo: GroupInfo, connReqContact: String, memberRole: GroupMemberRole)
    case groupLink(user: UserRef, groupInfo: GroupInfo, connReqContact: String, memberRole: GroupMemberRole)
    case groupLinkDeleted(user: UserRef, groupInfo: GroupInfo)
    case newMemberContact(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    case newMemberContactSentInv(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    case newMemberContactReceivedInv(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    // receiving file events
    case rcvFileAccepted(user: UserRef, chatItem: AChatItem)
    case rcvFileAcceptedSndCancelled(user: UserRef, rcvFileTransfer: RcvFileTransfer)
    case rcvFileStart(user: UserRef, chatItem: AChatItem)
    case rcvFileProgressXFTP(user: UserRef, chatItem: AChatItem, receivedSize: Int64, totalSize: Int64)
    case rcvFileComplete(user: UserRef, chatItem: AChatItem)
    case rcvFileCancelled(user: UserRef, chatItem: AChatItem, rcvFileTransfer: RcvFileTransfer)
    case rcvFileSndCancelled(user: UserRef, chatItem: AChatItem, rcvFileTransfer: RcvFileTransfer)
    case rcvFileError(user: UserRef, chatItem: AChatItem)
    // sending file events
    case sndFileStart(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileComplete(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileCancelled(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta, sndFileTransfers: [SndFileTransfer])
    case sndFileRcvCancelled(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileProgressXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta, sentSize: Int64, totalSize: Int64)
    case sndFileCompleteXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta)
    case sndFileError(user: UserRef, chatItem: AChatItem)
    // call events
    case callInvitation(callInvitation: RcvCallInvitation)
    case callOffer(user: UserRef, contact: Contact, callType: CallType, offer: WebRTCSession, sharedKey: String?, askConfirmation: Bool)
    case callAnswer(user: UserRef, contact: Contact, answer: WebRTCSession)
    case callExtraInfo(user: UserRef, contact: Contact, extraInfo: WebRTCExtraInfo)
    case callEnded(user: UserRef, contact: Contact)
    case callInvitations(callInvitations: [RcvCallInvitation])
    case ntfTokenStatus(status: NtfTknStatus)
    case ntfToken(token: DeviceToken, status: NtfTknStatus, ntfMode: NotificationsMode)
    case ntfMessages(user_: User?, connEntity_: ConnectionEntity?, msgTs: Date?, ntfMessages: [NtfMsgInfo])
    case ntfMessage(user: UserRef, connEntity: ConnectionEntity, ntfMessage: NtfMsgInfo)
    case contactConnectionDeleted(user: UserRef, connection: PendingContactConnection)
    // remote desktop responses/events
    case remoteCtrlList(remoteCtrls: [RemoteCtrlInfo])
    case remoteCtrlFound(remoteCtrl: RemoteCtrlInfo, ctrlAppInfo_: CtrlAppInfo?, appVersion: String, compatible: Bool)
    case remoteCtrlConnecting(remoteCtrl_: RemoteCtrlInfo?, ctrlAppInfo: CtrlAppInfo, appVersion: String)
    case remoteCtrlSessionCode(remoteCtrl_: RemoteCtrlInfo?, sessionCode: String)
    case remoteCtrlConnected(remoteCtrl: RemoteCtrlInfo)
    case remoteCtrlStopped(rcsState: RemoteCtrlSessionState, rcStopReason: RemoteCtrlStopReason)
    // misc
    case versionInfo(versionInfo: CoreVersionInfo, chatMigrations: [UpMigration], agentMigrations: [UpMigration])
    case cmdOk(user: UserRef?)
    case chatCmdError(user_: UserRef?, chatError: ChatError)
    case chatError(user_: UserRef?, chatError: ChatError)
    case archiveImported(archiveErrors: [ArchiveError])

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
            case .chatItemInfo: return "chatItemInfo"
            case .userProtoServers: return "userProtoServers"
            case .serverTestResult: return "serverTestResult"
            case .chatItemTTL: return "chatItemTTL"
            case .networkConfig: return "networkConfig"
            case .contactInfo: return "contactInfo"
            case .groupMemberInfo: return "groupMemberInfo"
            case .contactSwitchStarted: return "contactSwitchStarted"
            case .groupMemberSwitchStarted: return "groupMemberSwitchStarted"
            case .contactSwitchAborted: return "contactSwitchAborted"
            case .groupMemberSwitchAborted: return "groupMemberSwitchAborted"
            case .contactSwitch: return "contactSwitch"
            case .groupMemberSwitch: return "groupMemberSwitch"
            case .contactRatchetSyncStarted: return "contactRatchetSyncStarted"
            case .groupMemberRatchetSyncStarted: return "groupMemberRatchetSyncStarted"
            case .contactRatchetSync: return "contactRatchetSync"
            case .groupMemberRatchetSync: return "groupMemberRatchetSync"
            case .contactVerificationReset: return "contactVerificationReset"
            case .groupMemberVerificationReset: return "groupMemberVerificationReset"
            case .contactCode: return "contactCode"
            case .groupMemberCode: return "groupMemberCode"
            case .connectionVerified: return "connectionVerified"
            case .invitation: return "invitation"
            case .connectionIncognitoUpdated: return "connectionIncognitoUpdated"
            case .connectionPlan: return "connectionPlan"
            case .sentConfirmation: return "sentConfirmation"
            case .sentInvitation: return "sentInvitation"
            case .sentInvitationToContact: return "sentInvitationToContact"
            case .contactAlreadyExists: return "contactAlreadyExists"
            case .contactRequestAlreadyAccepted: return "contactRequestAlreadyAccepted"
            case .contactDeleted: return "contactDeleted"
            case .contactDeletedByContact: return "contactDeletedByContact"
            case .chatCleared: return "chatCleared"
            case .userProfileNoChange: return "userProfileNoChange"
            case .userProfileUpdated: return "userProfileUpdated"
            case .userPrivacy: return "userPrivacy"
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
            case .groupMemberUpdated: return "groupMemberUpdated"
            case .contactsSubscribed: return "contactsSubscribed"
            case .contactsDisconnected: return "contactsDisconnected"
            case .contactSubSummary: return "contactSubSummary"
            case .networkStatus: return "networkStatus"
            case .networkStatuses: return "networkStatuses"
            case .groupSubscribed: return "groupSubscribed"
            case .memberSubErrors: return "memberSubErrors"
            case .groupEmpty: return "groupEmpty"
            case .userContactLinkSubscribed: return "userContactLinkSubscribed"
            case .newChatItem: return "newChatItem"
            case .chatItemStatusUpdated: return "chatItemStatusUpdated"
            case .chatItemUpdated: return "chatItemUpdated"
            case .chatItemNotChanged: return "chatItemNotChanged"
            case .chatItemReaction: return "chatItemReaction"
            case .chatItemDeleted: return "chatItemDeleted"
            case .contactsList: return "contactsList"
            case .groupCreated: return "groupCreated"
            case .sentGroupInvitation: return "sentGroupInvitation"
            case .userAcceptedGroupSent: return "userAcceptedGroupSent"
            case .groupLinkConnecting: return "groupLinkConnecting"
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
            case .newMemberContact: return "newMemberContact"
            case .newMemberContactSentInv: return "newMemberContactSentInv"
            case .newMemberContactReceivedInv: return "newMemberContactReceivedInv"
            case .rcvFileAccepted: return "rcvFileAccepted"
            case .rcvFileAcceptedSndCancelled: return "rcvFileAcceptedSndCancelled"
            case .rcvFileStart: return "rcvFileStart"
            case .rcvFileProgressXFTP: return "rcvFileProgressXFTP"
            case .rcvFileComplete: return "rcvFileComplete"
            case .rcvFileCancelled: return "rcvFileCancelled"
            case .rcvFileSndCancelled: return "rcvFileSndCancelled"
            case .rcvFileError: return "rcvFileError"
            case .sndFileStart: return "sndFileStart"
            case .sndFileComplete: return "sndFileComplete"
            case .sndFileCancelled: return "sndFileCancelled"
            case .sndFileRcvCancelled: return "sndFileRcvCancelled"
            case .sndFileProgressXFTP: return "sndFileProgressXFTP"
            case .sndFileCompleteXFTP: return "sndFileCompleteXFTP"
            case .sndFileError: return "sndFileError"
            case .callInvitation: return "callInvitation"
            case .callOffer: return "callOffer"
            case .callAnswer: return "callAnswer"
            case .callExtraInfo: return "callExtraInfo"
            case .callEnded: return "callEnded"
            case .callInvitations: return "callInvitations"
            case .ntfTokenStatus: return "ntfTokenStatus"
            case .ntfToken: return "ntfToken"
            case .ntfMessages: return "ntfMessages"
            case .ntfMessage: return "ntfMessage"
            case .contactConnectionDeleted: return "contactConnectionDeleted"
            case .remoteCtrlList: return "remoteCtrlList"
            case .remoteCtrlFound: return "remoteCtrlFound"
            case .remoteCtrlConnecting: return "remoteCtrlConnecting"
            case .remoteCtrlSessionCode: return "remoteCtrlSessionCode"
            case .remoteCtrlConnected: return "remoteCtrlConnected"
            case .remoteCtrlStopped: return "remoteCtrlStopped"
            case .versionInfo: return "versionInfo"
            case .cmdOk: return "cmdOk"
            case .chatCmdError: return "chatCmdError"
            case .chatError: return "chatError"
            case .archiveImported: return "archiveImported"
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
            case let .chatItemInfo(u, chatItem, chatItemInfo): return withUser(u, "chatItem: \(String(describing: chatItem))\nchatItemInfo: \(String(describing: chatItemInfo))")
            case let .userProtoServers(u, servers): return withUser(u, "servers: \(String(describing: servers))")
            case let .serverTestResult(u, server, testFailure): return withUser(u, "server: \(server)\nresult: \(String(describing: testFailure))")
            case let .chatItemTTL(u, chatItemTTL): return withUser(u, String(describing: chatItemTTL))
            case let .networkConfig(networkConfig): return String(describing: networkConfig)
            case let .contactInfo(u, contact, connectionStats_, customUserProfile): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats_: \(String(describing: connectionStats_))\ncustomUserProfile: \(String(describing: customUserProfile))")
            case let .groupMemberInfo(u, groupInfo, member, connectionStats_): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats_: \(String(describing: connectionStats_))")
            case let .contactSwitchStarted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
            case let .groupMemberSwitchStarted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
            case let .contactSwitchAborted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
            case let .groupMemberSwitchAborted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
            case let .contactSwitch(u, contact, switchProgress): return withUser(u, "contact: \(String(describing: contact))\nswitchProgress: \(String(describing: switchProgress))")
            case let .groupMemberSwitch(u, groupInfo, member, switchProgress): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nswitchProgress: \(String(describing: switchProgress))")
            case let .contactRatchetSyncStarted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
            case let .groupMemberRatchetSyncStarted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
            case let .contactRatchetSync(u, contact, ratchetSyncProgress): return withUser(u, "contact: \(String(describing: contact))\nratchetSyncProgress: \(String(describing: ratchetSyncProgress))")
            case let .groupMemberRatchetSync(u, groupInfo, member, ratchetSyncProgress): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nratchetSyncProgress: \(String(describing: ratchetSyncProgress))")
            case let .contactVerificationReset(u, contact): return withUser(u, "contact: \(String(describing: contact))")
            case let .groupMemberVerificationReset(u, groupInfo, member): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))")
            case let .contactCode(u, contact, connectionCode): return withUser(u, "contact: \(String(describing: contact))\nconnectionCode: \(connectionCode)")
            case let .groupMemberCode(u, groupInfo, member, connectionCode): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionCode: \(connectionCode)")
            case let .connectionVerified(u, verified, expectedCode): return withUser(u, "verified: \(verified)\nconnectionCode: \(expectedCode)")
            case let .invitation(u, connReqInvitation, connection): return withUser(u, "connReqInvitation: \(connReqInvitation)\nconnection: \(connection)")
            case let .connectionIncognitoUpdated(u, toConnection): return withUser(u, String(describing: toConnection))
            case let .connectionPlan(u, connectionPlan): return withUser(u, String(describing: connectionPlan))
            case let .sentConfirmation(u, connection): return withUser(u, String(describing: connection))
            case let .sentInvitation(u, connection): return withUser(u, String(describing: connection))
            case let .sentInvitationToContact(u, contact, _): return withUser(u, String(describing: contact))
            case let .contactAlreadyExists(u, contact): return withUser(u, String(describing: contact))
            case let .contactRequestAlreadyAccepted(u, contact): return withUser(u, String(describing: contact))
            case let .contactDeleted(u, contact): return withUser(u, String(describing: contact))
            case let .contactDeletedByContact(u, contact): return withUser(u, String(describing: contact))
            case let .chatCleared(u, chatInfo): return withUser(u, String(describing: chatInfo))
            case .userProfileNoChange: return noDetails
            case let .userProfileUpdated(u, _, toProfile, _): return withUser(u, String(describing: toProfile))
            case let .userPrivacy(u, updatedUser): return withUser(u, String(describing: updatedUser))
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
            case let .groupMemberUpdated(u, groupInfo, fromMember, toMember): return withUser(u, "groupInfo: \(groupInfo)\nfromMember: \(fromMember)\ntoMember: \(toMember)")
            case let .contactsSubscribed(server, contactRefs): return "server: \(server)\ncontacts:\n\(String(describing: contactRefs))"
            case let .contactsDisconnected(server, contactRefs): return "server: \(server)\ncontacts:\n\(String(describing: contactRefs))"
            case let .contactSubSummary(u, contactSubscriptions): return withUser(u, String(describing: contactSubscriptions))
            case let .networkStatus(status, conns): return "networkStatus: \(String(describing: status))\nconnections: \(String(describing: conns))"
            case let .networkStatuses(u, statuses): return withUser(u, String(describing: statuses))
            case let .groupSubscribed(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .memberSubErrors(u, memberSubErrors): return withUser(u, String(describing: memberSubErrors))
            case let .groupEmpty(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case .userContactLinkSubscribed: return noDetails
            case let .newChatItem(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemStatusUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemNotChanged(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .chatItemReaction(u, added, reaction): return withUser(u, "added: \(added)\n\(String(describing: reaction))")
            case let .chatItemDeleted(u, deletedChatItem, toChatItem, byUser): return withUser(u, "deletedChatItem:\n\(String(describing: deletedChatItem))\ntoChatItem:\n\(String(describing: toChatItem))\nbyUser: \(byUser)")
            case let .contactsList(u, contacts): return withUser(u, String(describing: contacts))
            case let .groupCreated(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .sentGroupInvitation(u, groupInfo, contact, member): return withUser(u, "groupInfo: \(groupInfo)\ncontact: \(contact)\nmember: \(member)")
            case let .userAcceptedGroupSent(u, groupInfo, hostContact): return withUser(u, "groupInfo: \(groupInfo)\nhostContact: \(String(describing: hostContact))")
            case let .groupLinkConnecting(u, groupInfo, hostMember): return withUser(u, "groupInfo: \(groupInfo)\nhostMember: \(String(describing: hostMember))")
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
            case let .connectedToGroupMember(u, groupInfo, member, memberContact): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)\nmemberContact: \(String(describing: memberContact))")
            case let .groupRemoved(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .groupUpdated(u, toGroup): return withUser(u, String(describing: toGroup))
            case let .groupLinkCreated(u, groupInfo, connReqContact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\nconnReqContact: \(connReqContact)\nmemberRole: \(memberRole)")
            case let .groupLink(u, groupInfo, connReqContact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\nconnReqContact: \(connReqContact)\nmemberRole: \(memberRole)")
            case let .groupLinkDeleted(u, groupInfo): return withUser(u, String(describing: groupInfo))
            case let .newMemberContact(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
            case let .newMemberContactSentInv(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
            case let .newMemberContactReceivedInv(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
            case let .rcvFileAccepted(u, chatItem): return withUser(u, String(describing: chatItem))
            case .rcvFileAcceptedSndCancelled: return noDetails
            case let .rcvFileStart(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .rcvFileProgressXFTP(u, chatItem, receivedSize, totalSize): return withUser(u, "chatItem: \(String(describing: chatItem))\nreceivedSize: \(receivedSize)\ntotalSize: \(totalSize)")
            case let .rcvFileComplete(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .rcvFileCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .rcvFileSndCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .rcvFileError(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .sndFileStart(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileComplete(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileCancelled(u, chatItem, _, _): return withUser(u, String(describing: chatItem))
            case let .sndFileRcvCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileProgressXFTP(u, chatItem, _, sentSize, totalSize): return withUser(u, "chatItem: \(String(describing: chatItem))\nsentSize: \(sentSize)\ntotalSize: \(totalSize)")
            case let .sndFileCompleteXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
            case let .sndFileError(u, chatItem): return withUser(u, String(describing: chatItem))
            case let .callInvitation(inv): return String(describing: inv)
            case let .callOffer(u, contact, callType, offer, sharedKey, askConfirmation): return withUser(u, "contact: \(contact.id)\ncallType: \(String(describing: callType))\nsharedKey: \(sharedKey ?? "")\naskConfirmation: \(askConfirmation)\noffer: \(String(describing: offer))")
            case let .callAnswer(u, contact, answer): return withUser(u, "contact: \(contact.id)\nanswer: \(String(describing: answer))")
            case let .callExtraInfo(u, contact, extraInfo): return withUser(u, "contact: \(contact.id)\nextraInfo: \(String(describing: extraInfo))")
            case let .callEnded(u, contact): return withUser(u, "contact: \(contact.id)")
            case let .callInvitations(invs): return String(describing: invs)
            case let .ntfTokenStatus(status): return String(describing: status)
            case let .ntfToken(token, status, ntfMode): return "token: \(token)\nstatus: \(status.rawValue)\nntfMode: \(ntfMode.rawValue)"
            case let .ntfMessages(u, connEntity, msgTs, ntfMessages): return withUser(u, "connEntity: \(String(describing: connEntity))\nmsgTs: \(String(describing: msgTs))\nntfMessages: \(String(describing: ntfMessages))")
            case let .ntfMessage(u, connEntity, ntfMessage): return withUser(u, "connEntity: \(String(describing: connEntity))\nntfMessage: \(String(describing: ntfMessage))")
            case let .contactConnectionDeleted(u, connection): return withUser(u, String(describing: connection))
            case let .remoteCtrlList(remoteCtrls): return String(describing: remoteCtrls)
            case let .remoteCtrlFound(remoteCtrl, ctrlAppInfo_, appVersion, compatible): return "remoteCtrl:\n\(String(describing: remoteCtrl))\nctrlAppInfo_:\n\(String(describing: ctrlAppInfo_))\nappVersion: \(appVersion)\ncompatible: \(compatible)"
            case let .remoteCtrlConnecting(remoteCtrl_, ctrlAppInfo, appVersion): return "remoteCtrl_:\n\(String(describing: remoteCtrl_))\nctrlAppInfo:\n\(String(describing: ctrlAppInfo))\nappVersion: \(appVersion)"
            case let .remoteCtrlSessionCode(remoteCtrl_, sessionCode): return "remoteCtrl_:\n\(String(describing: remoteCtrl_))\nsessionCode: \(sessionCode)"
            case let .remoteCtrlConnected(remoteCtrl): return String(describing: remoteCtrl)
            case .remoteCtrlStopped: return noDetails
            case let .versionInfo(versionInfo, chatMigrations, agentMigrations): return "\(String(describing: versionInfo))\n\nchat migrations: \(chatMigrations.map(\.upName))\n\nagent migrations: \(agentMigrations.map(\.upName))"
            case .cmdOk: return noDetails
            case let .chatCmdError(u, chatError): return withUser(u, String(describing: chatError))
            case let .chatError(u, chatError): return withUser(u, String(describing: chatError))
            case let .archiveImported(archiveErrors): return String(describing: archiveErrors)
            }
        }
    }

    private var noDetails: String { get { "\(responseType): no details" } }

    private func withUser(_ u: (any UserLike)?, _ s: String) -> String {
        if let id = u?.userId {
            return "userId: \(id)\n\(s)"
        }
        return s
    }
}

public func chatError(_ chatResponse: ChatResponse) -> ChatErrorType? {
    switch chatResponse {
    case let .chatCmdError(_, .error(error)): return error
    case let .chatError(_, .error(error)): return error
    default: return nil
    }
}

public enum ConnectionPlan: Decodable {
    case invitationLink(invitationLinkPlan: InvitationLinkPlan)
    case contactAddress(contactAddressPlan: ContactAddressPlan)
    case groupLink(groupLinkPlan: GroupLinkPlan)
}

public enum InvitationLinkPlan: Decodable {
    case ok
    case ownLink
    case connecting(contact_: Contact?)
    case known(contact: Contact)
}

public enum ContactAddressPlan: Decodable {
    case ok
    case ownLink
    case connectingConfirmReconnect
    case connectingProhibit(contact: Contact)
    case known(contact: Contact)
    case contactViaAddress(contact: Contact)
}

public enum GroupLinkPlan: Decodable {
    case ok
    case ownLink(groupInfo: GroupInfo)
    case connectingConfirmReconnect
    case connectingProhibit(groupInfo_: GroupInfo?)
    case known(groupInfo: GroupInfo)
}

struct NewUser: Encodable {
    var profile: Profile?
    var sameServers: Bool
    var pastTimestamp: Bool
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
    var fileSource: CryptoFile?
    var quotedItemId: Int64?
    var msgContent: MsgContent
}

public struct XFTPFileConfig: Encodable {
    var minFileSize: Int64
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

public enum ServerProtocol: String, Decodable {
    case smp
    case xftp
}

public struct ProtoServersConfig: Codable {
    public var servers: [ServerCfg]
}

public struct UserProtoServers: Decodable {
    public var serverProtocol: ServerProtocol
    public var protoServers: [ServerCfg]
    public var presetServers: [String]
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

public enum ProtocolTestStep: String, Decodable, Equatable {
    case connect
    case disconnect
    case createQueue
    case secureQueue
    case deleteQueue
    case createFile
    case uploadFile
    case downloadFile
    case compareFile
    case deleteFile

    var text: String {
        switch self {
        case .connect: return NSLocalizedString("Connect", comment: "server test step")
        case .disconnect: return NSLocalizedString("Disconnect", comment: "server test step")
        case .createQueue: return NSLocalizedString("Create queue", comment: "server test step")
        case .secureQueue: return NSLocalizedString("Secure queue", comment: "server test step")
        case .deleteQueue: return NSLocalizedString("Delete queue", comment: "server test step")
        case .createFile: return NSLocalizedString("Create file", comment: "server test step")
        case .uploadFile: return NSLocalizedString("Upload file", comment: "server test step")
        case .downloadFile: return NSLocalizedString("Download file", comment: "server test step")
        case .compareFile: return NSLocalizedString("Compare file", comment: "server test step")
        case .deleteFile: return NSLocalizedString("Delete file", comment: "server test step")
        }
    }
}

public struct ProtocolTestFailure: Decodable, Error, Equatable {
    var testStep: ProtocolTestStep
    var testError: AgentErrorType

    public static func == (l: ProtocolTestFailure, r: ProtocolTestFailure) -> Bool {
        l.testStep == r.testStep
    }

    public var localizedDescription: String {
        let err = String.localizedStringWithFormat(NSLocalizedString("Test failed at step %@.", comment: "server test failure"), testStep.text)
        switch testError {
        case .SMP(.AUTH):
            return err + " " + NSLocalizedString("Server requires authorization to create queues, check password", comment: "server test error")
        case .XFTP(.AUTH):
            return err + " " + NSLocalizedString("Server requires authorization to upload, check password", comment: "server test error")
        case .BROKER(_, .NETWORK):
            return err + " " + NSLocalizedString("Possibly, certificate fingerprint in server address is incorrect", comment: "server test error")
        default:
            return err
        }
    }
}

public struct ServerAddress: Decodable {
    public var serverProtocol: ServerProtocol
    public var hostnames: [String]
    public var port: String
    public var keyHash: String
    public var basicAuth: String

    public init(serverProtocol: ServerProtocol, hostnames: [String], port: String, keyHash: String, basicAuth: String = "") {
        self.serverProtocol = serverProtocol
        self.hostnames = hostnames
        self.port = port
        self.keyHash = keyHash
        self.basicAuth = basicAuth
    }

    public var uri: String {
        "\(serverProtocol)://\(keyHash)\(basicAuth == "" ? "" : ":" + basicAuth)@\(hostnames.joined(separator: ","))"
    }

    public var valid: Bool {
        hostnames.count > 0 && Set(hostnames).count == hostnames.count
    }

    static func empty(_ serverProtocol: ServerProtocol) -> ServerAddress {
        ServerAddress(
            serverProtocol: serverProtocol,
            hostnames: [],
            port: "",
            keyHash: "",
            basicAuth: ""
        )
    }

    static public var sampleData = ServerAddress(
        serverProtocol: .smp,
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
    public var tcpTimeoutPerKb: Int // microseconds
    public var tcpKeepAlive: KeepAliveOpts?
    public var smpPingInterval: Int // microseconds
    public var smpPingCount: Int // times
    public var logTLSErrors: Bool

    public static let defaults: NetCfg = NetCfg(
        socksProxy: nil,
        sessionMode: TransportSessionMode.user,
        tcpConnectTimeout: 20_000_000,
        tcpTimeout: 15_000_000,
        tcpTimeoutPerKb: 45_000,
        tcpKeepAlive: KeepAliveOpts.defaults,
        smpPingInterval: 1200_000_000,
        smpPingCount: 3,
        logTLSErrors: false
    )

    public static let proxyDefaults: NetCfg = NetCfg(
        socksProxy: nil,
        sessionMode: TransportSessionMode.user,
        tcpConnectTimeout: 30_000_000,
        tcpTimeout: 20_000_000,
        tcpTimeoutPerKb: 60_000,
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

public enum NetworkStatus: Decodable, Equatable {
    case unknown
    case connected
    case disconnected
    case error(connectionError: String)

    public var statusString: LocalizedStringKey {
        get {
            switch self {
            case .connected: return "connected"
            case .error: return "error"
            default: return "connecting"
            }
        }
    }

    public var statusExplanation: LocalizedStringKey {
        get {
            switch self {
            case .connected: return "You are connected to the server used to receive messages from this contact."
            case let .error(err): return "Trying to connect to the server used to receive messages from this contact (error: \(err))."
            default: return "Trying to connect to the server used to receive messages from this contact."
            }
        }
    }

    public var imageName: String {
        get {
            switch self {
            case .unknown: return "circle.dotted"
            case .connected: return "circle.fill"
            case .disconnected: return "ellipsis.circle.fill"
            case .error: return "exclamationmark.circle.fill"
            }
        }
    }
}

public struct ConnNetworkStatus: Decodable {
    public var agentConnId: String
    public var networkStatus: NetworkStatus
}

public struct ChatSettings: Codable {
    public var enableNtfs: MsgFilter
    public var sendRcpts: Bool?
    public var favorite: Bool

    public init(enableNtfs: MsgFilter, sendRcpts: Bool?, favorite: Bool) {
        self.enableNtfs = enableNtfs
        self.sendRcpts = sendRcpts
        self.favorite = favorite
    }

    public static let defaults: ChatSettings = ChatSettings(enableNtfs: .all, sendRcpts: nil, favorite: false)
}

public enum MsgFilter: String, Codable {
    case none
    case all
    case mentions
}

public struct UserMsgReceiptSettings: Codable {
    public var enable: Bool
    public var clearOverrides: Bool

    public init(enable: Bool, clearOverrides: Bool) {
        self.enable = enable
        self.clearOverrides = clearOverrides
    }
}

public struct ConnectionStats: Decodable {
    public var connAgentVersion: Int
    public var rcvQueuesInfo: [RcvQueueInfo]
    public var sndQueuesInfo: [SndQueueInfo]
    public var ratchetSyncState: RatchetSyncState
    public var ratchetSyncSupported: Bool

    public var ratchetSyncAllowed: Bool {
        ratchetSyncSupported && [.allowed, .required].contains(ratchetSyncState)
    }

    public var ratchetSyncSendProhibited: Bool {
        [.required, .started, .agreed].contains(ratchetSyncState)
    }
}

public struct RcvQueueInfo: Codable {
    public var rcvServer: String
    public var rcvSwitchStatus: RcvSwitchStatus?
    public var canAbortSwitch: Bool
}

public enum RcvSwitchStatus: String, Codable {
    case switchStarted = "switch_started"
    case sendingQADD = "sending_qadd"
    case sendingQUSE = "sending_quse"
    case receivedMessage = "received_message"
}

public struct SndQueueInfo: Codable {
    public var sndServer: String
    public var sndSwitchStatus: SndSwitchStatus?
}

public enum SndSwitchStatus: String, Codable {
    case sendingQKEY = "sending_qkey"
    case sendingQTEST = "sending_qtest"
}

public enum QueueDirection: String, Decodable {
    case rcv
    case snd
}

public struct SwitchProgress: Decodable {
    public var queueDirection: QueueDirection
    public var switchPhase: SwitchPhase
    public var connectionStats: ConnectionStats
}

public struct RatchetSyncProgress: Decodable {
    public var ratchetSyncStatus: RatchetSyncState
    public var connectionStats: ConnectionStats
}

public enum RatchetSyncState: String, Decodable {
    case ok
    case allowed
    case required
    case started
    case agreed
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

// This notification mode is for app core, UI uses AppNotificationsMode.off to mean completely disable,
// and .local for periodic background checks
public enum NotificationsMode: String, Decodable, SelectableItem {
    case off = "OFF"
    case periodic = "PERIODIC"
    case instant = "INSTANT"

    public var label: LocalizedStringKey {
        switch self {
        case .off: "Local"
        case .periodic: "Periodically"
        case .instant: "Instantly"
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

public struct RemoteCtrlInfo: Decodable {
    public var remoteCtrlId: Int64
    public var ctrlDeviceName: String
    public var sessionState: RemoteCtrlSessionState?

    public var deviceViewName: String {
        ctrlDeviceName == "" ? "\(remoteCtrlId)" : ctrlDeviceName
    }
}

public enum RemoteCtrlSessionState: Decodable {
    case starting
    case searching
    case connecting
    case pendingConfirmation(sessionCode: String)
    case connected(sessionCode: String)
}

public enum RemoteCtrlStopReason: Decodable {
    case discoveryFailed(chatError: ChatError)
    case connectionFailed(chatError: ChatError)
    case setupFailed(chatError: ChatError)
    case disconnected
}

public struct CtrlAppInfo: Decodable {
    public var appVersionRange: AppVersionRange
    public var deviceName: String
}

public struct AppVersionRange: Decodable {
    public var minVersion: String
    public var maxVersion: String
}

public struct CoreVersionInfo: Decodable {
    public var version: String
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
    case errorRemoteCtrl(remoteCtrlError: RemoteCtrlError)
    case invalidJSON(json: String)
}

public enum ChatErrorType: Decodable {
    case noActiveUser
    case noConnectionUser(agentConnId: String)
    case noSndFileUser(agentSndFileId: String)
    case noRcvFileUser(agentRcvFileId: String)
    case userUnknown
    case activeUserExists
    case userExists
    case differentActiveUser(commandUserId: Int64, activeUserId: Int64)
    case cantDeleteActiveUser(userId: Int64)
    case cantDeleteLastUser(userId: Int64)
    case cantHideLastUser(userId: Int64)
    case hiddenUserAlwaysMuted(userId: Int64)
    case emptyUserPassword(userId: Int64)
    case userAlreadyHidden(userId: Int64)
    case userNotHidden(userId: Int64)
    case chatNotStarted
    case chatNotStopped
    case chatStoreChanged
    case connectionPlan(connectionPlan: ConnectionPlan)
    case invalidConnReq
    case invalidChatMessage(connection: Connection, message: String)
    case contactNotReady(contact: Contact)
    case contactNotActive(contact: Contact)
    case contactDisabled(contact: Contact)
    case connectionDisabled(connection: Connection)
    case groupUserRole(groupInfo: GroupInfo, requiredRole: GroupMemberRole)
    case groupMemberInitialRole(groupInfo: GroupInfo, initialRole: GroupMemberRole)
    case contactIncognitoCantInvite
    case groupIncognitoCantInvite
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
    case fileSize(filePath: String)
    case fileAlreadyReceiving(message: String)
    case fileCancelled(message: String)
    case fileCancel(fileId: Int64, message: String)
    case fileAlreadyExists(filePath: String)
    case fileRead(filePath: String, message: String)
    case fileWrite(filePath: String, message: String)
    case fileSend(fileId: Int64, agentError: String)
    case fileRcvChunk(message: String)
    case fileInternal(message: String)
    case fileImageType(filePath: String)
    case fileImageSize(filePath: String)
    case fileNotReceived(fileId: Int64)
    // case xFTPRcvFile
    // case xFTPSndFile
    case fallbackToSMPProhibited(fileId: Int64)
    case inlineFileProhibited(fileId: Int64)
    case invalidQuote
    case invalidChatItemUpdate
    case invalidChatItemDelete
    case hasCurrentCall
    case noCurrentCall
    case callContact(contactId: Int64)
    case callState
    case directMessagesProhibited(contact: Contact)
    case agentVersion
    case agentNoSubResult(agentConnId: String)
    case commandError(message: String)
    case serverProtocol
    case agentCommandError(message: String)
    case invalidFileDescription(message: String)
    case connectionIncognitoChangeProhibited
    case peerChatVRangeIncompatible
    case internalError(message: String)
    case exception(message: String)
}

public enum StoreError: Decodable {
    case duplicateName
    case userNotFound(userId: Int64)
    case userNotFoundByName(contactName: ContactName)
    case userNotFoundByContactId(contactId: Int64)
    case userNotFoundByGroupId(groupId: Int64)
    case userNotFoundByFileId(fileId: Int64)
    case userNotFoundByContactRequestId(contactRequestId: Int64)
    case contactNotFound(contactId: Int64)
    case contactNotFoundByName(contactName: ContactName)
    case contactNotFoundByMemberId(groupMemberId: Int64)
    case contactNotReady(contactName: ContactName)
    case duplicateContactLink
    case userContactLinkNotFound
    case contactRequestNotFound(contactRequestId: Int64)
    case contactRequestNotFoundByName(contactName: ContactName)
    case groupNotFound(groupId: Int64)
    case groupNotFoundByName(groupName: GroupName)
    case groupMemberNameNotFound(groupId: Int64, groupMemberName: ContactName)
    case groupMemberNotFound(groupMemberId: Int64)
    case groupMemberNotFoundByMemberId(memberId: String)
    case memberContactGroupMemberNotFound(contactId: Int64)
    case groupWithoutUser
    case duplicateGroupMember
    case groupAlreadyJoined
    case groupInvitationNotFound
    case sndFileNotFound(fileId: Int64)
    case sndFileInvalid(fileId: Int64)
    case rcvFileNotFound(fileId: Int64)
    case rcvFileDescrNotFound(fileId: Int64)
    case fileNotFound(fileId: Int64)
    case rcvFileInvalid(fileId: Int64)
    case rcvFileInvalidDescrPart
    case sharedMsgIdNotFoundByFileId(fileId: Int64)
    case fileIdNotFoundBySharedMsgId(sharedMsgId: String)
    case sndFileNotFoundXFTP(agentSndFileId: String)
    case rcvFileNotFoundXFTP(agentRcvFileId: String)
    case connectionNotFound(agentConnId: String)
    case connectionNotFoundById(connId: Int64)
    case connectionNotFoundByMemberId(groupMemberId: Int64)
    case pendingConnectionNotFound(connId: Int64)
    case introNotFound
    case uniqueID
    case internalError(message: String)
    case noMsgDelivery(connId: Int64, agentMsgId: String)
    case badChatItem(itemId: Int64)
    case chatItemNotFound(itemId: Int64)
    case chatItemNotFoundByText(text: String)
    case chatItemSharedMsgIdNotFound(sharedMsgId: String)
    case chatItemNotFoundByFileId(fileId: Int64)
    case chatItemNotFoundByGroupId(groupId: Int64)
    case profileNotFound(profileId: Int64)
    case duplicateGroupLink(groupInfo: GroupInfo)
    case groupLinkNotFound(groupInfo: GroupInfo)
    case hostMemberIdNotFound(groupId: Int64)
    case contactNotFoundByFileId(fileId: Int64)
    case noGroupSndStatus(itemId: Int64, groupMemberId: Int64)
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
    case XFTP(xftpErr: XFTPErrorType)
    case RCP(rcpErr: RCErrorType)
    case BROKER(brokerAddress: String, brokerErr: BrokerErrorType)
    case AGENT(agentErr: SMPAgentError)
    case INTERNAL(internalErr: String)
    case INACTIVE
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
    case RESPONSE(smpErr: String)
    case UNEXPECTED
    case NETWORK
    case HOST
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

public enum XFTPErrorType: Decodable {
    case BLOCK
    case SESSION
    case CMD(cmdErr: ProtocolCommandError)
    case AUTH
    case SIZE
    case QUOTA
    case DIGEST
    case CRYPTO
    case NO_FILE
    case HAS_FILE
    case FILE_IO
    case INTERNAL
}

public enum RCErrorType: Decodable {
    case `internal`(internalErr: String)
    case identity
    case noLocalAddress
    case tlsStartFailed
    case exception(exception: String)
    case ctrlAuth
    case ctrlNotFound
    case ctrlError(ctrlErr: String)
    case version
    case encrypt
    case decrypt
    case blockSize
    case syntax(syntaxErr: String)
}

public enum ProtocolCommandError: Decodable {
    case UNKNOWN
    case SYNTAX
    case PROHIBITED
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
    case A_CRYPTO
    case A_DUPLICATE
    case A_QUEUE(queueErr: String)
}

public enum ArchiveError: Decodable {
    case `import`(chatError: ChatError)
    case importFile(file: String, chatError: ChatError)
}

public enum RemoteCtrlError: Decodable {
    case inactive
    case badState
    case busy
    case timeout
    case disconnected(remoteCtrlId: Int64, reason: String)
    case badInvitation
    case badVersion(appVersion: String)
//    case protocolError(protocolError: RemoteProtocolError)
}
