//
//  APITypes.swift
//  SimpleX
//
//  Created by EP on 01/05/2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SimpleXChat
import SwiftUI

// some constructors are used in SEChatCommand or NSEChatCommand types as well - they must be syncronised
enum ChatCommand: ChatCmdProtocol {
    case showActiveUser
    case createActiveUser(profile: Profile?, pastTimestamp: Bool)
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
    case startChat(mainApp: Bool, enableSndFiles: Bool)
    case checkChatRunning
    case apiStopChat
    case apiActivateChat(restoreChat: Bool)
    case apiSuspendChat(timeoutMicroseconds: Int)
    case apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String)
    case apiSetEncryptLocalFiles(enable: Bool)
    case apiExportArchive(config: ArchiveConfig)
    case apiImportArchive(config: ArchiveConfig)
    case apiDeleteStorage
    case apiStorageEncryption(config: DBEncryptionConfig)
    case testStorageEncryption(key: String)
    case apiSaveSettings(settings: AppSettings)
    case apiGetSettings(settings: AppSettings)
    case apiGetChatTags(userId: Int64)
    case apiGetChats(userId: Int64)
    case apiGetChat(chatId: ChatId, pagination: ChatPagination, search: String)
    case apiGetChatItemInfo(type: ChatType, id: Int64, itemId: Int64)
    case apiSendMessages(type: ChatType, id: Int64, live: Bool, ttl: Int?, composedMessages: [ComposedMessage])
    case apiCreateChatTag(tag: ChatTagData)
    case apiSetChatTags(type: ChatType, id: Int64, tagIds: [Int64])
    case apiDeleteChatTag(tagId: Int64)
    case apiUpdateChatTag(tagId: Int64, tagData: ChatTagData)
    case apiReorderChatTags(tagIds: [Int64])
    case apiCreateChatItems(noteFolderId: Int64, composedMessages: [ComposedMessage])
    case apiReportMessage(groupId: Int64, chatItemId: Int64, reportReason: ReportReason, reportText: String)
    case apiUpdateChatItem(type: ChatType, id: Int64, itemId: Int64, updatedMessage: UpdatedMessage, live: Bool)
    case apiDeleteChatItem(type: ChatType, id: Int64, itemIds: [Int64], mode: CIDeleteMode)
    case apiDeleteMemberChatItem(groupId: Int64, itemIds: [Int64])
    case apiArchiveReceivedReports(groupId: Int64)
    case apiDeleteReceivedReports(groupId: Int64, itemIds: [Int64], mode: CIDeleteMode)
    case apiChatItemReaction(type: ChatType, id: Int64, itemId: Int64, add: Bool, reaction: MsgReaction)
    case apiGetReactionMembers(userId: Int64, groupId: Int64, itemId: Int64, reaction: MsgReaction)
    case apiPlanForwardChatItems(toChatType: ChatType, toChatId: Int64, itemIds: [Int64])
    case apiForwardChatItems(toChatType: ChatType, toChatId: Int64, fromChatType: ChatType, fromChatId: Int64, itemIds: [Int64], ttl: Int?)
    case apiGetNtfToken
    case apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode)
    case apiVerifyToken(token: DeviceToken, nonce: String, code: String)
    case apiCheckToken(token: DeviceToken)
    case apiDeleteToken(token: DeviceToken)
    case apiGetNtfConns(nonce: String, encNtfInfo: String)
    case apiGetConnNtfMessages(connMsgReqs: [ConnMsgReq])
    case apiNewGroup(userId: Int64, incognito: Bool, groupProfile: GroupProfile)
    case apiAddMember(groupId: Int64, contactId: Int64, memberRole: GroupMemberRole)
    case apiJoinGroup(groupId: Int64)
    case apiMembersRole(groupId: Int64, memberIds: [Int64], memberRole: GroupMemberRole)
    case apiBlockMembersForAll(groupId: Int64, memberIds: [Int64], blocked: Bool)
    case apiRemoveMembers(groupId: Int64, memberIds: [Int64], withMessages: Bool)
    case apiLeaveGroup(groupId: Int64)
    case apiListMembers(groupId: Int64)
    case apiUpdateGroupProfile(groupId: Int64, groupProfile: GroupProfile)
    case apiCreateGroupLink(groupId: Int64, memberRole: GroupMemberRole, short: Bool)
    case apiGroupLinkMemberRole(groupId: Int64, memberRole: GroupMemberRole)
    case apiDeleteGroupLink(groupId: Int64)
    case apiGetGroupLink(groupId: Int64)
    case apiCreateMemberContact(groupId: Int64, groupMemberId: Int64)
    case apiSendMemberContactInvitation(contactId: Int64, msg: MsgContent)
    case apiTestProtoServer(userId: Int64, server: String)
    case apiGetServerOperators
    case apiSetServerOperators(operators: [ServerOperator])
    case apiGetUserServers(userId: Int64)
    case apiSetUserServers(userId: Int64, userServers: [UserOperatorServers])
    case apiValidateServers(userId: Int64, userServers: [UserOperatorServers])
    case apiGetUsageConditions
    case apiSetConditionsNotified(conditionsId: Int64)
    case apiAcceptConditions(conditionsId: Int64, operatorIds: [Int64])
    case apiSetChatItemTTL(userId: Int64, seconds: Int64)
    case apiGetChatItemTTL(userId: Int64)
    case apiSetChatTTL(userId: Int64, type: ChatType, id: Int64, seconds: Int64?)
    case apiSetNetworkConfig(networkConfig: NetCfg)
    case apiGetNetworkConfig
    case apiSetNetworkInfo(networkInfo: UserNetworkInfo)
    case reconnectAllServers
    case reconnectServer(userId: Int64, smpServer: String)
    case apiSetChatSettings(type: ChatType, id: Int64, chatSettings: ChatSettings)
    case apiSetMemberSettings(groupId: Int64, groupMemberId: Int64, memberSettings: GroupMemberSettings)
    case apiContactInfo(contactId: Int64)
    case apiGroupMemberInfo(groupId: Int64, groupMemberId: Int64)
    case apiContactQueueInfo(contactId: Int64)
    case apiGroupMemberQueueInfo(groupId: Int64, groupMemberId: Int64)
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
    case apiAddContact(userId: Int64, short: Bool, incognito: Bool)
    case apiSetConnectionIncognito(connId: Int64, incognito: Bool)
    case apiChangeConnectionUser(connId: Int64, userId: Int64)
    case apiConnectPlan(userId: Int64, connLink: String)
    case apiConnect(userId: Int64, incognito: Bool, connLink: CreatedConnLink)
    case apiConnectContactViaAddress(userId: Int64, incognito: Bool, contactId: Int64)
    case apiDeleteChat(type: ChatType, id: Int64, chatDeleteMode: ChatDeleteMode)
    case apiClearChat(type: ChatType, id: Int64)
    case apiListContacts(userId: Int64)
    case apiUpdateProfile(userId: Int64, profile: Profile)
    case apiSetContactPrefs(contactId: Int64, preferences: Preferences)
    case apiSetContactAlias(contactId: Int64, localAlias: String)
    case apiSetGroupAlias(groupId: Int64, localAlias: String)
    case apiSetConnectionAlias(connId: Int64, localAlias: String)
    case apiSetUserUIThemes(userId: Int64, themes: ThemeModeOverrides?)
    case apiSetChatUIThemes(chatId: String, themes: ThemeModeOverrides?)
    case apiCreateMyAddress(userId: Int64, short: Bool)
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
    // WebRTC calls /
    case apiGetNetworkStatuses
    case apiChatRead(type: ChatType, id: Int64)
    case apiChatItemsRead(type: ChatType, id: Int64, itemIds: [Int64])
    case apiChatUnread(type: ChatType, id: Int64, unreadChat: Bool)
    case receiveFile(fileId: Int64, userApprovedRelays: Bool, encrypted: Bool?, inline: Bool?)
    case setFileToReceive(fileId: Int64, userApprovedRelays: Bool, encrypted: Bool?)
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
    case apiUploadStandaloneFile(userId: Int64, file: CryptoFile)
    case apiDownloadStandaloneFile(userId: Int64, url: String, file: CryptoFile)
    case apiStandaloneFileInfo(url: String)
    // misc
    case showVersion
    case getAgentSubsTotal(userId: Int64)
    case getAgentServersSummary(userId: Int64)
    case resetAgentServersStats
    case string(String)

    var cmdString: String {
        get {
            switch self {
            case .showActiveUser: return "/u"
            case let .createActiveUser(profile, pastTimestamp):
                let user = NewUser(profile: profile, pastTimestamp: pastTimestamp)
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
            case let .startChat(mainApp, enableSndFiles): return "/_start main=\(onOff(mainApp)) snd_files=\(onOff(enableSndFiles))"
            case .checkChatRunning: return "/_check running"
            case .apiStopChat: return "/_stop"
            case let .apiActivateChat(restore): return "/_app activate restore=\(onOff(restore))"
            case let .apiSuspendChat(timeoutMicroseconds): return "/_app suspend \(timeoutMicroseconds)"
            case let .apiSetAppFilePaths(filesFolder, tempFolder, assetsFolder): return "/set file paths \(encodeJSON(AppFilePaths(appFilesFolder: filesFolder, appTempFolder: tempFolder, appAssetsFolder: assetsFolder)))"
            case let .apiSetEncryptLocalFiles(enable): return "/_files_encrypt \(onOff(enable))"
            case let .apiExportArchive(cfg): return "/_db export \(encodeJSON(cfg))"
            case let .apiImportArchive(cfg): return "/_db import \(encodeJSON(cfg))"
            case .apiDeleteStorage: return "/_db delete"
            case let .apiStorageEncryption(cfg): return "/_db encryption \(encodeJSON(cfg))"
            case let .testStorageEncryption(key): return "/db test key \(key)"
            case let .apiSaveSettings(settings): return "/_save app settings \(encodeJSON(settings))"
            case let .apiGetSettings(settings): return "/_get app settings \(encodeJSON(settings))"
            case let .apiGetChatTags(userId): return "/_get tags \(userId)"
            case let .apiGetChats(userId): return "/_get chats \(userId) pcc=on"
            case let .apiGetChat(chatId, pagination, search): return "/_get chat \(chatId) \(pagination.cmdString)" +
                (search == "" ? "" : " search=\(search)")
            case let .apiGetChatItemInfo(type, id, itemId): return "/_get item info \(ref(type, id)) \(itemId)"
            case let .apiSendMessages(type, id, live, ttl, composedMessages):
                let msgs = encodeJSON(composedMessages)
                let ttlStr = ttl != nil ? "\(ttl!)" : "default"
                return "/_send \(ref(type, id)) live=\(onOff(live)) ttl=\(ttlStr) json \(msgs)"
            case let .apiCreateChatTag(tag): return "/_create tag \(encodeJSON(tag))"
            case let .apiSetChatTags(type, id, tagIds): return "/_tags \(ref(type, id)) \(tagIds.map({ "\($0)" }).joined(separator: ","))"
            case let .apiDeleteChatTag(tagId): return "/_delete tag \(tagId)"
            case let .apiUpdateChatTag(tagId, tagData): return "/_update tag \(tagId) \(encodeJSON(tagData))"
            case let .apiReorderChatTags(tagIds): return "/_reorder tags \(tagIds.map({ "\($0)" }).joined(separator: ","))"
            case let .apiCreateChatItems(noteFolderId, composedMessages):
                let msgs = encodeJSON(composedMessages)
                return "/_create *\(noteFolderId) json \(msgs)"
            case let .apiReportMessage(groupId, chatItemId, reportReason, reportText):
                return "/_report #\(groupId) \(chatItemId) reason=\(reportReason) \(reportText)"
            case let .apiUpdateChatItem(type, id, itemId, um, live): return "/_update item \(ref(type, id)) \(itemId) live=\(onOff(live)) \(um.cmdString)"
            case let .apiDeleteChatItem(type, id, itemIds, mode): return "/_delete item \(ref(type, id)) \(itemIds.map({ "\($0)" }).joined(separator: ",")) \(mode.rawValue)"
            case let .apiDeleteMemberChatItem(groupId, itemIds): return "/_delete member item #\(groupId) \(itemIds.map({ "\($0)" }).joined(separator: ","))"
            case let .apiArchiveReceivedReports(groupId): return "/_archive reports #\(groupId)"
            case let .apiDeleteReceivedReports(groupId, itemIds, mode): return "/_delete reports #\(groupId) \(itemIds.map({ "\($0)" }).joined(separator: ",")) \(mode.rawValue)"
            case let .apiChatItemReaction(type, id, itemId, add, reaction): return "/_reaction \(ref(type, id)) \(itemId) \(onOff(add)) \(encodeJSON(reaction))"
            case let .apiGetReactionMembers(userId, groupId, itemId, reaction): return "/_reaction members \(userId) #\(groupId) \(itemId) \(encodeJSON(reaction))"
            case let .apiPlanForwardChatItems(type, id, itemIds): return "/_forward plan \(ref(type, id)) \(itemIds.map({ "\($0)" }).joined(separator: ","))"
            case let .apiForwardChatItems(toChatType, toChatId, fromChatType, fromChatId, itemIds, ttl):
                let ttlStr = ttl != nil ? "\(ttl!)" : "default"
                return "/_forward \(ref(toChatType, toChatId)) \(ref(fromChatType, fromChatId)) \(itemIds.map({ "\($0)" }).joined(separator: ",")) ttl=\(ttlStr)"
            case .apiGetNtfToken: return "/_ntf get "
            case let .apiRegisterToken(token, notificationMode): return "/_ntf register \(token.cmdString) \(notificationMode.rawValue)"
            case let .apiVerifyToken(token, nonce, code): return "/_ntf verify \(token.cmdString) \(nonce) \(code)"
            case let .apiCheckToken(token): return "/_ntf check \(token.cmdString)"
            case let .apiDeleteToken(token): return "/_ntf delete \(token.cmdString)"
            case let .apiGetNtfConns(nonce, encNtfInfo): return "/_ntf conns \(nonce) \(encNtfInfo)"
            case let .apiGetConnNtfMessages(connMsgReqs): return "/_ntf conn messages \(connMsgReqs.map { $0.cmdString }.joined(separator: ","))"
            case let .apiNewGroup(userId, incognito, groupProfile): return "/_group \(userId) incognito=\(onOff(incognito)) \(encodeJSON(groupProfile))"
            case let .apiAddMember(groupId, contactId, memberRole): return "/_add #\(groupId) \(contactId) \(memberRole)"
            case let .apiJoinGroup(groupId): return "/_join #\(groupId)"
            case let .apiMembersRole(groupId, memberIds, memberRole): return "/_member role #\(groupId) \(memberIds.map({ "\($0)" }).joined(separator: ",")) \(memberRole.rawValue)"
            case let .apiBlockMembersForAll(groupId, memberIds, blocked): return "/_block #\(groupId) \(memberIds.map({ "\($0)" }).joined(separator: ",")) blocked=\(onOff(blocked))"
            case let .apiRemoveMembers(groupId, memberIds, withMessages): return "/_remove #\(groupId) \(memberIds.map({ "\($0)" }).joined(separator: ",")) messages=\(onOff(withMessages))"
            case let .apiLeaveGroup(groupId): return "/_leave #\(groupId)"
            case let .apiListMembers(groupId): return "/_members #\(groupId)"
            case let .apiUpdateGroupProfile(groupId, groupProfile): return "/_group_profile #\(groupId) \(encodeJSON(groupProfile))"
            case let .apiCreateGroupLink(groupId, memberRole, short): return "/_create link #\(groupId) \(memberRole) short=\(onOff(short))"
            case let .apiGroupLinkMemberRole(groupId, memberRole): return "/_set link role #\(groupId) \(memberRole)"
            case let .apiDeleteGroupLink(groupId): return "/_delete link #\(groupId)"
            case let .apiGetGroupLink(groupId): return "/_get link #\(groupId)"
            case let .apiCreateMemberContact(groupId, groupMemberId): return "/_create member contact #\(groupId) \(groupMemberId)"
            case let .apiSendMemberContactInvitation(contactId, mc): return "/_invite member contact @\(contactId) \(mc.cmdString)"
            case let .apiTestProtoServer(userId, server): return "/_server test \(userId) \(server)"
            case .apiGetServerOperators: return "/_operators"
            case let .apiSetServerOperators(operators): return "/_operators \(encodeJSON(operators))"
            case let .apiGetUserServers(userId): return "/_servers \(userId)"
            case let .apiSetUserServers(userId, userServers): return "/_servers \(userId) \(encodeJSON(userServers))"
            case let .apiValidateServers(userId, userServers): return "/_validate_servers \(userId) \(encodeJSON(userServers))"
            case .apiGetUsageConditions: return "/_conditions"
            case let .apiSetConditionsNotified(conditionsId): return "/_conditions_notified \(conditionsId)"
            case let .apiAcceptConditions(conditionsId, operatorIds): return "/_accept_conditions \(conditionsId) \(joinedIds(operatorIds))"
            case let .apiSetChatItemTTL(userId, seconds): return "/_ttl \(userId) \(chatItemTTLStr(seconds: seconds))"
            case let .apiGetChatItemTTL(userId): return "/_ttl \(userId)"
            case let .apiSetChatTTL(userId, type, id, seconds): return "/_ttl \(userId) \(ref(type, id)) \(chatItemTTLStr(seconds: seconds))"
            case let .apiSetNetworkConfig(networkConfig): return "/_network \(encodeJSON(networkConfig))"
            case .apiGetNetworkConfig: return "/network"
            case let .apiSetNetworkInfo(networkInfo): return "/_network info \(encodeJSON(networkInfo))"
            case .reconnectAllServers: return "/reconnect"
            case let .reconnectServer(userId, smpServer): return "/reconnect \(userId) \(smpServer)"
            case let .apiSetChatSettings(type, id, chatSettings): return "/_settings \(ref(type, id)) \(encodeJSON(chatSettings))"
            case let .apiSetMemberSettings(groupId, groupMemberId, memberSettings): return "/_member settings #\(groupId) \(groupMemberId) \(encodeJSON(memberSettings))"
            case let .apiContactInfo(contactId): return "/_info @\(contactId)"
            case let .apiGroupMemberInfo(groupId, groupMemberId): return "/_info #\(groupId) \(groupMemberId)"
            case let .apiContactQueueInfo(contactId): return "/_queue info @\(contactId)"
            case let .apiGroupMemberQueueInfo(groupId, groupMemberId): return "/_queue info #\(groupId) \(groupMemberId)"
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
            case let .apiAddContact(userId, short, incognito): return "/_connect \(userId) short=\(onOff(short)) incognito=\(onOff(incognito))"
            case let .apiSetConnectionIncognito(connId, incognito): return "/_set incognito :\(connId) \(onOff(incognito))"
            case let .apiChangeConnectionUser(connId, userId): return "/_set conn user :\(connId) \(userId)"
            case let .apiConnectPlan(userId, connLink): return "/_connect plan \(userId) \(connLink)"
            case let .apiConnect(userId, incognito, connLink): return "/_connect \(userId) incognito=\(onOff(incognito)) \(connLink.connFullLink) \(connLink.connShortLink ?? "")"
            case let .apiConnectContactViaAddress(userId, incognito, contactId): return "/_connect contact \(userId) incognito=\(onOff(incognito)) \(contactId)"
            case let .apiDeleteChat(type, id, chatDeleteMode): return "/_delete \(ref(type, id)) \(chatDeleteMode.cmdString)"
            case let .apiClearChat(type, id): return "/_clear chat \(ref(type, id))"
            case let .apiListContacts(userId): return "/_contacts \(userId)"
            case let .apiUpdateProfile(userId, profile): return "/_profile \(userId) \(encodeJSON(profile))"
            case let .apiSetContactPrefs(contactId, preferences): return "/_set prefs @\(contactId) \(encodeJSON(preferences))"
            case let .apiSetContactAlias(contactId, localAlias): return "/_set alias @\(contactId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiSetGroupAlias(groupId, localAlias): return "/_set alias #\(groupId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiSetConnectionAlias(connId, localAlias): return "/_set alias :\(connId) \(localAlias.trimmingCharacters(in: .whitespaces))"
            case let .apiSetUserUIThemes(userId, themes): return "/_set theme user \(userId) \(themes != nil ? encodeJSON(themes) : "")"
            case let .apiSetChatUIThemes(chatId, themes): return "/_set theme \(chatId) \(themes != nil ? encodeJSON(themes) : "")"
            case let .apiCreateMyAddress(userId, short): return "/_address \(userId) short=\(onOff(short))"
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
            case let .apiChatRead(type, id): return "/_read chat \(ref(type, id))"
            case let .apiChatItemsRead(type, id, itemIds): return "/_read chat items \(ref(type, id)) \(joinedIds(itemIds))"
            case let .apiChatUnread(type, id, unreadChat): return "/_unread chat \(ref(type, id)) \(onOff(unreadChat))"
            case let .receiveFile(fileId, userApprovedRelays, encrypt, inline): return "/freceive \(fileId)\(onOffParam("approved_relays", userApprovedRelays))\(onOffParam("encrypt", encrypt))\(onOffParam("inline", inline))"
            case let .setFileToReceive(fileId, userApprovedRelays, encrypt): return "/_set_file_to_receive \(fileId)\(onOffParam("approved_relays", userApprovedRelays))\(onOffParam("encrypt", encrypt))"
            case let .cancelFile(fileId): return "/fcancel \(fileId)"
            case let .setLocalDeviceName(displayName): return "/set device name \(displayName)"
            case let .connectRemoteCtrl(xrcpInv): return "/connect remote ctrl \(xrcpInv)"
            case .findKnownRemoteCtrl: return "/find remote ctrl"
            case let .confirmRemoteCtrl(rcId): return "/confirm remote ctrl \(rcId)"
            case let .verifyRemoteCtrlSession(sessCode): return "/verify remote ctrl \(sessCode)"
            case .listRemoteCtrls: return "/list remote ctrls"
            case .stopRemoteCtrl: return "/stop remote ctrl"
            case let .deleteRemoteCtrl(rcId): return "/delete remote ctrl \(rcId)"
            case let .apiUploadStandaloneFile(userId, file): return "/_upload \(userId) \(file.filePath)"
            case let .apiDownloadStandaloneFile(userId, link, file): return "/_download \(userId) \(link) \(file.filePath)"
            case let .apiStandaloneFileInfo(link): return "/_download info \(link)"
            case .showVersion: return "/version"
            case let .getAgentSubsTotal(userId): return "/get subs total \(userId)"
            case let .getAgentServersSummary(userId): return "/get servers summary \(userId)"
            case .resetAgentServersStats: return "/reset servers stats"
            case let .string(str): return str
            }
        }
    }

    var cmdType: String {
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
            case .checkChatRunning: return "checkChatRunning"
            case .apiStopChat: return "apiStopChat"
            case .apiActivateChat: return "apiActivateChat"
            case .apiSuspendChat: return "apiSuspendChat"
            case .apiSetAppFilePaths: return "apiSetAppFilePaths"
            case .apiSetEncryptLocalFiles: return "apiSetEncryptLocalFiles"
            case .apiExportArchive: return "apiExportArchive"
            case .apiImportArchive: return "apiImportArchive"
            case .apiDeleteStorage: return "apiDeleteStorage"
            case .apiStorageEncryption: return "apiStorageEncryption"
            case .testStorageEncryption: return "testStorageEncryption"
            case .apiSaveSettings: return "apiSaveSettings"
            case .apiGetSettings: return "apiGetSettings"
            case .apiGetChatTags: return "apiGetChatTags"
            case .apiGetChats: return "apiGetChats"
            case .apiGetChat: return "apiGetChat"
            case .apiGetChatItemInfo: return "apiGetChatItemInfo"
            case .apiSendMessages: return "apiSendMessages"
            case .apiCreateChatTag: return "apiCreateChatTag"
            case .apiSetChatTags: return "apiSetChatTags"
            case .apiDeleteChatTag: return "apiDeleteChatTag"
            case .apiUpdateChatTag: return "apiUpdateChatTag"
            case .apiReorderChatTags: return "apiReorderChatTags"
            case .apiCreateChatItems: return "apiCreateChatItems"
            case .apiReportMessage: return "apiReportMessage"
            case .apiUpdateChatItem: return "apiUpdateChatItem"
            case .apiDeleteChatItem: return "apiDeleteChatItem"
            case .apiConnectContactViaAddress: return "apiConnectContactViaAddress"
            case .apiDeleteMemberChatItem: return "apiDeleteMemberChatItem"
            case .apiArchiveReceivedReports: return "apiArchiveReceivedReports"
            case .apiDeleteReceivedReports: return "apiDeleteReceivedReports"
            case .apiChatItemReaction: return "apiChatItemReaction"
            case .apiGetReactionMembers: return "apiGetReactionMembers"
            case .apiPlanForwardChatItems: return "apiPlanForwardChatItems"
            case .apiForwardChatItems: return "apiForwardChatItems"
            case .apiGetNtfToken: return "apiGetNtfToken"
            case .apiRegisterToken: return "apiRegisterToken"
            case .apiVerifyToken: return "apiVerifyToken"
            case .apiCheckToken: return "apiCheckToken"
            case .apiDeleteToken: return "apiDeleteToken"
            case .apiGetNtfConns: return "apiGetNtfConns"
            case .apiGetConnNtfMessages: return "apiGetConnNtfMessages"
            case .apiNewGroup: return "apiNewGroup"
            case .apiAddMember: return "apiAddMember"
            case .apiJoinGroup: return "apiJoinGroup"
            case .apiMembersRole: return "apiMembersRole"
            case .apiBlockMembersForAll: return "apiBlockMembersForAll"
            case .apiRemoveMembers: return "apiRemoveMembers"
            case .apiLeaveGroup: return "apiLeaveGroup"
            case .apiListMembers: return "apiListMembers"
            case .apiUpdateGroupProfile: return "apiUpdateGroupProfile"
            case .apiCreateGroupLink: return "apiCreateGroupLink"
            case .apiGroupLinkMemberRole: return "apiGroupLinkMemberRole"
            case .apiDeleteGroupLink: return "apiDeleteGroupLink"
            case .apiGetGroupLink: return "apiGetGroupLink"
            case .apiCreateMemberContact: return "apiCreateMemberContact"
            case .apiSendMemberContactInvitation: return "apiSendMemberContactInvitation"
            case .apiTestProtoServer: return "apiTestProtoServer"
            case .apiGetServerOperators: return "apiGetServerOperators"
            case .apiSetServerOperators: return "apiSetServerOperators"
            case .apiGetUserServers: return "apiGetUserServers"
            case .apiSetUserServers: return "apiSetUserServers"
            case .apiValidateServers: return "apiValidateServers"
            case .apiGetUsageConditions: return "apiGetUsageConditions"
            case .apiSetConditionsNotified: return "apiSetConditionsNotified"
            case .apiAcceptConditions: return "apiAcceptConditions"
            case .apiSetChatItemTTL: return "apiSetChatItemTTL"
            case .apiGetChatItemTTL: return "apiGetChatItemTTL"
            case .apiSetChatTTL: return "apiSetChatTTL"
            case .apiSetNetworkConfig: return "apiSetNetworkConfig"
            case .apiGetNetworkConfig: return "apiGetNetworkConfig"
            case .apiSetNetworkInfo: return "apiSetNetworkInfo"
            case .reconnectAllServers: return "reconnectAllServers"
            case .reconnectServer: return "reconnectServer"
            case .apiSetChatSettings: return "apiSetChatSettings"
            case .apiSetMemberSettings: return "apiSetMemberSettings"
            case .apiContactInfo: return "apiContactInfo"
            case .apiGroupMemberInfo: return "apiGroupMemberInfo"
            case .apiContactQueueInfo: return "apiContactQueueInfo"
            case .apiGroupMemberQueueInfo: return "apiGroupMemberQueueInfo"
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
            case .apiChangeConnectionUser: return "apiChangeConnectionUser"
            case .apiConnectPlan: return "apiConnectPlan"
            case .apiConnect: return "apiConnect"
            case .apiDeleteChat: return "apiDeleteChat"
            case .apiClearChat: return "apiClearChat"
            case .apiListContacts: return "apiListContacts"
            case .apiUpdateProfile: return "apiUpdateProfile"
            case .apiSetContactPrefs: return "apiSetContactPrefs"
            case .apiSetContactAlias: return "apiSetContactAlias"
            case .apiSetGroupAlias: return "apiSetGroupAlias"
            case .apiSetConnectionAlias: return "apiSetConnectionAlias"
            case .apiSetUserUIThemes: return "apiSetUserUIThemes"
            case .apiSetChatUIThemes: return "apiSetChatUIThemes"
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
            case .apiChatItemsRead: return "apiChatItemsRead"
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
            case .apiUploadStandaloneFile: return "apiUploadStandaloneFile"
            case .apiDownloadStandaloneFile: return "apiDownloadStandaloneFile"
            case .apiStandaloneFileInfo: return "apiStandaloneFileInfo"
            case .showVersion: return "showVersion"
            case .getAgentSubsTotal: return "getAgentSubsTotal"
            case .getAgentServersSummary: return "getAgentServersSummary"
            case .resetAgentServersStats: return "resetAgentServersStats"
            case .string: return "console command"
            }
        }
    }

    func ref(_ type: ChatType, _ id: Int64) -> String {
        "\(type.rawValue)\(id)"
    }

    func joinedIds(_ ids: [Int64]) -> String {
        ids.map { "\($0)" }.joined(separator: ",")
    }

    func chatItemTTLStr(seconds: Int64?) -> String {
        if let seconds = seconds {
            return String(seconds)
        } else {
            return "default"
        }
    }

    var obfuscated: ChatCommand {
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
        case let .testStorageEncryption(key):
            return .testStorageEncryption(key: obfuscate(key))
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

// ChatResponse is split to three enums to reduce stack size used when parsing it, parsing large enums is very inefficient.
enum ChatResponse0: Decodable, ChatAPIResult {
    case activeUser(user: User)
    case usersList(users: [UserInfo])
    case chatStarted
    case chatRunning
    case chatStopped
    case apiChats(user: UserRef, chats: [ChatData])
    case apiChat(user: UserRef, chat: ChatData, navInfo: NavigationInfo?)
    case chatTags(user: UserRef, userTags: [ChatTag])
    case chatItemInfo(user: UserRef, chatItem: AChatItem, chatItemInfo: ChatItemInfo)
    case serverTestResult(user: UserRef, testServer: String, testFailure: ProtocolTestFailure?)
    case serverOperatorConditions(conditions: ServerOperatorConditions)
    case userServers(user: UserRef, userServers: [UserOperatorServers])
    case userServersValidation(user: UserRef, serverErrors: [UserServersError])
    case usageConditions(usageConditions: UsageConditions, conditionsText: String, acceptedConditions: UsageConditions?)
    case chatItemTTL(user: UserRef, chatItemTTL: Int64?)
    case networkConfig(networkConfig: NetCfg)
    case contactInfo(user: UserRef, contact: Contact, connectionStats_: ConnectionStats?, customUserProfile: Profile?)
    case groupMemberInfo(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats_: ConnectionStats?)
    case queueInfo(user: UserRef, rcvMsgInfo: RcvMsgInfo?, queueInfo: ServerQueueInfo)
    case contactSwitchStarted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberSwitchStarted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactSwitchAborted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberSwitchAborted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactRatchetSyncStarted(user: UserRef, contact: Contact, connectionStats: ConnectionStats)
    case groupMemberRatchetSyncStarted(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionStats: ConnectionStats)
    case contactCode(user: UserRef, contact: Contact, connectionCode: String)
    case groupMemberCode(user: UserRef, groupInfo: GroupInfo, member: GroupMember, connectionCode: String)
    case connectionVerified(user: UserRef, verified: Bool, expectedCode: String)
    case tagsUpdated(user: UserRef, userTags: [ChatTag], chatTags: [Int64])

    var responseType: String {
        switch self {
        case .activeUser: "activeUser"
        case .usersList: "usersList"
        case .chatStarted: "chatStarted"
        case .chatRunning: "chatRunning"
        case .chatStopped: "chatStopped"
        case .apiChats: "apiChats"
        case .apiChat: "apiChat"
        case .chatTags: "chatTags"
        case .chatItemInfo: "chatItemInfo"
        case .serverTestResult: "serverTestResult"
        case .serverOperatorConditions: "serverOperators"
        case .userServers: "userServers"
        case .userServersValidation: "userServersValidation"
        case .usageConditions: "usageConditions"
        case .chatItemTTL: "chatItemTTL"
        case .networkConfig: "networkConfig"
        case .contactInfo: "contactInfo"
        case .groupMemberInfo: "groupMemberInfo"
        case .queueInfo: "queueInfo"
        case .contactSwitchStarted: "contactSwitchStarted"
        case .groupMemberSwitchStarted: "groupMemberSwitchStarted"
        case .contactSwitchAborted: "contactSwitchAborted"
        case .groupMemberSwitchAborted: "groupMemberSwitchAborted"
        case .contactRatchetSyncStarted: "contactRatchetSyncStarted"
        case .groupMemberRatchetSyncStarted: "groupMemberRatchetSyncStarted"
        case .contactCode: "contactCode"
        case .groupMemberCode: "groupMemberCode"
        case .connectionVerified: "connectionVerified"
        case .tagsUpdated: "tagsUpdated"
        }
    }
    
    var details: String {
        switch self {
        case let .activeUser(user): return String(describing: user)
        case let .usersList(users): return String(describing: users)
        case .chatStarted: return noDetails
        case .chatRunning: return noDetails
        case .chatStopped: return noDetails
        case let .apiChats(u, chats): return withUser(u, String(describing: chats))
        case let .apiChat(u, chat, navInfo): return withUser(u, "chat: \(String(describing: chat))\nnavInfo: \(String(describing: navInfo))")
        case let .chatTags(u, userTags): return withUser(u, "userTags: \(String(describing: userTags))")
        case let .chatItemInfo(u, chatItem, chatItemInfo): return withUser(u, "chatItem: \(String(describing: chatItem))\nchatItemInfo: \(String(describing: chatItemInfo))")
        case let .serverTestResult(u, server, testFailure): return withUser(u, "server: \(server)\nresult: \(String(describing: testFailure))")
        case let .serverOperatorConditions(conditions): return "conditions: \(String(describing: conditions))"
        case let .userServers(u, userServers): return withUser(u, "userServers: \(String(describing: userServers))")
        case let .userServersValidation(u, serverErrors): return withUser(u, "serverErrors: \(String(describing: serverErrors))")
        case let .usageConditions(usageConditions, _, acceptedConditions): return "usageConditions: \(String(describing: usageConditions))\nacceptedConditions: \(String(describing: acceptedConditions))"
        case let .chatItemTTL(u, chatItemTTL): return withUser(u, String(describing: chatItemTTL))
        case let .networkConfig(networkConfig): return String(describing: networkConfig)
        case let .contactInfo(u, contact, connectionStats_, customUserProfile): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats_: \(String(describing: connectionStats_))\ncustomUserProfile: \(String(describing: customUserProfile))")
        case let .groupMemberInfo(u, groupInfo, member, connectionStats_): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats_: \(String(describing: connectionStats_))")
        case let .queueInfo(u, rcvMsgInfo, queueInfo):
            let msgInfo = if let info = rcvMsgInfo { encodeJSON(info) } else { "none" }
            return withUser(u, "rcvMsgInfo: \(msgInfo)\nqueueInfo: \(encodeJSON(queueInfo))")
        case let .contactSwitchStarted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
        case let .groupMemberSwitchStarted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
        case let .contactSwitchAborted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
        case let .groupMemberSwitchAborted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
        case let .contactRatchetSyncStarted(u, contact, connectionStats): return withUser(u, "contact: \(String(describing: contact))\nconnectionStats: \(String(describing: connectionStats))")
        case let .groupMemberRatchetSyncStarted(u, groupInfo, member, connectionStats): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionStats: \(String(describing: connectionStats))")
        case let .contactCode(u, contact, connectionCode): return withUser(u, "contact: \(String(describing: contact))\nconnectionCode: \(connectionCode)")
        case let .groupMemberCode(u, groupInfo, member, connectionCode): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nconnectionCode: \(connectionCode)")
        case let .connectionVerified(u, verified, expectedCode): return withUser(u, "verified: \(verified)\nconnectionCode: \(expectedCode)")
        case let .tagsUpdated(u, userTags, chatTags): return withUser(u, "userTags: \(String(describing: userTags))\nchatTags: \(String(describing: chatTags))")
        }
    }

    static func fallbackResult(_ type: String, _ json: NSDictionary) -> ChatResponse0? {
        if type == "apiChats" {
            if let r = parseApiChats(json) {
                return .apiChats(user: r.user, chats: r.chats)
            }
        } else if type == "apiChat" {
            if let jApiChat = json["apiChat"] as? NSDictionary,
               let user: UserRef = try? decodeObject(jApiChat["user"] as Any),
               let jChat = jApiChat["chat"] as? NSDictionary,
               let (chat, navInfo) = try? parseChatData(jChat, jApiChat["navInfo"] as? NSDictionary) {
                return .apiChat(user: user, chat: chat, navInfo: navInfo)
            }
        }
        return nil
    }
}

enum ChatResponse1: Decodable, ChatAPIResult {
    case invitation(user: UserRef, connLinkInvitation: CreatedConnLink, connection: PendingContactConnection)
    case connectionIncognitoUpdated(user: UserRef, toConnection: PendingContactConnection)
    case connectionUserChanged(user: UserRef, fromConnection: PendingContactConnection, toConnection: PendingContactConnection, newUser: UserRef)
    case connectionPlan(user: UserRef, connLink: CreatedConnLink, connectionPlan: ConnectionPlan)
    case sentConfirmation(user: UserRef, connection: PendingContactConnection)
    case sentInvitation(user: UserRef, connection: PendingContactConnection)
    case sentInvitationToContact(user: UserRef, contact: Contact, customUserProfile: Profile?)
    case contactAlreadyExists(user: UserRef, contact: Contact)
    case contactDeleted(user: UserRef, contact: Contact)
    case contactConnectionDeleted(user: UserRef, connection: PendingContactConnection)
    case groupDeletedUser(user: UserRef, groupInfo: GroupInfo)
    case chatCleared(user: UserRef, chatInfo: ChatInfo)
    case userProfileNoChange(user: User)
    case userProfileUpdated(user: User, fromProfile: Profile, toProfile: Profile, updateSummary: UserProfileUpdateSummary)
    case userPrivacy(user: User, updatedUser: User)
    case contactAliasUpdated(user: UserRef, toContact: Contact)
    case groupAliasUpdated(user: UserRef, toGroup: GroupInfo)
    case connectionAliasUpdated(user: UserRef, toConnection: PendingContactConnection)
    case contactPrefsUpdated(user: User, fromContact: Contact, toContact: Contact)
    case userContactLink(user: User, contactLink: UserContactLink)
    case userContactLinkUpdated(user: User, contactLink: UserContactLink)
    case userContactLinkCreated(user: User, connLinkContact: CreatedConnLink)
    case userContactLinkDeleted(user: User)
    case acceptingContactRequest(user: UserRef, contact: Contact)
    case contactRequestRejected(user: UserRef)
    case networkStatuses(user_: UserRef?, networkStatuses: [ConnNetworkStatus])
    case newChatItems(user: UserRef, chatItems: [AChatItem])
    case groupChatItemsDeleted(user: UserRef, groupInfo: GroupInfo, chatItemIDs: Set<Int64>, byUser: Bool, member_: GroupMember?)
    case forwardPlan(user: UserRef, chatItemIds: [Int64], forwardConfirmation: ForwardConfirmation?)
    case chatItemUpdated(user: UserRef, chatItem: AChatItem)
    case chatItemNotChanged(user: UserRef, chatItem: AChatItem)
    case chatItemReaction(user: UserRef, added: Bool, reaction: ACIReaction)
    case reactionMembers(user: UserRef, memberReactions: [MemberReaction])
    case chatItemsDeleted(user: UserRef, chatItemDeletions: [ChatItemDeletion], byUser: Bool)
    case contactsList(user: UserRef, contacts: [Contact])

    var responseType: String {
        switch self {
        case .invitation: "invitation"
        case .connectionIncognitoUpdated: "connectionIncognitoUpdated"
        case .connectionUserChanged: "connectionUserChanged"
        case .connectionPlan: "connectionPlan"
        case .sentConfirmation: "sentConfirmation"
        case .sentInvitation: "sentInvitation"
        case .sentInvitationToContact: "sentInvitationToContact"
        case .contactAlreadyExists: "contactAlreadyExists"
        case .contactDeleted: "contactDeleted"
        case .contactConnectionDeleted: "contactConnectionDeleted"
        case .groupDeletedUser: "groupDeletedUser"
        case .chatCleared: "chatCleared"
        case .userProfileNoChange: "userProfileNoChange"
        case .userProfileUpdated: "userProfileUpdated"
        case .userPrivacy: "userPrivacy"
        case .contactAliasUpdated: "contactAliasUpdated"
        case .groupAliasUpdated: "groupAliasUpdated"
        case .connectionAliasUpdated: "connectionAliasUpdated"
        case .contactPrefsUpdated: "contactPrefsUpdated"
        case .userContactLink: "userContactLink"
        case .userContactLinkUpdated: "userContactLinkUpdated"
        case .userContactLinkCreated: "userContactLinkCreated"
        case .userContactLinkDeleted: "userContactLinkDeleted"
        case .acceptingContactRequest: "acceptingContactRequest"
        case .contactRequestRejected: "contactRequestRejected"
        case .networkStatuses: "networkStatuses"
        case .newChatItems: "newChatItems"
        case .groupChatItemsDeleted: "groupChatItemsDeleted"
        case .forwardPlan: "forwardPlan"
        case .chatItemUpdated: "chatItemUpdated"
        case .chatItemNotChanged: "chatItemNotChanged"
        case .chatItemReaction: "chatItemReaction"
        case .reactionMembers: "reactionMembers"
        case .chatItemsDeleted: "chatItemsDeleted"
        case .contactsList: "contactsList"
        }
    }
    
    var details: String {
        switch self {
        case let .contactDeleted(u, contact): return withUser(u, String(describing: contact))
        case let .contactConnectionDeleted(u, connection): return withUser(u, String(describing: connection))
        case let .groupDeletedUser(u, groupInfo): return withUser(u, String(describing: groupInfo))
        case let .chatCleared(u, chatInfo): return withUser(u, String(describing: chatInfo))
        case .userProfileNoChange: return noDetails
        case let .userProfileUpdated(u, _, toProfile, _): return withUser(u, String(describing: toProfile))
        case let .userPrivacy(u, updatedUser): return withUser(u, String(describing: updatedUser))
        case let .contactAliasUpdated(u, toContact): return withUser(u, String(describing: toContact))
        case let .groupAliasUpdated(u, toGroup): return withUser(u, String(describing: toGroup))
        case let .connectionAliasUpdated(u, toConnection): return withUser(u, String(describing: toConnection))
        case let .contactPrefsUpdated(u, fromContact, toContact): return withUser(u, "fromContact: \(String(describing: fromContact))\ntoContact: \(String(describing: toContact))")
        case let .userContactLink(u, contactLink): return withUser(u, contactLink.responseDetails)
        case let .userContactLinkUpdated(u, contactLink): return withUser(u, contactLink.responseDetails)
        case let .userContactLinkCreated(u, connLink): return withUser(u, String(describing: connLink))
        case .userContactLinkDeleted: return noDetails
        case let .acceptingContactRequest(u, contact): return withUser(u, String(describing: contact))
        case .contactRequestRejected: return noDetails
        case let .networkStatuses(u, statuses): return withUser(u, String(describing: statuses))
        case let .newChatItems(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .groupChatItemsDeleted(u, gInfo, chatItemIDs, byUser, member_):
            return withUser(u, "chatItemIDs: \(String(describing: chatItemIDs))\ngroupInfo: \(String(describing: gInfo))\nbyUser: \(byUser)\nmember_: \(String(describing: member_))")
        case let .forwardPlan(u, chatItemIds, forwardConfirmation): return withUser(u, "items: \(chatItemIds) forwardConfirmation: \(String(describing: forwardConfirmation))")
        case let .chatItemUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .chatItemNotChanged(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .chatItemReaction(u, added, reaction): return withUser(u, "added: \(added)\n\(String(describing: reaction))")
        case let .reactionMembers(u, reaction): return withUser(u, "memberReactions: \(String(describing: reaction))")
        case let .chatItemsDeleted(u, items, byUser):
            let itemsString = items.map { item in
                "deletedChatItem:\n\(String(describing: item.deletedChatItem))\ntoChatItem:\n\(String(describing: item.toChatItem))" }.joined(separator: "\n")
            return withUser(u, itemsString + "\nbyUser: \(byUser)")
        case let .contactsList(u, contacts): return withUser(u, String(describing: contacts))
        case let .invitation(u, connLinkInvitation, connection): return withUser(u, "connLinkInvitation: \(connLinkInvitation)\nconnection: \(connection)")
        case let .connectionIncognitoUpdated(u, toConnection): return withUser(u, String(describing: toConnection))
        case let .connectionUserChanged(u, fromConnection, toConnection, newUser): return withUser(u, "fromConnection: \(String(describing: fromConnection))\ntoConnection: \(String(describing: toConnection))\nnewUserId: \(String(describing: newUser.userId))")
        case let .connectionPlan(u, connLink, connectionPlan): return withUser(u, "connLink: \(String(describing: connLink))\nconnectionPlan: \(String(describing: connectionPlan))")
        case let .sentConfirmation(u, connection): return withUser(u, String(describing: connection))
        case let .sentInvitation(u, connection): return withUser(u, String(describing: connection))
        case let .sentInvitationToContact(u, contact, _): return withUser(u, String(describing: contact))
        case let .contactAlreadyExists(u, contact): return withUser(u, String(describing: contact))
        }
    }
}

enum ChatResponse2: Decodable, ChatAPIResult {
    // group responses
    case groupCreated(user: UserRef, groupInfo: GroupInfo)
    case sentGroupInvitation(user: UserRef, groupInfo: GroupInfo, contact: Contact, member: GroupMember)
    case userAcceptedGroupSent(user: UserRef, groupInfo: GroupInfo, hostContact: Contact?)
    case userDeletedMembers(user: UserRef, groupInfo: GroupInfo, members: [GroupMember], withMessages: Bool)
    case leftMemberUser(user: UserRef, groupInfo: GroupInfo)
    case groupMembers(user: UserRef, group: SimpleXChat.Group)
    case membersRoleUser(user: UserRef, groupInfo: GroupInfo, members: [GroupMember], toRole: GroupMemberRole)
    case membersBlockedForAllUser(user: UserRef, groupInfo: GroupInfo, members: [GroupMember], blocked: Bool)
    case groupUpdated(user: UserRef, toGroup: GroupInfo)
    case groupLinkCreated(user: UserRef, groupInfo: GroupInfo, connLinkContact: CreatedConnLink, memberRole: GroupMemberRole)
    case groupLink(user: UserRef, groupInfo: GroupInfo, connLinkContact: CreatedConnLink, memberRole: GroupMemberRole)
    case groupLinkDeleted(user: UserRef, groupInfo: GroupInfo)
    case newMemberContact(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    case newMemberContactSentInv(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    // receiving file responses
    case rcvFileAccepted(user: UserRef, chatItem: AChatItem)
    case rcvFileAcceptedSndCancelled(user: UserRef, rcvFileTransfer: RcvFileTransfer)
    case standaloneFileInfo(fileMeta: MigrationFileLinkData?)
    case rcvStandaloneFileCreated(user: UserRef, rcvFileTransfer: RcvFileTransfer)
    case rcvFileCancelled(user: UserRef, chatItem_: AChatItem?, rcvFileTransfer: RcvFileTransfer)
    // sending file responses
    case sndFileCancelled(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, sndFileTransfers: [SndFileTransfer])
    case sndStandaloneFileCreated(user: UserRef, fileTransferMeta: FileTransferMeta) // returned by _upload
    case sndFileStartXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta) // not used
    case sndFileCancelledXFTP(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta)
    // call invitations
    case callInvitations(callInvitations: [RcvCallInvitation])
    // notifications
    case ntfTokenStatus(status: NtfTknStatus)
    case ntfToken(token: DeviceToken, status: NtfTknStatus, ntfMode: NotificationsMode, ntfServer: String)
    case ntfConns(ntfConns: [NtfConn])
    case connNtfMessages(receivedMsgs: [RcvNtfMsgInfo])
    // remote desktop responses
    case remoteCtrlList(remoteCtrls: [RemoteCtrlInfo])
    case remoteCtrlConnecting(remoteCtrl_: RemoteCtrlInfo?, ctrlAppInfo: CtrlAppInfo, appVersion: String)
    case remoteCtrlConnected(remoteCtrl: RemoteCtrlInfo)
    // misc
    case versionInfo(versionInfo: CoreVersionInfo, chatMigrations: [UpMigration], agentMigrations: [UpMigration])
    case cmdOk(user_: UserRef?)
    case agentSubsTotal(user: UserRef, subsTotal: SMPServerSubs, hasSession: Bool)
    case agentServersSummary(user: UserRef, serversSummary: PresentedServersSummary)
    case agentSubsSummary(user: UserRef, subsSummary: SMPServerSubs)
    case archiveExported(archiveErrors: [ArchiveError])
    case archiveImported(archiveErrors: [ArchiveError])
    case appSettings(appSettings: AppSettings)

    var responseType: String {
        switch self {
        case .groupCreated: "groupCreated"
        case .sentGroupInvitation: "sentGroupInvitation"
        case .userAcceptedGroupSent: "userAcceptedGroupSent"
        case .userDeletedMembers: "userDeletedMembers"
        case .leftMemberUser: "leftMemberUser"
        case .groupMembers: "groupMembers"
        case .membersRoleUser: "membersRoleUser"
        case .membersBlockedForAllUser: "membersBlockedForAllUser"
        case .groupUpdated: "groupUpdated"
        case .groupLinkCreated: "groupLinkCreated"
        case .groupLink: "groupLink"
        case .groupLinkDeleted: "groupLinkDeleted"
        case .newMemberContact: "newMemberContact"
        case .newMemberContactSentInv: "newMemberContactSentInv"
        case .rcvFileAccepted: "rcvFileAccepted"
        case .rcvFileAcceptedSndCancelled: "rcvFileAcceptedSndCancelled"
        case .standaloneFileInfo: "standaloneFileInfo"
        case .rcvStandaloneFileCreated: "rcvStandaloneFileCreated"
        case .rcvFileCancelled: "rcvFileCancelled"
        case .sndFileCancelled: "sndFileCancelled"
        case .sndStandaloneFileCreated: "sndStandaloneFileCreated"
        case .sndFileStartXFTP: "sndFileStartXFTP"
        case .sndFileCancelledXFTP: "sndFileCancelledXFTP"
        case .callInvitations: "callInvitations"
        case .ntfTokenStatus: "ntfTokenStatus"
        case .ntfToken: "ntfToken"
        case .ntfConns: "ntfConns"
        case .connNtfMessages: "connNtfMessages"
        case .remoteCtrlList: "remoteCtrlList"
        case .remoteCtrlConnecting: "remoteCtrlConnecting"
        case .remoteCtrlConnected: "remoteCtrlConnected"
        case .versionInfo: "versionInfo"
        case .cmdOk: "cmdOk"
        case .agentSubsTotal: "agentSubsTotal"
        case .agentServersSummary: "agentServersSummary"
        case .agentSubsSummary: "agentSubsSummary"
        case .archiveExported: "archiveExported"
        case .archiveImported: "archiveImported"
        case .appSettings: "appSettings"
        }
    }

    var details: String {
        switch self {
        case let .groupCreated(u, groupInfo): return withUser(u, String(describing: groupInfo))
        case let .sentGroupInvitation(u, groupInfo, contact, member): return withUser(u, "groupInfo: \(groupInfo)\ncontact: \(contact)\nmember: \(member)")
        case let .userAcceptedGroupSent(u, groupInfo, hostContact): return withUser(u, "groupInfo: \(groupInfo)\nhostContact: \(String(describing: hostContact))")
        case let .userDeletedMembers(u, groupInfo, members, withMessages): return withUser(u, "groupInfo: \(groupInfo)\nmembers: \(members)\nwithMessages: \(withMessages)")
        case let .leftMemberUser(u, groupInfo): return withUser(u, String(describing: groupInfo))
        case let .groupMembers(u, group): return withUser(u, String(describing: group))
        case let .membersRoleUser(u, groupInfo, members, toRole): return withUser(u, "groupInfo: \(groupInfo)\nmembers: \(members)\ntoRole: \(toRole)")
        case let .membersBlockedForAllUser(u, groupInfo, members, blocked): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(members)\nblocked: \(blocked)")
        case let .groupUpdated(u, toGroup): return withUser(u, String(describing: toGroup))
        case let .groupLinkCreated(u, groupInfo, connLinkContact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\nconnLinkContact: \(connLinkContact)\nmemberRole: \(memberRole)")
        case let .groupLink(u, groupInfo, connLinkContact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\nconnLinkContact: \(connLinkContact)\nmemberRole: \(memberRole)")
        case let .groupLinkDeleted(u, groupInfo): return withUser(u, String(describing: groupInfo))
        case let .newMemberContact(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
        case let .newMemberContactSentInv(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
        case let .rcvFileAccepted(u, chatItem): return withUser(u, String(describing: chatItem))
        case .rcvFileAcceptedSndCancelled: return noDetails
        case let .standaloneFileInfo(fileMeta): return String(describing: fileMeta)
        case .rcvStandaloneFileCreated: return noDetails
        case let .rcvFileCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileCancelled(u, chatItem, _, _): return withUser(u, String(describing: chatItem))
        case .sndStandaloneFileCreated: return noDetails
        case let .sndFileStartXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileCancelledXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .callInvitations(invs): return String(describing: invs)
        case let .ntfTokenStatus(status): return String(describing: status)
        case let .ntfToken(token, status, ntfMode, ntfServer): return "token: \(token)\nstatus: \(status.rawValue)\nntfMode: \(ntfMode.rawValue)\nntfServer: \(ntfServer)"
        case let .ntfConns(ntfConns): return String(describing: ntfConns)
        case let .connNtfMessages(receivedMsgs): return "receivedMsgs: \(String(describing: receivedMsgs))"
        case let .remoteCtrlList(remoteCtrls): return String(describing: remoteCtrls)
        case let .remoteCtrlConnecting(remoteCtrl_, ctrlAppInfo, appVersion): return "remoteCtrl_:\n\(String(describing: remoteCtrl_))\nctrlAppInfo:\n\(String(describing: ctrlAppInfo))\nappVersion: \(appVersion)"
        case let .remoteCtrlConnected(remoteCtrl): return String(describing: remoteCtrl)
        case let .versionInfo(versionInfo, chatMigrations, agentMigrations): return "\(String(describing: versionInfo))\n\nchat migrations: \(chatMigrations.map(\.upName))\n\nagent migrations: \(agentMigrations.map(\.upName))"
        case .cmdOk: return noDetails
        case let .agentSubsTotal(u, subsTotal, hasSession): return withUser(u, "subsTotal: \(String(describing: subsTotal))\nhasSession: \(hasSession)")
        case let .agentServersSummary(u, serversSummary): return withUser(u, String(describing: serversSummary))
        case let .agentSubsSummary(u, subsSummary): return withUser(u, String(describing: subsSummary))
        case let .archiveExported(archiveErrors): return String(describing: archiveErrors)
        case let .archiveImported(archiveErrors): return String(describing: archiveErrors)
        case let .appSettings(appSettings): return String(describing: appSettings)
        }
    }
}

enum ChatEvent: Decodable, ChatAPIResult {
    case chatSuspended
    case contactSwitch(user: UserRef, contact: Contact, switchProgress: SwitchProgress)
    case groupMemberSwitch(user: UserRef, groupInfo: GroupInfo, member: GroupMember, switchProgress: SwitchProgress)
    case contactRatchetSync(user: UserRef, contact: Contact, ratchetSyncProgress: RatchetSyncProgress)
    case groupMemberRatchetSync(user: UserRef, groupInfo: GroupInfo, member: GroupMember, ratchetSyncProgress: RatchetSyncProgress)
    case contactDeletedByContact(user: UserRef, contact: Contact)
    case contactConnected(user: UserRef, contact: Contact, userCustomProfile: Profile?)
    case contactConnecting(user: UserRef, contact: Contact)
    case contactSndReady(user: UserRef, contact: Contact)
    case receivedContactRequest(user: UserRef, contactRequest: UserContactRequest)
    case contactUpdated(user: UserRef, toContact: Contact)
    case groupMemberUpdated(user: UserRef, groupInfo: GroupInfo, fromMember: GroupMember, toMember: GroupMember)
    case contactsMerged(user: UserRef, intoContact: Contact, mergedContact: Contact)
    case networkStatus(networkStatus: NetworkStatus, connections: [String])
    case networkStatuses(user_: UserRef?, networkStatuses: [ConnNetworkStatus])
    case newChatItems(user: UserRef, chatItems: [AChatItem])
    case chatItemsStatusesUpdated(user: UserRef, chatItems: [AChatItem])
    case chatItemUpdated(user: UserRef, chatItem: AChatItem)
    case chatItemReaction(user: UserRef, added: Bool, reaction: ACIReaction)
    case chatItemsDeleted(user: UserRef, chatItemDeletions: [ChatItemDeletion], byUser: Bool)
    // group events
    case groupChatItemsDeleted(user: UserRef, groupInfo: GroupInfo, chatItemIDs: Set<Int64>, byUser: Bool, member_: GroupMember?)
    case receivedGroupInvitation(user: UserRef, groupInfo: GroupInfo, contact: Contact, memberRole: GroupMemberRole)
    case userAcceptedGroupSent(user: UserRef, groupInfo: GroupInfo, hostContact: Contact?)
    case groupLinkConnecting(user: UserRef, groupInfo: GroupInfo, hostMember: GroupMember)
    case businessLinkConnecting(user: UserRef, groupInfo: GroupInfo, hostMember: GroupMember, fromContact: Contact)
    case joinedGroupMemberConnecting(user: UserRef, groupInfo: GroupInfo, hostMember: GroupMember, member: GroupMember)
    case memberRole(user: UserRef, groupInfo: GroupInfo, byMember: GroupMember, member: GroupMember, fromRole: GroupMemberRole, toRole: GroupMemberRole)
    case memberBlockedForAll(user: UserRef, groupInfo: GroupInfo, byMember: GroupMember, member: GroupMember, blocked: Bool)
    case deletedMemberUser(user: UserRef, groupInfo: GroupInfo, member: GroupMember, withMessages: Bool)
    case deletedMember(user: UserRef, groupInfo: GroupInfo, byMember: GroupMember, deletedMember: GroupMember, withMessages: Bool)
    case leftMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case groupDeleted(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case userJoinedGroup(user: UserRef, groupInfo: GroupInfo)
    case joinedGroupMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember)
    case connectedToGroupMember(user: UserRef, groupInfo: GroupInfo, member: GroupMember, memberContact: Contact?)
    case groupUpdated(user: UserRef, toGroup: GroupInfo)
    case newMemberContactReceivedInv(user: UserRef, contact: Contact, groupInfo: GroupInfo, member: GroupMember)
    // receiving file events
    case rcvFileAccepted(user: UserRef, chatItem: AChatItem)
    case rcvFileAcceptedSndCancelled(user: UserRef, rcvFileTransfer: RcvFileTransfer)
    case rcvFileStart(user: UserRef, chatItem: AChatItem) // send by chats
    case rcvFileProgressXFTP(user: UserRef, chatItem_: AChatItem?, receivedSize: Int64, totalSize: Int64, rcvFileTransfer: RcvFileTransfer)
    case rcvFileComplete(user: UserRef, chatItem: AChatItem)
    case rcvStandaloneFileComplete(user: UserRef, targetPath: String, rcvFileTransfer: RcvFileTransfer)
    case rcvFileSndCancelled(user: UserRef, chatItem: AChatItem, rcvFileTransfer: RcvFileTransfer)
    case rcvFileError(user: UserRef, chatItem_: AChatItem?, agentError: AgentErrorType, rcvFileTransfer: RcvFileTransfer)
    case rcvFileWarning(user: UserRef, chatItem_: AChatItem?, agentError: AgentErrorType, rcvFileTransfer: RcvFileTransfer)
    // sending file events
    case sndFileStart(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileComplete(user: UserRef, chatItem: AChatItem, sndFileTransfer: SndFileTransfer)
    case sndFileRcvCancelled(user: UserRef, chatItem_: AChatItem?, sndFileTransfer: SndFileTransfer)
    case sndFileProgressXFTP(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, sentSize: Int64, totalSize: Int64)
    case sndFileRedirectStartXFTP(user: UserRef, fileTransferMeta: FileTransferMeta, redirectMeta: FileTransferMeta)
    case sndFileCompleteXFTP(user: UserRef, chatItem: AChatItem, fileTransferMeta: FileTransferMeta)
    case sndStandaloneFileComplete(user: UserRef, fileTransferMeta: FileTransferMeta, rcvURIs: [String])
    case sndFileError(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    case sndFileWarning(user: UserRef, chatItem_: AChatItem?, fileTransferMeta: FileTransferMeta, errorMessage: String)
    // call events
    case callInvitation(callInvitation: RcvCallInvitation)
    case callOffer(user: UserRef, contact: Contact, callType: CallType, offer: WebRTCSession, sharedKey: String?, askConfirmation: Bool)
    case callAnswer(user: UserRef, contact: Contact, answer: WebRTCSession)
    case callExtraInfo(user: UserRef, contact: Contact, extraInfo: WebRTCExtraInfo)
    case callEnded(user: UserRef, contact: Contact)
    case contactDisabled(user: UserRef, contact: Contact)
    // notification marker
    case ntfMessage(user: UserRef, connEntity: ConnectionEntity, ntfMessage: NtfMsgAckInfo)
    // remote desktop responses
    case remoteCtrlFound(remoteCtrl: RemoteCtrlInfo, ctrlAppInfo_: CtrlAppInfo?, appVersion: String, compatible: Bool)
    case remoteCtrlSessionCode(remoteCtrl_: RemoteCtrlInfo?, sessionCode: String)
    case remoteCtrlConnected(remoteCtrl: RemoteCtrlInfo)
    case remoteCtrlStopped(rcsState: RemoteCtrlSessionState, rcStopReason: RemoteCtrlStopReason)
    // pq
    case contactPQEnabled(user: UserRef, contact: Contact, pqEnabled: Bool)
    
    var responseType: String {
        switch self {
        case .chatSuspended: "chatSuspended"
        case .contactSwitch: "contactSwitch"
        case .groupMemberSwitch: "groupMemberSwitch"
        case .contactRatchetSync: "contactRatchetSync"
        case .groupMemberRatchetSync: "groupMemberRatchetSync"
        case .contactDeletedByContact: "contactDeletedByContact"
        case .contactConnected: "contactConnected"
        case .contactConnecting: "contactConnecting"
        case .contactSndReady: "contactSndReady"
        case .receivedContactRequest: "receivedContactRequest"
        case .contactUpdated: "contactUpdated"
        case .groupMemberUpdated: "groupMemberUpdated"
        case .contactsMerged: "contactsMerged"
        case .networkStatus: "networkStatus"
        case .networkStatuses: "networkStatuses"
        case .newChatItems: "newChatItems"
        case .chatItemsStatusesUpdated: "chatItemsStatusesUpdated"
        case .chatItemUpdated: "chatItemUpdated"
        case .chatItemReaction: "chatItemReaction"
        case .chatItemsDeleted: "chatItemsDeleted"
        case .groupChatItemsDeleted: "groupChatItemsDeleted"
        case .receivedGroupInvitation: "receivedGroupInvitation"
        case .userAcceptedGroupSent: "userAcceptedGroupSent"
        case .groupLinkConnecting: "groupLinkConnecting"
        case .businessLinkConnecting: "businessLinkConnecting"
        case .joinedGroupMemberConnecting: "joinedGroupMemberConnecting"
        case .memberRole: "memberRole"
        case .memberBlockedForAll: "memberBlockedForAll"
        case .deletedMemberUser: "deletedMemberUser"
        case .deletedMember: "deletedMember"
        case .leftMember: "leftMember"
        case .groupDeleted: "groupDeleted"
        case .userJoinedGroup: "userJoinedGroup"
        case .joinedGroupMember: "joinedGroupMember"
        case .connectedToGroupMember: "connectedToGroupMember"
        case .groupUpdated: "groupUpdated"
        case .newMemberContactReceivedInv: "newMemberContactReceivedInv"
        case .rcvFileAccepted: "rcvFileAccepted"
        case .rcvFileAcceptedSndCancelled: "rcvFileAcceptedSndCancelled"
        case .rcvFileStart: "rcvFileStart"
        case .rcvFileProgressXFTP: "rcvFileProgressXFTP"
        case .rcvFileComplete: "rcvFileComplete"
        case .rcvStandaloneFileComplete: "rcvStandaloneFileComplete"
        case .rcvFileSndCancelled: "rcvFileSndCancelled"
        case .rcvFileError: "rcvFileError"
        case .rcvFileWarning: "rcvFileWarning"
        case .sndFileStart: "sndFileStart"
        case .sndFileComplete: "sndFileComplete"
        case .sndFileRcvCancelled: "sndFileRcvCancelled"
        case .sndFileProgressXFTP: "sndFileProgressXFTP"
        case .sndFileRedirectStartXFTP: "sndFileRedirectStartXFTP"
        case .sndFileCompleteXFTP: "sndFileCompleteXFTP"
        case .sndStandaloneFileComplete: "sndStandaloneFileComplete"
        case .sndFileError: "sndFileError"
        case .sndFileWarning: "sndFileWarning"
        case .callInvitation: "callInvitation"
        case .callOffer: "callOffer"
        case .callAnswer: "callAnswer"
        case .callExtraInfo: "callExtraInfo"
        case .callEnded: "callEnded"
        case .contactDisabled: "contactDisabled"
        case .ntfMessage: "ntfMessage"
        case .remoteCtrlFound: "remoteCtrlFound"
        case .remoteCtrlSessionCode: "remoteCtrlSessionCode"
        case .remoteCtrlConnected: "remoteCtrlConnected"
        case .remoteCtrlStopped: "remoteCtrlStopped"
        case .contactPQEnabled: "contactPQEnabled"
        }
    }
    
    var details: String {
        switch self {
        case .chatSuspended: return noDetails
        case let .contactSwitch(u, contact, switchProgress): return withUser(u, "contact: \(String(describing: contact))\nswitchProgress: \(String(describing: switchProgress))")
        case let .groupMemberSwitch(u, groupInfo, member, switchProgress): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nswitchProgress: \(String(describing: switchProgress))")
        case let .contactRatchetSync(u, contact, ratchetSyncProgress): return withUser(u, "contact: \(String(describing: contact))\nratchetSyncProgress: \(String(describing: ratchetSyncProgress))")
        case let .groupMemberRatchetSync(u, groupInfo, member, ratchetSyncProgress): return withUser(u, "groupInfo: \(String(describing: groupInfo))\nmember: \(String(describing: member))\nratchetSyncProgress: \(String(describing: ratchetSyncProgress))")
        case let .contactDeletedByContact(u, contact): return withUser(u, String(describing: contact))
        case let .contactConnected(u, contact, _): return withUser(u, String(describing: contact))
        case let .contactConnecting(u, contact): return withUser(u, String(describing: contact))
        case let .contactSndReady(u, contact): return withUser(u, String(describing: contact))
        case let .receivedContactRequest(u, contactRequest): return withUser(u, String(describing: contactRequest))
        case let .contactUpdated(u, toContact): return withUser(u, String(describing: toContact))
        case let .groupMemberUpdated(u, groupInfo, fromMember, toMember): return withUser(u, "groupInfo: \(groupInfo)\nfromMember: \(fromMember)\ntoMember: \(toMember)")
        case let .contactsMerged(u, intoContact, mergedContact): return withUser(u, "intoContact: \(intoContact)\nmergedContact: \(mergedContact)")
        case let .networkStatus(status, conns): return "networkStatus: \(String(describing: status))\nconnections: \(String(describing: conns))"
        case let .networkStatuses(u, statuses): return withUser(u, String(describing: statuses))
        case let .newChatItems(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .chatItemsStatusesUpdated(u, chatItems):
            let itemsString = chatItems.map { chatItem in String(describing: chatItem) }.joined(separator: "\n")
            return withUser(u, itemsString)
        case let .chatItemUpdated(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .chatItemReaction(u, added, reaction): return withUser(u, "added: \(added)\n\(String(describing: reaction))")
        case let .chatItemsDeleted(u, items, byUser):
            let itemsString = items.map { item in
                "deletedChatItem:\n\(String(describing: item.deletedChatItem))\ntoChatItem:\n\(String(describing: item.toChatItem))" }.joined(separator: "\n")
            return withUser(u, itemsString + "\nbyUser: \(byUser)")
        case let .groupChatItemsDeleted(u, gInfo, chatItemIDs, byUser, member_):
            return withUser(u, "chatItemIDs: \(String(describing: chatItemIDs))\ngroupInfo: \(String(describing: gInfo))\nbyUser: \(byUser)\nmember_: \(String(describing: member_))")
        case let .receivedGroupInvitation(u, groupInfo, contact, memberRole): return withUser(u, "groupInfo: \(groupInfo)\ncontact: \(contact)\nmemberRole: \(memberRole)")
        case let .userAcceptedGroupSent(u, groupInfo, hostContact): return withUser(u, "groupInfo: \(groupInfo)\nhostContact: \(String(describing: hostContact))")
        case let .groupLinkConnecting(u, groupInfo, hostMember): return withUser(u, "groupInfo: \(groupInfo)\nhostMember: \(String(describing: hostMember))")
        case let .businessLinkConnecting(u, groupInfo, hostMember, fromContact): return withUser(u, "groupInfo: \(groupInfo)\nhostMember: \(String(describing: hostMember))\nfromContact: \(String(describing: fromContact))")
        case let .joinedGroupMemberConnecting(u, groupInfo, hostMember, member): return withUser(u, "groupInfo: \(groupInfo)\nhostMember: \(hostMember)\nmember: \(member)")
        case let .memberRole(u, groupInfo, byMember, member, fromRole, toRole): return withUser(u, "groupInfo: \(groupInfo)\nbyMember: \(byMember)\nmember: \(member)\nfromRole: \(fromRole)\ntoRole: \(toRole)")
        case let .memberBlockedForAll(u, groupInfo, byMember, member, blocked): return withUser(u, "groupInfo: \(groupInfo)\nbyMember: \(byMember)\nmember: \(member)\nblocked: \(blocked)")
        case let .deletedMemberUser(u, groupInfo, member, withMessages): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)\nwithMessages: \(withMessages)")
        case let .deletedMember(u, groupInfo, byMember, deletedMember, withMessages): return withUser(u, "groupInfo: \(groupInfo)\nbyMember: \(byMember)\ndeletedMember: \(deletedMember)\nwithMessages: \(withMessages)")
        case let .leftMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
        case let .groupDeleted(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
        case let .userJoinedGroup(u, groupInfo): return withUser(u, String(describing: groupInfo))
        case let .joinedGroupMember(u, groupInfo, member): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)")
        case let .connectedToGroupMember(u, groupInfo, member, memberContact): return withUser(u, "groupInfo: \(groupInfo)\nmember: \(member)\nmemberContact: \(String(describing: memberContact))")
        case let .groupUpdated(u, toGroup): return withUser(u, String(describing: toGroup))
        case let .newMemberContactReceivedInv(u, contact, groupInfo, member): return withUser(u, "contact: \(contact)\ngroupInfo: \(groupInfo)\nmember: \(member)")
        case let .rcvFileAccepted(u, chatItem): return withUser(u, String(describing: chatItem))
        case .rcvFileAcceptedSndCancelled: return noDetails
        case let .rcvFileStart(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .rcvFileProgressXFTP(u, chatItem, receivedSize, totalSize, _): return withUser(u, "chatItem: \(String(describing: chatItem))\nreceivedSize: \(receivedSize)\ntotalSize: \(totalSize)")
        case let .rcvStandaloneFileComplete(u, targetPath, _): return withUser(u, targetPath)
        case let .rcvFileComplete(u, chatItem): return withUser(u, String(describing: chatItem))
        case let .rcvFileSndCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .rcvFileError(u, chatItem, agentError, _): return withUser(u, "agentError: \(String(describing: agentError))\nchatItem: \(String(describing: chatItem))")
        case let .rcvFileWarning(u, chatItem, agentError, _): return withUser(u, "agentError: \(String(describing: agentError))\nchatItem: \(String(describing: chatItem))")
        case let .sndFileStart(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileComplete(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileRcvCancelled(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndFileProgressXFTP(u, chatItem, _, sentSize, totalSize): return withUser(u, "chatItem: \(String(describing: chatItem))\nsentSize: \(sentSize)\ntotalSize: \(totalSize)")
        case let .sndFileRedirectStartXFTP(u, _, redirectMeta): return withUser(u, String(describing: redirectMeta))
        case let .sndFileCompleteXFTP(u, chatItem, _): return withUser(u, String(describing: chatItem))
        case let .sndStandaloneFileComplete(u, _, rcvURIs): return withUser(u, String(rcvURIs.count))
        case let .sndFileError(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        case let .sndFileWarning(u, chatItem, _, err): return withUser(u, "error: \(String(describing: err))\nchatItem: \(String(describing: chatItem))")
        case let .callInvitation(inv): return String(describing: inv)
        case let .callOffer(u, contact, callType, offer, sharedKey, askConfirmation): return withUser(u, "contact: \(contact.id)\ncallType: \(String(describing: callType))\nsharedKey: \(sharedKey ?? "")\naskConfirmation: \(askConfirmation)\noffer: \(String(describing: offer))")
        case let .callAnswer(u, contact, answer): return withUser(u, "contact: \(contact.id)\nanswer: \(String(describing: answer))")
        case let .callExtraInfo(u, contact, extraInfo): return withUser(u, "contact: \(contact.id)\nextraInfo: \(String(describing: extraInfo))")
        case let .callEnded(u, contact): return withUser(u, "contact: \(contact.id)")
        case let .contactDisabled(u, contact): return withUser(u, String(describing: contact))
        case let .ntfMessage(u, connEntity, ntfMessage): return withUser(u, "connEntity: \(String(describing: connEntity))\nntfMessage: \(String(describing: ntfMessage))")
        case let .remoteCtrlFound(remoteCtrl, ctrlAppInfo_, appVersion, compatible): return "remoteCtrl:\n\(String(describing: remoteCtrl))\nctrlAppInfo_:\n\(String(describing: ctrlAppInfo_))\nappVersion: \(appVersion)\ncompatible: \(compatible)"
        case let .remoteCtrlSessionCode(remoteCtrl_, sessionCode): return "remoteCtrl_:\n\(String(describing: remoteCtrl_))\nsessionCode: \(sessionCode)"
        case let .remoteCtrlConnected(remoteCtrl): return String(describing: remoteCtrl)
        case let .remoteCtrlStopped(rcsState, rcStopReason): return "rcsState: \(String(describing: rcsState))\nrcStopReason: \(String(describing: rcStopReason))"
        case let .contactPQEnabled(u, contact, pqEnabled): return withUser(u, "contact: \(String(describing: contact))\npqEnabled: \(pqEnabled)")
        }
    }    
}

struct NewUser: Encodable {
    var profile: Profile?
    var pastTimestamp: Bool
}

enum ChatPagination {
    static let INITIAL_COUNT = 75
    static let PRELOAD_COUNT = 100
    static let UNTIL_PRELOAD_COUNT = 50

    case last(count: Int)
    case after(chatItemId: Int64, count: Int)
    case before(chatItemId: Int64, count: Int)
    case around(chatItemId: Int64, count: Int)
    case initial(count: Int)

    var cmdString: String {
        switch self {
        case let .last(count): return "count=\(count)"
        case let .after(chatItemId, count): return "after=\(chatItemId) count=\(count)"
        case let .before(chatItemId, count): return "before=\(chatItemId) count=\(count)"
        case let .around(chatItemId, count): return "around=\(chatItemId) count=\(count)"
        case let .initial(count): return "initial=\(count)"
        }
    }
}

enum ConnectionPlan: Decodable, Hashable {
    case invitationLink(invitationLinkPlan: InvitationLinkPlan)
    case contactAddress(contactAddressPlan: ContactAddressPlan)
    case groupLink(groupLinkPlan: GroupLinkPlan)
    case error(chatError: ChatError)
}

enum InvitationLinkPlan: Decodable, Hashable {
    case ok
    case ownLink
    case connecting(contact_: Contact?)
    case known(contact: Contact)
}

enum ContactAddressPlan: Decodable, Hashable {
    case ok
    case ownLink
    case connectingConfirmReconnect
    case connectingProhibit(contact: Contact)
    case known(contact: Contact)
    case contactViaAddress(contact: Contact)
}

enum GroupLinkPlan: Decodable, Hashable {
    case ok
    case ownLink(groupInfo: GroupInfo)
    case connectingConfirmReconnect
    case connectingProhibit(groupInfo_: GroupInfo?)
    case known(groupInfo: GroupInfo)
}

struct ChatTagData: Encodable {
    var emoji: String?
    var text: String
}

struct UpdatedMessage: Encodable {
    var msgContent: MsgContent
    var mentions: [String: Int64]
    
    var cmdString: String {
        "json \(encodeJSON(self))"
    }
}

enum ChatDeleteMode: Codable {
    case full(notify: Bool)
    case entity(notify: Bool)
    case messages

    var cmdString: String {
        switch self {
        case let .full(notify): "full notify=\(onOff(notify))"
        case let .entity(notify): "entity notify=\(onOff(notify))"
        case .messages: "messages"
        }
    }

    var isEntity: Bool {
        switch self {
        case .entity: return true
        default: return false
        }
    }
}

enum NetworkStatus: Decodable, Equatable {
    case unknown
    case connected
    case disconnected
    case error(connectionError: String)

    var statusString: LocalizedStringKey {
        switch self {
        case .connected: "connected"
        case .error: "error"
        default: "connecting"
        }
    }

    var statusExplanation: LocalizedStringKey {
        switch self {
        case .connected: "You are connected to the server used to receive messages from this contact."
        case let .error(err): "Trying to connect to the server used to receive messages from this contact (error: \(err))."
        default: "Trying to connect to the server used to receive messages from this contact."
        }
    }

    var imageName: String {
        switch self {
        case .unknown: "circle.dotted"
        case .connected: "circle.fill"
        case .disconnected: "ellipsis.circle.fill"
        case .error: "exclamationmark.circle.fill"
        }
    }
}

enum ForwardConfirmation: Decodable, Hashable {
    case filesNotAccepted(fileIds: [Int64])
    case filesInProgress(filesCount: Int)
    case filesMissing(filesCount: Int)
    case filesFailed(filesCount: Int)
}

struct ConnNetworkStatus: Decodable {
    var agentConnId: String
    var networkStatus: NetworkStatus
}

struct UserMsgReceiptSettings: Codable {
    var enable: Bool
    var clearOverrides: Bool
}


struct UserContactLink: Decodable, Hashable {
    var connLinkContact: CreatedConnLink
    var autoAccept: AutoAccept?

    var responseDetails: String {
        "connLinkContact: \(connLinkContact)\nautoAccept: \(AutoAccept.cmdString(autoAccept))"
    }
}

struct AutoAccept: Codable, Hashable {
    var businessAddress: Bool
    var acceptIncognito: Bool
    var autoReply: MsgContent?

    static func cmdString(_ autoAccept: AutoAccept?) -> String {
        guard let autoAccept = autoAccept else { return "off" }
        var s = "on"
        if autoAccept.acceptIncognito {
            s += " incognito=on"
        } else if autoAccept.businessAddress {
            s += " business"
        }
        guard let msg = autoAccept.autoReply else { return s }
        return s + " " + msg.cmdString
    }
}

struct DeviceToken: Decodable {
    var pushProvider: PushProvider
    var token: String

    var cmdString: String {
        "\(pushProvider) \(token)"
    }
}

enum PushEnvironment: String {
    case development
    case production
}

enum PushProvider: String, Decodable {
    case apns_dev
    case apns_prod

    init(env: PushEnvironment) {
        switch env {
        case .development: self = .apns_dev
        case .production: self = .apns_prod
        }
    }
}

// This notification mode is for app core, UI uses AppNotificationsMode.off to mean completely disable,
// and .local for periodic background checks
enum NotificationsMode: String, Decodable, SelectableItem {
    case off = "OFF"
    case periodic = "PERIODIC"
    case instant = "INSTANT"

    var label: LocalizedStringKey {
        switch self {
        case .off: "No push server"
        case .periodic: "Periodic"
        case .instant: "Instant"
        }
    }

    var icon: String {
        switch self {
        case .off: return "arrow.clockwise"
        case .periodic: return "timer"
        case .instant: return "bolt"
        }
    }

    var id: String { self.rawValue }

    static var values: [NotificationsMode] = [.instant, .periodic, .off]
}

struct RemoteCtrlInfo: Decodable {
    var remoteCtrlId: Int64
    var ctrlDeviceName: String
    var sessionState: RemoteCtrlSessionState?

    var deviceViewName: String {
        ctrlDeviceName == "" ? "\(remoteCtrlId)" : ctrlDeviceName
    }
}

enum RemoteCtrlSessionState: Decodable {
    case starting
    case searching
    case connecting
    case pendingConfirmation(sessionCode: String)
    case connected(sessionCode: String)
}

enum RemoteCtrlStopReason: Decodable {
    case discoveryFailed(chatError: ChatError)
    case connectionFailed(chatError: ChatError)
    case setupFailed(chatError: ChatError)
    case disconnected
}

struct CtrlAppInfo: Decodable {
    var appVersionRange: AppVersionRange
    var deviceName: String
}

struct AppVersionRange: Decodable {
    var minVersion: String
    var maxVersion: String
}

struct CoreVersionInfo: Decodable {
    var version: String
    var simplexmqVersion: String
    var simplexmqCommit: String
}

struct ArchiveConfig: Encodable {
    var archivePath: String
    var disableCompression: Bool?
}

struct DBEncryptionConfig: Codable {
    var currentKey: String
    var newKey: String
}

enum OperatorTag: String, Codable {
    case simplex = "simplex"
    case flux = "flux"
}

struct ServerOperatorInfo {
    var description: [String]
    var website: URL
    var selfhost: (text: String, link: URL)? = nil
    var logo: String
    var largeLogo: String
    var logoDarkMode: String
    var largeLogoDarkMode: String
}

let operatorsInfo: Dictionary<OperatorTag, ServerOperatorInfo> = [
    .simplex: ServerOperatorInfo(
        description: [
            "SimpleX Chat is the first communication network that has no user profile IDs of any kind, not even random numbers or identity keys.",
            "SimpleX Chat Ltd develops the communication software for SimpleX network."
        ],
        website: URL(string: "https://simplex.chat")!,
        logo: "decentralized",
        largeLogo: "logo",
        logoDarkMode: "decentralized-light",
        largeLogoDarkMode: "logo-light"
    ),
    .flux: ServerOperatorInfo(
        description: [
            "Flux is the largest decentralized cloud, based on a global network of user-operated nodes.",
            "Flux offers a powerful, scalable, and affordable cutting edge technology platform for all.",
            "Flux operates servers in SimpleX network to improve its privacy and decentralization."
        ],
        website: URL(string: "https://runonflux.com")!,
        selfhost: (text: "Self-host SimpleX servers on Flux", link: URL(string: "https://home.runonflux.io/apps/marketplace?q=simplex")!),
        logo: "flux_logo_symbol",
        largeLogo: "flux_logo",
        logoDarkMode: "flux_logo_symbol",
        largeLogoDarkMode: "flux_logo-light"
    ),
]

struct UsageConditions: Decodable {
    var conditionsId: Int64
    var conditionsCommit: String
    var notifiedAt: Date?
    var createdAt: Date

    static var sampleData = UsageConditions(
        conditionsId: 1,
        conditionsCommit: "11a44dc1fd461a93079f897048b46998db55da5c",
        notifiedAt: nil,
        createdAt: Date.now
    )
}

enum UsageConditionsAction: Decodable {
    case review(operators: [ServerOperator], deadline: Date?, showNotice: Bool)
    case accepted(operators: [ServerOperator])

    var showNotice: Bool {
        switch self {
        case let .review(_, _, showNotice): showNotice
        case .accepted: false
        }
    }
}

struct ServerOperatorConditions: Decodable {
    var serverOperators: [ServerOperator]
    var currentConditions: UsageConditions
    var conditionsAction: UsageConditionsAction?

    static var empty = ServerOperatorConditions(
        serverOperators: [],
        currentConditions: UsageConditions(conditionsId: 0, conditionsCommit: "empty", notifiedAt: nil, createdAt: .now),
        conditionsAction: nil
    )
}

enum ConditionsAcceptance: Equatable, Codable, Hashable {
    case accepted(acceptedAt: Date?, autoAccepted: Bool)
    // If deadline is present, it means there's a grace period to review and accept conditions during which user can continue to use the operator.
    // No deadline indicates it's required to accept conditions for the operator to start using it.
    case required(deadline: Date?)

    var conditionsAccepted: Bool {
        switch self {
        case .accepted: true
        case .required: false
        }
    }

    var usageAllowed: Bool {
        switch self {
        case .accepted: true
        case let .required(deadline): deadline != nil
        }
    }
}

struct ServerOperator: Identifiable, Equatable, Codable {
    var operatorId: Int64
    var operatorTag: OperatorTag?
    var tradeName: String
    var legalName: String?
    var serverDomains: [String]
    var conditionsAcceptance: ConditionsAcceptance
    var enabled: Bool
    var smpRoles: ServerRoles
    var xftpRoles: ServerRoles

    var id: Int64 { operatorId }

    static func == (l: ServerOperator, r: ServerOperator) -> Bool {
        l.operatorId == r.operatorId && l.operatorTag == r.operatorTag && l.tradeName == r.tradeName && l.legalName == r.legalName &&
        l.serverDomains == r.serverDomains && l.conditionsAcceptance == r.conditionsAcceptance && l.enabled == r.enabled &&
        l.smpRoles == r.smpRoles && l.xftpRoles == r.xftpRoles
    }

    var legalName_: String {
        legalName ?? tradeName
    }

    var info: ServerOperatorInfo {
        return if let operatorTag = operatorTag {
            operatorsInfo[operatorTag] ?? ServerOperator.dummyOperatorInfo
        } else {
            ServerOperator.dummyOperatorInfo
        }
    }

    static let dummyOperatorInfo = ServerOperatorInfo(
        description: ["Default"],
        website: URL(string: "https://simplex.chat")!,
        logo: "decentralized",
        largeLogo: "logo",
        logoDarkMode: "decentralized-light",
        largeLogoDarkMode: "logo-light"
    )

    func logo(_ colorScheme: ColorScheme) -> String {
        colorScheme == .light ? info.logo : info.logoDarkMode
    }

    func largeLogo(_ colorScheme: ColorScheme) -> String {
        colorScheme == .light ? info.largeLogo : info.largeLogoDarkMode
    }

    static var sampleData1 = ServerOperator(
        operatorId: 1,
        operatorTag: .simplex,
        tradeName: "SimpleX Chat",
        legalName: "SimpleX Chat Ltd",
        serverDomains: ["simplex.im"],
        conditionsAcceptance: .accepted(acceptedAt: nil, autoAccepted: false),
        enabled: true,
        smpRoles: ServerRoles(storage: true, proxy: true),
        xftpRoles: ServerRoles(storage: true, proxy: true)
    )
}

struct ServerRoles: Equatable, Codable {
    var storage: Bool
    var proxy: Bool
}

struct UserOperatorServers: Identifiable, Equatable, Codable {
    var `operator`: ServerOperator?
    var smpServers: [UserServer]
    var xftpServers: [UserServer]

    var id: String {
        if let op = self.operator {
            "\(op.operatorId)"
        } else {
            "nil operator"
        }
    }

    var operator_: ServerOperator {
        get {
            self.operator ?? ServerOperator(
                operatorId: 0,
                operatorTag: nil,
                tradeName: "",
                legalName: "",
                serverDomains: [],
                conditionsAcceptance: .accepted(acceptedAt: nil, autoAccepted: false),
                enabled: false,
                smpRoles: ServerRoles(storage: true, proxy: true),
                xftpRoles: ServerRoles(storage: true, proxy: true)
            )
        }
        set { `operator` = newValue }
    }

    static var sampleData1 = UserOperatorServers(
        operator: ServerOperator.sampleData1,
        smpServers: [UserServer.sampleData.preset],
        xftpServers: [UserServer.sampleData.xftpPreset]
    )

    static var sampleDataNilOperator = UserOperatorServers(
        operator: nil,
        smpServers: [UserServer.sampleData.preset],
        xftpServers: [UserServer.sampleData.xftpPreset]
    )
}

enum UserServersError: Decodable {
    case noServers(protocol: ServerProtocol, user: UserRef?)
    case storageMissing(protocol: ServerProtocol, user: UserRef?)
    case proxyMissing(protocol: ServerProtocol, user: UserRef?)
    case duplicateServer(protocol: ServerProtocol, duplicateServer: String, duplicateHost: String)

    var globalError: String? {
        switch self {
        case let .noServers(`protocol`, _):
            switch `protocol` {
            case .smp: return globalSMPError
            case .xftp: return globalXFTPError
            }
        case let .storageMissing(`protocol`, _):
            switch `protocol` {
            case .smp: return globalSMPError
            case .xftp: return globalXFTPError
            }
        case let .proxyMissing(`protocol`, _):
            switch `protocol` {
            case .smp: return globalSMPError
            case .xftp: return globalXFTPError
            }
        default: return nil
        }
    }

    var globalSMPError: String? {
        switch self {
        case let .noServers(.smp, user):
            let text = NSLocalizedString("No message servers.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        case let .storageMissing(.smp, user):
            let text = NSLocalizedString("No servers to receive messages.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        case let .proxyMissing(.smp, user):
            let text = NSLocalizedString("No servers for private message routing.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        default:
            return nil
        }
    }

    var globalXFTPError: String? {
        switch self {
        case let .noServers(.xftp, user):
            let text = NSLocalizedString("No media & file servers.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        case let .storageMissing(.xftp, user):
            let text = NSLocalizedString("No servers to send files.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        case let .proxyMissing(.xftp, user):
            let text = NSLocalizedString("No servers to receive files.", comment: "servers error")
            if let user = user {
                return userStr(user) + " " + text
            } else {
                return text
            }
        default:
            return nil
        }
    }

    private func userStr(_ user: UserRef) -> String {
        String.localizedStringWithFormat(NSLocalizedString("For chat profile %@:", comment: "servers error"), user.localDisplayName)
    }
}

struct UserServer: Identifiable, Equatable, Codable, Hashable {
    var serverId: Int64?
    var server: String
    var preset: Bool
    var tested: Bool?
    var enabled: Bool
    var deleted: Bool
    var createdAt = Date()

    static func == (l: UserServer, r: UserServer) -> Bool {
        l.serverId == r.serverId && l.server == r.server && l.preset == r.preset && l.tested == r.tested &&
        l.enabled == r.enabled && l.deleted == r.deleted
    }

    var id: String { "\(server) \(createdAt)" }

    static var empty = UserServer(serverId: nil, server: "", preset: false, tested: nil, enabled: false, deleted: false)

    var isEmpty: Bool {
        server.trimmingCharacters(in: .whitespaces) == ""
    }

    struct SampleData {
        var preset: UserServer
        var custom: UserServer
        var untested: UserServer
        var xftpPreset: UserServer
    }

    static var sampleData = SampleData(
        preset: UserServer(
            serverId: 1,
            server: "smp://abcd@smp8.simplex.im",
            preset: true,
            tested: true,
            enabled: true,
            deleted: false
        ),
        custom: UserServer(
            serverId: 2,
            server: "smp://abcd@smp9.simplex.im",
            preset: false,
            tested: false,
            enabled: false,
            deleted: false
        ),
        untested: UserServer(
            serverId: 3,
            server: "smp://abcd@smp10.simplex.im",
            preset: false,
            tested: nil,
            enabled: true,
            deleted: false
        ),
        xftpPreset: UserServer(
            serverId: 4,
            server: "xftp://abcd@xftp8.simplex.im",
            preset: true,
            tested: true,
            enabled: true,
            deleted: false
        )
    )

    enum CodingKeys: CodingKey {
        case serverId
        case server
        case preset
        case tested
        case enabled
        case deleted
    }
}

enum ProtocolTestStep: String, Decodable, Equatable {
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

struct ProtocolTestFailure: Decodable, Error, Equatable {
    var testStep: ProtocolTestStep
    var testError: AgentErrorType

    static func == (l: ProtocolTestFailure, r: ProtocolTestFailure) -> Bool {
        l.testStep == r.testStep
    }

    var localizedDescription: String {
        let err = String.localizedStringWithFormat(NSLocalizedString("Test failed at step %@.", comment: "server test failure"), testStep.text)
        switch testError {
        case .SMP(_, .AUTH):
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

struct MigrationFileLinkData: Codable {
    let networkConfig: NetworkConfig?

    struct NetworkConfig: Codable {
        let socksProxy: String?
        let networkProxy: NetworkProxy?
        let hostMode: HostMode?
        let requiredHostMode: Bool?

        func transformToPlatformSupported() -> NetworkConfig {
            return if let hostMode, let requiredHostMode {
                NetworkConfig(
                    socksProxy: nil,
                    networkProxy: nil,
                    hostMode: hostMode == .onionViaSocks ? .onionHost : hostMode,
                    requiredHostMode: requiredHostMode
                )
            } else { self }
        }
    }

    func addToLink(link: String) -> String {
        "\(link)&data=\(encodeJSON(self).addingPercentEncoding(withAllowedCharacters: .urlHostAllowed)!)"
    }

    static func readFromLink(link: String) -> MigrationFileLinkData? {
//        standaloneFileInfo(link)
        nil
    }
}

struct AppSettings: Codable, Equatable {
    var networkConfig: NetCfg? = nil
    var networkProxy: NetworkProxy? = nil
    var privacyEncryptLocalFiles: Bool? = nil
    var privacyAskToApproveRelays: Bool? = nil
    var privacyAcceptImages: Bool? = nil
    var privacyLinkPreviews: Bool? = nil
    var privacyShowChatPreviews: Bool? = nil
    var privacySaveLastDraft: Bool? = nil
    var privacyProtectScreen: Bool? = nil
    var privacyMediaBlurRadius: Int? = nil
    var notificationMode: AppSettingsNotificationMode? = nil
    var notificationPreviewMode: NotificationPreviewMode? = nil
    var webrtcPolicyRelay: Bool? = nil
    var webrtcICEServers: [String]? = nil
    var confirmRemoteSessions: Bool? = nil
    var connectRemoteViaMulticast: Bool? = nil
    var connectRemoteViaMulticastAuto: Bool? = nil
    var developerTools: Bool? = nil
    var confirmDBUpgrades: Bool? = nil
    var androidCallOnLockScreen: AppSettingsLockScreenCalls? = nil
    var iosCallKitEnabled: Bool? = nil
    var iosCallKitCallsInRecents: Bool? = nil
    var uiProfileImageCornerRadius: Double? = nil
    var uiChatItemRoundness: Double? = nil
    var uiChatItemTail: Bool? = nil
    var uiColorScheme: String? = nil
    var uiDarkColorScheme: String? = nil
    var uiCurrentThemeIds: [String: String]? = nil
    var uiThemes: [ThemeOverrides]? = nil
    var oneHandUI: Bool? = nil
    var chatBottomBar: Bool? = nil

    func prepareForExport() -> AppSettings {
        var empty = AppSettings()
        let def = AppSettings.defaults
        if networkConfig != def.networkConfig { empty.networkConfig = networkConfig }
        if networkProxy != def.networkProxy { empty.networkProxy = networkProxy }
        if privacyEncryptLocalFiles != def.privacyEncryptLocalFiles { empty.privacyEncryptLocalFiles = privacyEncryptLocalFiles }
        if privacyAskToApproveRelays != def.privacyAskToApproveRelays { empty.privacyAskToApproveRelays = privacyAskToApproveRelays }
        if privacyAcceptImages != def.privacyAcceptImages { empty.privacyAcceptImages = privacyAcceptImages }
        if privacyLinkPreviews != def.privacyLinkPreviews { empty.privacyLinkPreviews = privacyLinkPreviews }
        if privacyShowChatPreviews != def.privacyShowChatPreviews { empty.privacyShowChatPreviews = privacyShowChatPreviews }
        if privacySaveLastDraft != def.privacySaveLastDraft { empty.privacySaveLastDraft = privacySaveLastDraft }
        if privacyProtectScreen != def.privacyProtectScreen { empty.privacyProtectScreen = privacyProtectScreen }
        if privacyMediaBlurRadius != def.privacyMediaBlurRadius { empty.privacyMediaBlurRadius = privacyMediaBlurRadius }
        if notificationMode != def.notificationMode { empty.notificationMode = notificationMode }
        if notificationPreviewMode != def.notificationPreviewMode { empty.notificationPreviewMode = notificationPreviewMode }
        if webrtcPolicyRelay != def.webrtcPolicyRelay { empty.webrtcPolicyRelay = webrtcPolicyRelay }
        if webrtcICEServers != def.webrtcICEServers { empty.webrtcICEServers = webrtcICEServers }
        if confirmRemoteSessions != def.confirmRemoteSessions { empty.confirmRemoteSessions = confirmRemoteSessions }
        if connectRemoteViaMulticast != def.connectRemoteViaMulticast {empty.connectRemoteViaMulticast = connectRemoteViaMulticast }
        if connectRemoteViaMulticastAuto != def.connectRemoteViaMulticastAuto { empty.connectRemoteViaMulticastAuto = connectRemoteViaMulticastAuto }
        if developerTools != def.developerTools { empty.developerTools = developerTools }
        if confirmDBUpgrades != def.confirmDBUpgrades { empty.confirmDBUpgrades = confirmDBUpgrades }
        if androidCallOnLockScreen != def.androidCallOnLockScreen { empty.androidCallOnLockScreen = androidCallOnLockScreen }
        if iosCallKitEnabled != def.iosCallKitEnabled { empty.iosCallKitEnabled = iosCallKitEnabled }
        if iosCallKitCallsInRecents != def.iosCallKitCallsInRecents { empty.iosCallKitCallsInRecents = iosCallKitCallsInRecents }
        if uiProfileImageCornerRadius != def.uiProfileImageCornerRadius { empty.uiProfileImageCornerRadius = uiProfileImageCornerRadius }
        if uiChatItemRoundness != def.uiChatItemRoundness { empty.uiChatItemRoundness = uiChatItemRoundness }
        if uiChatItemTail != def.uiChatItemTail { empty.uiChatItemTail = uiChatItemTail }
        if uiColorScheme != def.uiColorScheme { empty.uiColorScheme = uiColorScheme }
        if uiDarkColorScheme != def.uiDarkColorScheme { empty.uiDarkColorScheme = uiDarkColorScheme }
        if uiCurrentThemeIds != def.uiCurrentThemeIds { empty.uiCurrentThemeIds = uiCurrentThemeIds }
        if uiThemes != def.uiThemes { empty.uiThemes = uiThemes }
        if oneHandUI != def.oneHandUI { empty.oneHandUI = oneHandUI }
        if chatBottomBar != def.chatBottomBar { empty.chatBottomBar = chatBottomBar }
        return empty
    }

    static var defaults: AppSettings {
        AppSettings (
            networkConfig: NetCfg.defaults,
            networkProxy: NetworkProxy.def,
            privacyEncryptLocalFiles: true,
            privacyAskToApproveRelays: true,
            privacyAcceptImages: true,
            privacyLinkPreviews: true,
            privacyShowChatPreviews: true,
            privacySaveLastDraft: true,
            privacyProtectScreen: false,
            privacyMediaBlurRadius: 0,
            notificationMode: AppSettingsNotificationMode.instant,
            notificationPreviewMode: NotificationPreviewMode.message,
            webrtcPolicyRelay: true,
            webrtcICEServers: [],
            confirmRemoteSessions: false,
            connectRemoteViaMulticast: true,
            connectRemoteViaMulticastAuto: true,
            developerTools: false,
            confirmDBUpgrades: false,
            androidCallOnLockScreen: AppSettingsLockScreenCalls.show,
            iosCallKitEnabled: true,
            iosCallKitCallsInRecents: false,
            uiProfileImageCornerRadius: 22.5,
            uiChatItemRoundness: 0.75,
            uiChatItemTail: true,
            uiColorScheme: DefaultTheme.SYSTEM_THEME_NAME,
            uiDarkColorScheme: DefaultTheme.SIMPLEX.themeName,
            uiCurrentThemeIds: nil as [String: String]?,
            uiThemes: nil as [ThemeOverrides]?,
            oneHandUI: true,
            chatBottomBar: true
        )
    }
}

enum AppSettingsNotificationMode: String, Codable {
    case off
    case periodic
    case instant

    func toNotificationsMode() -> NotificationsMode {
        switch self {
        case .instant: .instant
        case .periodic: .periodic
        case .off: .off
        }
    }

    static func from(_ mode: NotificationsMode) -> AppSettingsNotificationMode {
        switch mode {
        case .instant: .instant
        case .periodic: .periodic
        case .off: .off
        }
    }
}

//enum NotificationPreviewMode: Codable {
//    case hidden
//    case contact
//    case message
//}

enum AppSettingsLockScreenCalls: String, Codable {
    case disable
    case show
    case accept
}

struct UserNetworkInfo: Codable, Equatable {
    let networkType: UserNetworkType
    let online: Bool
}

enum UserNetworkType: String, Codable {
    case none
    case cellular
    case wifi
    case ethernet
    case other

    var text: LocalizedStringKey {
        switch self {
        case .none: "No network connection"
        case .cellular: "Cellular"
        case .wifi: "WiFi"
        case .ethernet: "Wired ethernet"
        case .other: "Other"
        }
    }
}

struct RcvMsgInfo: Codable {
    var msgId: Int64
    var msgDeliveryId: Int64
    var msgDeliveryStatus: String
    var agentMsgId: Int64
    var agentMsgMeta: String
}

struct ServerQueueInfo: Codable {
    var server: String
    var rcvId: String
    var sndId: String
    var ntfId: String?
    var status: String
    var info: QueueInfo
}

struct QueueInfo: Codable {
    var qiSnd: Bool
    var qiNtf: Bool
    var qiSub: QSub?
    var qiSize: Int
    var qiMsg: MsgInfo?
}

struct QSub: Codable {
    var qSubThread: QSubThread
    var qDelivered: String?
}

enum QSubThread: String, Codable {
    case noSub
    case subPending
    case subThread
    case prohibitSub
}

struct MsgInfo: Codable {
    var msgId: String
    var msgTs: Date
    var msgType: MsgType
}

enum MsgType: String, Codable {
    case message
    case quota
}

struct PresentedServersSummary: Codable {
    var statsStartedAt: Date
    var allUsersSMP: SMPServersSummary
    var allUsersXFTP: XFTPServersSummary
    var currentUserSMP: SMPServersSummary
    var currentUserXFTP: XFTPServersSummary
}

struct SMPServersSummary: Codable {
    var smpTotals: SMPTotals
    var currentlyUsedSMPServers: [SMPServerSummary]
    var previouslyUsedSMPServers: [SMPServerSummary]
    var onlyProxiedSMPServers: [SMPServerSummary]
}

struct SMPTotals: Codable {
    var sessions: ServerSessions
    var subs: SMPServerSubs
    var stats: AgentSMPServerStatsData
}

struct SMPServerSummary: Codable, Identifiable {
    var smpServer: String
    var known: Bool?
    var sessions: ServerSessions?
    var subs: SMPServerSubs?
    var stats: AgentSMPServerStatsData?

    var id: String { smpServer }

    var hasSubs: Bool { subs != nil }

    var sessionsOrNew: ServerSessions { sessions ?? ServerSessions.newServerSessions }

    var subsOrNew: SMPServerSubs { subs ?? SMPServerSubs.newSMPServerSubs }
}

struct ServerSessions: Codable {
    var ssConnected: Int
    var ssErrors: Int
    var ssConnecting: Int

    static var newServerSessions = ServerSessions(
        ssConnected: 0,
        ssErrors: 0,
        ssConnecting: 0
    )

    var hasSess: Bool { ssConnected > 0 }
}

struct SMPServerSubs: Codable {
    var ssActive: Int
    var ssPending: Int

    static var newSMPServerSubs = SMPServerSubs(
        ssActive: 0,
        ssPending: 0
    )

    var total: Int { ssActive + ssPending }

    var shareOfActive: Double {
        guard total != 0 else { return 0.0 }
        return Double(ssActive) / Double(total)
    }
}

struct AgentSMPServerStatsData: Codable {
    var _sentDirect: Int
    var _sentViaProxy: Int
    var _sentProxied: Int
    var _sentDirectAttempts: Int
    var _sentViaProxyAttempts: Int
    var _sentProxiedAttempts: Int
    var _sentAuthErrs: Int
    var _sentQuotaErrs: Int
    var _sentExpiredErrs: Int
    var _sentOtherErrs: Int
    var _recvMsgs: Int
    var _recvDuplicates: Int
    var _recvCryptoErrs: Int
    var _recvErrs: Int
    var _ackMsgs: Int
    var _ackAttempts: Int
    var _ackNoMsgErrs: Int
    var _ackOtherErrs: Int
    var _connCreated: Int
    var _connSecured: Int
    var _connCompleted: Int
    var _connDeleted: Int
    var _connDelAttempts: Int
    var _connDelErrs: Int
    var _connSubscribed: Int
    var _connSubAttempts: Int
    var _connSubIgnored: Int
    var _connSubErrs: Int
    var _ntfKey: Int
    var _ntfKeyAttempts: Int
    var _ntfKeyDeleted: Int
    var _ntfKeyDeleteAttempts: Int
}

struct XFTPServersSummary: Codable {
    var xftpTotals: XFTPTotals
    var currentlyUsedXFTPServers: [XFTPServerSummary]
    var previouslyUsedXFTPServers: [XFTPServerSummary]
}

struct XFTPTotals: Codable {
    var sessions: ServerSessions
    var stats: AgentXFTPServerStatsData
}

struct XFTPServerSummary: Codable, Identifiable {
    var xftpServer: String
    var known: Bool?
    var sessions: ServerSessions?
    var stats: AgentXFTPServerStatsData?
    var rcvInProgress: Bool
    var sndInProgress: Bool
    var delInProgress: Bool

    var id: String { xftpServer }
}

struct AgentXFTPServerStatsData: Codable {
    var _uploads: Int
    var _uploadsSize: Int64
    var _uploadAttempts: Int
    var _uploadErrs: Int
    var _downloads: Int
    var _downloadsSize: Int64
    var _downloadAttempts: Int
    var _downloadAuthErrs: Int
    var _downloadErrs: Int
    var _deletions: Int
    var _deleteAttempts: Int
    var _deleteErrs: Int
}

struct AgentNtfServerStatsData: Codable {
    var _ntfCreated: Int
    var _ntfCreateAttempts: Int
    var _ntfChecked: Int
    var _ntfCheckAttempts: Int
    var _ntfDeleted: Int
    var _ntfDelAttempts: Int
}
