//
//  NotificationService.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import UserNotifications
import OSLog
import StoreKit
import CallKit
import SimpleXChat

let logger = Logger()

let suspendingDelay: UInt64 = 2_000_000_000

typealias NtfStream = AsyncStream<NSENotification>

actor PendingNtfs {
    static let shared = PendingNtfs()
    private var ntfStreams: [String: NtfStream] = [:]
    private var ntfConts: [String: NtfStream.Continuation] = [:]

    func createStream(_ id: String) {
        logger.debug("PendingNtfs.createStream: \(id, privacy: .public)")
        if ntfStreams.index(forKey: id) == nil {
            ntfStreams[id] = AsyncStream { cont in
                ntfConts[id] = cont
                logger.debug("PendingNtfs.createStream: store continuation")
            }
        }
    }

    func readStream(_ id: String, for nse: NotificationService, msgCount: Int = 1, showNotifications: Bool) async {
        logger.debug("PendingNtfs.readStream: \(id, privacy: .public) \(msgCount, privacy: .public)")
        if let s = ntfStreams[id] {
            logger.debug("PendingNtfs.readStream: has stream")
            var rcvCount = max(1, msgCount)
            for await ntf in s {
                nse.setBestAttemptNtf(showNotifications ? ntf : .empty)
                rcvCount -= 1
                if rcvCount == 0 || ntf.categoryIdentifier == ntfCategoryCallInvitation { break }
            }
            logger.debug("PendingNtfs.readStream: exiting")
        }
    }

    func writeStream(_ id: String, _ ntf: NSENotification) {
        logger.debug("PendingNtfs.writeStream: \(id, privacy: .public)")
        if let cont = ntfConts[id] {
            logger.debug("PendingNtfs.writeStream: writing ntf")
            cont.yield(ntf)
        }
    }
}

enum NSENotification {
    case nse(notification: UNMutableNotificationContent)
    case callkit(invitation: RcvCallInvitation)
    case empty

    var categoryIdentifier: String? {
        switch self {
        case let .nse(ntf): return ntf.categoryIdentifier
        case .callkit: return ntfCategoryCallInvitation
        case .empty: return nil
        }
    }
}

class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptNtf: NSENotification?
    var badgeCount: Int = 0

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("DEBUGGING: NotificationService.didReceive")
        if let ntf = request.content.mutableCopy() as? UNMutableNotificationContent {
            setBestAttemptNtf(ntf)
        }
        self.contentHandler = contentHandler
        registerGroupDefaults()
        let appState = appStateGroupDefault.get()
        switch appState {
        case .suspended:
            logger.debug("NotificationService: app is suspended")
            setBadgeCount()
            receiveNtfMessages(request, contentHandler)
        case .suspending:
            logger.debug("NotificationService: app is suspending")
            setBadgeCount()
            Task {
                var state = appState
                for _ in 1...5 {
                    _ = try await Task.sleep(nanoseconds: suspendingDelay)
                    state = appStateGroupDefault.get()
                    if state == .suspended || state != .suspending { break }
                }
                logger.debug("NotificationService: app state is \(state.rawValue, privacy: .public)")
                if state.inactive {
                    receiveNtfMessages(request, contentHandler)
                } else {
                    deliverBestAttemptNtf()
                }
            }
        default:
            logger.debug("NotificationService: app state is \(appState.rawValue, privacy: .public)")
            deliverBestAttemptNtf()
        }
    }

    func receiveNtfMessages(_ request: UNNotificationRequest, _ contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("NotificationService: receiveNtfMessages")
        if case .documents = dbContainerGroupDefault.get() {
            deliverBestAttemptNtf()
            return
        }
        let userInfo = request.content.userInfo
        if let ntfData = userInfo["notificationData"] as? [AnyHashable : Any],
           let nonce = ntfData["nonce"] as? String,
           let encNtfInfo = ntfData["message"] as? String,
           let dbStatus = startChat() {
            if case .ok = dbStatus,
               let ntfMsgInfo = apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo) {
                logger.debug("NotificationService: receiveNtfMessages: apiGetNtfMessage \(String(describing: ntfMsgInfo), privacy: .public)")
                if let connEntity = ntfMsgInfo.connEntity {
                    setBestAttemptNtf(
                        ntfMsgInfo.ntfsEnabled
                        ? .nse(notification: createConnectionEventNtf(ntfMsgInfo.user, connEntity))
                        : .empty
                    )
                    if let id = connEntity.id {
                        Task {
                            logger.debug("NotificationService: receiveNtfMessages: in Task, connEntity id \(id, privacy: .public)")
                            await PendingNtfs.shared.createStream(id)
                            await PendingNtfs.shared.readStream(id, for: self, msgCount: ntfMsgInfo.ntfMessages.count, showNotifications: ntfMsgInfo.user.showNotifications)
                            deliverBestAttemptNtf()
                        }
                    }
                }
                return
            } else {
                setBestAttemptNtf(createErrorNtf(dbStatus))
            }
        }
        deliverBestAttemptNtf()
    }

    override func serviceExtensionTimeWillExpire() {
        logger.debug("DEBUGGING: NotificationService.serviceExtensionTimeWillExpire")
        deliverBestAttemptNtf()
    }

    func setBadgeCount() {
        badgeCount = ntfBadgeCountGroupDefault.get() + 1
        ntfBadgeCountGroupDefault.set(badgeCount)
    }

    func setBestAttemptNtf(_ ntf: UNMutableNotificationContent) {
        setBestAttemptNtf(.nse(notification: ntf))
    }

    func setBestAttemptNtf(_ ntf: NSENotification) {
        logger.debug("NotificationService.setBestAttemptNtf")
        if case let .nse(notification) = ntf {
            notification.badge = badgeCount as NSNumber
            bestAttemptNtf = .nse(notification: notification)
        } else {
            bestAttemptNtf = ntf
        }
    }

    private func deliverBestAttemptNtf() {
        logger.debug("NotificationService.deliverBestAttemptNtf")
        if let handler = contentHandler, let ntf = bestAttemptNtf {
            switch ntf {
            case let .nse(content): handler(content)
            case let .callkit(invitation):
                CXProvider.reportNewIncomingVoIPPushPayload([
                    "displayName": invitation.contact.displayName,
                    "contactId": invitation.contact.id,
                    "media": invitation.callType.media.rawValue
                ]) { error in
                    if error == nil {
                        handler(UNMutableNotificationContent())
                    } else {
                        logger.debug("reportNewIncomingVoIPPushPayload success to CallController for \(invitation.contact.id)")
                        handler(createCallInvitationNtf(invitation))
                    }
                }
            case .empty: handler(UNMutableNotificationContent())
            }
            bestAttemptNtf = nil
        }
    }
}

var chatStarted = false
var networkConfig: NetCfg = getNetCfg()
var xftpConfig: XFTPFileConfig? = getXFTPCfg()

func startChat() -> DBMigrationResult? {
    hs_init(0, nil)
    if chatStarted { return .ok }
    let (_, dbStatus) = chatMigrateInit(confirmMigrations: defaultMigrationConfirmation())
    if dbStatus != .ok {
        resetChatCtrl()
        return dbStatus
    }
    if let user = apiGetActiveUser() {
        logger.debug("active user \(String(describing: user))")
        do {
            try setNetworkConfig(networkConfig)
            try apiSetTempFolder(tempFolder: getTempFilesDirectory().path)
            try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
            try setXFTPConfig(xftpConfig)
            let justStarted = try apiStartChat()
            chatStarted = true
            if justStarted {
                chatLastStartGroupDefault.set(Date.now)
                Task { await receiveMessages() }
            }
            return .ok
        } catch {
            logger.error("NotificationService startChat error: \(responseError(error), privacy: .public)")
        }
    } else {
        logger.debug("no active user")
    }
    return nil
}

func receiveMessages() async {
    logger.debug("NotificationService receiveMessages")
    while true {
        updateNetCfg()
        if let msg = await chatRecvMsg() {
            if let (id, ntf) = await receivedMsgNtf(msg) {
                await PendingNtfs.shared.createStream(id)
                await PendingNtfs.shared.writeStream(id, ntf)
            }
        }
    }
}

func chatRecvMsg() async -> ChatResponse? {
    await withCheckedContinuation { cont in
        let resp = recvSimpleXMsg()
        cont.resume(returning: resp)
    }
}

private let isInChina = SKStorefront().countryCode == "CHN"
private func useCallKit() -> Bool { !isInChina && callKitEnabledGroupDefault.get() }

func receivedMsgNtf(_ res: ChatResponse) async -> (String, NSENotification)? {
    logger.debug("NotificationService processReceivedMsg: \(res.responseType)")
    switch res {
    case let .contactConnected(user, contact, _):
        return (contact.id, .nse(notification: createContactConnectedNtf(user, contact)))
//        case let .contactConnecting(contact):
//            TODO profile update
    case let .receivedContactRequest(user, contactRequest):
        return (UserContact(contactRequest: contactRequest).id, .nse(notification: createContactRequestNtf(user, contactRequest)))
    case let .newChatItem(user, aChatItem):
        let cInfo = aChatItem.chatInfo
        var cItem = aChatItem.chatItem
        if !cInfo.ntfsEnabled {
            ntfBadgeCountGroupDefault.set(max(0, ntfBadgeCountGroupDefault.get() - 1))
        }
        if let file = cItem.autoReceiveFile() {
            cItem = autoReceiveFile(file, encrypted: cItem.encryptLocalFile) ?? cItem
        }
        let ntf: NSENotification = cInfo.ntfsEnabled ? .nse(notification: createMessageReceivedNtf(user, cInfo, cItem)) : .empty
        return cItem.showNotification ? (aChatItem.chatId, ntf) : nil
    case let .rcvFileSndCancelled(_, aChatItem, _):
        cleanupFile(aChatItem)
        return nil
    case let .sndFileComplete(_, aChatItem, _):
        cleanupDirectFile(aChatItem)
        return nil
    case let .sndFileRcvCancelled(_, aChatItem, _):
        cleanupDirectFile(aChatItem)
        return nil
    case let .sndFileCompleteXFTP(_, aChatItem, _):
        cleanupFile(aChatItem)
        return nil
    case let .callInvitation(invitation):
        // Do not post it without CallKit support, iOS will stop launching the app without showing CallKit
        return (
            invitation.contact.id,
            useCallKit() ? .callkit(invitation: invitation) : .nse(notification: createCallInvitationNtf(invitation))
        )
    default:
        logger.debug("NotificationService processReceivedMsg ignored event: \(res.responseType)")
        return nil
    }
}

func updateNetCfg() {
    let newNetConfig = getNetCfg()
    if newNetConfig != networkConfig {
        logger.debug("NotificationService applying changed network config")
        do {
            try setNetworkConfig(networkConfig)
            networkConfig = newNetConfig
        } catch {
            logger.error("NotificationService apply changed network config error: \(responseError(error), privacy: .public)")
        }
    }
}

func apiGetActiveUser() -> User? {
    let r = sendSimpleXCmd(.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd response: \(String(describing: r))")
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(_, .error(.noActiveUser)): return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() throws -> Bool {
    let r = sendSimpleXCmd(.startChat(subscribe: false, expire: false, xftp: false))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiSetTempFolder(tempFolder: String) throws {
    let r = sendSimpleXCmd(.setTempFolder(tempFolder: tempFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = sendSimpleXCmd(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}

func setXFTPConfig(_ cfg: XFTPFileConfig?) throws {
    let r = sendSimpleXCmd(.apiSetXFTPConfig(config: cfg))
    if case .cmdOk = r { return }
    throw r
}

func apiGetNtfMessage(nonce: String, encNtfInfo: String) -> NtfMessages? {
    guard apiGetActiveUser() != nil else {
        logger.debug("no active user")
        return nil
    }
    let r = sendSimpleXCmd(.apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo))
    if case let .ntfMessages(user, connEntity, msgTs, ntfMessages) = r, let user = user {
        return NtfMessages(user: user, connEntity: connEntity, msgTs: msgTs, ntfMessages: ntfMessages)
    } else if case let .chatCmdError(_, error) = r {
        logger.debug("apiGetNtfMessage error response: \(String.init(describing: error))")
    } else {
        logger.debug("apiGetNtfMessage ignored response: \(r.responseType, privacy: .public) \(String.init(describing: r), privacy: .private)")
    }
    return nil
}

func apiReceiveFile(fileId: Int64, encrypted: Bool, inline: Bool? = nil) -> AChatItem? {
    let r = sendSimpleXCmd(.receiveFile(fileId: fileId, encrypted: encrypted, inline: inline))
    if case let .rcvFileAccepted(_, chatItem) = r { return chatItem }
    logger.error("receiveFile error: \(responseError(r))")
    return nil
}

func apiSetFileToReceive(fileId: Int64, encrypted: Bool) {
    let r = sendSimpleXCmd(.setFileToReceive(fileId: fileId, encrypted: encrypted))
    if case .cmdOk = r { return }
    logger.error("setFileToReceive error: \(responseError(r))")
}

func autoReceiveFile(_ file: CIFile, encrypted: Bool) -> ChatItem? {
    switch file.fileProtocol {
    case .smp:
        return apiReceiveFile(fileId: file.fileId, encrypted: encrypted)?.chatItem
    case .xftp:
        apiSetFileToReceive(fileId: file.fileId, encrypted: encrypted)
        return nil
    }
}

func setNetworkConfig(_ cfg: NetCfg) throws {
    let r = sendSimpleXCmd(.apiSetNetworkConfig(networkConfig: cfg))
    if case .cmdOk = r { return }
    throw r
}

struct NtfMessages {
    var user: User
    var connEntity: ConnectionEntity?
    var msgTs: Date?
    var ntfMessages: [NtfMsgInfo]

    var ntfsEnabled: Bool {
        user.showNotifications && (connEntity?.ntfsEnabled ?? false)
    }
}
