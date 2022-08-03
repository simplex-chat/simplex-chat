//
//  NotificationService.swift
//  SimpleX NSE
//
//  Created by Evgeny on 26/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import UserNotifications
import OSLog
import SimpleXChat

let logger = Logger()

let suspendingDelay: UInt64 = 2_000_000_000

typealias NtfStream = AsyncStream<UNMutableNotificationContent>

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

    func readStream(_ id: String, for nse: NotificationService, msgCount: Int = 1) async {
        logger.debug("PendingNtfs.readStream: \(id, privacy: .public) \(msgCount, privacy: .public)")
        if let s = ntfStreams[id] {
            logger.debug("PendingNtfs.readStream: has stream")
            var rcvCount = max(1, msgCount)
            for await ntf in s {
                nse.setBestAttemptNtf(ntf)
                rcvCount -= 1
                if rcvCount == 0 || ntf.categoryIdentifier == ntfCategoryCallInvitation { break }
            }
            logger.debug("PendingNtfs.readStream: exiting")
        }
    }

    func writeStream(_ id: String, _ ntf: UNMutableNotificationContent) {
        logger.debug("PendingNtfs.writeStream: \(id, privacy: .public)")
        if let cont = ntfConts[id] {
            logger.debug("PendingNtfs.writeStream: writing ntf")
            cont.yield(ntf)
        }
    }
}



class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptNtf: UNMutableNotificationContent?
    var badgeCount: Int = 0

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("NotificationService.didReceive")
        badgeCount = ntfBadgeCountGroupDefault.get() + 1
        ntfBadgeCountGroupDefault.set(badgeCount)
        setBestAttemptNtf(request.content.mutableCopy() as? UNMutableNotificationContent)
        self.contentHandler = contentHandler
        registerGroupDefaults()
        let appState = appStateGroupDefault.get()
        switch appState {
        case .suspended:
            logger.debug("NotificationService: app is suspended")
            receiveNtfMessages(request, contentHandler)
        case .suspending:
            logger.debug("NotificationService: app is suspending")
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
           let _ = startChat() {
            logger.debug("NotificationService: receiveNtfMessages: chat is started")
            if let ntfMsgInfo = apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo) {
                logger.debug("NotificationService: receiveNtfMessages: apiGetNtfMessage \(String(describing: ntfMsgInfo), privacy: .public)")
                if let connEntity = ntfMsgInfo.connEntity {
                    setBestAttemptNtf(createConnectionEventNtf(connEntity))
                    if let id = connEntity.id {
                        Task {
                            logger.debug("NotificationService: receiveNtfMessages: in Task, connEntity id \(id, privacy: .public)")
                            await PendingNtfs.shared.createStream(id)
                            await PendingNtfs.shared.readStream(id, for: self, msgCount: ntfMsgInfo.ntfMessages.count)
                            deliverBestAttemptNtf()
                        }
                    }
                }
                return
            }
        }
        deliverBestAttemptNtf()
    }

    override func serviceExtensionTimeWillExpire() {
        logger.debug("NotificationService.serviceExtensionTimeWillExpire")
        deliverBestAttemptNtf()
    }

    func setBestAttemptNtf(_ ntf: UNMutableNotificationContent?) {
        logger.debug("NotificationService.setBestAttemptNtf")
        bestAttemptNtf = ntf
        bestAttemptNtf?.badge = badgeCount as NSNumber
    }

    private func deliverBestAttemptNtf() {
        logger.debug("NotificationService.deliverBestAttemptNtf")
        if let handler = contentHandler, let content = bestAttemptNtf {
            handler(content)
            bestAttemptNtf = nil
        }
    }
}

func startChat() -> User? {
    hs_init(0, nil)
    if let user = apiGetActiveUser() {
        logger.debug("active user \(String(describing: user))")
        do {
            try setNetworkConfig(getNetCfg())
            let justStarted = try apiStartChat()
            if justStarted {
                try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
                chatLastStartGroupDefault.set(Date.now)
                Task { await receiveMessages() }
            }
            return user
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

func receivedMsgNtf(_ res: ChatResponse) async -> (String, UNMutableNotificationContent)? {
    logger.debug("NotificationService processReceivedMsg: \(res.responseType)")
    switch res {
    case let .contactConnected(contact):
        return (contact.id, createContactConnectedNtf(contact))
//        case let .contactConnecting(contact):
//            TODO profile update
    case let .receivedContactRequest(contactRequest):
        return (UserContact(contactRequest: contactRequest).id, createContactRequestNtf(contactRequest))
    case let .newChatItem(aChatItem):
        let cInfo = aChatItem.chatInfo
        var cItem = aChatItem.chatItem
        if case .image = cItem.content.msgContent {
           if let file = cItem.file,
              file.fileSize <= maxImageSize,
              privacyAcceptImagesGroupDefault.get() {
               cItem = apiReceiveFile(fileId: file.fileId)?.chatItem ?? cItem
           }
        }
        return cItem.isCall() ? nil : (aChatItem.chatId, createMessageReceivedNtf(cInfo, cItem))
    case let .callInvitation(invitation):
        return (invitation.contact.id, createCallInvitationNtf(invitation))
    default:
        logger.debug("NotificationService processReceivedMsg ignored event: \(res.responseType)")
        return nil
    }
}

func apiGetActiveUser() -> User? {
    let _ = getChatCtrl()
    let r = sendSimpleXCmd(.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd responce: \(String(describing: r))")
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(.error(.noActiveUser)): return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() throws -> Bool {
    let r = sendSimpleXCmd(.startChat(subscribe: false))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiSetFilesFolder(filesFolder: String) throws {
    let r = sendSimpleXCmd(.setFilesFolder(filesFolder: filesFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiGetNtfMessage(nonce: String, encNtfInfo: String) -> NtfMessages? {
    let r = sendSimpleXCmd(.apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo))
    if case let .ntfMessages(connEntity, msgTs, ntfMessages) = r {
        return NtfMessages(connEntity: connEntity, msgTs: msgTs, ntfMessages: ntfMessages)
    }
    logger.debug("apiGetNtfMessage ignored response: \(String.init(describing: r), privacy: .public)")
    return nil
}

func apiReceiveFile(fileId: Int64) -> AChatItem? {
    let r = sendSimpleXCmd(.receiveFile(fileId: fileId))
    if case let .rcvFileAccepted(chatItem) = r { return chatItem }
    logger.error("receiveFile error: \(responseError(r))")
    return nil
}

func setNetworkConfig(_ cfg: NetCfg) throws {
    let r = sendSimpleXCmd(.apiSetNetworkConfig(networkConfig: cfg))
    if case .cmdOk = r { return }
    throw r
}

struct NtfMessages {
    var connEntity: ConnectionEntity?
    var msgTs: Date?
    var ntfMessages: [NtfMsgInfo]
}
