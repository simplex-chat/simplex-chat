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

let appSuspendingDelay: UInt64 = 2_500_000_000

typealias SuspendSchedule = (delay: TimeInterval, timeout: Int)

let nseSuspendSchedule: SuspendSchedule = (2, 4)

let fastNSESuspendSchedule: SuspendSchedule = (1, 1)

enum NSENotification {
    case nse(UNMutableNotificationContent)
    case callkit(RcvCallInvitation)
    case empty
    case msgInfo(NtfMsgInfo)

    var isCallInvitation: Bool {
        switch self {
        case let .nse(ntf): ntf.categoryIdentifier == ntfCategoryCallInvitation
        case .callkit: true
        case .empty: false
        case .msgInfo: false
        }
    }
}

// Once the last thread in the process completes processing chat controller is suspended, and the database is closed, to avoid
// background crashes and contention for database with the application (both UI and background fetch triggered either on schedule
// or when background notification is received.
class NSEThreads {
    static let shared = NSEThreads()
    private static let queue = DispatchQueue(label: "chat.simplex.app.SimpleX-NSE.notification-threads.lock")
    private var allThreads: Set<UUID> = []
    private var activeThreads: [(UUID, NotificationService)] = []

    func newThread() -> UUID {
        NSEThreads.queue.sync {
            let (_, t) = allThreads.insert(UUID())
            return t
        }
    }

    func startThread(_ t: UUID, _ service: NotificationService) {
        NSEThreads.queue.sync {
            if allThreads.contains(t) {
                activeThreads.append((t, service))
            } else {
                logger.warning("NotificationService startThread: thread \(t) was removed before it started")
            }
        }
    }

    func processNotification(_ id: ChatId, _ ntf: NSENotification) async -> Void {
        var waitTime: Int64 = 5_000_000000
        while waitTime > 0 {
            if let (_, nse) = rcvEntityThread(id),
               nse.shouldProcessNtf && nse.processReceivedNtf(ntf) {
                break
            } else {
                try? await Task.sleep(nanoseconds: 10_000000)
                waitTime -= 10_000000
            }
        }
    }

    private func rcvEntityThread(_ id: ChatId) -> (UUID, NotificationService)? {
        NSEThreads.queue.sync {
            activeThreads.first(where: { (_, nse) in nse.receiveEntityId == id })
        }
    }

    func endThread(_ t: UUID) -> Bool {
        NSEThreads.queue.sync {
            let tActive: UUID? = if let index = activeThreads.firstIndex(where: { $0.0 == t }) {
                activeThreads.remove(at: index).0
            } else {
                nil
            }
            let t = allThreads.remove(t)
            if tActive != nil && activeThreads.isEmpty {
                return true
            }
            if t != nil && allThreads.isEmpty {
                NSEChatState.shared.set(.suspended)
            }
            return false
        }
    }

    var noThreads: Bool {
        allThreads.isEmpty
    }
}

// Notification service extension creates a new instance of the class and calls didReceive for each notification.
// Each didReceive is called in its own thread, but multiple calls can be made in one process, and, empirically, there is never
// more than one process of notification service extension exists at a time.
// Soon after notification service delivers the last notification it is either suspended or terminated.
class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    var bestAttemptNtf: NSENotification?
    var badgeCount: Int = 0
    // thread is added to allThreads here - if thread did not start chat,
    // chat does not need to be suspended but NSE state still needs to be set to "suspended".
    var threadId: UUID? = NSEThreads.shared.newThread()
    var notificationInfo: NtfMessages?
    var receiveEntityId: String?
    var expectedMessages: Set<String> = []
    // return true if the message is taken - it prevents sending it to another NotificationService instance for processing
    var shouldProcessNtf = false
    var appSubscriber: AppSubscriber?
    var returnedSuspension = false

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("DEBUGGING: NotificationService.didReceive")
        let ntf = if let ntf_ = request.content.mutableCopy() as? UNMutableNotificationContent { ntf_ } else { UNMutableNotificationContent() }
        setBestAttemptNtf(ntf)
        self.contentHandler = contentHandler
        registerGroupDefaults()
        let appState = appStateGroupDefault.get()
        logger.debug("NotificationService: app is \(appState.rawValue)")
        switch appState {
        case .stopped:
            setBadgeCount()
            setBestAttemptNtf(createAppStoppedNtf())
            deliverBestAttemptNtf()
        case .suspended:
            setBadgeCount()
            receiveNtfMessages(request, contentHandler)
        case .suspending:
            setBadgeCount()
            Task {
                let state: AppState = await withCheckedContinuation { cont in
                    appSubscriber = appStateSubscriber { s in
                        if s == .suspended { appSuspension(s) }
                    }
                    DispatchQueue.global().asyncAfter(deadline: .now() + Double(appSuspendTimeout) + 1) {
                        logger.debug("NotificationService: appSuspension timeout")
                        appSuspension(appStateGroupDefault.get())
                    }

                    @Sendable
                    func appSuspension(_ s: AppState) {
                        if !self.returnedSuspension {
                            self.returnedSuspension = true
                            self.appSubscriber = nil // this disposes of appStateSubscriber
                            cont.resume(returning: s)
                        }
                    }
                }
                logger.debug("NotificationService: app state is now \(state.rawValue)")
                if state.inactive {
                    receiveNtfMessages(request, contentHandler)
                } else {
                    deliverBestAttemptNtf()
                }
            }
        default:
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
           // check it here again
           appStateGroupDefault.get().inactive {
            // thread is added to activeThreads tracking set here - if thread started chat it needs to be suspended
            if let t = threadId { NSEThreads.shared.startThread(t, self) }
            let dbStatus = startChat()
            if case .ok = dbStatus,
               let ntfInfo = apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo) {
                logger.debug("NotificationService: receiveNtfMessages: apiGetNtfMessage \(String(describing: ntfInfo.ntfMessages.count))")
                if let connEntity = ntfInfo.connEntity_ {
                    setBestAttemptNtf(
                        ntfInfo.ntfsEnabled
                        ? .nse(createConnectionEventNtf(ntfInfo.user, connEntity))
                        : .empty
                    )
                    if let id = connEntity.id, ntfInfo.msgTs != nil {
                        notificationInfo = ntfInfo
                        receiveEntityId = id
                        expectedMessages = Set(ntfInfo.ntfMessages.map { $0.msgId })
                        shouldProcessNtf = true
                        return
                    }
                }
            } else if let dbStatus = dbStatus {
                setBestAttemptNtf(createErrorNtf(dbStatus))
            }
        }
        deliverBestAttemptNtf()
    }

    override func serviceExtensionTimeWillExpire() {
        logger.debug("DEBUGGING: NotificationService.serviceExtensionTimeWillExpire")
        deliverBestAttemptNtf(urgent: true)
    }

    func processReceivedNtf(_ ntf: NSENotification) -> Bool {
        guard let ntfInfo = notificationInfo, let msgTs = ntfInfo.msgTs else { return false }
        if !ntfInfo.user.showNotifications {
            self.setBestAttemptNtf(.empty)
        }
        if case let .msgInfo(info) = ntf {
            let found = expectedMessages.remove(info.msgId)
            if found != nil {
                logger.debug("NotificationService processNtf: msgInfo, last: \(self.expectedMessages.isEmpty)")
                if expectedMessages.isEmpty {
                    self.deliverBestAttemptNtf()
                }
                return true
            } else if info.msgTs > msgTs {
                logger.debug("NotificationService processNtf: unexpected msgInfo, let other instance to process it, stopping this one")
                self.deliverBestAttemptNtf()
                return false
            } else {
                logger.debug("NotificationService processNtf: unknown message, let other instance to process it")
                return false
            }
        } else if ntfInfo.user.showNotifications {
            logger.debug("NotificationService processNtf: setting best attempt")
            self.setBestAttemptNtf(ntf)
            if ntf.isCallInvitation {
                self.deliverBestAttemptNtf()
            }
            return true
        }
        return false
    }

    func setBadgeCount() {
        badgeCount = ntfBadgeCountGroupDefault.get() + 1
        ntfBadgeCountGroupDefault.set(badgeCount)
    }

    func setBestAttemptNtf(_ ntf: UNMutableNotificationContent) {
        setBestAttemptNtf(.nse(ntf))
    }

    func setBestAttemptNtf(_ ntf: NSENotification) {
        logger.debug("NotificationService.setBestAttemptNtf")
        if case let .nse(notification) = ntf {
            notification.badge = badgeCount as NSNumber
            bestAttemptNtf = .nse(notification)
        } else {
            bestAttemptNtf = ntf
        }
    }

    private func deliverBestAttemptNtf(urgent: Bool = false) {
        logger.debug("NotificationService.deliverBestAttemptNtf")
        // stop processing other messages
        shouldProcessNtf = false

        let suspend: Bool
        if let t = threadId {
            threadId = nil
            suspend = NSEThreads.shared.endThread(t) && NSEThreads.shared.noThreads
        } else {
            suspend = false
        }
        deliverCallkitOrNotification(urgent: urgent, suspend: suspend)
    }

    private func deliverCallkitOrNotification(urgent: Bool, suspend: Bool = false) {
        if case .callkit = bestAttemptNtf {
            logger.debug("NotificationService.deliverCallkitOrNotification: will suspend, callkit")
            if urgent {
                // suspending NSE even though there may be other notifications
                // to allow the app to process callkit call
                suspendChat(0)
                deliverNotification()
            } else {
                // suspending NSE with delay and delivering after the suspension
                // because pushkit notification must be processed without delay
                // to avoid app termination
                DispatchQueue.global().asyncAfter(deadline: .now() + fastNSESuspendSchedule.delay) {
                    suspendChat(fastNSESuspendSchedule.timeout)
                    DispatchQueue.global().asyncAfter(deadline: .now() + Double(fastNSESuspendSchedule.timeout)) {
                        self.deliverNotification()
                    }
                }
            }
        } else {
            if suspend {
                logger.debug("NotificationService.deliverCallkitOrNotification: will suspend")
                if urgent {
                    suspendChat(0)
                } else {
                    // suspension is delayed to allow chat core finalise any processing
                    // (e.g., send delivery receipts)
                    DispatchQueue.global().asyncAfter(deadline: .now() + nseSuspendSchedule.delay) {
                        if NSEThreads.shared.noThreads {
                            suspendChat(nseSuspendSchedule.timeout)
                        }
                    }
                }
            }
            deliverNotification()
        }
    }

    private func deliverNotification() {
        if let handler = contentHandler, let ntf = bestAttemptNtf {
            contentHandler = nil
            bestAttemptNtf = nil
            let deliver: (UNMutableNotificationContent?) -> Void = { ntf in
                let useNtf = if let ntf = ntf {
                    appStateGroupDefault.get().running ? UNMutableNotificationContent() : ntf
                } else {
                    UNMutableNotificationContent()
                }
                handler(useNtf)
            }
            switch ntf {
            case let .nse(content): deliver(content)
            case let .callkit(invitation):
                logger.debug("NotificationService reportNewIncomingVoIPPushPayload for \(invitation.contact.id)")
                CXProvider.reportNewIncomingVoIPPushPayload([
                    "displayName": invitation.contact.displayName,
                    "contactId": invitation.contact.id,
                    "media": invitation.callType.media.rawValue
                ]) { error in
                    logger.debug("reportNewIncomingVoIPPushPayload result: \(error)")
                    deliver(error == nil ? nil : createCallInvitationNtf(invitation))
                }
            case .empty: deliver(nil) // used to mute notifications that did not unsubscribe yet
            case .msgInfo: deliver(nil) // unreachable, the best attempt is never set to msgInfo
            }
        }
    }
}

// nseStateGroupDefault must not be used in NSE directly, only via this singleton
class NSEChatState {
    static let shared = NSEChatState()
    private var value_ = NSEState.created

    var value: NSEState {
        value_
    }

    func set(_ state: NSEState) {
        nseStateGroupDefault.set(state)
        sendNSEState(state)
        value_ = state
    }

    init() {
        // This is always set to .created state, as in case previous start of NSE crashed in .active state, it is stored correctly.
        // Otherwise the app will be activating slower
        set(.created)
    }
}

var appSubscriber: AppSubscriber = appStateSubscriber { state in
    logger.debug("NotificationService: appSubscriber")
    if state.running && NSEChatState.shared.value.canSuspend {
        logger.debug("NotificationService: appSubscriber app state \(state.rawValue), suspending")
        suspendChat(fastNSESuspendSchedule.timeout)
    }
}

func appStateSubscriber(onState: @escaping (AppState) -> Void) -> AppSubscriber {
    appMessageSubscriber { msg in
        if case let .state(state) = msg {
            logger.debug("NotificationService: appStateSubscriber \(state.rawValue)")
            onState(state)
        }
    }
}

var receiverStarted = false
let startLock = DispatchSemaphore(value: 1)
let suspendLock = DispatchSemaphore(value: 1)
var networkConfig: NetCfg = getNetCfg()

// startChat uses semaphore startLock to ensure that only one didReceive thread can start chat controller
// Subsequent calls to didReceive will be waiting on semaphore and won't start chat again, as it will be .active
func startChat() -> DBMigrationResult? {
    logger.debug("NotificationService: startChat")
    // only skip creating if there is chat controller
    if case .active = NSEChatState.shared.value, hasChatCtrl() { return .ok }

    startLock.wait()
    defer { startLock.signal() }
    
    if hasChatCtrl() {
        return switch NSEChatState.shared.value {
        case .created: doStartChat()
        case .starting: .ok // it should never get to this branch, as it would be waiting for start on startLock
        case .active: .ok
        case .suspending: activateChat()
        case .suspended: activateChat()
        }
    } else {
        // Ignore state in preference if there is no chat controller.
        // State in preference may have failed to update e.g. because of a crash.
        NSEChatState.shared.set(.created)
        return doStartChat()
    }
}

func doStartChat() -> DBMigrationResult? {
    logger.debug("NotificationService: doStartChat")
    haskell_init_nse()
    let (_, dbStatus) = chatMigrateInit(confirmMigrations: defaultMigrationConfirmation(), backgroundMode: true)
    logger.debug("NotificationService: doStartChat \(String(describing: dbStatus))")
    if dbStatus != .ok {
        resetChatCtrl()
        NSEChatState.shared.set(.created)
        return dbStatus
    }
    let state = NSEChatState.shared.value
    NSEChatState.shared.set(.starting)
    if let user = apiGetActiveUser() {
        logger.debug("NotificationService active user \(String(describing: user))")
        do {
            try setNetworkConfig(networkConfig)
            try apiSetTempFolder(tempFolder: getTempFilesDirectory().path)
            try apiSetFilesFolder(filesFolder: getAppFilesDirectory().path)
            try apiSetEncryptLocalFiles(privacyEncryptLocalFilesGroupDefault.get())
            // prevent suspension while starting chat
            suspendLock.wait()
            defer { suspendLock.signal() }
            if NSEChatState.shared.value == .starting {
                updateNetCfg()
                let justStarted = try apiStartChat()
                NSEChatState.shared.set(.active)
                if justStarted {
                    chatLastStartGroupDefault.set(Date.now)
                    Task {
                        if !receiverStarted {
                            receiverStarted = true
                            await receiveMessages()
                        }
                    }
                }
                return .ok
            }
        } catch {
            logger.error("NotificationService startChat error: \(responseError(error))")
        }
    } else {
        logger.debug("NotificationService: no active user")
    }
    if NSEChatState.shared.value == .starting { NSEChatState.shared.set(state) }
    return nil
}

func activateChat() -> DBMigrationResult? {
    logger.debug("NotificationService: activateChat")
    let state = NSEChatState.shared.value
    NSEChatState.shared.set(.active)
    if apiActivateChat() {
        logger.debug("NotificationService: activateChat: after apiActivateChat")
        return .ok
    } else {
        NSEChatState.shared.set(state)
        return nil
    }
}

// suspendChat uses semaphore suspendLock to ensure that only one suspension can happen.
func suspendChat(_ timeout: Int) {
    logger.debug("NotificationService: suspendChat")
    let state = NSEChatState.shared.value
    if !state.canSuspend {
        logger.error("NotificationService suspendChat called, current state: \(state.rawValue)")
    } else if hasChatCtrl() {
        // only suspend if we have chat controller to avoid crashes when suspension is
        // attempted when chat controller was not created
        suspendLock.wait()
        defer { suspendLock.signal() }

        NSEChatState.shared.set(.suspending)
        if apiSuspendChat(timeoutMicroseconds: timeout * 1000000) {
            logger.debug("NotificationService: suspendChat: after apiSuspendChat")
            DispatchQueue.global().asyncAfter(deadline: .now() + Double(timeout) + 1, execute: chatSuspended)
        } else {
            NSEChatState.shared.set(state)
        }
    }
}

func chatSuspended() {
    logger.debug("NotificationService chatSuspended")
    if case .suspending = NSEChatState.shared.value {
        NSEChatState.shared.set(.suspended)
        chatCloseStore()
        logger.debug("NotificationService chatSuspended: suspended")
    }
}

// A single loop is used per Notification service extension process to receive and process all messages depending on the NSE state
// If the extension is not active yet, or suspended/suspending, or the app is running, the notifications will not be received.
func receiveMessages() async {
    logger.debug("NotificationService receiveMessages")
    while true {
        switch NSEChatState.shared.value {
        // it should never get to "created" and "starting" branches, as NSE state is set to .active before the loop start
        case .created: await delayWhenInactive()
        case .starting: await delayWhenInactive()
        case .active: await receiveMsg()
        case .suspending: await receiveMsg()
        case .suspended: await delayWhenInactive()
        }
    }

    func receiveMsg() async {
        if let msg = await chatRecvMsg() {
            logger.debug("NotificationService receiveMsg: message")
            if let (id, ntf) = await receivedMsgNtf(msg) {
                logger.debug("NotificationService receiveMsg: notification")
                await NSEThreads.shared.processNotification(id, ntf)
            }
        }
    }

    func delayWhenInactive() async {
        logger.debug("NotificationService delayWhenInactive")
        _ = try? await Task.sleep(nanoseconds: 1000_000000)
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
    logger.debug("NotificationService receivedMsgNtf: \(res.responseType)")
    switch res {
    case let .contactConnected(user, contact, _):
        return (contact.id, .nse(createContactConnectedNtf(user, contact)))
//        case let .contactConnecting(contact):
//            TODO profile update
    case let .receivedContactRequest(user, contactRequest):
        return (UserContact(contactRequest: contactRequest).id, .nse(createContactRequestNtf(user, contactRequest)))
    case let .newChatItem(user, aChatItem):
        let cInfo = aChatItem.chatInfo
        var cItem = aChatItem.chatItem
        if !cInfo.ntfsEnabled {
            ntfBadgeCountGroupDefault.set(max(0, ntfBadgeCountGroupDefault.get() - 1))
        }
        if let file = cItem.autoReceiveFile() {
            cItem = autoReceiveFile(file) ?? cItem
        }
        let ntf: NSENotification = cInfo.ntfsEnabled ? .nse(createMessageReceivedNtf(user, cInfo, cItem)) : .empty
        return cItem.showNotification ? (aChatItem.chatId, ntf) : nil
    case let .rcvFileSndCancelled(_, aChatItem, _):
        cleanupFile(aChatItem)
        return nil
    case let .sndFileComplete(_, aChatItem, _):
        cleanupDirectFile(aChatItem)
        return nil
    case let .sndFileRcvCancelled(_, aChatItem, _):
        if let aChatItem = aChatItem {
            cleanupDirectFile(aChatItem)
        }
        return nil
    case let .callInvitation(invitation):
        // Do not post it without CallKit support, iOS will stop launching the app without showing CallKit
        return (
            invitation.contact.id,
            useCallKit() ? .callkit(invitation) : .nse(createCallInvitationNtf(invitation))
        )
    case let .ntfMessage(_, connEntity, ntfMessage):
        return if let id = connEntity.id { (id, .msgInfo(ntfMessage)) } else { nil }
    case .chatSuspended:
        chatSuspended()
        return nil
    case let .chatError(_, err):
        logger.error("NotificationService receivedMsgNtf error: \(String(describing: err))")
        return nil
    default:
        logger.debug("NotificationService receivedMsgNtf ignored event: \(res.responseType)")
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
            logger.error("NotificationService apply changed network config error: \(responseError(error))")
        }
    }
}

func apiGetActiveUser() -> User? {
    let r = sendSimpleXCmd(.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd response: \(r.responseType)")
    switch r {
    case let .activeUser(user): return user
    case .chatCmdError(_, .error(.noActiveUser)):
        logger.debug("apiGetActiveUser sendSimpleXCmd no active user")
        return nil
    case let .chatCmdError(_, err):
        logger.debug("apiGetActiveUser sendSimpleXCmd error: \(String(describing: err))")
        return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() throws -> Bool {
    let r = sendSimpleXCmd(.startChat(mainApp: false))
    switch r {
    case .chatStarted: return true
    case .chatRunning: return false
    default: throw r
    }
}

func apiActivateChat() -> Bool {
    chatReopenStore()
    let r = sendSimpleXCmd(.apiActivateChat(restoreChat: false))
    if case .cmdOk = r { return true }
    logger.error("NotificationService apiActivateChat error: \(String(describing: r))")
    return false
}

func apiSuspendChat(timeoutMicroseconds: Int) -> Bool {
    let r = sendSimpleXCmd(.apiSuspendChat(timeoutMicroseconds: timeoutMicroseconds))
    if case .cmdOk = r { return true }
    logger.error("NotificationService apiSuspendChat error: \(String(describing: r))")
    return false
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

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r = sendSimpleXCmd(.apiSetEncryptLocalFiles(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiGetNtfMessage(nonce: String, encNtfInfo: String) -> NtfMessages? {
    guard apiGetActiveUser() != nil else {
        logger.debug("no active user")
        return nil
    }
    let r = sendSimpleXCmd(.apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo))
    if case let .ntfMessages(user, connEntity_, msgTs, ntfMessages) = r, let user = user {
        logger.debug("apiGetNtfMessage response ntfMessages: \(ntfMessages.count)")
        return NtfMessages(user: user, connEntity_: connEntity_, msgTs: msgTs, ntfMessages: ntfMessages)
    } else if case let .chatCmdError(_, error) = r {
        logger.debug("apiGetNtfMessage error response: \(String.init(describing: error))")
    } else {
        logger.debug("apiGetNtfMessage ignored response: \(r.responseType) \(String.init(describing: r))")
    }
    return nil
}

func apiReceiveFile(fileId: Int64, encrypted: Bool, inline: Bool? = nil) -> AChatItem? {
    let userApprovedRelays = !privacyAskToApproveRelaysGroupDefault.get()
    let r = sendSimpleXCmd(.receiveFile(fileId: fileId, userApprovedRelays: userApprovedRelays, encrypted: encrypted, inline: inline))
    if case let .rcvFileAccepted(_, chatItem) = r { return chatItem }
    logger.error("receiveFile error: \(responseError(r))")
    return nil
}

func apiSetFileToReceive(fileId: Int64, encrypted: Bool) {
    let userApprovedRelays = !privacyAskToApproveRelaysGroupDefault.get()
    let r = sendSimpleXCmd(.setFileToReceive(fileId: fileId, userApprovedRelays: userApprovedRelays, encrypted: encrypted))
    if case .cmdOk = r { return }
    logger.error("setFileToReceive error: \(responseError(r))")
}

func autoReceiveFile(_ file: CIFile) -> ChatItem? {
    let encrypted = privacyEncryptLocalFilesGroupDefault.get()
    switch file.fileProtocol {
    case .smp:
        return apiReceiveFile(fileId: file.fileId, encrypted: encrypted)?.chatItem
    case .xftp:
        apiSetFileToReceive(fileId: file.fileId, encrypted: encrypted)
        return nil
    case .local:
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
    var connEntity_: ConnectionEntity?
    var msgTs: Date?
    var ntfMessages: [NtfMsgInfo]

    var ntfsEnabled: Bool {
        user.showNotifications && (connEntity_?.ntfsEnabled ?? false)
    }
}
