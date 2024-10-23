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
}

public enum NSENotificationData {
    case connectionEvent(_ user: User, _ connEntity: ConnectionEntity)
    case contactConnected(_ user: any UserLike, _ contact: Contact)
    case contactRequest(_ user: any UserLike, _ contactRequest: UserContactRequest)
    case messageReceived(_ user: any UserLike, _ cInfo: ChatInfo, _ cItem: ChatItem)
    case callInvitation(_ invitation: RcvCallInvitation)
    case msgInfo(NtfMsgAckInfo)
    case noNtf

    var callInvitation: RcvCallInvitation? {
        switch self {
        case let .callInvitation(invitation): invitation
        default: nil
        }
    }

    func notificationContent(_ badgeCount: Int) -> UNMutableNotificationContent {
        return switch self {
        case let .connectionEvent(user, connEntity): createConnectionEventNtf(user, connEntity, badgeCount)
        case let .contactConnected(user, contact): createContactConnectedNtf(user, contact, badgeCount)
        case let .contactRequest(user, contactRequest): createContactRequestNtf(user, contactRequest, badgeCount)
        case let .messageReceived(user, cInfo, cItem): createMessageReceivedNtf(user, cInfo, cItem, badgeCount)
        case let .callInvitation(invitation): createCallInvitationNtf(invitation, badgeCount)
        case .msgInfo: UNMutableNotificationContent()
        case .noNtf: UNMutableNotificationContent()
        }
    }

    var notificationEvent: NSENotificationData? {
        return switch self {
        case .connectionEvent: self
        case .contactConnected: self
        case .contactRequest: self
        case .messageReceived: self
        case .callInvitation: self
        case .msgInfo: nil
        case .noNtf: nil
        }
    }

    var newMsgData: (any UserLike, ChatInfo)? {
        return switch self {
        case let .messageReceived(user, cInfo, _): (user, cInfo)
        default: nil
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

    func processNotification(_ id: ChatId, _ ntf: NSENotificationData) async -> Void {
        var waitTime: Int64 = 5_000_000000
        while waitTime > 0 {
            if let (_, nse) = rcvEntityThread(id),
               nse.shouldProcessNtf && nse.processReceivedNtf(id, ntf) {
                break
            } else {
                try? await Task.sleep(nanoseconds: 10_000000)
                waitTime -= 10_000000
            }
        }
    }

    private func rcvEntityThread(_ id: ChatId) -> (UUID, NotificationService)? {
        NSEThreads.queue.sync {
            // this selects the earliest thread that:
            // 1) has this connection in nse.expectedMessages
            // 2) has not completed processing messages for this connection (not ready)
            activeThreads.first(where: { (_, nse) in nse.expectedMessages[id]?.ready == false })
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

struct ExpectedMessage {
    var ntfMessage: UserNtfMessage
    var receiveConnId: String?
    var expectedMsgId: String?
    var allowedGetNextAttempts: Int = 3
    var msgBestAttemptNtf: NSENotificationData?
    var ready: Bool
}

// Notification service extension creates a new instance of the class and calls didReceive for each notification.
// Each didReceive is called in its own thread, but multiple calls can be made in one process, and, empirically, there is never
// more than one process of notification service extension exists at a time.
// Soon after notification service delivers the last notification it is either suspended or terminated.
class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    // served as notification if no message attempts (msgBestAttemptNtf) could be produced
    var serviceBestAttemptNtf: NSENotification?
    var badgeCount: Int = 0
    // thread is added to allThreads here - if thread did not start chat,
    // chat does not need to be suspended but NSE state still needs to be set to "suspended".
    var threadId: UUID? = NSEThreads.shared.newThread()
    var expectedMessages: Dictionary<String, ExpectedMessage> = [:] // key is receiveEntityId
    // return true if the message is taken - it prevents sending it to another NotificationService instance for processing
    var shouldProcessNtf = false
    var appSubscriber: AppSubscriber?
    var returnedSuspension = false

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("DEBUGGING: NotificationService.didReceive")
        let ntf = if let ntf_ = request.content.mutableCopy() as? UNMutableNotificationContent { ntf_ } else { UNMutableNotificationContent() }
        setServiceBestAttemptNtf(ntf)
        self.contentHandler = contentHandler
        registerGroupDefaults()
        let appState = appStateGroupDefault.get()
        logger.debug("NotificationService: app is \(appState.rawValue)")
        switch appState {
        case .stopped:
            setBadgeCount()
            setServiceBestAttemptNtf(createAppStoppedNtf(badgeCount))
            deliverBestAttemptNtf()
        case .suspended:
            receiveNtfMessages(request, contentHandler)
        case .suspending:
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
        case .active: contentHandler(UNMutableNotificationContent())
        case .activating: contentHandler(UNMutableNotificationContent())
        case .bgRefresh: contentHandler(UNMutableNotificationContent())
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
               let ntfMessages = apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo) {
                logger.debug("NotificationService: receiveNtfMessages: apiGetNtfMessage ntfMessages count = \(ntfMessages.count)")
                for ntfMessage in ntfMessages {
                    addExpectedMessage(ntfMessage: ntfMessage)
                }
                if !expectedMessages.isEmpty {
                    shouldProcessNtf = true
                    return
                }
            } else if let dbStatus = dbStatus {
                setServiceBestAttemptNtf(createErrorNtf(dbStatus, badgeCount))
            }
        }
        deliverBestAttemptNtf()
    }

    func addExpectedMessage(ntfMessage: UserNtfMessage) {
        if let connEntity = ntfMessage.connEntity_,
           let receiveEntityId = connEntity.id, ntfMessage.expectedMsg_ != nil {
            let expectedMsgId = ntfMessage.expectedMsg_?.msgId
            let receivedMsgId = ntfMessage.receivedMsg_?.msgId
            logger.debug("NotificationService: addExpectedMessage: expectedMsgId = \(expectedMsgId ?? "nil", privacy: .private), receivedMsgId = \(receivedMsgId ?? "nil", privacy: .private)")
            expectedMessages[receiveEntityId] = ExpectedMessage(
                ntfMessage: ntfMessage,
                receiveConnId: connEntity.conn.agentConnId,
                expectedMsgId: expectedMsgId,
                allowedGetNextAttempts: 3,
                msgBestAttemptNtf: ntfMessage.defaultBestAttemptNtf,
                ready: false
            )
        }
    }

    override func serviceExtensionTimeWillExpire() {
        logger.debug("DEBUGGING: NotificationService.serviceExtensionTimeWillExpire")
        deliverBestAttemptNtf(urgent: true)
    }

    var expectingMoreMessages: Bool {
        !expectedMessages.allSatisfy { $0.value.ready }
    }

    // Boolean value returned from this function indicates whether outer receiving loop should continue to receive next messages, or wait for current message to be assigned to another thread (via timed while loop)
    func processReceivedNtf(_ id: ChatId, _ ntf: NSENotificationData) -> Bool {
        guard let expectedMessage = expectedMessages[id] else {
            return expectingMoreMessages
        }
        guard let expectedMsgTs = expectedMessage.ntfMessage.expectedMsg_?.msgTs else {
            expectedMessages[id]?.ready = true
            return expectingMoreMessages
        }
        if case let .msgInfo(info) = ntf {
            if info.msgId == expectedMessage.expectedMsgId {
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): expected")
                expectedMessages[id]?.expectedMsgId = nil
                expectedMessages[id]?.ready = true
                self.deliverBestAttemptNtf()
                return true
            } else if let msgTs = info.msgTs_, msgTs > expectedMsgTs {
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unexpected msgInfo, let other instance to process it, stopping this one")
                expectedMessages[id]?.ready = true
                self.deliverBestAttemptNtf()
                return expectingMoreMessages
            } else if (expectedMessages[id]?.allowedGetNextAttempts ?? 0) > 0, let receiveConnId = expectedMessages[id]?.receiveConnId {
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unexpected msgInfo, get next message")
                expectedMessages[id]?.allowedGetNextAttempts -= 1
                if let receivedMsg = apiGetConnNtfMessage(connId: receiveConnId) {
                    logger.debug("NotificationService processNtf, on apiGetConnNtfMessage: msgInfo msgId = \(info.msgId, privacy: .private), receivedMsg msgId = \(receivedMsg.msgId, privacy: .private)")
                    return true
                } else {
                    logger.debug("NotificationService processNtf, on apiGetConnNtfMessage: msgInfo msgId = \(info.msgId, privacy: .private): no next message, deliver best attempt")
                    expectedMessages[id]?.ready = true
                    self.deliverBestAttemptNtf()
                    return expectingMoreMessages
                }
            } else {
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unknown message, let other instance to process it")
                expectedMessages[id]?.ready = true
                self.deliverBestAttemptNtf()
                return expectingMoreMessages
            }
        } else if expectedMessage.ntfMessage.user.showNotifications {
            logger.debug("NotificationService processNtf: setting best attempt")
            if ntf.notificationEvent != nil {
                setBadgeCount()
            }
            let prevBestAttempt = expectedMessages[id]?.msgBestAttemptNtf
            if prevBestAttempt?.callInvitation != nil {
                if ntf.callInvitation != nil { // replace with newer call
                    expectedMessages[id]?.msgBestAttemptNtf = ntf
                } // otherwise keep call as best attempt
            } else {
                expectedMessages[id]?.msgBestAttemptNtf = ntf
            }
            return true
        }
        expectedMessages[id]?.ready = true
        return expectingMoreMessages
    }

    func setBadgeCount() {
        badgeCount = ntfBadgeCountGroupDefault.get() + 1
        ntfBadgeCountGroupDefault.set(badgeCount)
    }

    func setServiceBestAttemptNtf(_ ntf: UNMutableNotificationContent) {
        logger.debug("NotificationService.setServiceBestAttemptNtf")
        serviceBestAttemptNtf = .nse(ntf)
    }

    private func deliverBestAttemptNtf(urgent: Bool = false) {
        if (urgent || !expectingMoreMessages) {
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
    }

    private func deliverCallkitOrNotification(urgent: Bool, suspend: Bool = false) {
        if useCallKit() && expectedMessages.contains(where: { $0.value.msgBestAttemptNtf?.callInvitation != nil }) {
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
        if let handler = contentHandler, let ntf = prepareNotification() {
            contentHandler = nil
            serviceBestAttemptNtf = nil
            switch ntf {
            case let .nse(content):
                content.badge = badgeCount as NSNumber
                handler(content)
            case let .callkit(invitation):
                logger.debug("NotificationService reportNewIncomingVoIPPushPayload for \(invitation.contact.id)")
                CXProvider.reportNewIncomingVoIPPushPayload([
                    "displayName": invitation.contact.displayName,
                    "contactId": invitation.contact.id,
                    "callUUID": invitation.callUUID ?? "",
                    "media": invitation.callType.media.rawValue,
                    "callTs": invitation.callTs.timeIntervalSince1970
                ]) { error in
                    logger.debug("reportNewIncomingVoIPPushPayload result: \(error)")
                    handler(error == nil ? UNMutableNotificationContent() : createCallInvitationNtf(invitation, self.badgeCount))
                }
            case .empty:
                handler(UNMutableNotificationContent()) // used to mute notifications that did not unsubscribe yet
            }
        }
    }

    private func prepareNotification() -> NSENotification? {
        if expectedMessages.isEmpty {
            return serviceBestAttemptNtf
        } else if let callNtfKV = expectedMessages.first(where: { $0.value.msgBestAttemptNtf?.callInvitation != nil }),
                  let callInv = callNtfKV.value.msgBestAttemptNtf?.callInvitation,
                  let callNtf = callNtfKV.value.msgBestAttemptNtf {
            return useCallKit() ? .callkit(callInv) : .nse(callNtf.notificationContent(badgeCount))
        } else {
            let ntfEvents = expectedMessages.compactMap { $0.value.msgBestAttemptNtf?.notificationEvent }
            if ntfEvents.isEmpty {
                return .empty
            } else if let ntfEvent = ntfEvents.count == 1 ? ntfEvents.first : nil {
                return .nse(ntfEvent.notificationContent(badgeCount))
            } else {
                return .nse(createJointNtf(ntfEvents))
            }
        }
    }

    private func createJointNtf(_ ntfEvents: [NSENotificationData]) -> UNMutableNotificationContent {
        let previewMode = ntfPreviewModeGroupDefault.get()
        let newMsgsData: [(any UserLike, ChatInfo)] = ntfEvents.compactMap { $0.newMsgData }
        if !newMsgsData.isEmpty, let userId = newMsgsData.first?.0.userId {
            let newMsgsChats: [ChatInfo] = newMsgsData.map { $0.1 }
            let uniqueChatsNames = uniqueNewMsgsChatsNames(newMsgsChats)
            var body: String
            if previewMode == .hidden {
                body = String.localizedStringWithFormat(NSLocalizedString("New messages in %d chats", comment: "notification body"), uniqueChatsNames.count)
            } else {
                body = String.localizedStringWithFormat(NSLocalizedString("From: %@", comment: "notification body"), newMsgsChatsNamesStr(uniqueChatsNames))
            }
            return createNotification(
                categoryIdentifier: ntfCategoryManyEvents,
                title: NSLocalizedString("New messages", comment: "notification"),
                body: body,
                userInfo: ["userId": userId],
                badgeCount: badgeCount
            )
        } else {
            return createNotification(
                categoryIdentifier: ntfCategoryManyEvents,
                title: NSLocalizedString("New events", comment: "notification"),
                body: String.localizedStringWithFormat(NSLocalizedString("%d new events", comment: "notification body"), ntfEvents.count),
                badgeCount: badgeCount
            )
        }
    }

    private func uniqueNewMsgsChatsNames(_ newMsgsChats: [ChatInfo]) -> [String] {
        var seenChatIds = Set<ChatId>()
        var uniqueChatsNames: [String] = []
        for chat in newMsgsChats {
            if !seenChatIds.contains(chat.id) {
                seenChatIds.insert(chat.id)
                uniqueChatsNames.append(chat.chatViewName)
            }
        }
        return uniqueChatsNames
    }

    private func newMsgsChatsNamesStr(_ names: [String]) -> String {
        return switch names.count {
        case 1: names[0]
        case 2: "\(names[0]) and \(names[1])"
        case 3: "\(names[0] + ", " + names[1]) and \(names[2])"
        default:
            names.count > 3
            ? "\(names[0]), \(names[1]) and \(names.count - 2) other chats"
            : ""
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

let seSubscriber = seMessageSubscriber {
    switch $0 {
    case let .state(state):
        if state == .sendingMessage && NSEChatState.shared.value.canSuspend {
            logger.debug("NotificationService: seSubscriber app state \(state.rawValue), suspending")
            suspendChat(fastNSESuspendSchedule.timeout)
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
            try apiSetAppFilePaths(filesFolder: getAppFilesDirectory().path, tempFolder: getTempFilesDirectory().path, assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path)
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

func receivedMsgNtf(_ res: ChatResponse) async -> (String, NSENotificationData)? {
    logger.debug("NotificationService receivedMsgNtf: \(res.responseType)")
    switch res {
    case let .contactConnected(user, contact, _):
        return (contact.id, .contactConnected(user, contact))
//        case let .contactConnecting(contact):
//            TODO profile update
    case let .receivedContactRequest(user, contactRequest):
        return (UserContact(contactRequest: contactRequest).id, .contactRequest(user, contactRequest))
    case let .newChatItems(user, chatItems):
        // Received items are created one at a time
        if let chatItem = chatItems.first {
            let cInfo = chatItem.chatInfo
            var cItem = chatItem.chatItem
            if let file = cItem.autoReceiveFile() {
                cItem = autoReceiveFile(file) ?? cItem
            }
            let ntf: NSENotificationData = (cInfo.ntfsEnabled && cItem.showNotification) ? .messageReceived(user, cInfo, cItem) : .noNtf
            return (chatItem.chatId, ntf)
        } else {
            return nil
        }
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
        return (invitation.contact.id, .callInvitation(invitation))
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
    let r = sendSimpleXCmd(.startChat(mainApp: false, enableSndFiles: false))
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

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String) throws {
    let r = sendSimpleXCmd(.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder))
    if case .cmdOk = r { return }
    throw r
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r = sendSimpleXCmd(.apiSetEncryptLocalFiles(enable: enable))
    if case .cmdOk = r { return }
    throw r
}

func apiGetNtfMessage(nonce: String, encNtfInfo: String) -> [UserNtfMessage]? {
    guard apiGetActiveUser() != nil else {
        logger.debug("no active user")
        return nil
    }
    let r = sendSimpleXCmd(.apiGetNtfMessage(nonce: nonce, encNtfInfo: encNtfInfo))
    if case let .ntfMessages(ntfMessages) = r {
        logger.debug("apiGetNtfMessage response ntfMessages: \(ntfMessages.count)")
        return ntfMessages.compactMap { toUserNtfMessage($0) }
    } else if case let .chatCmdError(_, error) = r {
        logger.debug("apiGetNtfMessage error response: \(String.init(describing: error))")
    } else {
        logger.debug("apiGetNtfMessage ignored response: \(r.responseType) \(String.init(describing: r))")
    }
    return nil
}

func toUserNtfMessage(_ ntfMessage: NtfMessage) -> UserNtfMessage? {
    if let user = ntfMessage.user_ {
        return UserNtfMessage(user: user, connEntity_: ntfMessage.connEntity_, expectedMsg_: ntfMessage.expectedMsg_, receivedMsg_: ntfMessage.receivedMsg_)
    } else {
        return nil
    }
}

func apiGetConnNtfMessage(connId: String) -> NtfMsgInfo? {
    guard apiGetActiveUser() != nil else {
        logger.debug("no active user")
        return nil
    }
    let r = sendSimpleXCmd(.apiGetConnNtfMessage(connId: connId))
    if case let .connNtfMessage(receivedMsg_) = r {
        logger.debug("apiGetConnNtfMessage response receivedMsg_: \(receivedMsg_ == nil ? 0 : 1)")
        return receivedMsg_
    }
    logger.debug("apiGetConnNtfMessage error: \(responseError(r))")
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

struct UserNtfMessage {
    var user: User
    var connEntity_: ConnectionEntity?
    var expectedMsg_: NtfMsgInfo?
    var receivedMsg_: NtfMsgInfo?

    var defaultBestAttemptNtf: NSENotificationData {
        return if !user.showNotifications {
            .noNtf
        } else if let connEntity = connEntity_ {
            switch connEntity {
            case let .rcvDirectMsgConnection(_, contact):
                contact?.chatSettings.enableNtfs == .all
                ? .connectionEvent(user, connEntity)
                : .noNtf
            case let .rcvGroupMsgConnection(_, groupInfo, _):
                groupInfo.chatSettings.enableNtfs == .all
                ? .connectionEvent(user, connEntity)
                : .noNtf
            case .sndFileConnection: .noNtf
            case .rcvFileConnection: .noNtf
            case let .userContactConnection(_, userContact):
                userContact.groupId == nil
                ? .connectionEvent(user, connEntity)
                : .noNtf
            }
        } else {
            .noNtf
        }
    }
}
