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

public enum NSENotificationData {
    case connectionEvent(_ user: User, _ connEntity: ConnectionEntity)
    case contactConnected(_ user: any UserLike, _ contact: Contact)
    case contactRequest(_ user: any UserLike, _ contactRequest: UserContactRequest)
    case messageReceived(_ user: any UserLike, _ cInfo: ChatInfo, _ cItem: ChatItem)
    case callInvitation(_ invitation: RcvCallInvitation)
    case msgInfo(NtfMsgAckInfo)
    case noNtf

    @inline(__always)
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

    @inline(__always)
    var notificationEvent: NSENotificationData? {
        switch self {
        case .connectionEvent: self
        case .contactConnected: self
        case .contactRequest: self
        case .messageReceived: self
        case .callInvitation: self
        case .msgInfo: nil
        case .noNtf: nil
        }
    }

    @inline(__always)
    var newMsgNtf: NSENotificationData? {
        switch self {
        case .messageReceived: self
        default: nil
        }
    }
}

// Once the last thread in the process completes processing chat controller is suspended, and the database is closed, to avoid
// background crashes and contention for database with the application (both UI and background fetch triggered either on schedule
// or when background notification is received.
class NSEThreads {
    static let shared = NSEThreads()
    private let queue = DispatchQueue(label: "chat.simplex.app.SimpleX-NSE.notification-threads.lock")
    private var allThreads: Set<UUID> = []
    private var activeThreads: [(threadId: UUID, nse: NotificationService)] = []
    private var droppedNotifications: [(entityId: ChatId, ntf: NSENotificationData)] = []

    @inline(__always)
    private init() {} // only shared instance can be used

    @inline(__always)
    func newThread() -> UUID {
        queue.sync {
            let (_, t) = allThreads.insert(UUID())
            return t
        }
    }

    @inline(__always)
    func startThread(_ t: UUID, _ service: NotificationService) {
        queue.sync {
            if allThreads.contains(t) {
                activeThreads.append((t, service))
            } else {
                logger.warning("NotificationService startThread: thread \(t) was removed before it started")
            }
        }
    }

    // atomically:
    // - checks that passed NSE instance can start processing passed notification entity,
    // - adds it to the passed NSE instance,
    // - marks as started, if no other NSE instance is processing it.
    // Making all these steps atomic prevents a race condition between threads when both will be added and none will be started
    @inline(__always)
    func startEntity(_ nse: NotificationService, _ ntfEntity: NotificationEntity) -> Bool {
        queue.sync {
            // checking that none of activeThreads with another NSE instance processes the same entity and is not ready
            let canStart = !activeThreads.contains(where: { (tId, otherNSE) in
                tId != nse.threadId
                && otherNSE.notificationEntities.contains(where: { (id, otherEntity) in
                    id == ntfEntity.entityId
                    && otherEntity.expectedMsg != nil
                })
            })
            // atomically add entity to passed NSE instance
            let id = ntfEntity.entityId
            nse.notificationEntities[id] = ntfEntity
            if canStart {
                // and set as started, so it cannot be chosen to start by another NSE entity in nextThread
                nse.notificationEntities[id]?.startedProcessingNewMsgs = true
            }
            return canStart
        }
    }

    @inline(__always)
    func addDroppedNtf(_ id: ChatId, _ ntf: NSENotificationData) {
        queue.sync { droppedNotifications.append((id, ntf)) }
    }

    // atomically remove and return first dropped notification for the passed entity
    @inline(__always)
    func takeDroppedNtf(_ ntfEntity: NotificationEntity) -> (entityId: ChatId, ntf: NSENotificationData)? {
        queue.sync {
            if droppedNotifications.isEmpty {
                nil
            } else if let i = droppedNotifications.firstIndex(where: { (id, _) in id == ntfEntity.entityId }) {
                droppedNotifications.remove(at: i)
            } else {
                nil
            }
        }
    }

    // passes notification for processing to NSE instance chosen by rcvEntityThread
    @inline(__always)
    func processNotification(_ id: ChatId, _ ntf: NSENotificationData) async -> Void {
        if let (nse, ntfEntity, expectedMsg) = rcvEntityThread(id, ntf) {
            logger.debug("NotificationService processNotification \(id): found nse thread expecting message")
            if nse.processReceivedNtf(ntfEntity, expectedMsg, ntf) {
                nse.finalizeEntity(id)
            }
        }
    }

    // atomically:
    // - chooses active NSE instance that is ready to process notifications and expects message for passed entity ID
    // - returns all dependencies for processing (notification entity and expected message)
    // - adds notification to droppedNotifications if no ready NSE instance is found for the entity
    @inline(__always)
    private func rcvEntityThread(_ id: ChatId, _ ntf: NSENotificationData) -> (NotificationService, NotificationEntity, NtfMsgInfo)? {
        queue.sync {
            // this selects the earliest thread that:
            // 1) has this connection entity in nse.notificationEntitites
            // 2) has not completed processing messages for this connection entity (not ready)
            let r = activeThreads.lazy.compactMap({ (_, nse) in
                let ntfEntity = nse.notificationEntities[id]
                return if let ntfEntity, let expectedMsg = ntfEntity.expectedMsg, ntfEntity.shouldProcessNtf {
                    (nse, ntfEntity, expectedMsg)
                } else {
                    nil
                }
            }).first
            if r == nil { droppedNotifications.append((id, ntf)) }
            return r
        }
    }

    // Atomically mark entity in the passed NSE instance as not expecting messages,
    // and signal the next NSE instance with this entity to start its processing.
    @inline(__always)
    func signalNextThread(_ nse: NotificationService, _ id: ChatId) {
        queue.sync {
            nse.notificationEntities[id]?.expectedMsg = nil
            nse.notificationEntities[id]?.shouldProcessNtf = false
            let next = activeThreads.first(where: { (_, nseNext) in
                if let ntfEntity = nseNext.notificationEntities[id] {
                    ntfEntity.expectedMsg != nil && !ntfEntity.startedProcessingNewMsgs
                } else {
                    false
                }
            })
            if let (tNext, nseNext) = next {
                if let t = nse.threadId { logger.debug("NotificationService thread \(t): signalNextThread: signal next thread \(tNext) for entity \(id)") }
                nseNext.notificationEntities[id]?.startedProcessingNewMsgs = true
                nseNext.notificationEntities[id]?.semaphore.signal()
            }
        }
    }

    @inline(__always)
    func endThread(_ t: UUID) -> Bool {
        queue.sync {
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

    @inline(__always)
    var noThreads: Bool {
        allThreads.isEmpty
    }
}

// NotificationEntity is a processing state for notifications from a single connection entity (message queue).
// Each NSE instance within NSE process can have more than one NotificationEntity.
// NotificationEntities of an NSE instance are processed concurrently, as messages arrive in any order.
// NotificationEntities for the same connection across multiple NSE instances (NSEThreads) are processed sequentially, so that the earliest NSE instance receives the earliest messages.
// The reason for this complexity is to process all required messages within allotted 30 seconds,
// accounting for the possibility that multiple notifications may be delivered concurrently. 
struct NotificationEntity {
    var ntfConn: NtfConn
    var entityId: ChatId

    // expectedMsg == nil means that entity already has the best attempt to deliver, and no more messages are expected.
    // It happens when:
    // - the user is muted (set to nil in mkNotificationEntity)
    // - apiGetNtfConns returns that there are no new messages (msgId in notification matches previously received),
    // - messaging server fails to respond or replies that there are no messages (apiGetConnNtfMessages / getConnNtfMessage),
    // - the message is received with the correct ID or timestamp (set to nil in signalNextThread).
    var expectedMsg: NtfMsgInfo?
    var allowedGetNextAttempts: Int = 3
    var msgBestAttemptNtf: NSENotificationData

    // startedProcessingNewMsgs determines that the entity stared processing events once it processed dropped notifications.
    // It remains true when shouldProcessNtf is set to false, to prevent NSE from being chosen as the next for the entity.
    // It is atomically set to true by startThead or by nextThread
    var startedProcessingNewMsgs: Bool = false

    // shouldProcessNtf determines that NSE should process events for this entity,
    // it is atomically set:
    // - to true in processDroppedNotifications in case dropped notification is not chosen for delivery, and more messages are needed.
    // - to false in nextThread
    var shouldProcessNtf: Bool = false

    // this semaphone is used to wait for another NSE instance processing events for the same entity
    var semaphore: DispatchSemaphore = DispatchSemaphore(value: 0)

    var connMsgReq: ConnMsgReq? {
        if let expectedMsg {
            ConnMsgReq(msgConnId: ntfConn.agentConnId, msgDbQueueId: ntfConn.agentDbQueueId, msgTs: expectedMsg.msgTs)
        } else {
            nil
        }
    }
}

// Notification service extension creates a new instance of the class and calls didReceive for each notification.
// Each didReceive is called in its own thread, but multiple calls can be made in one process, and, empirically, there is never
// more than one process of notification service extension exists at a time.
// Soon after notification service delivers the last notification it is either suspended or terminated.
class NotificationService: UNNotificationServiceExtension {
    var contentHandler: ((UNNotificationContent) -> Void)?
    // served as notification if no message attempts (msgBestAttemptNtf) could be produced
    var serviceBestAttemptNtf: UNMutableNotificationContent?
    var badgeCount: Int = 0
    // thread is added to allThreads here - if thread did not start chat,
    // chat does not need to be suspended but NSE state still needs to be set to "suspended".
    var threadId: UUID? = NSEThreads.shared.newThread()
    var notificationEntities: Dictionary<String, NotificationEntity> = [:] // key is entityId
    var appSubscriber: AppSubscriber?
    var returnedSuspension = false

    override func didReceive(_ request: UNNotificationRequest, withContentHandler contentHandler: @escaping (UNNotificationContent) -> Void) {
        logger.debug("DEBUGGING: NotificationService.didReceive")
        let receivedNtf = if let ntf_ = request.content.mutableCopy() as? UNMutableNotificationContent { ntf_ } else { UNMutableNotificationContent() }
        setServiceBestAttemptNtf(receivedNtf)
        self.contentHandler = contentHandler
        registerGroupDefaults()
        let appState = appStateGroupDefault.get()
        logger.debug("NotificationService: app is \(appState.rawValue)")
        switch appState {
        case .stopped:
//            Use this block to debug notificaitons delivery in CLI, with "ejected" database and stopped chat
//            if let nrData = ntfRequestData(request) {
//                logger.debug("NotificationService get notification connections: /_ntf conns \(nrData.nonce) \(nrData.encNtfInfo)")
//                contentHandler(receivedNtf)
//                return;
//            }
            setBadgeCount()
            contentHandler(createAppStoppedNtf(badgeCount))
        case .suspended:
            setExpirationTimer()
            receiveNtfMessages(request)
        case .suspending:
            // while application is suspending, the current instance will be waiting
            setExpirationTimer()
            Task {
                let state: AppState = await withCheckedContinuation { cont in
                    // this subscriber uses message delivery via NSFileCoordinator to communicate between the app and NSE
                    appSubscriber = appStateSubscriber { s in
                        if s == .suspended { appSuspension(s) }
                    }
                    // this is a fallback timeout, in case message from the app does not arrive
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
                if state.inactive && self.contentHandler != nil {
                    receiveNtfMessages(request)
                } else {
                    contentHandler(receivedNtf)
                }
            }
        case .active: contentHandler(receivedNtf)
        case .activating: contentHandler(receivedNtf)
        case .bgRefresh: contentHandler(receivedNtf)
        }
    }

    // This timer compensates for the scenarios when serviceExtensionTimeWillExpire does not fire at all.
    // It is not clear why in some cases it does not fire, possibly it is a bug,
    // or it depends on what the current thread is doing at the moment.
    // If notification is not delivered and not cancelled, no further notifications will be processed.
    @inline(__always)
    private func setExpirationTimer() -> Void {
        DispatchQueue.main.asyncAfter(deadline: .now() + 30) {
            self.deliverBestAttemptNtf(urgent: true)
        }
    }

    @inline(__always)
    private func ntfRequestData(_ request: UNNotificationRequest) -> (nonce: String, encNtfInfo: String)? {
        if let ntfData = request.content.userInfo["notificationData"] as? [AnyHashable : Any],
           let nonce = ntfData["nonce"] as? String,
           let encNtfInfo = ntfData["message"] as? String {
            (nonce, encNtfInfo)
        } else {
            nil
        }
    }

    // This function triggers notification message delivery for connection entities referenced in the notification.
    // Notification may reference multiple connection entities (message queues) in order to compensate for Apple servers
    // only delivering the latest notification, so it allows receiving messages from up to 6 contacts and groups from a
    // single notification. This aggregation is handled by a notification server and is delivered via APNS servers in
    // e2e encrypted envelope, and the app core prevents duplicate processing by keeping track of the last processed message.

    // The process steps:
    // 0. apiGetConnNtfMessages or getConnNtfMessage get messages from the server for passed connection entities.
    // We don't know in advance which chat events will be delivered from app core for a given notification,
    // it may be a message, but it can also be contact request, various protocol confirmations, calls, etc.,
    // this function only returns metadata for the expected chat events.
    // This metadata is correlated with .ntfMessage core event / .msgInfo notification marker -
    // this marker allows determining when some message completed processing.
    // 1. receiveMessages: singleton loop receiving events from core.
    // 2. receivedMsgNtf: maps core events to notification events.
    // 3. NSEThreads.shared.processNotification: chooses which notification service instance in the current process should process notification.
    // While most of the time we observe that notifications are delivered sequentially, nothing in the documentation confirms it is sequential,
    // and from various sources it follows that each instance executes in its own thread, so concurrency is expected.
    // 4. processReceivedNtf: one of the instances of NSE processes notification event, deciding whether to request further messages
    // for a given connection entity (via getConnNtfMessage) or that the correct message was received and notification can be delivered (deliverBestAttemptNtf).
    // It is based on .msgInfo markers that indicate that message with a given timestamp was processed.
    // 5. deliverBestAttemptNtf: is called multiple times, once each connection receives enough messages (based on .msgInfo marker).
    // If further messages are expected, this function does nothing (unless it is called with urgent flag from timeout/expiration handlers).
    func receiveNtfMessages(_ request: UNNotificationRequest) {
        logger.debug("NotificationService: receiveNtfMessages")
        if case .documents = dbContainerGroupDefault.get() {
            deliverBestAttemptNtf()
            return
        }
        if let nrData = ntfRequestData(request),
           // Check that the app is still inactive before starting the core.
           appStateGroupDefault.get().inactive {
            // thread is added to activeThreads tracking set here - if thread started chat it needs to be suspended
            guard let t = threadId else { return }
            NSEThreads.shared.startThread(t, self)
            let dbStatus = startChat()
            // If database is opened successfully, get the list of connection entities (group members, contacts)
            // that are referenced in the encrypted notification metadata.
            if case .ok = dbStatus,
               let ntfConns = apiGetNtfConns(nonce: nrData.nonce, encNtfInfo: nrData.encNtfInfo) {
                logger.debug("NotificationService: receiveNtfMessages: apiGetNtfConns ntfConns count = \(ntfConns.count)")
                // uncomment localDisplayName in ConnectionEntity
                // logger.debug("NotificationService: receiveNtfMessages: apiGetNtfConns ntfConns \(String(describing: ntfConns.map { $0.connEntity.localDisplayName }))")

                // Prepare expected messages - they will be delivered to the reception loop in this chain:
                // They are atomically added to the instance notificationEntities inside msgReqs loop, to avoid any race conditions.
                let ntfEntities = ntfConns.compactMap(mkNotificationEntity)

                // collect notification message requests for all connection entities
                let msgReqs: [(chatId: String, connMsgReq: ConnMsgReq)] = ntfEntities.compactMap { ntfEntity -> (chatId: String, connMsgReq: ConnMsgReq)? in
                    // No need to request messages for connection entities that are "ready",
                    // e.g. for muted users or when the message is not expected based on notification.
                    let id = ntfEntity.entityId
                    if let expectedMsg = ntfEntity.expectedMsg {
                        if NSEThreads.shared.startEntity(self, ntfEntity) { // atomically checks and adds ntfEntity to NSE
                            // process any notifications "postponed" by the previous instance
                            let completed = processDroppedNotifications(ntfEntity, expectedMsg)
                            return if !completed, let connMsgReq = notificationEntities[id]?.connMsgReq {
                                (id, connMsgReq)
                            } else {
                                nil
                            }
                        } else {
                            // wait for another instance processing the same connection entity
                            logger.debug("NotificationService thread \(t, privacy: .private): receiveNtfMessages: entity \(id, privacy: .private) waiting on semaphore")
                            // this semaphore will be released by signalNextThread function, that looks up the instance
                            // waiting for the connection entity via activeThreads in NSEThreads
                            notificationEntities[id]?.semaphore.wait()
                            logger.debug("NotificationService thread \(t, privacy: .private): receiveNtfMessages: entity \(id, privacy: .private) proceeding after semaphore")
                            Task {
                                // process any notifications "postponed" by the previous instance
                                let completed = processDroppedNotifications(ntfEntity, expectedMsg)
                                // Request messages from the server for this connection entity.
                                // It triggers event delivery to receiveMessages loop (see above).
                                if !completed, let connMsgReq = notificationEntities[id]?.connMsgReq,
                                   let rcvMsg = getConnNtfMessage(connMsgReq: connMsgReq),
                                   rcvMsg.noMsg {
                                    // if server returns error or "no message", deliver what we have for this connection entity.
                                    finalizeEntity(id) // also releases any waiting threads for this entity
                                }
                            }
                            return nil
                        }
                    } else { // no expected message
                        notificationEntities[id] = ntfEntity
                        return nil
                    }
                }

                // Request messages for all connection entities that were not used by other instances.
                // It triggers event delivery to receiveMessages loop (see above).
                if !msgReqs.isEmpty,
                   let rcvMsgs = apiGetConnNtfMessages(connMsgReqs: msgReqs.map { $0.connMsgReq }) {
                    for i in 0 ..< min(msgReqs.count, rcvMsgs.count) { // a sanity check, API always returns the same size
                        if rcvMsgs[i].noMsg {
                            // mark entity as ready if there are no message on the server (or on error)
                            finalizeEntity(msgReqs[i].chatId)
                        }
                    }
                }
            } else if let dbStatus = dbStatus {
                setServiceBestAttemptNtf(createErrorNtf(dbStatus, badgeCount))
            }
        }
        // try to deliver the best attempt before exiting
        deliverBestAttemptNtf()
    }

    @inline(__always)
    func mkNotificationEntity(ntfConn: NtfConn) -> NotificationEntity? {
        if let rcvEntityId = ntfConn.connEntity.id {
            // don't receive messages for muted user profile
            let expectedMsg: NtfMsgInfo? = if ntfConn.user.showNotifications { ntfConn.expectedMsg_ } else { nil }
            return NotificationEntity(
                ntfConn: ntfConn,
                entityId: rcvEntityId,
                expectedMsg: expectedMsg,
                msgBestAttemptNtf: defaultBestAttemptNtf(ntfConn)
            )
        }
        return nil
    }

    // Processes notifications received and postponed by the previous NSE instance
    func processDroppedNotifications(_ ntfEntity: NotificationEntity, _ expectedMsg: NtfMsgInfo) -> Bool {
        var completed = false
        while !completed {
            if let dropped = NSEThreads.shared.takeDroppedNtf(ntfEntity) {
                completed = processReceivedNtf(ntfEntity, expectedMsg, dropped.ntf)
            } else {
                break
            }
        }
        if completed {
            finalizeEntity(ntfEntity.entityId)
        } else {
            notificationEntities[ntfEntity.entityId]?.shouldProcessNtf = true
        }
        return completed
    }

    override func serviceExtensionTimeWillExpire() {
        logger.debug("DEBUGGING: NotificationService.serviceExtensionTimeWillExpire")
        deliverBestAttemptNtf(urgent: true)
    }

    @inline(__always)
    var expectingMoreMessages: Bool {
        notificationEntities.contains { $0.value.expectedMsg != nil }
    }

    // processReceivedNtf returns "completed" - true when no more messages for the passed entity should be processed by the current NSE instance.
    // This is used to call finalizeEntity(id) and by processDroppedNotifications to decide if further processing is needed.
    func processReceivedNtf(_ ntfEntity: NotificationEntity, _ expectedMsg: NtfMsgInfo, _ ntf: NSENotificationData) -> Bool {
        let id = ntfEntity.entityId
        if case let .msgInfo(info) = ntf {
            if info.msgId == expectedMsg.msgId {
                // The message for this instance is processed, no more expected, deliver.
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): expected")
                return true
            } else if let msgTs = info.msgTs_, msgTs > expectedMsg.msgTs {
                // Otherwise check timestamp - if it is after the currently expected timestamp, preserve .msgInfo marker for the next instance.
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unexpected msgInfo, let other instance to process it, stopping this one")
                NSEThreads.shared.addDroppedNtf(id, ntf)
                return true
            } else if ntfEntity.allowedGetNextAttempts > 0, let connMsgReq = ntfEntity.connMsgReq {
                // Otherwise this instance expects more messages, and still has allowed attempts -
                // request more messages with getConnNtfMessage.
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unexpected msgInfo, get next message")
                notificationEntities[id]?.allowedGetNextAttempts -= 1
                let receivedMsg = getConnNtfMessage(connMsgReq: connMsgReq)
                if case let .info(msg) = receivedMsg, let msg {
                    // Server delivered message, it will be processed in the loop - see the comments in receiveNtfMessages.
                    logger.debug("NotificationService processNtf, on getConnNtfMessage: msgInfo msgId = \(info.msgId, privacy: .private), receivedMsg msgId = \(msg.msgId, privacy: .private)")
                    return false
                } else {
                    // Server reported no messages or error, deliver what we have.
                    logger.debug("NotificationService processNtf, on getConnNtfMessage: msgInfo msgId = \(info.msgId, privacy: .private): no next message, deliver best attempt")
                    return true
                }
            } else {
                // Current instance needs more messages, but ran out of attempts - deliver what we have.
                logger.debug("NotificationService processNtf: msgInfo msgId = \(info.msgId, privacy: .private): unknown message, let other instance to process it")
                return true
            }
        } else if ntfEntity.ntfConn.user.showNotifications {
            // This is the notification event for the user with enabled notifications.
            logger.debug("NotificationService processNtf: setting best attempt")
            if ntf.notificationEvent != nil {
                setBadgeCount()
            }
            // If previous "best attempt" is not a call, or if the current notification is a call, replace best attempt.
            // NOTE: we are delaying it until notification marker to make sure we are not delivering stale calls that can't be connected.
            // A better logic could be to check whether we have a call in the best attempt while processing .msgInfo marker above.
            // If the best attempt is a call, and its marker is received, and the call is recent (e.g., the last 30 seconds), it would deliver at once,
            // instead of requesting further messages.
            if ntfEntity.msgBestAttemptNtf.callInvitation == nil || ntf.callInvitation != nil {
                notificationEntities[id]?.msgBestAttemptNtf = ntf
            } // otherwise keep call as best attempt
            return false
        } else {
            // We should not get to this branch, as notifications are not delivered for muted users.
            return true
        }
    }

    func finalizeEntity(_ entityId: ChatId) {
        if let t = threadId { logger.debug("NotificationService thread \(t): entityReady: entity \(entityId)") }
        NSEThreads.shared.signalNextThread(self, entityId)
        deliverBestAttemptNtf()
    }

    func setBadgeCount() {
        badgeCount = ntfBadgeCountGroupDefault.get() + 1
        ntfBadgeCountGroupDefault.set(badgeCount)
    }

    @inline(__always)
    func setServiceBestAttemptNtf(_ ntf: UNMutableNotificationContent) {
        logger.debug("NotificationService.setServiceBestAttemptNtf")
        serviceBestAttemptNtf = ntf
    }

    private func deliverBestAttemptNtf(urgent: Bool = false) {
        logger.debug("NotificationService.deliverBestAttemptNtf urgent: \(urgent) expectingMoreMessages: \(self.expectingMoreMessages)")
        if let handler = contentHandler, urgent || !expectingMoreMessages {
            if urgent {
                contentHandler = nil
            }
            logger.debug("NotificationService.deliverBestAttemptNtf")
            // stop processing other messages
            for (key, _) in notificationEntities {
                notificationEntities[key]?.shouldProcessNtf = false
            }

            let suspend: Bool
            if let t = threadId {
                threadId = nil
                suspend = NSEThreads.shared.endThread(t) && NSEThreads.shared.noThreads
            } else {
                suspend = false
            }
            deliverCallkitOrNotification(urgent: urgent, suspend: suspend, handler: handler)
        }
    }

    @inline(__always)
    private func deliverCallkitOrNotification(urgent: Bool, suspend: Bool = false, handler: @escaping (UNNotificationContent) -> Void) {
        let callInv = notificationEntities.lazy.compactMap({ $0.value.msgBestAttemptNtf.callInvitation }).first
        if callInv != nil && useCallKit() {
            logger.debug("NotificationService.deliverCallkitOrNotification: will suspend, callkit")
            // suspending NSE even though there may be other notifications
            // to allow the app to process callkit call
            if urgent {
                suspendChat(0)
                deliverNotification(handler, callInv)
            } else {
                // when not "urgent", suspending NSE with delay and delivering after the suspension
                // because pushkit notification must be processed without delay
                // to avoid app termination.
                DispatchQueue.global().asyncAfter(deadline: .now() + fastNSESuspendSchedule.delay) {
                    suspendChat(fastNSESuspendSchedule.timeout)
                    DispatchQueue.global().asyncAfter(deadline: .now() + Double(fastNSESuspendSchedule.timeout)) {
                        self.deliverNotification(handler, callInv)
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
            deliverNotification(handler, callInv)
        }
    }

    private func deliverNotification(_ handler: @escaping (UNNotificationContent) -> Void, _ callInv: RcvCallInvitation?) {
        if let serviceNtf = serviceBestAttemptNtf {
            serviceBestAttemptNtf = nil
            contentHandler = nil
            if let callInv {
                if useCallKit() {
                    logger.debug("NotificationService reportNewIncomingVoIPPushPayload for \(callInv.contact.id)")
                    CXProvider.reportNewIncomingVoIPPushPayload([
                        "displayName": callInv.contact.displayName,
                        "contactId": callInv.contact.id,
                        "callUUID": callInv.callUUID ?? "",
                        "media": callInv.callType.media.rawValue,
                        "callTs": callInv.callTs.timeIntervalSince1970
                    ]) { error in
                        logger.debug("reportNewIncomingVoIPPushPayload result: \(error)")
                        handler(error == nil ? UNMutableNotificationContent() : createCallInvitationNtf(callInv, self.badgeCount))
                    }
                } else {
                    handler(createCallInvitationNtf(callInv, badgeCount))
                }
            } else if notificationEntities.isEmpty {
                handler(serviceNtf)
            } else {
                handler(prepareNotification())
            }
        }
    }

    @inline(__always)
    private func prepareNotification() -> UNMutableNotificationContent {
        // uncomment localDisplayName in ConnectionEntity
        // let conns = self.notificationEntities.compactMap { $0.value.ntfConn.connEntity.localDisplayName }
        // logger.debug("NotificationService prepareNotification for \(String(describing: conns))")
        let ntfs = notificationEntities.compactMap { $0.value.msgBestAttemptNtf.notificationEvent }
        let newMsgNtfs = ntfs.compactMap({ $0.newMsgNtf })
        let useNtfs = if newMsgNtfs.isEmpty { ntfs } else { newMsgNtfs }
        return createNtf(useNtfs)
        
        func createNtf(_ ntfs: [NSENotificationData]) -> UNMutableNotificationContent {
            logger.debug("NotificationService prepareNotification: \(ntfs.count) events")
            return switch ntfs.count {
            case 0: UNMutableNotificationContent() // used to mute notifications that did not unsubscribe yet
            case 1: ntfs[0].notificationContent(badgeCount)
            default: createJointNtf(ntfs)
            }
        }
    }

    // NOTE: this can be improved when there are two or more connection entity events when no messages were delivered.
    // Possibly, it is better to postpone this improvement until message priority is added to prevent notifications in muted groups,
    // unless it is a mention, a reply or some other high priority message marked for notification delivery.
    @inline(__always)
    private func createJointNtf(_ ntfs: [NSENotificationData]) -> UNMutableNotificationContent {
        let previewMode = ntfPreviewModeGroupDefault.get()
        logger.debug("NotificationService.createJointNtf ntfs: \(ntfs.count)")
        let (userId, chatsNames) = newMsgsChatsNames(ntfs)
        if !chatsNames.isEmpty, let userId {
            let body = if previewMode == .hidden {
                String.localizedStringWithFormat(NSLocalizedString("From %d chat(s)", comment: "notification body"), chatsNames.count)
            } else {
                String.localizedStringWithFormat(NSLocalizedString("From: %@", comment: "notification body"), newMsgsChatsNamesStr(chatsNames))
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
                body: String.localizedStringWithFormat(NSLocalizedString("%d new events", comment: "notification body"), ntfs.count),
                badgeCount: badgeCount
            )
        }
    }

    @inline(__always)
    private func newMsgsChatsNames(_ ntfs: [NSENotificationData]) ->  (Int64?, [String]) {
        var seenChatIds = Set<ChatId>()
        var chatsNames: [String] = []
        var userId: Int64?
        for ntf in ntfs {
            switch ntf {
            case let .messageReceived(user, chat, _):
                if seenChatIds.isEmpty { userId = user.userId }
                if !seenChatIds.contains(chat.id) {
                    seenChatIds.insert(chat.id)
                    chatsNames.append(chat.chatViewName)
                }
            default: ()
            }
        }
        return (userId, chatsNames)
    }

    @inline(__always)
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

    @inline(__always)
    var value: NSEState { value_ }

    func set(_ state: NSEState) {
        nseStateGroupDefault.set(state)
        sendNSEState(state)
        value_ = state
    }

    private init() {
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
        logger.debug("NotificationService active user \(user.displayName)")
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
        switch await chatRecvMsg() {
        case let .result(msg):
            logger.debug("NotificationService receiveMsg: message")
            if let (id, ntf) = await receivedMsgNtf(msg) {
                logger.debug("NotificationService receiveMsg: notification")
                await NSEThreads.shared.processNotification(id, ntf)
            }
        case let .error(err):
            logger.error("NotificationService receivedMsgNtf error: \(String(describing: err))")
        case let .invalid(type, _):
            logger.error("NotificationService receivedMsgNtf invalid: \(type)")
        case .none: ()
        }
    }

    func delayWhenInactive() async {
        logger.debug("NotificationService delayWhenInactive")
        _ = try? await Task.sleep(nanoseconds: 1000_000000)
    }
}

func chatRecvMsg() async -> APIResult<NSEChatEvent>? {
    await withCheckedContinuation { cont in
        let resp: APIResult<NSEChatEvent>? = recvSimpleXMsg()
        cont.resume(returning: resp)
    }
}

private let isInChina = SKStorefront().countryCode == "CHN"

@inline(__always)
private func useCallKit() -> Bool { !isInChina && callKitEnabledGroupDefault.get() }

@inline(__always)
func receivedMsgNtf(_ res: NSEChatEvent) async -> (String, NSENotificationData)? {
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
            let ntf: NSENotificationData = (cInfo.ntfsEnabled(chatItem: cItem) && cItem.showNotification) ? .messageReceived(user, cInfo, cItem) : .noNtf
            let chatIdOrMemberId = if case let .groupRcv(groupMember) = chatItem.chatItem.chatDir {
                groupMember.id
            } else {
                chatItem.chatInfo.id
            }
            return (chatIdOrMemberId, ntf)
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
    }
}

@inline(__always)
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
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.showActiveUser)
    logger.debug("apiGetActiveUser sendSimpleXCmd response: \(r.responseType)")
    switch r {
    case let .result(.activeUser(user)): return user
    case .error(.error(.noActiveUser)):
        logger.debug("apiGetActiveUser sendSimpleXCmd no active user")
        return nil
    case let .error(err):
        logger.debug("apiGetActiveUser sendSimpleXCmd error: \(String(describing: err))")
        return nil
    default:
        logger.error("NotificationService apiGetActiveUser unexpected response: \(String(describing: r))")
        return nil
    }
}

func apiStartChat() throws -> Bool {
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.startChat(mainApp: false, enableSndFiles: false))
    switch r {
    case .result(.chatStarted): return true
    case .result(.chatRunning): return false
    default: throw r.unexpected
    }
}

func apiActivateChat() -> Bool {
    chatReopenStore()
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiActivateChat(restoreChat: false))
    if case .result(.cmdOk) = r { return true }
    logger.error("NotificationService apiActivateChat error: \(String(describing: r))")
    return false
}

func apiSuspendChat(timeoutMicroseconds: Int) -> Bool {
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiSuspendChat(timeoutMicroseconds: timeoutMicroseconds))
    if case .result(.cmdOk) = r { return true }
    logger.error("NotificationService apiSuspendChat error: \(String(describing: r))")
    return false
}

func apiSetAppFilePaths(filesFolder: String, tempFolder: String, assetsFolder: String) throws {
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiSetAppFilePaths(filesFolder: filesFolder, tempFolder: tempFolder, assetsFolder: assetsFolder))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiSetEncryptLocalFiles(_ enable: Bool) throws {
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiSetEncryptLocalFiles(enable: enable))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func apiGetNtfConns(nonce: String, encNtfInfo: String) -> [NtfConn]? {
    guard apiGetActiveUser() != nil else {
        logger.debug("NotificationService: no active user")
        return nil
    }
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiGetNtfConns(nonce: nonce, encNtfInfo: encNtfInfo))
    if case let .result(.ntfConns(ntfConns)) = r {
        logger.debug("NotificationService apiGetNtfConns response ntfConns: \(ntfConns.count) conections")
        return ntfConns
    } else if case let .error(error) = r {
        logger.debug("NotificationService apiGetNtfMessage error response: \(String.init(describing: error))")
    } else {
        logger.debug("NotificationService apiGetNtfMessage ignored response: \(r.responseType) \(String.init(describing: r))")
    }
    return nil
}

func apiGetConnNtfMessages(connMsgReqs: [ConnMsgReq]) -> [RcvNtfMsgInfo]? {
    guard apiGetActiveUser() != nil else {
        logger.debug("no active user")
        return nil
    }
//    logger.debug("NotificationService apiGetConnNtfMessages command: \(NSEChatCommand.apiGetConnNtfMessages(connMsgReqs: connMsgReqs).cmdString)")
    logger.debug("NotificationService apiGetConnNtfMessages requests: \(connMsgReqs.count)")
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiGetConnNtfMessages(connMsgReqs: connMsgReqs))
    if case let .result(.connNtfMessages(msgs)) = r {
//        logger.debug("NotificationService apiGetConnNtfMessages responses: \(String(describing: msgs))")
        logger.debug("NotificationService apiGetConnNtfMessages responses: total \(msgs.count), expecting messages \(msgs.count { !$0.noMsg }), errors \(msgs.count { $0.isError })")
        return msgs
    }
    logger.debug("NotificationService apiGetConnNtfMessages error: \(responseError(r.unexpected))")
    return nil
}

func getConnNtfMessage(connMsgReq: ConnMsgReq) -> RcvNtfMsgInfo? {
    let r = apiGetConnNtfMessages(connMsgReqs: [connMsgReq])
    return if let r, r.count > 0 { r[0] } else { nil }
}

func apiReceiveFile(fileId: Int64, encrypted: Bool, inline: Bool? = nil) -> AChatItem? {
    let userApprovedRelays = !privacyAskToApproveRelaysGroupDefault.get()
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.receiveFile(fileId: fileId, userApprovedRelays: userApprovedRelays, encrypted: encrypted, inline: inline))
    if case let .result(.rcvFileAccepted(_, chatItem)) = r { return chatItem }
    logger.error("receiveFile error: \(responseError(r.unexpected))")
    return nil
}

func apiSetFileToReceive(fileId: Int64, encrypted: Bool) {
    let userApprovedRelays = !privacyAskToApproveRelaysGroupDefault.get()
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.setFileToReceive(fileId: fileId, userApprovedRelays: userApprovedRelays, encrypted: encrypted))
    if case .result(.cmdOk) = r { return }
    logger.error("setFileToReceive error: \(responseError(r.unexpected))")
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
    let r: APIResult<NSEChatResponse> = sendSimpleXCmd(NSEChatCommand.apiSetNetworkConfig(networkConfig: cfg))
    if case .result(.cmdOk) = r { return }
    throw r.unexpected
}

func defaultBestAttemptNtf(_ ntfConn: NtfConn) -> NSENotificationData {
    let user = ntfConn.user
    let connEntity = ntfConn.connEntity
    return if !user.showNotifications {
        .noNtf
    } else {
        switch ntfConn.connEntity {
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
    }
}
