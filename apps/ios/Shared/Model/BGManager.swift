//
//  BGManager.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 08/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import BackgroundTasks
import SimpleXChat

private let receiveTaskId = "chat.simplex.app.receive"

// TCP timeout + 2 sec
private let waitForMessages: TimeInterval = 6

// This is the smallest interval between refreshes, and also target interval in "off" mode
private let bgRefreshInterval: TimeInterval = 600 // 10 minutes

// This intervals are used for background refresh in instant and periodic modes
private let periodicBgRefreshInterval: TimeInterval = 1200 // 20 minutes

private let maxBgRefreshInterval: TimeInterval = 2400 // 40 minutes

private let maxTimerCount = 9

class BGManager {
    static let shared = BGManager()
    var chatReceiver: ChatReceiver?
    var bgTimer: Timer?
    var completed = true
    var timerCount = 0

    func register() {
        logger.debug("BGManager.register")
        BGTaskScheduler.shared.register(forTaskWithIdentifier: receiveTaskId, using: nil) { task in
            self.handleRefresh(task as! BGAppRefreshTask)
        }
    }

    func schedule(interval: TimeInterval? = nil) {
        if !ChatModel.shared.ntfEnableLocal {
            logger.debug("BGManager.schedule: disabled")
            return
        }
        logger.debug("BGManager.schedule")
        let request = BGAppRefreshTaskRequest(identifier: receiveTaskId)
        request.earliestBeginDate = Date(timeIntervalSinceNow: interval ?? runInterval)
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            logger.error("BGManager.schedule error: \(error.localizedDescription)")
        }
    }

    var runInterval: TimeInterval {
        switch ChatModel.shared.notificationMode {
        case .instant: maxBgRefreshInterval
        case .periodic: periodicBgRefreshInterval
        case .off: bgRefreshInterval
        }
    }

    var lastRanLongAgo: Bool {
        Date.now.timeIntervalSince(chatLastBackgroundRunGroupDefault.get()) > runInterval
    }

    private func handleRefresh(_ task: BGAppRefreshTask) {
        if !ChatModel.shared.ntfEnableLocal {
            logger.debug("BGManager.handleRefresh: disabled")
            return
        }
        logger.debug("BGManager.handleRefresh")
        let shouldRun_ = lastRanLongAgo
        if allowBackgroundRefresh() && shouldRun_ {
            schedule()
            let completeRefresh = completionHandler {
                task.setTaskCompleted(success: true)
            }
            task.expirationHandler = { completeRefresh("expirationHandler") }
            receiveMessages(completeRefresh)
        } else {
            schedule(interval: shouldRun_ ? bgRefreshInterval : runInterval)
            logger.debug("BGManager.completionHandler: already active, not started")
            task.setTaskCompleted(success: true)
        }
    }

    func completionHandler(_ complete: @escaping () -> Void) -> ((String) -> Void) {
        { reason in
            logger.debug("BGManager.completionHandler: \(reason)")
            if !self.completed {
                self.completed = true
                self.chatReceiver?.stop()
                self.chatReceiver = nil
                self.bgTimer?.invalidate()
                self.bgTimer = nil
                self.timerCount = 0
                suspendBgRefresh()
                complete()
            }
        }
    }

    func receiveMessages(_ completeReceiving: @escaping (String) -> Void) {
        if (!self.completed) {
            logger.debug("BGManager.receiveMessages: in progress, exiting")
            return
        }
        self.completed = false
        DispatchQueue.main.async {
            chatLastBackgroundRunGroupDefault.set(Date.now)
            let m = ChatModel.shared
            if (!m.chatInitialized) {
                setAppState(.bgRefresh)
                do {
                    try initializeChat(start: true)
                } catch let error {
                    fatalError("Failed to start or load chats: \(responseError(error))")
                }
            }
            activateChat(appState: .bgRefresh)
            if m.currentUser == nil {
                completeReceiving("no current user")
                return
            }
            logger.debug("BGManager.receiveMessages: starting chat")
            let cr = ChatReceiver()
            self.chatReceiver = cr
            cr.start()
            RunLoop.current.add(Timer(timeInterval: 2, repeats: true) { timer in
                logger.debug("BGManager.receiveMessages: timer")
                self.bgTimer = timer
                self.timerCount += 1
                if cr.lastMsgTime.distance(to: Date.now) >= waitForMessages {
                    completeReceiving("timer (no messages after \(waitForMessages) seconds)")
                } else if self.timerCount >= maxTimerCount {
                    completeReceiving("timer (called \(maxTimerCount) times")
                }
            }, forMode: .default)
        }
    }
}
