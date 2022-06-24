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

private let bgRefreshInterval: TimeInterval = 450

class BGManager {
    static let shared = BGManager()
    var chatReceiver: ChatReceiver?
    var bgTimer: Timer?
    var completed = true

    func register() {
        logger.debug("BGManager.register")
        BGTaskScheduler.shared.register(forTaskWithIdentifier: receiveTaskId, using: nil) { task in
            self.handleRefresh(task as! BGAppRefreshTask)
        }
    }

    func schedule() {
        logger.debug("BGManager.schedule")
        let request = BGAppRefreshTaskRequest(identifier: receiveTaskId)
        request.earliestBeginDate = Date(timeIntervalSinceNow: bgRefreshInterval)
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            logger.error("BGManager.schedule error: \(error.localizedDescription)")
        }
    }

    private func handleRefresh(_ task: BGAppRefreshTask) {
        logger.debug("BGManager.handleRefresh")
        schedule()
        let completeRefresh = completionHandler {
            task.setTaskCompleted(success: true)
        }
        task.expirationHandler = { completeRefresh("expirationHandler") }
        receiveMessages(completeRefresh)
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
            do {
                try initializeChat(start: true)
            } catch let error {
                fatalError("Failed to start or load chats: \(responseError(error))")
            }
            if ChatModel.shared.currentUser == nil {
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
                if cr.lastMsgTime.distance(to: Date.now) >= waitForMessages {
                    completeReceiving("timer (no messages after \(waitForMessages) seconds)")
                }
            }, forMode: .default)
        }
    }
}
