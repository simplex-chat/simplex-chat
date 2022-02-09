//
//  BGManager.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 08/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import BackgroundTasks

private let receiveTaskId = "chat.simplex.app.receive"

// TCP timeout + 2 sec
private let waitForMessages: TimeInterval = 6

class BGManager {
    private var bgTimer: Timer?

    static let shared = BGManager()

    func register() {
        logger.debug("BGManager.register")
        BGTaskScheduler.shared.register(forTaskWithIdentifier: receiveTaskId, using: nil) { task in
            self.handleRefresh(RefreshTask(task as! BGAppRefreshTask))
        }
    }

    func schedule() {
        logger.debug("BGManager.schedule")
        let request = BGAppRefreshTaskRequest(identifier: receiveTaskId)
        request.earliestBeginDate = Date(timeIntervalSinceNow: 10 * 60)
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            logger.error("BGManager.schedule error: \(error.localizedDescription)")
        }
    }

    private func handleRefresh(_ task: RefreshTask) {
        logger.debug("BGManager.handleRefresh")
        schedule()
        task.expirationHandler = {
            logger.debug("BGManager.handleRefresh expirationHandler")
            ChatReceiver.shared.stop()
            task.setTaskCompleted(success: true)
        }
        DispatchQueue.main.async {
            initializeChat()
            if ChatModel.shared.currentUser == nil {
                task.setTaskCompleted(success: true)
                return
            }
            logger.debug("BGManager.handleRefresh: starting chat")
            ChatReceiver.shared.start(bgTask: task)
            RunLoop.current.add(Timer(timeInterval: 2, repeats: true) { timer in
                self.bgTimer = timer
                logger.debug("BGManager.handleRefresh: timer")
                if ChatReceiver.shared.lastMsgTime.distance(to: Date.now) >= waitForMessages {
                    logger.debug("BGManager.handleRefresh: timer: stopping")
                    ChatReceiver.shared.stop()
                    task.setTaskCompleted(success: true)
                    timer.invalidate()
                    self.bgTimer = nil
                }
            }, forMode: .default)
        }
    }

    func invalidateStopTimer() {
        logger.debug("BGManager.invalidateStopTimer?")
        if let timer = bgTimer {
            timer.invalidate()
            bgTimer = nil
            logger.debug("BGManager.invalidateStopTimer: done")
        }
    }

    class RefreshTask {
        private let task: BGAppRefreshTask
        var completed = false

        internal init(_ task: BGAppRefreshTask) {
            self.task = task
        }

        var expirationHandler: (() -> Void)? {
            set { task.expirationHandler = newValue }
            get { task.expirationHandler }
        }

        func setTaskCompleted(success: Bool) {
            if !completed { task.setTaskCompleted(success: success) }
            completed = true
        }
    }
}
