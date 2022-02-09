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



class BGManager {
    var chatModel: ChatModel?

    static let shared = BGManager()

    func register() {
        print("*** BGManager register")
        BGTaskScheduler.shared.register(forTaskWithIdentifier: receiveTaskId, using: nil) { task in
            self.handler(task as! BGAppRefreshTask)
        }
    }

    func schedule(_ chatModel: ChatModel) {
        self.chatModel = chatModel
        print("*** BGManager schedule", Date.now.formatted())
        let request = BGAppRefreshTaskRequest(identifier: receiveTaskId)
        request.earliestBeginDate = Date(timeIntervalSinceNow: 10 * 60)
        do {
            try BGTaskScheduler.shared.submit(request)
        } catch {
            print("Cant schedule app refresh: \(error)")
        }
    }

    func handler(_ task: BGAppRefreshTask) {
        print("*** BGManager handler")
        guard let chatModel = self.chatModel else { return }
        print("*** BGManager handler has chatModel")
        schedule(chatModel)
        task.expirationHandler = {
            print("*** BGManager handler - in expiration handler")
            ChatReceiver.shared.stop()
            task.setTaskCompleted(success: true)
            print("*** BGManager handler completed in expiration handler")
        }
        DispatchQueue.main.async {
            initializeChat(chatModel)
            if chatModel.currentUser == nil {
                task.setTaskCompleted(success: true)
                return
            }
            print("*** BGManager handler - starting chat")
            ChatReceiver.shared.start(chatModel, bgTask: task)
            RunLoop.current.add(Timer(timeInterval: 20, repeats: false) { _ in
                print("*** BGManager handler - in timer")
                ChatReceiver.shared.stop()
                task.setTaskCompleted(success: true)
                print("*** BGManager handler completed on timer")
            }, forMode: .default)
        }
        print("*** BGManager handler - exiting function...")
        // TODO chat should send NO MORE MESSAGES event
        // task.setTaskCompleted(success: true)
    }
}
