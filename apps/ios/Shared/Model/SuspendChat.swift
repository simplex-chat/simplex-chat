//
//  SuspendChat.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
import SimpleXChat

private let suspendLockQueue = DispatchQueue(label: "chat.simplex.app.suspend.lock")

let appSuspendTimeout: Int = 15 // seconds

let bgSuspendTimeout: Int = 5 // seconds

private func _suspendChat(timeout: Int) {
    appStateGroupDefault.set(.suspending)
    apiSuspendChat(timeoutMicroseconds: timeout * 1000000)
    let endTask = beginBGTask(chatSuspended)
    DispatchQueue.global().asyncAfter(deadline: .now() + Double(timeout) + 1, execute: endTask)
}

func suspendChat() {
    suspendLockQueue.sync {
        _suspendChat(timeout: appSuspendTimeout)
    }
}

func suspendBgRefresh() {
    suspendLockQueue.sync {
        if case .bgRefresh = appStateGroupDefault.get()  {
            _suspendChat(timeout: bgSuspendTimeout)
        }
    }
}

func chatSuspended() {
    suspendLockQueue.sync {
        if case .suspending = appStateGroupDefault.get() {
            logger.debug("chatSuspended")
            appStateGroupDefault.set(.suspended)
            if ChatModel.shared.chatRunning == true {
                ChatReceiver.shared.stop()
            }
        }
    }
}

func activateChat(appState: AppState = .active) {
    suspendLockQueue.sync {
        appStateGroupDefault.set(appState)
        apiActivateChat()
    }
}
