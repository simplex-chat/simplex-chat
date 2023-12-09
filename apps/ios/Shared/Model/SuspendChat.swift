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

let terminationTimeout: Int = 3 // seconds

let activationDelay: Double = 1.5 // seconds

private func _suspendChat(timeout: Int) {
    // this is a redundant check to prevent logical errors, like the one fixed in this PR
    let state = AppChatState.shared.value
    if !state.canSuspend {
        logger.error("_suspendChat called, current state: \(state.rawValue, privacy: .public)")
    } else if ChatModel.ok {
        AppChatState.shared.set(.suspending)
        apiSuspendChat(timeoutMicroseconds: timeout * 1000000)
        let endTask = beginBGTask(chatSuspended)
        DispatchQueue.global().asyncAfter(deadline: .now() + Double(timeout) + 1, execute: endTask)
    } else {
        AppChatState.shared.set(.suspended)
    }
}

func suspendChat() {
    suspendLockQueue.sync {
        _suspendChat(timeout: appSuspendTimeout)
    }
}

func suspendBgRefresh() {
    suspendLockQueue.sync {
        if case .bgRefresh = AppChatState.shared.value  {
            _suspendChat(timeout: bgSuspendTimeout)
        }
    }
}

func terminateChat() {
    logger.debug("terminateChat")
    suspendLockQueue.sync {
        switch AppChatState.shared.value {
        case .suspending:
            // suspend instantly if already suspending
            _chatSuspended()
            // when apiSuspendChat is called with timeout 0, it won't send any events on suspension
            if ChatModel.ok { apiSuspendChat(timeoutMicroseconds: 0) }
            chatCloseStore()
        case .suspended:
            chatCloseStore()
        case .stopped:
            chatCloseStore()
        default:
            // the store will be closed in _chatSuspended when event is received
            _suspendChat(timeout: terminationTimeout)
        }
    }
}

func chatSuspended() {
    suspendLockQueue.sync {
        if case .suspending = AppChatState.shared.value {
            _chatSuspended()
        }
    }
}

private func _chatSuspended() {
    logger.debug("_chatSuspended")
    AppChatState.shared.set(.suspended)
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.stop()
    }
    chatCloseStore()
}

func setAppState(_ appState: AppState) {
    suspendLockQueue.sync {
        AppChatState.shared.set(appState)
    }
}

func activateChat(appState: AppState = .active) {
    logger.debug("DEBUGGING: activateChat")
    suspendLockQueue.sync {
        AppChatState.shared.set(appState)
        if ChatModel.ok { apiActivateChat() }
        logger.debug("DEBUGGING: activateChat: after apiActivateChat")
    }
}

func initChatAndMigrate(refreshInvitations: Bool = true) {
    let m = ChatModel.shared
    if (!m.chatInitialized) {
        do {
            m.v3DBMigration = v3DBMigrationDefault.get()
            try initializeChat(start: m.v3DBMigration.startChat, refreshInvitations: refreshInvitations)
        } catch let error {
            fatalError("Failed to start or load chats: \(responseError(error))")
        }
    }
}

func startChatAndActivate(dispatchQueue: DispatchQueue = DispatchQueue.main, _ completion: @escaping () -> Void) {
    logger.debug("DEBUGGING: startChatAndActivate")
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.start()
        logger.debug("DEBUGGING: startChatAndActivate: after ChatReceiver.shared.start")
    }
    if .active == AppChatState.shared.value {
        completion()
    } else if nseStateGroupDefault.get().inactive {
        activate()
    } else {
        suspendLockQueue.sync {
            AppChatState.shared.set(.activating)
        }
        waitNSESuspended(timeout: 10, dispatchQueue: dispatchQueue) { ok in
            if AppChatState.shared.value == .activating {
                activate()
            }
        }
        // TODO can be replaced with Mach messenger to notify the NSE to terminate and continue after reply, with timeout
//        dispatchQueue.asyncAfter(deadline: .now() + activationDelay) {
//            if AppChatState.shared.value == .activating {
//                activate()
//            }
//        }
    }

    func activate() {
        logger.debug("DEBUGGING: startChatAndActivate: before activateChat")
        activateChat()
        completion()
        logger.debug("DEBUGGING: startChatAndActivate: after activateChat")
    }
}

class AppChatState {
    static let shared = AppChatState()
    private var value_ = appStateGroupDefault.get()

    var value: AppState {
        value_
    }

    func set(_ state: AppState) {
        appStateGroupDefault.set(state)
        sendAppState(state)
        value_ = state
    }
}
