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
import SwiftUI

private let suspendLockQueue = DispatchQueue(label: "chat.simplex.app.suspend.lock")

let bgSuspendTimeout: Int = 5 // seconds

let terminationTimeout: Int = 3 // seconds

let activationDelay: TimeInterval = 1.5

let nseSuspendTimeout: TimeInterval = 5

private func _suspendChat(timeout: Int) {
    // this is a redundant check to prevent logical errors, like the one fixed in this PR
    let state = AppChatState.shared.value
    if !state.canSuspend {
        logger.error("_suspendChat called, current state: \(state.rawValue)")
    } else if ChatModel.ok {
        AppChatState.shared.set(.suspending)
        apiSuspendChat(timeoutMicroseconds: timeout * 1000000)
        let endTask = beginBGTask(chatSuspended)
        DispatchQueue.global().asyncAfter(deadline: .now() + Double(timeout) + 1, execute: endTask)
    } else {
        AppChatState.shared.set(.suspended)
    }
}

let seSubscriber = seMessageSubscriber {
    switch $0 {
    case let .state(state):
        switch state {
        case .inactive:
            if AppChatState.shared.value.inactive { activateChat() }
        case .sendingMessage:
            if AppChatState.shared.value.canSuspend { suspendChat() }
        }
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
        m.v3DBMigration = v3DBMigrationDefault.get()
        if AppChatState.shared.value == .stopped && storeDBPassphraseGroupDefault.get() && kcDatabasePassword.get() != nil {
            initialize(start: true, confirmStart: true)
        } else {
            initialize(start: true)
        }
    }

    func initialize(start: Bool, confirmStart: Bool = false) {
        do {
            try initializeChat(start: m.v3DBMigration.startChat && start, confirmStart: m.v3DBMigration.startChat && confirmStart, refreshInvitations: refreshInvitations)
        } catch let error {
            AlertManager.shared.showAlertMsg(
                title: start ? "Error starting chat" : "Error opening chat",
                message: "Please contact developers.\nError: \(responseError(error))"
            )
        }
    }
}

func startChatForCall() {
    logger.debug("DEBUGGING: startChatForCall")
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.start()
        logger.debug("DEBUGGING: startChatForCall: after ChatReceiver.shared.start")
    }
    if .active != AppChatState.shared.value {
        logger.debug("DEBUGGING: startChatForCall: before activateChat")
        activateChat()
        logger.debug("DEBUGGING: startChatForCall: after activateChat")
    }
}

func startChatAndActivate(_ completion: @escaping () -> Void) {
    logger.debug("DEBUGGING: startChatAndActivate")
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.start()
        logger.debug("DEBUGGING: startChatAndActivate: after ChatReceiver.shared.start")
    }
    if case .active = AppChatState.shared.value {
        completion()
    } else if nseStateGroupDefault.get().inactive {
        activate()
    } else {
        // setting app state to "activating" to notify NSE that it should suspend
        setAppState(.activating)
        waitNSESuspended(timeout: nseSuspendTimeout) { ok in
            if !ok {
                // if for some reason NSE failed to suspend,
                // e.g., it crashed previously without setting its state to "suspended",
                // set it to "suspended" state anyway, so that next time app
                // does not have to wait when activating.
                nseStateGroupDefault.set(.suspended)
            }
            if AppChatState.shared.value == .activating {
                activate()
            }
        }
    }

    func activate() {
        logger.debug("DEBUGGING: startChatAndActivate: before activateChat")
        activateChat()
        completion()
        logger.debug("DEBUGGING: startChatAndActivate: after activateChat")
    }
}

// appStateGroupDefault must not be used in the app directly, only via this singleton
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
