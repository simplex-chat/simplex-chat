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

private func _suspendChat(timeout: Int) {
    // this is a redundant check to prevent logical errors, like the one fixed in this PR
    let state = appStateGroupDefault.get()
    if !state.canSuspend {
        logger.error("_suspendChat called, current state: \(state.rawValue, privacy: .public)")
    } else if ChatModel.ok {
        appStateGroupDefault.set(.suspending)
        apiSuspendChat(timeoutMicroseconds: timeout * 1000000)
        let endTask = beginBGTask(chatSuspended)
        DispatchQueue.global().asyncAfter(deadline: .now() + Double(timeout) + 1, execute: endTask)
    } else {
        appStateGroupDefault.set(.suspended)
    }
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

private var terminating = false

func terminateChat() {
    logger.debug("terminateChat")
    suspendLockQueue.sync {
        switch appStateGroupDefault.get() {
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
            terminating = true
            // the store will be closed in _chatSuspended when event is received
            _suspendChat(timeout: terminationTimeout)
        }
    }
}

func chatSuspended() {
    suspendLockQueue.sync {
        if case .suspending = appStateGroupDefault.get() {
            _chatSuspended()
        }
    }
}

private func _chatSuspended() {
    logger.debug("_chatSuspended")
    appStateGroupDefault.set(.suspended)
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.stop()
    }
    if terminating {
         chatCloseStore()
    }
}

func activateChat(appState: AppState = .active) {
    logger.debug("DEBUGGING: activateChat")
    terminating = false
    suspendLockQueue.sync {
        appStateGroupDefault.set(appState)
        if ChatModel.ok { apiActivateChat() }
        logger.debug("DEBUGGING: activateChat: after apiActivateChat")
    }
}

func initChatAndMigrate(refreshInvitations: Bool = true) {
    terminating = false
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

func startChatAndActivate() {
    terminating = false
    logger.debug("DEBUGGING: startChatAndActivate")
    if ChatModel.shared.chatRunning == true {
        ChatReceiver.shared.start()
        logger.debug("DEBUGGING: startChatAndActivate: after ChatReceiver.shared.start")
    }
    if .active != appStateGroupDefault.get()  {
        logger.debug("DEBUGGING: startChatAndActivate: before activateChat")
        activateChat()
        logger.debug("DEBUGGING: startChatAndActivate: after activateChat")
    }
}
