//
//  SuspendChat.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
@preconcurrency import BackgroundTasks
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

@MainActor
func suspendChatForBackground() {
    if CallController.useCallKit() && ChatModel.shared.activeCall != nil {
        CallController.shared.shouldSuspendChat = true
    } else {
        if AppChatState.shared.value.canSuspend {
            suspendChat()
        }
        BGManager.shared.schedule()
    }
}

private let remoteCtrlKeepAliveTaskId = "chat.simplex.app.remote-control.keepalive.session"

private func remoteCtrlConnectedSubtitle(_ seconds: Int64) -> String {
    let duration = durationText(Int(seconds))
    return String.localizedStringWithFormat(
        NSLocalizedString("Connected for %@", comment: "continued background activity duration"),
        seconds < 3600 ? "00:\(duration)" : duration
    )
}

@MainActor
final class RemoteCtrlBGKeepAlive {
    static let shared = RemoteCtrlBGKeepAlive()

    private var registered = false
    private var continuedTask: BGTask?
    private var continuedProgressTask: Task<Void, Never>?
    private var remoteCtrlElapsedSecondsProvider: (() -> Int64)?
    private var legacyTask: UIBackgroundTaskIdentifier = .invalid
    private var legacyTaskToken: UUID?
    private var sessionTerminationInProgress = false
    private var deferredRemoteStoppedSession: RemoteCtrlSession?
    private(set) var continuedProcessingAccepted = false

    private init() {}

    @available(iOS 26.0, *)
    func startContinuedProcessing() {
        let clock = ContinuousClock()
        let connectedAt = clock.now
        guard ChatModel.shared.activeRemoteCtrl, registerContinuedProcessing() else { return }

        let request = BGContinuedProcessingTaskRequest(
            identifier: remoteCtrlKeepAliveTaskId,
            title: NSLocalizedString("Connected to desktop", comment: "continued background activity title"),
            subtitle: remoteCtrlConnectedSubtitle(0)
        )
        request.strategy = .fail
        do {
            try BGTaskScheduler.shared.submit(request)
            remoteCtrlElapsedSecondsProvider = {
                connectedAt.duration(to: clock.now).components.seconds
            }
            continuedProcessingAccepted = true
        } catch {
            logger.error("RemoteCtrlBGKeepAlive.submit error: \(error.localizedDescription)")
        }
    }

    func handleAppBackgrounding() {
        if let session = deferredRemoteStoppedSession {
            // Restore locally before the scene path suspends chat if backgrounding
            // happens during the remote-stop recovery delay.
            deferredRemoteStoppedSession = nil
            completeSessionTermination(session, success: true, suspendForBackground: true)
        } else if !keepSessionInBackground() {
            suspendChatForBackground()
        }
    }

    private func keepSessionInBackground() -> Bool {
        guard ChatModel.shared.activeRemoteCtrl || sessionTerminationInProgress else { return false }
        if continuedTask != nil { return true }
        if legacyTask == .invalid {
            let token = UUID()
            legacyTaskToken = token
            legacyTask = UIApplication.shared.beginBackgroundTask {
                Task { @MainActor in
                    await RemoteCtrlBGKeepAlive.shared.expireLegacyTask(token)
                }
            }
            if legacyTask == .invalid {
                legacyTaskToken = nil
                finish(success: false)
            }
        }
        return legacyTask != .invalid
    }

    func stopLegacyTask() {
        guard legacyTask != .invalid else { return }
        legacyTaskToken = nil
        UIApplication.shared.endBackgroundTask(legacyTask)
        legacyTask = .invalid
    }

    func disconnectRemoteCtrl() async throws {
        guard !sessionTerminationInProgress else { return }
        sessionTerminationInProgress = true
        let session = ChatModel.shared.remoteCtrlSession
        do {
            try await stopRemoteCtrl()
        } catch {
            if ChatModel.shared.remoteCtrlSession != nil {
                sessionTerminationInProgress = false
                throw error
            }
        }
        let suspendForBackground = UIApplication.shared.applicationState == .background
        completeSessionTermination(session, success: true, suspendForBackground: suspendForBackground)
    }

    func handleRemoteCtrlStopped(_ session: RemoteCtrlSession) {
        ChatModel.shared.remoteCtrlSession = nil
        guard !sessionTerminationInProgress else { return }
        sessionTerminationInProgress = true
        if case .connected = session.sessionState {
            // This delay is needed to cancel the session that fails on network failure,
            // e.g. when user did not grant permission to access local network yet.
            deferredRemoteStoppedSession = session
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                guard let session = RemoteCtrlBGKeepAlive.shared.deferredRemoteStoppedSession else { return }
                RemoteCtrlBGKeepAlive.shared.deferredRemoteStoppedSession = nil
                let suspendForBackground = UIApplication.shared.applicationState == .background
                RemoteCtrlBGKeepAlive.shared.completeSessionTermination(
                    session,
                    success: true,
                    suspendForBackground: suspendForBackground
                )
            }
        } else {
            let suspendForBackground = UIApplication.shared.applicationState == .background
            completeSessionTermination(session, success: true, suspendForBackground: suspendForBackground)
        }
    }

    private func expireLegacyTask(_ token: UUID) async {
        guard legacyTaskToken == token else { return }
        await expire()
    }

    @available(iOS 26.0, *)
    private func expireContinuedProcessingTask(_ task: BGContinuedProcessingTask) async {
        guard continuedTask === task else { return }
        await expire()
    }

    private func expire() async {
        guard !sessionTerminationInProgress else { return }
        sessionTerminationInProgress = true
        let session = ChatModel.shared.remoteCtrlSession
        try? await stopRemoteCtrl()
        let suspendForBackground = UIApplication.shared.applicationState == .background
        completeSessionTermination(session, success: false, suspendForBackground: suspendForBackground)
    }

    private func completeSessionTermination(
        _ session: RemoteCtrlSession?,
        success: Bool,
        suspendForBackground: Bool
    ) {
        if case .connected = session?.sessionState {
            switchToLocalSession()
        } else {
            ChatModel.shared.remoteCtrlSession = nil
        }
        finish(success: success)
        if suspendForBackground {
            suspendChatForBackground()
        }
        sessionTerminationInProgress = false
    }

    @available(iOS 26.0, *)
    private func registerContinuedProcessing() -> Bool {
        if registered { return true }
        registered = BGTaskScheduler.shared.register(forTaskWithIdentifier: remoteCtrlKeepAliveTaskId, using: nil) { task in
            guard let task = task as? BGContinuedProcessingTask else {
                task.setTaskCompleted(success: false)
                return
            }
            Task { @MainActor in
                RemoteCtrlBGKeepAlive.shared.handleContinuedProcessing(task)
            }
        }
        return registered
    }

    @available(iOS 26.0, *)
    private func handleContinuedProcessing(_ task: BGContinuedProcessingTask) {
        guard ChatModel.shared.activeRemoteCtrl,
              continuedProcessingAccepted,
              !sessionTerminationInProgress else {
            task.setTaskCompleted(success: false)
            return
        }
        continuedTask = task
        task.expirationHandler = { [weak task] in
            Task { @MainActor in
                guard let task else { return }
                await RemoteCtrlBGKeepAlive.shared.expireContinuedProcessingTask(task)
            }
        }
        let progressWindow: Int64 = 600
        task.progress.totalUnitCount = progressWindow
        task.progress.completedUnitCount = 0
        continuedProgressTask = Task { @MainActor [weak task] in
            let clock = ContinuousClock()
            guard let task,
                  let elapsedSeconds = RemoteCtrlBGKeepAlive.shared.remoteCtrlElapsedSecondsProvider else { return }
            while !Task.isCancelled,
                  RemoteCtrlBGKeepAlive.shared.continuedTask === task,
                  ChatModel.shared.activeRemoteCtrl {
                do {
                    try await clock.sleep(for: .seconds(1))
                } catch {
                    break
                }
                guard RemoteCtrlBGKeepAlive.shared.continuedTask === task,
                      ChatModel.shared.activeRemoteCtrl else { break }
                task.progress.completedUnitCount += 1
                if task.progress.completedUnitCount * 2 > task.progress.totalUnitCount {
                    task.progress.totalUnitCount += progressWindow
                }
                let connectedSeconds = elapsedSeconds()
                task.updateTitle(task.title, subtitle: remoteCtrlConnectedSubtitle(connectedSeconds))
            }
        }
        stopLegacyTask()
    }

    private func finish(success: Bool) {
        continuedProgressTask?.cancel()
        continuedProgressTask = nil
        remoteCtrlElapsedSecondsProvider = nil
        if #available(iOS 26.0, *) {
            BGTaskScheduler.shared.cancel(taskRequestWithIdentifier: remoteCtrlKeepAliveTaskId)
            continuedTask?.setTaskCompleted(success: success)
            continuedTask = nil
            continuedProcessingAccepted = false
        }
        stopLegacyTask()
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
