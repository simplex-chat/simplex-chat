//
//  SuspendChat.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 26/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
import ActivityKit
import AVFoundation
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

@MainActor
private final class RemoteCtrlBackgroundAudio {
    static let shared = RemoteCtrlBackgroundAudio()

    private var player: AVQueuePlayer?
    private var looper: AVPlayerLooper?

    private init() {}

    var isRunning: Bool {
        player != nil
    }

    func start() {
        guard player == nil else { return }
        guard #available(iOS 16.1, *) else { return }
        guard ActivityAuthorizationInfo().areActivitiesEnabled else { return }

        do {
            try AVAudioSession.sharedInstance().setCategory(
                .playback,
                mode: .default,
                options: .mixWithOthers
            )
        } catch {
            logger.error("RemoteCtrlBackgroundAudio audio session error: \(error.localizedDescription)")
        }

        guard let url = Bundle.main.url(
            forResource: "sample",
            withExtension: "mp3",
            subdirectory: "sounds"
        ) else {
            logger.error("RemoteCtrlBackgroundAudio sample.mp3 not found")
            return
        }

        let queuePlayer = AVQueuePlayer()
        let item = AVPlayerItem(asset: AVURLAsset(url: url))
        player = queuePlayer
        looper = AVPlayerLooper(player: queuePlayer, templateItem: item)
        queuePlayer.seek(to: .zero)
        queuePlayer.play()
        logger.debug("RemoteCtrlBackgroundAudio started")
    }

    func stop() {
        player?.pause()
        looper = nil
        player = nil
        do {
            try AVAudioSession.sharedInstance().setActive(
                false,
                options: .notifyOthersOnDeactivation
            )
        } catch {
            logger.error("RemoteCtrlBackgroundAudio deactivation error: \(error.localizedDescription)")
        }
        logger.debug("RemoteCtrlBackgroundAudio stopped")
    }
}

@available(iOS 16.1, *)
@MainActor
private final class RemoteCtrlLiveActivityManager {
    static let shared = RemoteCtrlLiveActivityManager()

    private var activity: Activity<RemoteCtrlActivityAttributes>?
    private var lifecycleTask: Task<Void, Never>?
    private var generation = 0

    private init() {}

    func start(desktopName: String) {
        guard ActivityAuthorizationInfo().areActivitiesEnabled else { return }
        guard activity == nil else { return }
        generation += 1
        let requestedGeneration = generation
        let previousTask = lifecycleTask
        lifecycleTask = Task {
            await previousTask?.value
            guard requestedGeneration == generation else { return }
            for staleActivity in Activity<RemoteCtrlActivityAttributes>.activities {
                await staleActivity.end(using: nil, dismissalPolicy: .immediate)
            }
            guard requestedGeneration == generation else { return }
            do {
                let state = RemoteCtrlActivityAttributes.ContentState(connectedAt: .now)
                activity = try Activity.request(
                    attributes: RemoteCtrlActivityAttributes(desktopName: desktopName),
                    contentState: state,
                    pushType: nil
                )
            } catch {
                logger.error("RemoteCtrlLiveActivity request error: \(error.localizedDescription)")
            }
        }
    }

    func stop() {
        generation += 1
        activity = nil
        let previousTask = lifecycleTask
        lifecycleTask = Task {
            await previousTask?.value
            for activeActivity in Activity<RemoteCtrlActivityAttributes>.activities {
                await activeActivity.end(using: nil, dismissalPolicy: .immediate)
            }
        }
    }
}

@MainActor
final class RemoteCtrlBGKeepAlive {
    static let shared = RemoteCtrlBGKeepAlive()

    private var sessionTerminationInProgress = false
    private var deferredRemoteStoppedSession: RemoteCtrlSession?

    private init() {}

    var backgroundAudioActive: Bool {
        RemoteCtrlBackgroundAudio.shared.isRunning
    }

    func start() {
        // Threat model: a verified desktop, including a malicious or stalled peer,
        // can keep audio and network work active until the user disconnects here.
        // Every local termination path releases the keepalive and restores local UI.
        guard ChatModel.shared.activeRemoteCtrl else { return }
        if #available(iOS 16.1, *),
           let session = ChatModel.shared.remoteCtrlSession,
           case let .connected(remoteCtrl, _) = session.sessionState {
            RemoteCtrlLiveActivityManager.shared.start(desktopName: remoteCtrl.deviceViewName)
        }
        RemoteCtrlBackgroundAudio.shared.start()
    }

    func handleAppBackgrounding() {
        if let session = deferredRemoteStoppedSession {
            // Restore locally before the scene path suspends chat if backgrounding
            // happens during the remote-stop recovery delay.
            deferredRemoteStoppedSession = nil
            completeSessionTermination(session, suspendForBackground: true)
        } else if !backgroundAudioActive {
            suspendChatForBackground()
        }
    }

    func disconnectRemoteCtrl() async throws {
        guard !sessionTerminationInProgress else { return }
        sessionTerminationInProgress = true
        let session = ChatModel.shared.remoteCtrlSession
        do {
            try await stopRemoteCtrl()
        } catch {
            let suspendForBackground = UIApplication.shared.applicationState == .background
            completeSessionTermination(session, suspendForBackground: suspendForBackground)
            throw error
        }
        let suspendForBackground = UIApplication.shared.applicationState == .background
        completeSessionTermination(session, suspendForBackground: suspendForBackground)
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
                    suspendForBackground: suspendForBackground
                )
            }
        } else {
            let suspendForBackground = UIApplication.shared.applicationState == .background
            completeSessionTermination(session, suspendForBackground: suspendForBackground)
        }
    }

    private func completeSessionTermination(
        _ session: RemoteCtrlSession?,
        suspendForBackground: Bool
    ) {
        if case .connected = session?.sessionState {
            switchToLocalSession()
        } else {
            ChatModel.shared.remoteCtrlSession = nil
        }
        if #available(iOS 16.1, *) {
            RemoteCtrlLiveActivityManager.shared.stop()
        }
        RemoteCtrlBackgroundAudio.shared.stop()
        UIApplication.shared.isIdleTimerDisabled = false
        if suspendForBackground {
            suspendChatForBackground()
        }
        sessionTerminationInProgress = false
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
