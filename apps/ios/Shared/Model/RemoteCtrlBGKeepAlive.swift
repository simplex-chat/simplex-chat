//
//  RemoteCtrlBGKeepAlive.swift
//  SimpleX (iOS)
//
//  Copyright (c) 2026 SimpleX Chat. All rights reserved.
//

import Foundation
import UIKit
@preconcurrency import BackgroundTasks
import SimpleXChat

private let remoteCtrlKeepAliveTaskPrefix = "chat.simplex.app.remote-control.keepalive"
private let remoteCtrlKeepAliveTaskWildcardId = "\(remoteCtrlKeepAliveTaskPrefix).*"
private let remoteCtrlKeepAliveTaskId = "\(remoteCtrlKeepAliveTaskPrefix).session"

@MainActor
final class RemoteCtrlBGKeepAlive {
    static let shared = RemoteCtrlBGKeepAlive()

    private let continuedProcessingTitle = "Desktop background session"
    private let continuedProcessingMaxSeconds: Int64 = 15 * 60

    private var running = false
    private var stopping = false
    private var registeredContinuedProcessing = false
    private var startedAt: Date?
    private var progressTimer: Timer?
    private var endTask: ((Bool) -> Void)?

    private init() {}

    func start() {
        guard !running, !stopping else { return }
        guard ChatModel.shared.activeRemoteCtrl else { return }

        running = true
        startedAt = .now

        if #available(iOS 26.0, *) {
            startContinuedProcessing()
        } else {
            startShortBackgroundTask()
        }
    }

    func stopKeepingSession() {
        guard running else { return }
        finishKeepingSession(success: true)
    }

    func explicitDisconnect() async throws {
        guard !stopping else { return }
        stopping = true
        defer { stopping = false }
        do {
            try await stopRemoteCtrl()
            finishKeepingSession(success: true)
            clearRemoteCtrlSessionAfterStop()
        } catch {
            finishKeepingSession(success: false)
            throw error
        }
    }

    func expire(force: Bool = false) async {
        guard force || running || endTask != nil else { return }
        guard !stopping else {
            finishKeepingSession(success: false)
            return
        }
        stopping = true
        defer { stopping = false }
        try? await stopRemoteCtrl()
        clearRemoteCtrlSessionAfterStop()
        finishKeepingSession(success: false)
    }

    private func startShortBackgroundTask() {
        var taskId: UIBackgroundTaskIdentifier = .invalid
        taskId = UIApplication.shared.beginBackgroundTask(withName: "RemoteCtrlBGKeepAlive") {
            Task { @MainActor in
                await RemoteCtrlBGKeepAlive.shared.expire()
            }
        }
        guard taskId != .invalid else {
            Task { @MainActor in
                await RemoteCtrlBGKeepAlive.shared.expire(force: true)
            }
            return
        }
        endTask = { _ in
            if taskId != .invalid {
                UIApplication.shared.endBackgroundTask(taskId)
                taskId = .invalid
            }
        }
    }

    @available(iOS 26.0, *)
    private func startContinuedProcessing() {
        guard registerContinuedProcessing() else {
            startShortBackgroundTask()
            return
        }

        let taskId = continuedProcessingTaskId()
        let request = BGContinuedProcessingTaskRequest(
            identifier: taskId,
            title: continuedProcessingTitle,
            subtitle: continuedProcessingSubtitle()
        )
        request.strategy = .fail

        do {
            try BGTaskScheduler.shared.submit(request)
            endTask = { _ in
                BGTaskScheduler.shared.cancel(taskRequestWithIdentifier: taskId)
            }
        } catch {
            startShortBackgroundTask()
        }
    }

    @available(iOS 26.0, *)
    private func registerContinuedProcessing() -> Bool {
        if registeredContinuedProcessing { return true }
        registeredContinuedProcessing = BGTaskScheduler.shared.register(forTaskWithIdentifier: remoteCtrlKeepAliveTaskWildcardId, using: nil) { task in
            guard let task = task as? BGContinuedProcessingTask else {
                task.setTaskCompleted(success: false)
                return
            }
            Task { @MainActor in
                RemoteCtrlBGKeepAlive.shared.handleContinuedProcessing(task)
            }
        }
        return registeredContinuedProcessing
    }

    @available(iOS 26.0, *)
    private func handleContinuedProcessing(_ task: BGContinuedProcessingTask) {
        guard running, ChatModel.shared.activeRemoteCtrl else {
            task.setTaskCompleted(success: false)
            return
        }

        task.expirationHandler = {
            Task { @MainActor in
                await RemoteCtrlBGKeepAlive.shared.expire()
            }
        }
        startProgressUpdates(task)
        endTask = { [weak task] success in
            self.stopProgressUpdates()
            task?.setTaskCompleted(success: success)
        }
    }

    @available(iOS 26.0, *)
    private func startProgressUpdates(_ task: BGContinuedProcessingTask) {
        stopProgressUpdates()
        task.progress.totalUnitCount = continuedProcessingMaxSeconds
        updateContinuedProcessing(task)
        progressTimer = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) { [weak task] _ in
            Task { @MainActor in
                guard let task = task else { return }
                RemoteCtrlBGKeepAlive.shared.updateContinuedProcessing(task)
            }
        }
    }

    @available(iOS 26.0, *)
    private func updateContinuedProcessing(_ task: BGContinuedProcessingTask) {
        task.updateTitle(continuedProcessingTitle, subtitle: continuedProcessingSubtitle())
        let elapsed = elapsedSeconds()
        task.progress.completedUnitCount = min(elapsed, continuedProcessingMaxSeconds)
        if elapsed >= continuedProcessingMaxSeconds {
            Task { @MainActor in
                await RemoteCtrlBGKeepAlive.shared.expire(force: true)
            }
        }
    }

    private func stopProgressUpdates() {
        progressTimer?.invalidate()
        progressTimer = nil
    }

    private func finishKeepingSession(success: Bool) {
        running = false
        stopProgressUpdates()
        let end = endTask
        endTask = nil
        startedAt = nil
        end?(success)
    }

    private func clearRemoteCtrlSessionAfterStop() {
        if case .connected = ChatModel.shared.remoteCtrlSession?.sessionState {
            switchToLocalSession()
        } else {
            ChatModel.shared.remoteCtrlSession = nil
        }
    }

    private func continuedProcessingSubtitle() -> String {
        "Ends in \(formatDuration(max(0, continuedProcessingMaxSeconds - elapsedSeconds())))"
    }

    private func elapsedSeconds() -> Int64 {
        guard let startedAt = startedAt else { return 0 }
        return max(0, Int64(Date.now.timeIntervalSince(startedAt)))
    }

    private func continuedProcessingTaskId() -> String {
        remoteCtrlKeepAliveTaskId
    }

    private func formatDuration(_ seconds: Int64) -> String {
        let hours = seconds / 3600
        let minutes = (seconds % 3600) / 60
        let secs = seconds % 60
        if hours > 0 {
            return "\(hours)h \(minutes)m"
        } else if minutes > 0 {
            return "\(minutes)m \(secs)s"
        } else {
            return "\(secs)s"
        }
    }
}
