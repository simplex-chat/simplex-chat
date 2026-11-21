import ActivityKit
import AVFoundation
import Combine
import SwiftUI
import SimpleXChat

private let silenceSampleRate: Double = 44_100
private let silenceFrames: AVAudioFrameCount = 4_096

private enum RemoteCtrlKeepAliveError: Error {
    case silenceBuffer
}

private struct AudioSessionConfig {
    var category: AVAudioSession.Category
    var mode: AVAudioSession.Mode
    var options: AVAudioSession.CategoryOptions
    var prefersNoInterruptionsFromSystemAlerts: Bool
}

protocol RemoteCtrlKeepAliveServices: AnyObject {
    func startAudio(onFailure: @escaping (String) -> Void) -> Bool
    func stopAudio()
    func showIndicator(_ desktopName: String?)
    func endIndicator()
}

final class RemoteCtrlKeepAlive: ObservableObject {
    static let shared = RemoteCtrlKeepAlive()

    @Published private(set) var running = false
    var services: RemoteCtrlKeepAliveServices = RemoteCtrlSystemServices()
    private var sessionSubscription: AnyCancellable?

    func observeSession() {
        services.endIndicator()
        sessionSubscription = ChatModel.shared.$remoteCtrlSession
            .map { session -> RemoteCtrlInfo? in
                if case let .connected(remoteCtrl, _) = session?.sessionState { remoteCtrl } else { nil }
            }
            .removeDuplicates { $0?.remoteCtrlId == $1?.remoteCtrlId }
            .sink { [weak self] remoteCtrl in
                if let remoteCtrl {
                    self?.sessionConnected(remoteCtrl)
                } else {
                    self?.sessionEnded()
                }
            }
    }

    func disconnect() async {
        do {
            try await stopRemoteCtrlSession()
        } catch let error {
            logger.error("RemoteCtrlKeepAlive disconnect error: \(responseError(error))")
        }
    }

    func stop() {
        guard running else { return }
        running = false
        services.stopAudio()
        services.endIndicator()
    }

    private func sessionConnected(_ remoteCtrl: RemoteCtrlInfo) {
        if ChatModel.shared.activeCall == nil && services.startAudio(onFailure: { [weak self] reason in self?.audioFailed(reason) }) {
            running = true
            services.showIndicator(ChatModel.shared.notificationPreview == .hidden ? nil : remoteCtrl.deviceViewName)
        }
        AppDelegate.keepScreenOn(!running)
    }

    private func sessionEnded() {
        AppDelegate.keepScreenOn(false)
        guard running else { return }
        stop()
        if UIApplication.shared.applicationState == .background {
            DispatchQueue.main.async {
                switchToLocalSession()
                if AppChatState.shared.value.canSuspend {
                    suspendChatInBackground()
                }
            }
        }
    }

    private func audioFailed(_ reason: String) {
        guard running else { return }
        logger.error("RemoteCtrlKeepAlive audio stopped: \(reason)")
        let endTask = beginBGTask()
        Task {
            await disconnect()
            endTask()
        }
    }
}

final class RemoteCtrlSystemServices: RemoteCtrlKeepAliveServices {
    private var savedAudioSession: AudioSessionConfig?
    private var audioEngine: AVAudioEngine?
    private var engineObserver: NSObjectProtocol?
    private var audioSessionObservers: [NSObjectProtocol] = []
    private var audioFailed: ((String) -> Void)?
    private var activityStateTask: Task<Void, Never>?

    func startAudio(onFailure: @escaping (String) -> Void) -> Bool {
        let session = AVAudioSession.sharedInstance()
        savedAudioSession = AudioSessionConfig(
            category: session.category,
            mode: session.mode,
            options: session.categoryOptions,
            prefersNoInterruptionsFromSystemAlerts: session.prefersNoInterruptionsFromSystemAlerts
        )
        do {
            try session.setCategory(.playback, mode: .default, options: .mixWithOthers)
            try session.setPrefersNoInterruptionsFromSystemAlerts(true)
            try session.setActive(true)
            try playSilence()
        } catch let error {
            logger.error("RemoteCtrlKeepAlive audio error: \(error.localizedDescription)")
            stopAudio()
            return false
        }
        audioFailed = onFailure
        let nc = NotificationCenter.default
        audioSessionObservers = [
            nc.addObserver(forName: AVAudioSession.interruptionNotification, object: session, queue: .main) { [weak self] notification in
                self?.audioInterrupted(notification)
            },
            nc.addObserver(forName: AVAudioSession.mediaServicesWereResetNotification, object: session, queue: .main) { [weak self] _ in
                self?.audioFailed?("media services were reset")
            }
        ]
        return true
    }

    func stopAudio() {
        audioFailed = nil
        audioSessionObservers.forEach { NotificationCenter.default.removeObserver($0) }
        audioSessionObservers = []
        stopEngine()
        guard let saved = savedAudioSession else { return }
        savedAudioSession = nil
        let session = AVAudioSession.sharedInstance()
        do {
            try session.setActive(false)
            try session.setCategory(saved.category, mode: saved.mode, options: saved.options)
            try session.setPrefersNoInterruptionsFromSystemAlerts(saved.prefersNoInterruptionsFromSystemAlerts)
        } catch let error {
            logger.error("RemoteCtrlKeepAlive audio session restore error: \(error.localizedDescription)")
        }
    }

    func showIndicator(_ desktopName: String?) {
        if #available(iOS 16.1, *), startLiveActivity(desktopName) { return }
        NtfManager.shared.notifyRemoteCtrlConnected(desktopName)
    }

    func endIndicator() {
        NtfManager.shared.removeRemoteCtrlNotification()
        activityStateTask?.cancel()
        activityStateTask = nil
        if #available(iOS 16.1, *) {
            let activities = Activity<RemoteCtrlActivityAttributes>.activities
            if !activities.isEmpty {
                let endTask = beginBGTask()
                Task {
                    for activity in activities {
                        await activity.end(using: nil, dismissalPolicy: .immediate)
                    }
                    endTask()
                }
            }
        }
    }

    private func playSilence() throws {
        guard let format = AVAudioFormat(standardFormatWithSampleRate: silenceSampleRate, channels: 1),
              let buffer = zeroFilledBuffer(format) else {
            throw RemoteCtrlKeepAliveError.silenceBuffer
        }
        let engine = AVAudioEngine()
        let player = AVAudioPlayerNode()
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode, format: format)
        player.scheduleBuffer(buffer, at: nil, options: .loops)
        try engine.start()
        player.play()
        audioEngine = engine
        engineObserver = NotificationCenter.default.addObserver(forName: .AVAudioEngineConfigurationChange, object: engine, queue: .main) { [weak self] _ in
            self?.restartAudio()
        }
    }

    private func zeroFilledBuffer(_ format: AVAudioFormat) -> AVAudioPCMBuffer? {
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: silenceFrames) else { return nil }
        buffer.frameLength = silenceFrames
        for channel in UnsafeMutableAudioBufferListPointer(buffer.mutableAudioBufferList) {
            if let data = channel.mData {
                memset(data, 0, Int(channel.mDataByteSize))
            }
        }
        return buffer
    }

    private func audioInterrupted(_ notification: Notification) {
        guard let info = notification.userInfo,
              let typeValue = info[AVAudioSessionInterruptionTypeKey] as? UInt,
              let type = AVAudioSession.InterruptionType(rawValue: typeValue) else { return }
        switch type {
        case .began:
            if let reasonValue = info[AVAudioSessionInterruptionReasonKey] as? UInt,
               AVAudioSession.InterruptionReason(rawValue: reasonValue) == .appWasSuspended {
                restartAudio()
            }
        case .ended:
            restartAudio()
        @unknown default:
            break
        }
    }

    private func restartAudio() {
        guard let audioFailed else { return }
        stopEngine()
        do {
            try AVAudioSession.sharedInstance().setActive(true)
            try playSilence()
        } catch let error {
            audioFailed(error.localizedDescription)
        }
    }

    private func stopEngine() {
        if let engineObserver {
            NotificationCenter.default.removeObserver(engineObserver)
        }
        engineObserver = nil
        audioEngine?.stop()
        audioEngine = nil
    }

    @available(iOS 16.1, *)
    private func startLiveActivity(_ desktopName: String?) -> Bool {
        guard ActivityAuthorizationInfo().areActivitiesEnabled else { return false }
        do {
            let activity = try Activity.request(
                attributes: RemoteCtrlActivityAttributes(desktopName: desktopName, connectedAt: .now),
                contentState: RemoteCtrlActivityAttributes.ContentState(),
                pushType: nil
            )
            activityStateTask = Task {
                for await state in activity.activityStateUpdates where state == .ended || state == .dismissed {
                    await MainActor.run {
                        if !Task.isCancelled {
                            NtfManager.shared.notifyRemoteCtrlConnected(desktopName)
                        }
                    }
                    break
                }
            }
            return true
        } catch let error {
            logger.error("RemoteCtrlKeepAlive Live Activity error: \(error.localizedDescription)")
            return false
        }
    }
}
