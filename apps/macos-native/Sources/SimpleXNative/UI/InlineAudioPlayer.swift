import AVFoundation
import AppKit
import SwiftUI

struct InlineAudioPlayer: View {
    @StateObject private var controller: InlineAudioPlayerController

    let title: String
    let fallbackDuration: Int?
    let close: () -> Void

    init(url: URL, title: String, fallbackDuration: Int?, close: @escaping () -> Void) {
        _controller = StateObject(wrappedValue: InlineAudioPlayerController(url: url))
        self.title = title
        self.fallbackDuration = fallbackDuration
        self.close = close
    }

    var body: some View {
        Group {
            if let errorMessage = controller.errorMessage {
                HStack(spacing: 12) {
                    Label(errorMessage, systemImage: "exclamationmark.triangle")
                        .foregroundStyle(.secondary)
                    Spacer(minLength: 0)
                    openExternallyButton
                    closeButton
                }
            } else {
                TimelineView(.periodic(from: .now, by: 0.25)) { _ in
                    VStack(alignment: .leading, spacing: 8) {
                        HStack(spacing: 8) {
                            playPauseButton
                            Image(systemName: "waveform")
                                .foregroundStyle(.secondary)
                                .accessibilityHidden(true)
                            Text(title)
                                .font(.callout.weight(.medium))
                                .lineLimit(1)
                            Spacer(minLength: 8)
                            openExternallyButton
                            closeButton
                        }

                        Slider(value: playbackPosition, in: 0...maximumDuration) {
                            Text("Playback Position")
                        }
                        .accessibilityValue("\(elapsedLabel) of \(durationLabel)")
                        .accessibilityInputLabels(["Playback Position", "Seek"])

                        HStack {
                            Text(elapsedLabel)
                            Spacer(minLength: 8)
                            Text(durationLabel)
                        }
                        .font(.caption.monospacedDigit())
                        .foregroundStyle(.secondary)
                    }
                }
            }
        }
        .frame(minWidth: 280, idealWidth: 360, maxWidth: 400)
        .accessibilityElement(children: .contain)
        .onAppear { controller.play() }
        .onDisappear { controller.stop() }
    }

    private var playPauseButton: some View {
        Button(action: controller.togglePlayback) {
            Image(systemName: controller.isPlaying ? "pause.fill" : "play.fill")
                .frame(width: 20, height: 20)
        }
        .buttonStyle(.borderless)
        .frame(width: 44, height: 44)
        .contentShape(Rectangle())
        .help(controller.isPlaying ? "Pause" : "Play")
        .accessibilityLabel(controller.isPlaying ? "Pause" : "Play") // [VERIFY] Matches the visible playback state.
        .accessibilityInputLabels(controller.isPlaying ? ["Pause"] : ["Play"])
    }

    private var openExternallyButton: some View {
        Button(action: controller.openExternally) {
            Image(systemName: "arrow.up.forward.square")
                .frame(width: 20, height: 20)
        }
        .buttonStyle(.borderless)
        .frame(width: 44, height: 44)
        .contentShape(Rectangle())
        .help("Open Externally")
        .accessibilityLabel("Open Externally") // [VERIFY] Matches the visible tooltip.
        .accessibilityInputLabels(["Open Externally", "Open Audio"])
    }

    private var closeButton: some View {
        Button(action: close) {
            Image(systemName: "xmark")
                .frame(width: 20, height: 20)
        }
        .buttonStyle(.borderless)
        .frame(width: 44, height: 44)
        .contentShape(Rectangle())
        .help("Close Player")
        .accessibilityLabel("Close Player") // [VERIFY] Matches the visible tooltip.
        .accessibilityInputLabels(["Close Player", "Close Audio"])
    }

    private var playbackPosition: Binding<Double> {
        Binding(
            get: { min(controller.currentTime, maximumDuration) },
            set: { value in controller.seek(to: value) }
        )
    }

    private var maximumDuration: Double {
        max(controller.duration, Double(fallbackDuration ?? 0), 1)
    }

    private var elapsedLabel: String {
        AudioPlaybackTime.label(controller.currentTime)
    }

    private var durationLabel: String {
        AudioPlaybackTime.label(max(controller.duration, Double(fallbackDuration ?? 0)))
    }
}

@MainActor
final class InlineAudioPlayerController: NSObject, ObservableObject, AVAudioPlayerDelegate {
    @Published private(set) var isPlaying = false
    @Published private(set) var errorMessage: String?

    private let url: URL
    private var player: AVAudioPlayer?

    init(url: URL) {
        self.url = url
        do {
            player = try AVAudioPlayer(contentsOf: url)
        } catch {
            player = nil
            errorMessage = "This audio could not be played inline."
        }
        super.init()
        player?.delegate = self
        player?.prepareToPlay()
    }

    var currentTime: Double {
        player?.currentTime ?? 0
    }

    var duration: Double {
        player?.duration ?? 0
    }

    func play() {
        guard let player else { return }
        if player.currentTime >= player.duration { player.currentTime = 0 }
        isPlaying = player.play()
    }

    func togglePlayback() {
        isPlaying ? pause() : play()
    }

    func pause() {
        player?.pause()
        isPlaying = false
    }

    func stop() {
        player?.stop()
        isPlaying = false
    }

    func seek(to time: Double) {
        player?.currentTime = min(max(time, 0), duration)
    }

    func openExternally() {
        pause()
        NSWorkspace.shared.open(url)
    }

    nonisolated func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        Task { @MainActor [weak self] in
            self?.isPlaying = false
        }
    }
}

enum AudioPlaybackTime {
    static func label(_ seconds: Double) -> String {
        let normalized = seconds.isFinite ? max(seconds, 0) : 0
        return Duration.seconds(normalized).formatted(.time(pattern: .minuteSecond))
    }
}
