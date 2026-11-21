import ActivityKit
import SwiftUI
import WidgetKit

private let desktopIcon = "desktopcomputer"
private let compactTimerWidth: CGFloat = 44

@main
struct RemoteCtrlLiveActivity: Widget {
    var body: some WidgetConfiguration {
        ActivityConfiguration(for: RemoteCtrlActivityAttributes.self) { context in
            VStack(alignment: .leading, spacing: 12) {
                HStack(spacing: 12) {
                    Image(systemName: desktopIcon)
                        .font(.title2)
                    VStack(alignment: .leading, spacing: 2) {
                        Text("Connected to desktop")
                            .font(.headline)
                        desktopText(context.attributes)
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
                    }
                    .lineLimit(1)
                }
                disconnectButton()
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding()
        } dynamicIsland: { context in
            DynamicIsland {
                DynamicIslandExpandedRegion(.leading) {
                    Image(systemName: desktopIcon)
                }
                DynamicIslandExpandedRegion(.trailing) {
                    connectedTimer(context.attributes)
                        .monospacedDigit()
                        .multilineTextAlignment(.trailing)
                }
                DynamicIslandExpandedRegion(.center) {
                    Text("Connected to desktop")
                        .lineLimit(1)
                }
                DynamicIslandExpandedRegion(.bottom) {
                    disconnectButton()
                }
            } compactLeading: {
                Image(systemName: desktopIcon)
            } compactTrailing: {
                connectedTimer(context.attributes)
                    .monospacedDigit()
                    .frame(width: compactTimerWidth)
            } minimal: {
                Image(systemName: desktopIcon)
            }
        }
    }

    private func desktopText(_ attributes: RemoteCtrlActivityAttributes) -> Text {
        if let desktopName = attributes.desktopName {
            Text(desktopName) + Text(verbatim: " · ") + connectedTimer(attributes)
        } else {
            connectedTimer(attributes)
        }
    }

    private func connectedTimer(_ attributes: RemoteCtrlActivityAttributes) -> Text {
        Text(attributes.connectedAt, style: .timer)
    }

    @ViewBuilder private func disconnectButton() -> some View {
        if #available(iOS 17.0, *) {
            Button(intent: DisconnectDesktopIntent()) {
                Label("Disconnect", systemImage: "multiply")
            }
        }
    }
}
