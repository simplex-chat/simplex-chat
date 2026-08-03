import ActivityKit
import Foundation
import SwiftUI
import WidgetKit

@main
struct RemoteCtrlLiveActivity: Widget {
    var body: some WidgetConfiguration {
        ActivityConfiguration(for: RemoteCtrlActivityAttributes.self) { context in
            GeometryReader { proxy in
                HStack(spacing: 12) {
                    simplexLogo(size: 36)
                    VStack(alignment: .leading, spacing: 2) {
                        Text(context.state.reconnecting == true ? "Reconnecting to desktop" : "Connected to desktop")
                            .font(.headline)
                            .lineLimit(1)
                        Text(context.attributes.desktopName)
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                    .layoutPriority(1)
                    Spacer(minLength: 8)
                    if context.state.reconnecting == true {
                        reconnectingIcon(size: 20)
                    } else {
                        connectedTimer(context.state.connectedAt)
                            .font(.headline.monospacedDigit())
                            .multilineTextAlignment(.trailing)
                            .lineLimit(1)
                    }
                }
                .frame(width: proxy.size.width, height: proxy.size.height)
            }
            .frame(height: 64)
            .padding(.horizontal, 14)
        } dynamicIsland: { context in
            DynamicIsland {
                DynamicIslandExpandedRegion(.leading) {
                    simplexLogo(size: 24)
                }
                DynamicIslandExpandedRegion(.trailing) {
                    if context.state.reconnecting == true {
                        reconnectingIcon(size: 16)
                    } else {
                        connectedTimer(context.state.connectedAt)
                            .font(.caption.monospacedDigit())
                    }
                }
                DynamicIslandExpandedRegion(.bottom) {
                    Text(context.state.reconnecting == true
                         ? "Reconnecting to \(context.attributes.desktopName)"
                         : "Connected to \(context.attributes.desktopName)")
                        .font(.subheadline.weight(.semibold))
                        .lineLimit(1)
                }
            } compactLeading: {
                simplexLogo(size: 16)
            } compactTrailing: {
                if context.state.reconnecting == true {
                    reconnectingIcon(size: 14)
                        .frame(width: 40)
                } else {
                    connectedTimer(context.state.connectedAt)
                        .font(.caption2.monospacedDigit())
                        .frame(width: 40)
                }
            } minimal: {
                if context.state.reconnecting == true {
                    reconnectingIcon(size: 14)
                } else {
                    simplexLogo(size: 14)
                }
            }
        }
    }

    private func simplexLogo(size: CGFloat) -> some View {
        Image("icon-transparent")
            .resizable()
            .scaledToFit()
            .frame(width: size, height: size)
            .accessibilityHidden(true)
    }

    private func connectedTimer(_ connectedAt: Date) -> Text {
        Text(
            timerInterval: connectedAt...connectedAt.addingTimeInterval(8 * 60 * 60),
            countsDown: false,
            showsHours: true
        )
    }

    private func reconnectingIcon(size: CGFloat) -> some View {
        Image(systemName: "arrow.clockwise")
            .font(.system(size: size, weight: .semibold))
            .accessibilityLabel("Reconnecting")
    }
}
