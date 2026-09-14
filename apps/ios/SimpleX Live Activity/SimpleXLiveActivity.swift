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
                        Text("Connected to desktop")
                            .font(.headline)
                            .lineLimit(1)
                        Text(context.attributes.desktopName)
                            .font(.subheadline)
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                    }
                    .layoutPriority(1)
                    Spacer(minLength: 8)
                    connectedTimer(context.state.connectedAt)
                        .font(.headline.monospacedDigit())
                        .multilineTextAlignment(.trailing)
                        .lineLimit(1)
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
                    connectedTimer(context.state.connectedAt)
                        .font(.caption.monospacedDigit())
                }
                DynamicIslandExpandedRegion(.bottom) {
                    Text("Connected to \(context.attributes.desktopName)")
                        .font(.subheadline.weight(.semibold))
                        .lineLimit(1)
                }
            } compactLeading: {
                simplexLogo(size: 16)
            } compactTrailing: {
                connectedTimer(context.state.connectedAt)
                    .font(.caption2.monospacedDigit())
                    .frame(width: 40)
            } minimal: {
                simplexLogo(size: 14)
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
}
