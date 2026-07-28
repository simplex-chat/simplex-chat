import ActivityKit
import Foundation
import SwiftUI
import WidgetKit

@main
struct RemoteCtrlLiveActivity: Widget {
    var body: some WidgetConfiguration {
        ActivityConfiguration(for: RemoteCtrlActivityAttributes.self) { context in
            HStack(spacing: 14) {
                simplexLogo(size: 44)
                VStack(alignment: .leading, spacing: 4) {
                    Text("Connected to desktop")
                        .font(.headline)
                    Text(context.attributes.desktopName)
                        .font(.subheadline)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                }
                Spacer(minLength: 12)
                connectedTimer(context.state.connectedAt)
                    .font(.headline.monospacedDigit())
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, 16)
            .padding(.vertical, 14)
            .activityBackgroundTint(Color(uiColor: .systemBackground))
            .activitySystemActionForegroundColor(.primary)
        } dynamicIsland: { context in
            DynamicIsland {
                DynamicIslandExpandedRegion(.leading) {
                    simplexLogo(size: 30)
                }
                DynamicIslandExpandedRegion(.trailing) {
                    connectedTimer(context.state.connectedAt)
                        .font(.caption.monospacedDigit())
                }
                DynamicIslandExpandedRegion(.bottom) {
                    Text("Connected to \(context.attributes.desktopName)")
                        .lineLimit(1)
                        .frame(maxWidth: .infinity)
                }
            } compactLeading: {
                simplexLogo(size: 22)
            } compactTrailing: {
                connectedTimer(context.state.connectedAt)
                    .font(.caption2.monospacedDigit())
                    .frame(width: 40)
            } minimal: {
                simplexLogo(size: 20)
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
        Text(timerInterval: connectedAt...Date.distantFuture, countsDown: false)
    }
}
