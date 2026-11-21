import ActivityKit
import AppIntents
import Foundation

@available(iOS 16.1, *)
struct RemoteCtrlActivityAttributes: ActivityAttributes {
    struct ContentState: Codable, Hashable {}

    var desktopName: String?
    var connectedAt: Date
}

@available(iOS 17.0, *)
struct DisconnectDesktopIntent: LiveActivityIntent {
    static let title: LocalizedStringResource = "Disconnect desktop"
    static let isDiscoverable = false

    func perform() async throws -> some IntentResult {
        #if !LIVE_ACTIVITY_EXTENSION
        await RemoteCtrlKeepAlive.shared.disconnect()
        #endif
        return .result()
    }
}
