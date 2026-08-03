import ActivityKit
import Foundation

@available(iOS 16.1, *)
struct RemoteCtrlActivityAttributes: ActivityAttributes {
    struct ContentState: Codable, Hashable {
        var connectedAt: Date
        var reconnecting: Bool?
    }

    var desktopName: String
}
