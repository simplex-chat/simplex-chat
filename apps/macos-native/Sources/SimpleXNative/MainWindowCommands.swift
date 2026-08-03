import AppKit
import SwiftUI

struct MainWindowCommands: Commands {
    static let windowID = "main"

    @Environment(\.openWindow) private var openWindow

    var body: some Commands {
        CommandGroup(replacing: .newItem) {
            Button("New Window") {
                openWindow(id: Self.windowID)
                NSApp.activate(ignoringOtherApps: true)
            }
            .keyboardShortcut("n")
        }
    }
}
