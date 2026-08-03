import SwiftUI

@main
struct SimpleXNativeApp: App {
    @StateObject private var model = AppModel()

    var body: some Scene {
        WindowGroup {
            RootView(model: model)
                .frame(minWidth: 760, minHeight: 520)
        }
        .defaultSize(width: 1120, height: 720)
        .windowToolbarStyle(.unifiedCompact)
        .commands {
            SidebarCommands()
            CommandGroup(replacing: .newItem) {}
            CommandMenu("Conversation") {
                Button("Refresh") { model.refresh() }
                    .keyboardShortcut("r")
            }
        }

        Settings {
            Form {
                LabeledContent("Appearance", value: "Follows macOS")
                LabeledContent("Profile", value: model.profile?.displayName ?? "Locked")
            }
            .formStyle(.grouped)
            .padding(20)
            .frame(width: 420)
        }
    }
}
