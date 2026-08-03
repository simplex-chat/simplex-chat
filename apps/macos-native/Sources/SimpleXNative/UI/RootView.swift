import SwiftUI

struct RootView: View {
    @ObservedObject var model: AppModel
    @ObservedObject var notifications: NativeNotificationManager
    @AppStorage("desktopSidebarCollapsed") private var sidebarCollapsed = false

    private var columnVisibility: Binding<NavigationSplitViewVisibility> {
        Binding(
            get: { sidebarCollapsed ? .detailOnly : .all },
            set: { sidebarCollapsed = $0 == .detailOnly }
        )
    }

    var body: some View {
        Group {
            switch model.phase {
            case .locked, .opening:
                UnlockView(model: model)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                    .background(.background)
            case .ready, .failed:
                NavigationSplitView(columnVisibility: columnVisibility) {
                    SidebarView(model: model)
                        .navigationSplitViewColumnWidth(min: 240, ideal: 320, max: 480)
                } detail: {
                    ConversationView(model: model)
                        .navigationSplitViewColumnWidth(min: 520, ideal: 800)
                }
                .alert("SimpleX", isPresented: Binding(
                    get: { if case .failed = model.phase { true } else { false } },
                    set: { if !$0 { model.phase = .ready } }
                )) {
                    Button("OK") { model.phase = .ready }
                } message: {
                    if case let .failed(message) = model.phase { Text(message) }
                }
            }
        }
        .alert("Stay Updated?", isPresented: $notifications.showingPermissionExplanation) {
            Button("Allow Notifications") {
                notifications.respondToPermissionExplanation(requestPermission: true)
            }
            Button("Not Now", role: .cancel) {
                notifications.respondToPermissionExplanation(requestPermission: false)
            }
        } message: {
            Text("SimpleX can use native Mac notifications for messages, contact requests, and calls. You can choose how much message detail appears in Settings.")
        }
    }
}
