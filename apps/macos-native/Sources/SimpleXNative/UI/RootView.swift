import SwiftUI

struct RootView: View {
    @ObservedObject var model: AppModel
    @State private var columnVisibility: NavigationSplitViewVisibility = .all

    var body: some View {
        Group {
            switch model.phase {
            case .locked, .opening:
                UnlockView(model: model)
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
                    .background(.background)
            case .ready, .failed:
                NavigationSplitView(columnVisibility: $columnVisibility) {
                    SidebarView(model: model)
                        .navigationSplitViewColumnWidth(min: 240, ideal: 320, max: 480)
                } detail: {
                    ConversationView(model: model)
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
    }
}
