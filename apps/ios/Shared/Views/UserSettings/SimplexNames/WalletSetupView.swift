import SwiftUI
import SimpleXChat

// How this profile gets its recovery key, asked once. Sharing the key across
// profiles is the default because it means one thing to write down. A separate
// key exists for a profile kept deliberately apart.
struct WalletSetupView: View {
    @Environment(\.dismiss) var dismiss
    let hasOtherSeed: Bool
    @State private var busy = false

    var body: some View {
        List {
            Section {
                if hasOtherSeed {
                    Button { setup("existing") } label: { Label("Use the recovery key I already have", systemImage: "checkmark").foregroundColor(.accentColor) }
                }
                Button { setup("new") } label: {
                    Label(hasOtherSeed ? "Give this profile its own recovery key" : "Create a recovery key", systemImage: "plus").foregroundColor(.accentColor)
                }
                NavigationLink { ImportRecoveryKeyView() } label: { Label("Restore names from a recovery key", systemImage: "square.and.arrow.down") }
            } footer: {
                Text(hasOtherSeed
                    ? "Using the key you already have means one set of words to write down, and it covers every profile. A separate key keeps this profile's names unconnected to your others, but is a second thing to keep safe."
                    : "You will be shown the key afterwards. Write it down — it is the only way to get your names back if you lose this device.")
            }
        }
        .navigationTitle("Set up names for this profile")
    }

    private func setup(_ arg: String) {
        Task { busy = true; defer { busy = false }; if await apiNameSetup(arg) { await MainActor.run { dismiss() } } }
    }
}
