import SwiftUI
import SimpleXChat

// The recovery key for the user's names. For most users this is the only backup
// they have of anything. The stakes are impersonation, not loss: whoever holds
// it can point a name at their own address.
struct NameRecoveryKeyView: View {
    @State private var phrase: String? = nil
    @State private var loadFailed = false
    @State private var saved = false
    @State private var revealed = false

    var body: some View {
        List {
            Section { Text("These words control your names. Anyone who has them controls your names too.").foregroundColor(.secondary) }

            if loadFailed {
                Section { Text("Could not load your names. Check your connection and reopen this screen.").foregroundColor(.orange) }
            } else if phrase == nil {
                Section { Text("You will get a recovery key when you get your first name.").foregroundColor(.secondary) }
            } else if !revealed {
                Section {
                    Button { revealed = true } label: { Label("Show recovery key", systemImage: "eye").foregroundColor(.accentColor) }
                } footer: { Text("Make sure no one can see your screen.") }
            } else {
                Section {
                    Text(phrase!).font(.body.monospaced()).fontWeight(.medium)
                    Button("Copy") { UIPasteboard.general.string = phrase }.foregroundColor(.accentColor)
                } header: {
                    Text("Your recovery key")
                } footer: {
                    Text("Write these words down and keep them somewhere safe and offline. Anyone who takes a photo of this screen can take your names.")
                }
                Section {
                    if saved { Text("You marked this as saved.").foregroundColor(.secondary) }
                    else { Button { markSaved() } label: { Label("I have saved it", systemImage: "checkmark").foregroundColor(.accentColor) } }
                }
            }

            Section("If someone else gets this key") {
                Text("They can point your name at their own profile, so people looking for you would find them instead.").foregroundColor(.red)
            }
        }
        .navigationTitle("Recovery key")
        .task {
            if let r = await apiNameRecoveryKey() { phrase = r.phrase; saved = r.saved } else { loadFailed = true }
        }
    }

    private func markSaved() { Task { if await apiNameRecoveryKeySaved() { saved = true } } }
}
