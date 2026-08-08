import SwiftUI
import SimpleXChat

// Restoring names from a recovery key. A profile that already has a key cannot
// import a different one — the core refuses it — so this says so plainly.
struct ImportRecoveryKeyView: View {
    @Environment(\.dismiss) var dismiss
    @State private var phrase = ""
    @State private var working = false
    @State private var hasWallet: Bool? = nil

    private var words: [String] { phrase.split(whereSeparator: { $0.isWhitespace }).map(String.init) }
    private var looksComplete: Bool { [12, 15, 18, 21, 24].contains(words.count) }

    var body: some View {
        List {
            if hasWallet == true {
                Section { Text("This profile already has a recovery key, so you cannot import a different one here. To use another key, set up a new profile and import it there.").foregroundColor(.secondary) }
            } else {
                Section("Recovery key") {
                    TextField("your recovery words, separated by spaces", text: $phrase, axis: .vertical)
                        .disabled(working)
                } footer: {
                    if phrase.isEmpty { Text("Enter the words in the order they were shown to you.") }
                    else if words.count == 1 { Text("1 word entered — a recovery key has 12, 15, 18, 21 or 24.").foregroundColor(looksComplete ? .secondary : .orange) }
                    else { Text(String.localizedStringWithFormat(NSLocalizedString("%d words entered — a recovery key has 12, 15, 18, 21 or 24.", comment: ""), words.count)).foregroundColor(looksComplete ? .secondary : .orange) }
                }
                Section {
                    Button(working ? "Restoring…" : "Restore") { doImport() }.disabled(working || !looksComplete)
                }
            }
        }
        .navigationTitle("Restore names from a recovery key")
        .task { hasWallet = (await apiNameStatus()).map { if case let .nameStatus(_, w, _, _) = $0 { return w } else { return false } } ?? false }
    }

    private func doImport() {
        Task {
            working = true; defer { working = false }
            guard await apiNameSetup("import \(phrase.trimmingCharacters(in: .whitespaces))") else { return }
            let found = await apiNameRescan() ?? 0
            await MainActor.run {
                let msg = found == 0 ? NSLocalizedString("Recovery key added. Open Your names to see what it holds.", comment: "alert")
                    : String.localizedStringWithFormat(NSLocalizedString("Recovery key added, and %d names were sent to you. Check Your names and names sent to you.", comment: "alert"), found)
                showAlert(NSLocalizedString("Restored", comment: "alert"), message: msg)
                dismiss()
            }
        }
    }
}
