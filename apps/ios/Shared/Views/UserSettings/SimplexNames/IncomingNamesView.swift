import SwiftUI
import SimpleXChat

// Names other people have sent to this profile. Nothing is claimed or shown as
// yours until you accept; declining writes nothing and tells no one.
struct IncomingNamesView: View {
    @State private var incoming: [IncomingName] = []
    @State private var canReceive = true
    @State private var loading = true

    var body: some View {
        List {
            if !canReceive && !loading {
                Section {
                    Text("No one can send you a name yet.").foregroundColor(.secondary)
                    Button("Let people send me names") { enableReceiving() }
                } footer: {
                    Text("Turn this on so your contacts can send you names. Your profile starts carrying a marker that lets them send you a name and nothing else.")
                }
            } else if incoming.isEmpty {
                Section {
                    Text(loading ? "Checking…" : "No names have been sent to you").foregroundColor(.secondary)
                } footer: {
                    Text("Your profile already carries everything your contacts need to send you a name — there is nothing to set up. Anything sent to you appears here, and nothing is added to your profile until you accept it.")
                }
            } else {
                Section {
                    ForEach(incoming, id: \.inAddress) { item in
                        VStack(alignment: .leading, spacing: 8) {
                            Text(item.inNames.joined(separator: ", ")).fontWeight(.medium)
                            HStack(spacing: 16) {
                                Button("Accept") { accept(item) }.foregroundColor(.accentColor)
                                Button("Decline", role: .destructive) { decline(item) }.foregroundColor(.red)
                            }
                        }
                    }
                } header: {
                    Text("Waiting for your decision")
                } footer: {
                    Text("Accepting adds the name to your names. Declining removes it from this list and tells no one.")
                }
            }

            if canReceive {
                Section {
                    Button { rescan() } label: { Label("Check for names sent to you", systemImage: "magnifyingglass") }
                        .disabled(loading)
                } footer: {
                    Text("Names are normally delivered by message. Check here if you restored this device from your recovery key, or think you missed one.")
                }
            }
        }
        .navigationTitle("Names sent to you")
        .task { await load() }
    }

    private func load() async {
        canReceive = (await apiNameStatus()).map { if case let .nameStatus(_, w, _, _) = $0 { return w } else { return false } } ?? false
        incoming = canReceive ? (await apiNameIncoming() ?? []) : []
        ChatModel.shared.namesWaiting = incoming.count
        loading = false
    }

    private func enableReceiving() {
        Task {
            loading = true
            let addr = await apiNameAddress()
            await load()
            if addr == nil {
                await MainActor.run { showAlert(NSLocalizedString("Could not set up names for this profile", comment: "alert")) }
            }
        }
    }

    private func rescan() {
        Task {
            loading = true
            let found = await apiNameRescan()
            await load()
            if let f = found {
                let text = f == 0 ? NSLocalizedString("No new names found.", comment: "alert")
                    : f == 1 ? NSLocalizedString("Found a name sent to you — see the list above.", comment: "alert")
                    : String.localizedStringWithFormat(NSLocalizedString("Found %d names sent to you — see the list above.", comment: "alert"), f)
                await MainActor.run { showAlert(NSLocalizedString("Check complete", comment: "alert"), message: text) }
            }
        }
    }

    private func accept(_ item: IncomingName) {
        showAlert(
            NSLocalizedString("Accept these names?", comment: "alert"),
            message: String.localizedStringWithFormat(NSLocalizedString("%@ will be added to your names. You can choose to show it on your profile afterwards.", comment: "alert"), item.inNames.joined(separator: ", ")),
            actions: {[
                UIAlertAction(title: NSLocalizedString("Accept", comment: ""), style: .default) { _ in
                    Task { _ = await apiNameAccept(item.inAddress); await load() }
                },
                cancelAlertAction
            ]}
        )
    }

    private func decline(_ item: IncomingName) {
        showAlert(
            NSLocalizedString("Decline this name?", comment: "alert"),
            message: NSLocalizedString("No one is told. The name is already yours and stays where it is, but it will not be shown here again and cannot be brought back from the app. Only decline if you do not want it.", comment: "alert"),
            actions: {[
                UIAlertAction(title: NSLocalizedString("Decline", comment: ""), style: .destructive) { _ in
                    Task { _ = await apiNameDecline(item.inAddress); await load() }
                },
                cancelAlertAction
            ]}
        )
    }
}
