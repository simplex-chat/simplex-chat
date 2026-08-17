import SwiftUI
import SimpleXChat

// Entry point for everything about this profile's SimpleX names: names it holds,
// names sent to it, and the recovery key that survives losing the device.
struct SimplexNamesView: View {
    @State private var names: [OwnedName]? = nil
    @State private var incomingCount = 0
    @State private var hasKey = false
    @State private var keySaved = true
    @State private var anySeed = false
    @State private var loadFailed = false

    var body: some View {
        List {
            Section(NSLocalizedString("Your names", comment: "section")) {
                if loadFailed {
                    Text("Could not load your names. Check your connection and reopen this screen.")
                        .foregroundColor(.orange)
                } else if names == nil {
                    Text("Checking…").foregroundColor(.secondary)
                } else if names!.isEmpty {
                    Text("You do not have a name yet").foregroundColor(.secondary)
                } else {
                    ForEach(names!, id: \.onFqdn) { n in
                        let expired = Int64(n.onExpires) - Int64(Date().timeIntervalSince1970) < 0
                        NavigationLink {
                            NameDetailView(fqdn: n.onFqdn)
                        } label: {
                            HStack {
                                Text(n.onFqdn).foregroundColor(expired ? .secondary : .primary)
                                if expired {
                                    Spacer()
                                    Text("expired").foregroundColor(.secondary)
                                }
                            }
                        }
                    }
                }
            } footer: {
                Text("A SimpleX name lets people find you without sharing a link. If you save your recovery key, you can point the name at a new profile after losing this device.")
            }

            Section {
                NavigationLink { BuyNameView() } label: {
                    Label("Buy a name", systemImage: "plus").foregroundColor(.accentColor)
                }
                NavigationLink { IncomingNamesView() } label: {
                    HStack {
                        Label("Names sent to you", systemImage: "envelope")
                        if incomingCount > 0 {
                            Spacer()
                            Text("\(incomingCount)").foregroundColor(.accentColor)
                        }
                    }
                }
                if !hasKey {
                    NavigationLink { WalletSetupView(hasOtherSeed: anySeed) } label: {
                        Label("Set up names for this profile", systemImage: "plus").foregroundColor(.accentColor)
                    }
                }
                NavigationLink { ImportRecoveryKeyView() } label: {
                    Label("Restore names from a recovery key", systemImage: "square.and.arrow.down")
                }
                if hasKey {
                    NavigationLink { NameRecoveryKeyView() } label: {
                        Label("Recovery key", systemImage: "key")
                            .foregroundColor(keySaved ? .primary : .orange)
                    }
                }
            }
        }
        .navigationTitle("SimpleX names")
        .task { await load() }
    }

    private func load() async {
        // Status first, and nothing else until it confirms a wallet: every other
        // call creates the wallet as a side effect, so asking for the list first
        // would give a wallet to anyone who merely opened this screen.
        let st = await apiNameStatus()
        loadFailed = st == nil
        if case let .nameStatus(_, hasWallet, saved, seed) = st {
            hasKey = hasWallet
            keySaved = saved
            anySeed = seed
            if hasWallet {
                names = await apiNameList()
                incomingCount = (await apiNameIncoming())?.count ?? 0
                ChatModel.shared.namesWaiting = incomingCount
            } else {
                names = []
            }
        }
    }
}
