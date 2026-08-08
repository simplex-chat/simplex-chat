import SwiftUI
import SimpleXChat

// One name: when it expires, where it points, and what can be done with it.
struct NameDetailView: View {
    let fqdn: String
    @State private var info: ChatResponse2? = nil
    @State private var loadFailed = false
    @State private var busy = false

    private var myLink: String? { ChatModel.shared.userAddress?.connLinkContact.simplexChatUri }

    var body: some View {
        List {
            if case let .nameInfo(_, _, _, contact, channel, expires, credits) = info {
                let links = contact + channel
                let pointsAtMe = myLink != nil && links.contains(myLink!)
                if expiryDays(expires) < 0 {
                    Section { Text("This name has expired. You have 90 days to extend it and keep it; after that anyone can take it.").foregroundColor(.red) }
                }
                Section("Points to") {
                    if links.isEmpty { Text("This name does not point anywhere yet.").foregroundColor(.orange) }
                    else { ForEach(links, id: \.self) { Text($0).font(.footnote.monospaced()).foregroundColor(.secondary) } }
                    if !pointsAtMe && myLink != nil {
                        Button(busy ? "Updating…" : "Point at this profile") { repoint() }.disabled(busy)
                    } else if pointsAtMe {
                        Text("This name points at this profile.").foregroundColor(.green)
                    }
                }
                Section("How long you have it") {
                    HStack { Text("Expires"); Spacer(); Text(expiryText(expires)).foregroundColor(expiryColor(expires)) }
                    HStack { Text("Changes left"); Spacer(); Text("\(credits)").foregroundColor(.secondary) }
                } footer: {
                    Text("Pointing this name somewhere new uses one change. Extending the name adds 10 more.\nNothing renews automatically. After it expires, anyone can take the name.")
                }
                Section {
                    Button(busy ? "Extending…" : "Extend this name") { renew() }.disabled(busy).foregroundColor(.accentColor)
                    Button { addExpiryReminder(expires) } label: { Text("Add expiry to my calendar").foregroundColor(.accentColor) }
                    NavigationLink { GiveNameView(fqdn: fqdn) } label: { Label("Give this name away", systemImage: "person.crop.circle").foregroundColor(.accentColor) }
                }
            } else {
                Section { Text(loadFailed ? "Could not load your names. Check your connection and reopen this screen." : "Checking…").foregroundColor(loadFailed ? .orange : .secondary) }
            }
        }
        .navigationTitle(fqdn)
        .task { await reload() }
    }

    private func reload() async {
        let r = await apiNameInfo(fqdn)
        info = r
        loadFailed = r == nil
    }

    private func repoint() {
        guard let link = myLink else { return }
        showAlert(NSLocalizedString("Point at this profile?", comment: "alert"),
                  message: String.localizedStringWithFormat(NSLocalizedString("%@ will point at this profile, so people using the name reach you. Your address becomes part of a public record, and this uses one of your remaining changes.", comment: "alert"), fqdn),
                  actions: {[
                    UIAlertAction(title: NSLocalizedString("Point at this profile", comment: ""), style: .default) { _ in
                        Task { busy = true; defer { busy = false }; if await apiNameSetLink(fqdn, link: link) { await reload() } }
                    }, cancelAlertAction
                  ]})
    }

    private func renew() {
        showAlert(NSLocalizedString("Extend this name?", comment: "alert"),
                  message: String.localizedStringWithFormat(NSLocalizedString("%@ will be yours for another year.", comment: "alert"), fqdn),
                  actions: {[
                    UIAlertAction(title: NSLocalizedString("Extend this name", comment: ""), style: .default) { _ in
                        Task {
                            busy = true; defer { busy = false }
                            let key = "renew:\(ChatModel.shared.remoteHostId ?? -1):\(fqdn)"
                            guard let token = NamePayment.purchaseFor(key, years: 1) else { return }
                            if let r = await apiNameRenew(fqdn, years: 1, payment: token) {
                                NamePayment.spent(key); await reload()
                                let msg = r.reRegistered
                                    ? String.localizedStringWithFormat(NSLocalizedString("%@ had expired, so it was registered again. It is yours for another year.", comment: "alert"), fqdn)
                                    : String.localizedStringWithFormat(NSLocalizedString("%@ is yours for another year.", comment: "alert"), fqdn)
                                await MainActor.run { showAlert(NSLocalizedString("Extended", comment: "alert"), message: msg) }
                            }
                        }
                    }, cancelAlertAction
                  ]})
    }

    private func expiryDays(_ e: Int) -> Int { Int(floor(Double(Int64(e) - Int64(Date().timeIntervalSince1970)) / 86400)) }
    private func expiryText(_ e: Int) -> String {
        let d = expiryDays(e)
        if d < 0 { return NSLocalizedString("Expired", comment: "") }
        if d == 0 { return NSLocalizedString("Today", comment: "") }
        if d == 1 { return NSLocalizedString("Tomorrow", comment: "") }
        if d < 30 { return String.localizedStringWithFormat(NSLocalizedString("In %d days", comment: ""), d) }
        let f = DateFormatter(); f.dateStyle = .medium
        return f.string(from: Date(timeIntervalSince1970: TimeInterval(e)))
    }
    private func expiryColor(_ e: Int) -> Color { let d = expiryDays(e); return d < 0 ? .red : d < 30 ? .orange : .primary }
}
