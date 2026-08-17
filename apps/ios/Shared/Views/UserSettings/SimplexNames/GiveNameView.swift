import SwiftUI
import SimpleXChat

// Giving a name to a contact. Only contacts whose profile carries a receiving
// address can be chosen; the name is sent to a one-time address derived from it.
struct GiveNameView: View {
    @Environment(\.dismiss) var dismiss
    let fqdn: String
    private var label: String { fqdn.replacingOccurrences(of: ".simplex", with: "") }

    private var candidates: [Contact] {
        ChatModel.shared.chats.compactMap { if case let .direct(c) = $0.chatInfo { return c } else { return nil } }
    }
    private var canReceive: [Contact] { candidates.filter { $0.profile.metaAddress != nil } }
    private var cannotReceive: Int { candidates.count - canReceive.count }

    var body: some View {
        List {
            Section("Give to") {
                if canReceive.isEmpty {
                    Text("None of your contacts can receive a name yet.").foregroundColor(.secondary)
                } else {
                    ForEach(canReceive) { ct in
                        Button(ct.profile.profileViewName) { confirm(ct) }
                    }
                }
            } footer: {
                if cannotReceive == 1 { Text("One of your contacts cannot receive names yet — they need a newer app version, or to turn receiving on.") }
                else if cannotReceive > 0 { Text(String.localizedStringWithFormat(NSLocalizedString("%d of your contacts cannot receive names yet — they need a newer app version, or to turn receiving on.", comment: ""), cannotReceive)) }
                else { Text("Only contacts who can receive names are listed.") }
            }
        }
        .navigationTitle("Give this name away")
    }

    private func confirm(_ ct: Contact) {
        showAlert(NSLocalizedString("Give the name away?", comment: "alert"),
                  message: String.localizedStringWithFormat(NSLocalizedString("%1$@ will belong to %2$@. Only they will be able to change or move it after this.", comment: "alert"), fqdn, ct.profile.profileViewName),
                  actions: {[
                    UIAlertAction(title: NSLocalizedString("Give it away", comment: ""), style: .destructive) { _ in
                        Task {
                            if await apiNameGift(label, recipient: "@\(ct.localDisplayName)") {
                                if let u = try? await apiGetActiveUser() { await MainActor.run { ChatModel.shared.updateUser(u) } }
                                await MainActor.run {
                                    showAlert(NSLocalizedString("Name given away", comment: "alert"), message: String.localizedStringWithFormat(NSLocalizedString("%1$@ now belongs to %2$@. It appears under names given to them.", comment: "alert"), fqdn, ct.profile.profileViewName))
                                    dismiss()
                                }
                            }
                        }
                    }, cancelAlertAction
                  ]})
    }
}
