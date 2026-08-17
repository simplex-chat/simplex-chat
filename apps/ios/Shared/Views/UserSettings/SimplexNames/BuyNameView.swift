import SwiftUI
import SimpleXChat

// Buying a name: choose it, see availability and price, pay. Payment goes
// through NamePayment, the only place that knows about the store.
struct BuyNameView: View {
    @Environment(\.dismiss) var dismiss
    @State private var label = ""
    @State private var years = 1
    @State private var quote: (available: Bool, priceCents: Int)? = nil
    @State private var checking = false
    @State private var buying = false
    @State private var pointAtMe = true

    private var myLink: String? {
        ChatModel.shared.userAddress?.connLinkContact.simplexChatUri()
    }
    private var validLabel: Bool {
        let l = label.trimmingCharacters(in: .whitespaces).lowercased()
        return l.count >= 6 && l.range(of: "^[a-z0-9]+(-[a-z0-9]+)*$", options: .regularExpression) != nil
    }

    var body: some View {
        List {
            Section {
                HStack {
                    TextField("yourname", text: $label)
                        .autocorrectionDisabled()
                        .textInputAutocapitalization(.never)
                    Text(".simplex").foregroundColor(.secondary)
                    statusIcon
                }
            } header: {
                Text("Choose a name")
            } footer: { statusFooter }

            Section {
                Stepper(value: $years, in: 1...10) {
                    Text(years == 1 ? "1 year" : "\(years) years")
                }
            } header: {
                Text("How long")
            } footer: {
                Text("You pay once for the whole period. Nothing renews automatically, and the app will not remind you — note the end date.")
            }

            Section {
                Toggle("Point this name at my profile", isOn: $pointAtMe)
                    .disabled(myLink == nil)
            } footer: {
                if myLink == nil {
                    Text("You do not have a SimpleX address yet, so there is nothing for the name to point at. Create one first, or buy the name to give away.").foregroundColor(.orange)
                } else if pointAtMe {
                    Text("People using the name will reach this profile.")
                } else {
                    Text("The name will point nowhere. Buy it this way if it is for someone else — you can give it to them afterwards.")
                }
            }

            if let q = quote, q.available {
                Section {
                    Button {
                        buy()
                    } label: {
                        Text(buying ? "Registering…" : "Buy this name — \(priceText(q.priceCents))")
                    }
                    .disabled(buying)
                }
            }

            if !NamePayment.isLive {
                Section("Development build") {
                    Text("Test build: no payment is taken and nothing is registered. Names disappear when the app closes.")
                        .foregroundColor(.orange)
                }
            }
        }
        .navigationTitle("Buy a name")
        .onChange(of: label) { _ in Task { await requote() } }
        .onAppear { if myLink == nil { pointAtMe = false } }
    }

    @ViewBuilder private var statusIcon: some View {
        if !validLabel { Image(systemName: "exclamationmark.circle").foregroundColor(.orange) }
        else if checking { ProgressView() }
        else if let q = quote { Image(systemName: q.available ? "checkmark.circle" : "xmark.circle").foregroundColor(q.available ? .green : .red) }
        else { Image(systemName: "exclamationmark.circle").foregroundColor(.orange) }
    }

    @ViewBuilder private var statusFooter: some View {
        let l = label.trimmingCharacters(in: .whitespaces).lowercased()
        if l.isEmpty { Text("Letters, digits and hyphens. At least 6 characters.") }
        else if l.range(of: "^[a-z0-9]+(-[a-z0-9]+)*$", options: .regularExpression) == nil { Text("Use letters, digits and single hyphens between them.").foregroundColor(.orange) }
        else if l.count < 6 { Text("A bit short — names need at least 6 characters.").foregroundColor(.orange) }
        else if checking { Text("Checking…") }
        else if quote == nil { Text("Could not check that name — it may be reserved, or the service is unreachable.").foregroundColor(.orange) }
        else if let q = quote, q.available { Text("Available — \(priceText(q.priceCents))").foregroundColor(.green) }
        else { Text("That name is already taken.").foregroundColor(.red) }
    }

    private func requote() async {
        quote = nil
        guard validLabel else { return }
        checking = true
        try? await Task.sleep(nanoseconds: 400_000_000)
        quote = await apiNameQuote(label.trimmingCharacters(in: .whitespaces).lowercased())
        checking = false
    }

    private func buy() {
        let l = label.trimmingCharacters(in: .whitespaces).lowercased()
        Task {
            buying = true
            defer { buying = false }
            let key = "buy:\(l):\(years)"
            guard let token = NamePayment.purchaseFor(key, years: years) else { return }
            let link = pointAtMe ? myLink : nil
            if let fqdn = await apiNameBuy(l, years: years, payment: token, link: link) {
                NamePayment.spent(key)
                if pointAtMe { _ = try? await apiSetUserDomain(fqdn) }
                await MainActor.run {
                    showAlert(NSLocalizedString("The name is yours", comment: "alert"), message: String.localizedStringWithFormat(NSLocalizedString("%@ now belongs to you.", comment: "alert"), fqdn))
                    dismiss()
                }
            }
        }
    }

    private func priceText(_ cents: Int) -> String {
        let total = cents * years
        return String(format: "$%d.%02d", total / 100, total % 100)
    }
}
