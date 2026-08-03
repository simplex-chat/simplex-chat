import SwiftUI

struct UnlockView: View {
    @ObservedObject var model: AppModel
    @State private var passphrase = ""
    @FocusState private var passphraseFocused: Bool

    var body: some View {
        VStack(spacing: 24) {
            Image(systemName: "lock.shield")
                .font(.largeTitle)
                .foregroundStyle(.secondary)
                .accessibilityHidden(true)

            VStack(spacing: 8) {
                Text("Encrypted database")
                    .font(.title2.weight(.semibold))
                Text("Enter the passphrase for your existing SimpleX desktop profile.")
                    .foregroundStyle(.secondary)
                    .multilineTextAlignment(.center)
            }

            VStack(alignment: .leading, spacing: 8) {
                SecureField("Database passphrase", text: $passphrase)
                    .textFieldStyle(.roundedBorder)
                    .focused($passphraseFocused)
                    .onSubmit(open)

                if case let .locked(message?) = model.phase {
                    Label(message, systemImage: "exclamationmark.triangle.fill")
                        .foregroundStyle(.red)
                        .font(.callout)
                }
            }

            Button("Open Chat", action: open)
                .buttonStyle(.borderedProminent)
                .controlSize(.large)
                .disabled(passphrase.isEmpty || model.phase == .opening)

            if model.phase == .opening {
                ProgressView("Opening profile…")
                    .controlSize(.small)
            }
        }
        .padding(32)
        .frame(maxWidth: 420)
        .onAppear { passphraseFocused = true }
    }

    private func open() {
        guard !passphrase.isEmpty else { return }
        model.unlock(passphrase: passphrase)
    }
}
