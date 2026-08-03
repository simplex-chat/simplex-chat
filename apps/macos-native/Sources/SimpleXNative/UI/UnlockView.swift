import AppKit
import SwiftUI

struct UnlockView: View {
    @ObservedObject var model: AppModel
    @State private var passphrase = ""
    @State private var rememberPassphrase = true
    @FocusState private var passphraseFocused: Bool

    var body: some View {
        VStack(spacing: 20) {
            Image(nsImage: NSApp.applicationIconImage)
                .resizable()
                .scaledToFit()
                .frame(width: 64, height: 64)
                .accessibilityLabel("SimpleX Chat")

            VStack(spacing: 8) {
                Text("Welcome back")
                    .font(.title2.weight(.semibold))
                Text("Enter your database passphrase to unlock this SimpleX profile.")
                    .foregroundStyle(.secondary)
                    .multilineTextAlignment(.center)
            }

            if model.phase == .opening, passphrase.isEmpty {
                ProgressView("Unlocking with Mac Keychain…")
                    .controlSize(.small)
            } else {
                VStack(alignment: .leading, spacing: 8) {
                    SecureField("Database passphrase", text: $passphrase)
                        .textFieldStyle(.roundedBorder)
                        .focused($passphraseFocused)
                        .onSubmit(open)

                    if model.keychainPassphraseStorageAvailable {
                        Toggle("Remember in Mac Keychain", isOn: $rememberPassphrase)
                    }

                    if case let .locked(message?) = model.phase {
                        Label(message, systemImage: "exclamationmark.triangle.fill")
                            .foregroundStyle(.red)
                            .font(.callout)
                    }
                }

                Button("Open Chat", action: open)
                    .buttonStyle(.borderedProminent)
                    .controlSize(.regular)
                    .disabled(passphrase.isEmpty || model.phase == .opening)

                if model.phase == .opening {
                    ProgressView("Opening profile…")
                        .controlSize(.small)
                }
            }
        }
        .padding(32)
        .frame(width: 380)
        .onAppear {
            rememberPassphrase = model.keychainPassphraseStorageAvailable
            passphraseFocused = true
        }
    }

    private func open() {
        guard !passphrase.isEmpty else { return }
        model.unlock(
            passphrase: passphrase,
            rememberPassphrase: rememberPassphrase && model.keychainPassphraseStorageAvailable
        )
    }
}
