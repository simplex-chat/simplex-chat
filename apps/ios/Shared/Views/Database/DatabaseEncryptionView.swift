//
//  DatabaseEncryptionView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 04/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum DatabaseEncryptionAlert: Identifiable {
    case keychainRemoveKey
    case keychainKeySaved
    case encryptDatabase
    case changeDatabaseKey
    case databaseEncrypted
    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case .keychainRemoveKey: return "keychainRemoveKey"
        case .keychainKeySaved: return "keychainKeySaved"
        case .encryptDatabase: return "encryptDatabase"
        case .changeDatabaseKey: return "changeDatabaseKey"
        case .databaseEncrypted: return "databaseEncrypted"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct DatabaseEncryptionView: View {
    @EnvironmentObject private var m: ChatModel
    @State private var alert: DatabaseEncryptionAlert? = nil
    @State private var progressIndicator = false
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var initialRandomDBPassphrase = initialRandomDBPassphraseGroupDefault.get()
    @State private var currentKey = ""
    @State private var newKey = ""
    @State private var confirmNewKey = ""
    @State private var currentKeyShown = false

    var body: some View {
        ZStack {
            databaseEncryptionView()
            if progressIndicator {
                ProgressView().scaleEffect(2)
            }
        }
    }

    private func databaseEncryptionView() -> some View {
        List {
            Section {
                settingsRow("key") {
                    Toggle("Save passphrase in Keychain", isOn: $useKeychain)
                    .onChange(of: useKeychain) { _ in
                        if (useKeychain) {
                            storeDBPassphraseGroupDefault.set(true)
                        } else {
                            alert = .keychainRemoveKey
                        }
                    }
                    .disabled(initialRandomDBPassphrase)
                }

                if !initialRandomDBPassphrase && m.chatDbEncrypted == true {
                    DatabaseKeyField(key: $currentKey, placeholder: "Current passphrase…", valid: validKey(currentKey))
                }

                DatabaseKeyField(key: $newKey, placeholder: "New passphrase…", valid: validKey(newKey))
                DatabaseKeyField(key: $confirmNewKey, placeholder: "Confirm new passphrase…", valid: confirmNewKey == "" || newKey == confirmNewKey)

                settingsRow("lock.rotation") {
                    Button("Update database passphrase") {
                        encryptDatabase()
                    }
                }
                .disabled(
                    currentKey == newKey ||
                    newKey != confirmNewKey ||
                    newKey == "" ||
                    !validKey(currentKey) ||
                    !validKey(newKey)
                )
            } header: {
                Text("")
            } footer: {
                VStack(alignment: .leading, spacing: 16) {
                    if m.chatDbEncrypted == false {
                        Text("Your chat database is not encrypted - set passphrase to encrypt it.")
                    } else if useKeychain {
                        Text("iOS Keychain is used to securely store passphrase - it allows receiving push notifications.")
                        if initialRandomDBPassphrase {
                            Text("Database is encrypted using a random passphrase, you can change it.")
                        } else {
                            Text("**Please note**: you will NOT be able to recover or change passphrase if you lose it.")
                        }
                    } else {
                        Text("You have to enter passphrase every time the app starts - it is not stored on the device.")
                        Text("**Please note**: you will NOT be able to recover or change passphrase if you lose it.")
                        if  m.notificationMode == .instant && m.notificationPreview != .hidden {
                            Text("**Warning**: Instant push notifications require passphrase saved in Keychain.")
                        }
                    }
                }
                .padding(.top, 1)
                .font(.callout)
            }
        }
        .onAppear {
            if initialRandomDBPassphrase { currentKey = getDatabaseKey() ?? "" }
        }
        .disabled(m.chatRunning != false)
        .alert(item: $alert) { item in databaseEncryptionAlert(item) }
    }

    private func encryptDatabase() {
        progressIndicator = true
        Task {
            do {
                try await apiStorageEncryption(currentKey: currentKey, newKey: newKey)
                initialRandomDBPassphraseGroupDefault.set(false)
                if useKeychain {
                    if setDatabaseKey(newKey) {
                        await operationEnded(.databaseEncrypted)
                    } else {
                        await operationEnded(.error(title: "Keychain error", error: "Error saving passphrase to keychain"))
                    }
                }
                await MainActor.run {
                    m.chatDbEncrypted = true
                    initialRandomDBPassphrase = false
                    currentKey = ""
                    newKey = ""
                    confirmNewKey = ""
                }
            } catch let error {
                await operationEnded(.error(title: "Error encrypting database", error: responseError(error)))
            }
        }
    }

    private func databaseEncryptionAlert(_ alertItem: DatabaseEncryptionAlert) -> Alert {
        switch alertItem {
        case .keychainRemoveKey:
            return Alert(
                title: Text("Remove passphrase from keychain?"),
                message: Text("Instant push notifications will be hidden!\nPlease store passphrase securely, you will NOT be able to access chat if you lose it."),
                primaryButton: .destructive(Text("Remove")) {
                    if removeDatabaseKey() {
                        storeDBPassphraseGroupDefault.set(false)
                    } else {
                        alert = .error(title: "Keychain error", error: "Failed to remove passphrase")
                    }
                },
                secondaryButton: .cancel() {
                    useKeychain = true
                }
            )
        case .keychainKeySaved:
            return Alert(
                title: Text("Passphrase will be saved"),
                message: Text("After you update the database passphrase it will be saved in the keychain - this is required for push notifications to show messages.")
            )
        case .encryptDatabase:
            return Alert(
                title: Text("Encrypt database?"),
                message: Text("Database will be encrypted and the passphrase stored in the keychain. Please store password safely."),
                primaryButton: .default(Text("Encrypt")) {
                    encryptDatabase()
                },
                secondaryButton: .cancel()
            )
        case .changeDatabaseKey:
            return Alert(
                title: Text("Change database passphrase?"),
                message:
                    useKeychain
                    ? Text("Database encryption passphrase will be updated and stored in the keychain. Please store password safely, if you lose it, you will not be able to change passphrase")
                    : Text(""),
                primaryButton: .default(Text("Update")) {
                    encryptDatabase()
                },
                secondaryButton: .cancel()
            )
        case .databaseEncrypted:
            return Alert(title: Text("Database encrypted!"))
        case let .error(title, error):
            return Alert(title: Text(title), message: Text("\(error)"))
        }
    }

    private func operationEnded(_ dbAlert: DatabaseEncryptionAlert) async {
        await MainActor.run {
            m.chatDbChanged = true
            progressIndicator = false
            alert = dbAlert
        }
    }
}


struct DatabaseKeyField: View {
    @Binding var key: String
    var placeholder: LocalizedStringKey
    var valid: Bool
    @State private var showKey = false

    var body: some View {
        ZStack(alignment: .leading) {
            Image(systemName: valid ? (showKey ? "eye.slash" : "eye") : "exclamationmark.circle")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
                .foregroundColor(valid ? .secondary : .red)
                .onTapGesture { showKey = !showKey }
            textField()
                .disableAutocorrection(true)
                .autocapitalization(.none)
                .submitLabel(.done)
                .padding(.leading, 36)
        }
    }

    @ViewBuilder func textField() -> some View {
        if showKey {
            TextField(placeholder, text: $key)
        } else {
            SecureField(placeholder, text: $key)
        }
    }
}

func validKey(_ s: String) -> Bool {
    for c in s { if c.isWhitespace || !c.isASCII { return false } }
    return true
}

struct DatabaseEncryptionView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseEncryptionView()
    }
}
