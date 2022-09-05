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
    case encryptDatabase
    case decryptDatabase
    case changeDatabaseKey
    case databaseEncrypted
    case databaseDecrypted
    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case .encryptDatabase: return "encryptDatabase"
        case .decryptDatabase: return "decryptDatabase"
        case .changeDatabaseKey: return "changeDatabaseKey"
        case .databaseEncrypted: return "databaseEncrypted"
        case .databaseDecrypted: return "databaseDecrypted"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct DatabaseEncryptionView: View {
    @EnvironmentObject private var m: ChatModel
    @State private var dbKey = ""
    @State private var alert: DatabaseEncryptionAlert? = nil
    @State private var progressIndicator = false
    @State private var useKeychain = false

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
            Section("") {
                if m.chatDbKey == "" {
                    databasePasswordField("Set database password…", .encryptDatabase)
                } else {
                    databasePasswordField("Change database password…", .changeDatabaseKey)
                }
                if m.chatDbKey != "" {
                    settingsRow("lock.open") {
                        Button("Remove database password") {
                            alert = .decryptDatabase
                        }
                    }
                }
                settingsRow("key") {
                    Toggle("Store password in keychain", isOn: $useKeychain)
                    .onChange(of: useKeychain) { _ in
                        if (useKeychain) {

                        } else {

                        }
                    }
                }
            }
        }
        .disabled(m.chatRunning != false)
        .alert(item: $alert) { item in databaseEncryptionAlert(item) }
    }

    private func databasePasswordField(_ placeholder: LocalizedStringKey, _ actionAlert: DatabaseEncryptionAlert) -> some View {
        settingsRow("lock") {
            TextField(placeholder, text: $dbKey)
                .disableAutocorrection(true)
                .autocapitalization(.none)
                .submitLabel(.done)
                .onSubmit {
                    if dbKey != "" { alert = actionAlert }
                }
        }
    }

    private func encryptDatabase() {
        progressIndicator = true
        Task {
            do {
                try await apiEncryptStorage(dbKey)
                if setDatabaseKey(dbKey) {
                    await MainActor.run {
                        m.chatDbKey = dbKey
                        dbKey = ""
                    }
                    await operationEnded(.databaseEncrypted)
                } else {
                    await operationEnded(.error(title: "Keychain error", error: "Error saving password to keychain"))
                }
            } catch let error {
                await operationEnded(.error(title: "Error encrypting database", error: responseError(error)))
            }
        }
    }

    private func decryptDatabase() {
        progressIndicator = true
        Task {
            do {
                try await apiDecryptStorage()
                if setDatabaseKey("") {
                    await MainActor.run {
                        m.chatDbKey = ""
                        dbKey = ""
                    }
                    await operationEnded(.databaseDecrypted)
                } else {
                    await operationEnded(.error(title: "Keychain error", error: "Error removing password from keychain"))
                }
            } catch let error {
                await operationEnded(.error(title: "Error decrypting database", error: responseError(error)))
            }
        }
    }

    private func databaseEncryptionAlert(_ alertItem: DatabaseEncryptionAlert) -> Alert {
        switch alertItem {
        case .encryptDatabase:
            return Alert(
                title: Text("Set database password?"),
                message: Text("Database will be encrypted and the password stored in the keychain. Please store password safely."),
                primaryButton: .default(Text("Encrypt")) {
                    encryptDatabase()
                },
                secondaryButton: .cancel()
            )
        case .decryptDatabase:
            return Alert(
                title: Text("Remove database password?"),
                message: Text("Database will be decrypted."),
                primaryButton: .destructive(Text("Decrypt")) {
                    decryptDatabase()
                },
                secondaryButton: .cancel()
            )
        case .changeDatabaseKey:
            return Alert(
                title: Text("Change database password?"),
                message: Text("Database encryption password will be updated and stored in the keychain. Please store password safely"),
                primaryButton: .default(Text("Update")) {
                    encryptDatabase()
                },
                secondaryButton: .cancel()
            )
        case .databaseEncrypted:
            return Alert(title: Text("Database encrypted!"))
        case .databaseDecrypted:
            return Alert(title: Text("Database decrypted!"))
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

struct DatabaseEncryptionView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseEncryptionView()
    }
}
