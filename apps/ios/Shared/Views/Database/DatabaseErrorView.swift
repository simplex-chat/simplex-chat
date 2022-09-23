//
//  DatabaseErrorView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 04/09/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct DatabaseErrorView: View {
    @EnvironmentObject var m: ChatModel
    @State var status: DBMigrationResult
    @State private var dbKey = ""
    @State private var storedDBKey = getDatabaseKey()
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var showRestoreDbButton = false

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            switch status {
            case let .errorNotADatabase(dbFile):
                if useKeychain && storedDBKey != nil && storedDBKey != "" {
                    Text("Wrong database passphrase").font(.title)
                    Text("Database passphrase is different from saved in the keychain.")
                    databaseKeyField(onSubmit: saveAndRunChat)
                    saveAndOpenButton()
                    Text("File: \(dbFile)")
                } else {
                    Text("Encrypted database").font(.title)
                    Text("Database passphrase is required to open chat.")
                    if useKeychain {
                        databaseKeyField(onSubmit: saveAndRunChat)
                        saveAndOpenButton()
                    } else {
                        databaseKeyField(onSubmit: runChat)
                        openChatButton()
                    }
                }
            case let .error(dbFile, migrationError):
                Text("Database error")
                    .font(.title)
                Text("File: \(dbFile)")
                Text("Error: \(migrationError)")
            case .errorKeychain:
                Text("Keychain error")
                    .font(.title)
                Text("Cannot access keychain to save database password")
            case let .unknown(json):
                Text("Database error")
                    .font(.title)
                Text("Unknown database error: \(json)")
            case .ok:
                EmptyView()
            }
            if showRestoreDbButton {
                Spacer().frame(height: 10)
                Text("The attempt to change database passphrase was not completed.")
                restoreDbButton()
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .topLeading)
        .onAppear() { showRestoreDbButton = shouldShowRestoreDbButton() }
    }

    private func databaseKeyField(onSubmit: @escaping () -> Void) -> some View {
        DatabaseKeyField(key: $dbKey, placeholder: "Enter passphrase…", valid: validKey(dbKey), onSubmit: onSubmit)
    }

    private func saveAndOpenButton() -> some View {
        Button("Save passphrase and open chat") {
            saveAndRunChat()
        }
    }

    private func openChatButton() -> some View {
        Button("Open chat") {
            runChat()
        }
    }

    private func saveAndRunChat() {
        if setDatabaseKey(dbKey) {
            storeDBPassphraseGroupDefault.set(true)
            initialRandomDBPassphraseGroupDefault.set(false)
        }
        runChat()
    }

    private func runChat() {
        do {
            resetChatCtrl()
            try initializeChat(start: m.v3DBMigration.startChat, dbKey: dbKey)
            if let s = m.chatDbStatus {
                status = s
                let am = AlertManager.shared
                switch s {
                case .errorNotADatabase:
                    am.showAlertMsg(
                        title: "Wrong passphrase!",
                        message: "Enter correct passphrase."
                    )
                case .errorKeychain:
                    am.showAlertMsg(title: "Keychain error")
                case let .error(_, error):
                    am.showAlert(Alert(
                        title: Text("Database error"),
                        message: Text(error)
                    ))
                case let .unknown(error):
                    am.showAlert(Alert(
                        title: Text("Unknown error"),
                        message: Text(error)
                    ))
                case .ok: ()
                }
            }
        } catch let error {
            logger.error("initializeChat \(responseError(error))")
        }
    }

    private func shouldShowRestoreDbButton() -> Bool {
        if !encryptionStartedDefault.get() { return false }
        let startedAt = encryptionStartedAtDefault.get()
        // In case there is a small difference between saved encryptionStartedAt time and last modified timestamp on a file
        let safeDiffInTime = TimeInterval(10)
        return hasBackup(newerThan: startedAt - safeDiffInTime)
    }

    private func restoreDbButton() -> some View {
        Button() {
            AlertManager.shared.showAlert(Alert(
                title: Text("Restore database backup?"),
                message: Text("Please enter the previous password after restoring database backup. This action can not be undone."),
                primaryButton: .destructive(Text("Restore")) {
                    restoreDb()
                },
                secondaryButton: .cancel()
            ))
        } label: {
            Text("Restore database backup").foregroundColor(.red)
        }
    }

    private func restoreDb() {
        do {
            try restoreBackup()
            showRestoreDbButton = false
            encryptionStartedDefault.set(false)
        } catch {
            AlertManager.shared.showAlert(Alert(
                title: Text("Restore database error"),
                message: Text(error.localizedDescription)
            ))
        }
    }
}

struct DatabaseErrorView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseErrorView(status: .errorNotADatabase(dbFile: "simplex_v1_chat.db"))
    }
}
