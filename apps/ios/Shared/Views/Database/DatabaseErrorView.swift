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

    var body: some View {
        VStack(alignment: .leading, spacing: 16) {
            switch status {
            case let .errorNotADatabase(dbFile):
                if useKeychain && storedDBKey != nil && storedDBKey != "" {
                    Text("Wrong database passphrase").font(.title)
                    Text("Database passphrase is different from saved in the keychain.")
                    databaseKeyField(onSubmit: saveAndRunChat)
                    saveAndOpenButton()
                    Spacer()
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
                    Spacer()
                }
            case let .error(dbFile, migrationError):
                Text("Database error")
                    .font(.title)
                Text("File: \(dbFile)")
                Text("Error: \(migrationError)")
                Spacer()
            case .errorKeychain:
                Text("Keychain error")
                    .font(.title)
                Text("Cannot access keychain to save database password")
                Spacer()
            case let .unknown(json):
                Text("Database error")
                    .font(.title)
                Text("Unknown database error: \(json)")
                Spacer()
            case .ok:
                EmptyView()
            }
        }
        .padding()
        .frame(maxHeight: .infinity)
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
}

struct DatabaseErrorView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseErrorView(status: .errorNotADatabase(dbFile: "simplex_v1_chat.db"))
    }
}
