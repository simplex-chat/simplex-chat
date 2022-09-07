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
    var status: DBMigrationResult
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
                    DatabaseKeyField(key: $dbKey, placeholder: "Enter passphrase…", valid: validKey(dbKey))
                    saveAndOpenButton()
                    Spacer()
                    Text("File: \(dbFile)")
                } else {
                    Text("Encrypted database").font(.title)
                    Text("Database passphrase is required to open chat.")
                    DatabaseKeyField(key: $dbKey, placeholder: "Enter passphrase…", valid: validKey(dbKey))
                    if useKeychain {
                        saveAndOpenButton()
                    } else {
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
        .frame(maxHeight: .infinity)    }

    private func saveAndOpenButton() -> some View {
        Button("Save passphrase and open chat") {
            if setDatabaseKey(dbKey) {
                storeDBPassphraseGroupDefault.set(true)
                initialRandomDBPassphraseGroupDefault.set(false)
            }
            do {
                try initializeChat(start: m.v3DBMigration.startChat, dbKey: dbKey)
            } catch let error {
                logger.error("initializeChat \(responseError(error))")
            }
        }
    }

    private func openChatButton() -> some View {
        Button("Open chat") {
            do {
                try initializeChat(start: m.v3DBMigration.startChat, dbKey: dbKey)
            } catch let error {
                logger.error("initializeChat \(responseError(error))")
            }
        }
    }
}

struct DatabaseErrorView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseErrorView(status: .errorNotADatabase(dbFile: "simplex_v1_chat.db"))
    }
}
