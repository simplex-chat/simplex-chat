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
    @State private var storedDBKey = kcDatabasePassword.get()
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var showRestoreDbButton = false
    @State private var starting = false

    var body: some View {
        ZStack {
            databaseErrorView().disabled(starting)
            if starting {
                ProgressView().scaleEffect(2)
            }
        }
    }

    @ViewBuilder private func databaseErrorView() -> some View {
        VStack(alignment: .leading, spacing: 16) {
            switch status {
            case let .errorNotADatabase(dbFile):
                if useKeychain && storedDBKey != nil && storedDBKey != "" {
                    titleText("Wrong database passphrase")
                    Text("Database passphrase is different from saved in the keychain.")
                    databaseKeyField(onSubmit: saveAndRunChat)
                    saveAndOpenButton()
                    fileNameText(dbFile)
                } else {
                    titleText("Encrypted database")
                    Text("Database passphrase is required to open chat.")
                    if useKeychain {
                        databaseKeyField(onSubmit: saveAndRunChat)
                        saveAndOpenButton()
                    } else {
                        databaseKeyField(onSubmit: { runChat() })
                        openChatButton()
                    }
                }
            case let .errorMigration(dbFile, migrationError):
                switch migrationError {
                case let .upgrade(upMigrations):
                    titleText("Database upgrade")
                    Button("Upgrade and open chat") { runChat(confirmMigrations: .yesUp) }
                    fileNameText(dbFile)
                    migrationsText(upMigrations.map(\.upName))
                case let .downgrade(downMigrations):
                    titleText("Database downgrade")
                    Text("Warning: you may lose some data!").bold()
                    Button("Downgrade and open chat") { runChat(confirmMigrations: .yesUpDown) }
                    fileNameText(dbFile)
                    migrationsText(downMigrations)
                case let .migrationError(mtrError):
                    titleText("Incompatible database version")
                    fileNameText(dbFile)
                    Text("Error: ") + Text(mtrErrorDescription(mtrError))
                }
            case let .errorSQL(dbFile, migrationSQLError):
                titleText("Database error")
                fileNameText(dbFile)
                Text("Error: \(migrationSQLError)")
            case .errorKeychain:
                titleText("Keychain error")
                Text("Cannot access keychain to save database password")
            case .invalidConfirmation:
                // this can only happen if incorrect parameter is passed
                Text(String("Invalid migration confirmation")).font(.title)
            case let .unknown(json):
                titleText("Database error")
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
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .onAppear() { showRestoreDbButton = shouldShowRestoreDbButton() }
    }

    private func titleText(_ s: LocalizedStringKey) -> Text {
        Text(s).font(.title)
    }

    private func fileNameText(_ f: String) -> Text {
        Text("File: \((f as NSString).lastPathComponent)")
    }

    private func migrationsText(_ ms: [String]) -> Text {
        Text("Migrations: \(ms.joined(separator: ", "))")
    }

    private func mtrErrorDescription(_ err: MTRError) -> LocalizedStringKey {
        switch err {
        case let .noDown(dbMigrations):
            return "database version is newer than the app, but no down migration for: \(dbMigrations.joined(separator: ", "))"
        case let .different(appMigration, dbMigration):
            return "different migration in the app/database: \(appMigration) / \(dbMigration)"
        }
    }

    private func databaseKeyField(onSubmit: @escaping () -> Void) -> some View {
        PassphraseField(key: $dbKey, placeholder: "Enter passphrase…", valid: validKey(dbKey), onSubmit: onSubmit)
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
        if kcDatabasePassword.set(dbKey) {
            storeDBPassphraseGroupDefault.set(true)
            initialRandomDBPassphraseGroupDefault.set(false)
        }
        runChat()
    }

    private func runChat(confirmMigrations: MigrationConfirmation? = nil) {
        starting = true
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
            runChatSync(confirmMigrations: confirmMigrations)
            starting = false
        }
    }

    private func runChatSync(confirmMigrations: MigrationConfirmation? = nil) {
        do {
            resetChatCtrl()
            try initializeChat(start: m.v3DBMigration.startChat, confirmStart: m.v3DBMigration.startChat && AppChatState.shared.value == .stopped, dbKey: useKeychain ? nil : dbKey, confirmMigrations: confirmMigrations)
            if let s = m.chatDbStatus {
                status = s
                let am = AlertManager.shared
                switch s {
                case .invalidConfirmation:
                    am.showAlert(Alert(title: Text(String("Invalid migration confirmation"))))
                case .errorNotADatabase:
                    am.showAlertMsg(
                        title: "Wrong passphrase!",
                        message: "Enter correct passphrase."
                    )
                case .errorKeychain:
                    am.showAlertMsg(title: "Keychain error")
                case let .errorSQL(_, error):
                    am.showAlert(Alert(
                        title: Text("Database error"),
                        message: Text(error)
                    ))
                case let .unknown(error):
                    am.showAlert(Alert(
                        title: Text("Unknown error"),
                        message: Text(error)
                    ))
                case .errorMigration: ()
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
