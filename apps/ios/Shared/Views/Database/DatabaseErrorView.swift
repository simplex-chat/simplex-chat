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
    @EnvironmentObject var theme: AppTheme
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

    private func databaseErrorView() -> some View {
        VStack(alignment: .center, spacing: 20) {
            switch status {
            case let .errorNotADatabase(dbFile):
                if useKeychain && storedDBKey != nil && storedDBKey != "" {
                    titleText("Wrong database passphrase")
                    Text("Database passphrase is different from saved in the keychain.")
                        .font(.callout)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .padding(.horizontal, 25)

                    databaseKeyField(onSubmit: saveAndRunChat)
                    Spacer()
                    VStack(spacing: 10) {
                        saveAndOpenButton()
                        fileNameText(dbFile)
                    }
                } else {
                    titleText("Encrypted database")
                    Text("Database passphrase is required to open chat.")
                        .font(.callout)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .padding(.horizontal, 25)
                        .padding(.bottom, 5)

                    if useKeychain {
                        databaseKeyField(onSubmit: saveAndRunChat)
                        Spacer()
                        saveAndOpenButton()
                    } else {
                        databaseKeyField(onSubmit: { runChat() })
                        Spacer()
                        openChatButton()
                    }
                }
            case let .errorMigration(dbFile, migrationError):
                switch migrationError {
                case let .upgrade(upMigrations):
                    titleText("Database upgrade")
                    migrationsText(upMigrations.map(\.upName))
                    Spacer()
                    VStack(spacing: 10) {
                        Button("Upgrade and open chat") {
                            runChat(confirmMigrations: .yesUp)
                        }.buttonStyle(OnboardingButtonStyle(isDisabled: false))
                        fileNameText(dbFile)
                    }
                case let .downgrade(downMigrations):
                    titleText("Database downgrade")
                    Text("Warning: you may lose some data!")
                        .bold()
                        .padding(.horizontal, 25)
                        .multilineTextAlignment(.center)

                    migrationsText(downMigrations)
                    Spacer()
                    VStack(spacing: 10) {
                        Button("Downgrade and open chat") {
                            runChat(confirmMigrations: .yesUpDown)
                        }.buttonStyle(OnboardingButtonStyle(isDisabled: false))
                        fileNameText(dbFile)
                    }
                case let .migrationError(mtrError):
                    titleText("Incompatible database version")
                    fileNameText(dbFile, font: .callout)
                    errorView(Text(mtrErrorDescription(mtrError)))
                }
            case let .errorSQL(dbFile, migrationSQLError):
                titleText("Database error")
                fileNameText(dbFile, font: .callout)
                errorView(Text("Error: \(migrationSQLError)"))
            case .errorKeychain:
                titleText("Keychain error")
                errorView(Text("Cannot access keychain to save database password"))
            case .invalidConfirmation:
                // this can only happen if incorrect parameter is passed
                titleText("Invalid migration confirmation")
                errorView()

            case let .unknown(json):
                titleText("Database error")
                errorView(Text("Unknown database error: \(json)"))
            case .ok:
                EmptyView()
            }
            if showRestoreDbButton {
                Spacer()
                Text("The attempt to change database passphrase was not completed.")
                    .multilineTextAlignment(.center)
                    .padding(.horizontal, 25)
                    .font(.footnote)

                restoreDbButton()
            }
        }
        .padding(.horizontal, 25)
        .padding(.top, 75)
        .padding(.bottom, 25)
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .onAppear() { showRestoreDbButton = shouldShowRestoreDbButton() }
    }

    private func titleText(_ s: LocalizedStringKey) -> some View {
        Text(s).font(.largeTitle).bold().multilineTextAlignment(.center)
    }

    private func fileNameText(_ f: String, font: Font = .caption) -> Text {
        Text("File: \((f as NSString).lastPathComponent)").font(font)
    }

    private func migrationsText(_ ms: [String]) -> some View {
        (Text("Migrations:").font(.subheadline) + textNewLine + Text(ms.joined(separator: "\n")).font(.caption))
            .multilineTextAlignment(.center)
            .padding(.horizontal, 25)
    }

    private func databaseKeyField(onSubmit: @escaping () -> Void) -> some View {
        PassphraseField(key: $dbKey, placeholder: "Enter passphrase…", valid: validKey(dbKey), onSubmit: onSubmit)
            .padding(.vertical, 10)
            .padding(.horizontal)
            .background(
                RoundedRectangle(cornerRadius: 10, style: .continuous)
                    .fill(Color(uiColor: .tertiarySystemFill))
            )
    }

    private func saveAndOpenButton() -> some View {
        Button("Save passphrase and open chat") {
            saveAndRunChat()
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
    }

    private func openChatButton() -> some View {
        Button("Open chat") {
            runChat()
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
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
            Text("Restore database backup")
        }
        .buttonStyle(OnboardingButtonStyle(isDisabled: false))
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
    
    private func errorView(_ s: Text? = nil) -> some View {
        VStack(spacing: 35) {
            Image(systemName: "exclamationmark.triangle.fill")
                .resizable()
                .frame(width: 50, height: 50)
                .foregroundColor(.red)
            
            if let text = s {
                text
                    .multilineTextAlignment(.center)
                    .font(.footnote)
            }
        }
        .padding()
        .frame(maxWidth: .infinity)
    }
}

struct DatabaseErrorView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseErrorView(status: .errorNotADatabase(dbFile: "simplex_v1_chat.db"))
    }
}
