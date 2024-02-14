//
//  MigrateToAnotherDevice.swift
//  SimpleX (iOS)
//
//  Created by Avently on 14.02.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

public enum MigrationState: Equatable {
    case initial
    case chatStopInProgress
    case chatStopFailed(reason: String)
    case passphraseNotSet
    case passphraseConfirmation
    case uploadConfirmation
    case uploadProgress(uploadedKb: Int64, totalKb: Int64)
    case uploadDone(link: String)
}

struct MigrateToAnotherDevice: View {
    @EnvironmentObject var m: ChatModel
    @State private var migrationState: MigrationState = .initial
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var alert: MigrateToAnotherDeviceViewAlert?
    private let chatWasStoppedInitially: Bool = AppChatState.shared.value == .stopped

    enum MigrateToAnotherDeviceViewAlert: Identifiable {
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        VStack {
            List {
                switch migrationState {
                case .initial: EmptyView()
                case .chatStopInProgress:
                    progressView("Stopping chat")
                case let .chatStopFailed(reason):
                    chatStopFailedView(reason)
                case .passphraseNotSet:
                    passphraseNotSetView()
                case .passphraseConfirmation:
                    PassphraseConfirmationView(migrationState: $migrationState)
                case .uploadConfirmation:
                    uploadConfirmationView()
                case let .uploadProgress(uploaded, total):
                    uploadProgressView(uploaded, totalKb: total)
                case let .uploadDone(link):
                    uploadDoneView(link)
                }
            }
        }
        .onAppear {
            if case .initial = migrationState {
                if AppChatState.shared.value == .stopped {
                    migrationState = initialRandomDBPassphraseGroupDefault.get() ? .passphraseNotSet : .passphraseConfirmation
                } else {
                    migrationState = .chatStopInProgress
                    stopChat()
                }
            }
        }
        .onDisappear {
            if !chatWasStoppedInitially {
                Task {
                    try? startChat(refreshInvitations: true)
                }
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func chatStopFailedView(_ reason: String) -> some View {
        Section {
            Text(reason)
            Button(action: stopChat) {
                settingsRow("stop.fill", color: .red) {
                    Text("Stop chat")
                }
            }
        } header: {
            Text("Error stopping chat")
        } footer: {
            Text("In order to continue, chat should be stopped")
        }
    }

    private func passphraseNotSetView() -> some View {
        Section {
            Text("Database is encrypted using a random passphrase. Please set your own password before migrating.")
            NavigationLink {
                DatabaseEncryptionView(useKeychain: $useKeychain)
                    .navigationTitle("Database passphrase")
            } label: {
                settingsRow("lock.open", color: .secondary) {
                    Text("Set passphrase")
                }
            }
        } header: {
            Text("Set passphrase to export")
        }
        .onAppear {
            if !initialRandomDBPassphraseGroupDefault.get() {
                migrationState = .uploadConfirmation
            }
        }
    }

    private func uploadConfirmationView() -> some View {
        Section {
            Button(action: startUploading) {
                settingsRow("tray.and.arrow.up", color: .secondary) {
                    Text("Start uploading")
                }
            }
        } header: {
            Text("Confirm upload")
        } footer: {
            Text("Do you want to start uploading now?")
        }
    }

    private func uploadProgressView(_ uploadedKb: Int64, totalKb: Int64) -> some View {
        progressView("Uploaded \(ByteCountFormatter.string(fromByteCount: uploadedKb, countStyle: .binary)) from \(ByteCountFormatter.string(fromByteCount: totalKb, countStyle: .binary))")
    }

    private func uploadDoneView(_ link: String) -> some View {
        Section {
            SimpleXLinkQRCode(uri: link)
            shareLinkButton(link)
        } header: {
            Text("Link to uploaded archive")
        }
    }

    private func shareLinkButton(_ link: String) -> some View {
        Button {
            showShareSheet(items: [simplexChatLink(link)])
        } label: {
            Label("Share", systemImage: "square.and.arrow.up")
        }
    }

    private func stopChat() {
        Task {
            do {
                try await stopChatAsync()
                migrationState = initialRandomDBPassphraseGroupDefault.get() ? .passphraseNotSet : .passphraseConfirmation
            } catch let e {
                migrationState = .chatStopFailed(reason: e.localizedDescription)
            }
        }
    }

    private func startUploading() {
        Task {
            for i in 1...10 {
                try? await Task.sleep(nanoseconds: 100_000000)
                await MainActor.run {
                    migrationState = .uploadProgress(uploadedKb: Int64(100 * i), totalKb: 1000)
                    if i == 10 {
                        migrationState = .uploadDone(link: "https://simplex.chat")
                    }
                }
            }
        }
    }
}

private struct PassphraseConfirmationView: View {
    @Binding var migrationState: MigrationState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var currentKey: String = ""
    @State private var verifyingPassphrase: Bool = false
    @State private var alert: PassphraseConfirmationViewAlert?

    enum PassphraseConfirmationViewAlert: Identifiable {
        case wrongPassphrase(title: LocalizedStringKey = "Wrong passphrase!", message: LocalizedStringKey = "Enter correct passphrase.")
        case invalidConfirmation(title: LocalizedStringKey = "Invalid migration confirmation")
        case keychainError(_ title: LocalizedStringKey = "Keychain error")
        case databaseError(_ title: LocalizedStringKey = "Database error", message: String)
        case unknownError(_ title: LocalizedStringKey = "Unknown error", message: String)
        case error(title: LocalizedStringKey, error: LocalizedStringKey = "")

        var id: String {
            switch self {
            case .wrongPassphrase: return "wrongPassphrase"
            case .invalidConfirmation: return "invalidConfirmation"
            case .keychainError: return "keychainError"
            case let .databaseError(title, message): return "\(title) \(message)"
            case let .unknownError(title, message): return "\(title) \(message)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        ZStack(alignment: .topLeading) {
            VStack {
                Section {
                    VStack {
                        PassphraseField(key: $currentKey, placeholder: "Current passphrase…", valid: validKey(currentKey))
                        Button(action: { checkDatabasePassphrase(currentKey, $verifyingPassphrase) }) {
                            settingsRow(useKeychain ? "key" : "lock", color: .secondary) {
                                Text("Check passphrase")
                            }
                        }
                    }
                } header: {
                    Text("Enter database passphrase")
                } footer: {
                    Text("Make sure you remember database passphrase before migrating")
                }
            }
            if (verifyingPassphrase) {
                progressView("Checking passphrase…")
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .wrongPassphrase(title, message):
                return Alert(title: Text(title), message: Text(message))
            case let .invalidConfirmation(title):
                return Alert(title: Text(title))
            case let .keychainError(title):
                return Alert(title: Text(title))
            case let .databaseError(title, message):
                return Alert(title: Text(title), message: Text(message))
            case let .unknownError(title, message):
                return Alert(title: Text(title), message: Text(message))
            case let .error(title, error):
                return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    private func checkDatabasePassphrase(_ dbKey: String, _ verifyingPassphrase: Binding<Bool>) {
        verifyingPassphrase.wrappedValue = true
        defer {
            verifyingPassphrase.wrappedValue = false
        }
        do {
            resetChatCtrl()
            try initializeChat(start: false, confirmStart: false, dbKey: dbKey, confirmMigrations: nil)
            if let s = ChatModel.shared.chatDbStatus {
                let am = AlertManager.shared
                switch s {
                case .invalidConfirmation:
                    am.showAlert(Alert(title: Text(String("Invalid migration confirmation"))))
                case .errorNotADatabase:
                    alert = .wrongPassphrase()
                case .errorKeychain:
                    alert = .keychainError()
                case let .errorSQL(_, error):
                    alert = .databaseError(message: error)
                case let .unknown(error):
                    alert = .unknownError(message: error)
                case .errorMigration: ()
                case .ok:
                    migrationState = .uploadConfirmation
                }
            }
        } catch let error {
            logger.error("initializeChat \(responseError(error))")
        }
    }
}

private func progressView(_ text: LocalizedStringKey) -> some View {
    VStack {
        ProgressView().scaleEffect(2)
        Text(text)
            .padding()
    }
    .frame(maxWidth: .infinity, maxHeight: .infinity )
}

struct MigrateToAnotherDevice_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAnotherDevice()
    }
}
