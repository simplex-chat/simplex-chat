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
    case archiving
    case uploadProgress(uploadedBytes: Int64, totalBytes: Int64, archivePath: URL)
    case linkCreation(totalBytes: Int64, archivePath: URL)
    case linkShown(link: String, archivePath: URL)
}

struct MigrateToAnotherDevice: View {
    @EnvironmentObject var m: ChatModel
    @State private var migrationState: MigrationState = .initial
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var initialRandomDBPassphrase = initialRandomDBPassphraseGroupDefault.get()
    @State private var alert: MigrateToAnotherDeviceViewAlert?
    @State private var confirmPassphraseAlert: PassphraseConfirmationView.PassphraseConfirmationViewAlert?
    private let chatWasStoppedInitially: Bool = AppChatState.shared.value == .stopped

    enum MigrateToAnotherDeviceViewAlert: Identifiable {
        case error(title: LocalizedStringKey, error: String = "")

        var id: String {
            switch self {
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        VStack {
            switch migrationState {
            case .initial: EmptyView()
            case .chatStopInProgress:
                chatStopInProgressView()
            case let .chatStopFailed(reason):
                chatStopFailedView(reason)
            case .passphraseNotSet:
                passphraseNotSetView()
            case .passphraseConfirmation:
                PassphraseConfirmationView(migrationState: $migrationState, alert: $confirmPassphraseAlert)
            case .uploadConfirmation:
                uploadConfirmationView()
            case .archiving:
                archivingView()
            case let .uploadProgress(uploaded, total, archivePath):
                uploadProgressView(uploaded, totalBytes: total, archivePath)
            case let .linkCreation(totalBytes, archivePath):
                linkCreationView(totalBytes, archivePath)
            case let .linkShown(link, archivePath):
                linkView(link, archivePath)
            }
        }
        .onAppear {
            if case .initial = migrationState {
                if AppChatState.shared.value == .stopped {
                    migrationState = initialRandomDBPassphrase ? .passphraseNotSet : .passphraseConfirmation
                } else {
                    migrationState = .chatStopInProgress
                    stopChat()
                }
            }
        }
        .onDisappear {
            if case .linkShown = migrationState {} else if !chatWasStoppedInitially {
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
        .alert(item: $confirmPassphraseAlert) { alert in
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

    private func chatStopInProgressView() -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Stopping chat")
                }
            }
            progressView()
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
        DatabaseEncryptionView(useKeychain: $useKeychain, migration: true)
            .onChange(of: initialRandomDBPassphrase) { initial in
                if !initial {
                    migrationState = .uploadConfirmation
                }
            }
    }

    private func uploadConfirmationView() -> some View {
        List {
            Section {
                Text("All your contacts, conversations and files will be archived and uploaded as encrypted file to configured XFTP relays")
                Button(action: { migrationState = .archiving }) {
                    settingsRow("tray.and.arrow.up", color: .secondary) {
                        Text("Archive and upload")
                    }
                }
            } header: {
                Text("Confirm upload")
            }
        }
    }

    private func archivingView() -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Archiving database…")
                }
            }
            progressView()
        }
        .onAppear {
            exportArchive()
        }
    }

    private func uploadProgressView(_ uploadedBytes: Int64, totalBytes: Int64, _ archivePath: URL) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Uploading archive…")
                }
            }
            let ratio = Float(uploadedBytes) / Float(totalBytes)
            largeProgressView(ratio, "\(Int(ratio * 100))%\n\(ByteCountFormatter.string(fromByteCount: totalBytes, countStyle: .binary)) uploaded")
        }
        .onAppear {
            startUploading(totalBytes, archivePath)
        }
    }

    private func linkCreationView(_ totalBytes: Int64, _ archivePath: URL) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Creating archive link…")
                }
            }
            largeProgressView(1, "\(ByteCountFormatter.string(fromByteCount: totalBytes, countStyle: .binary))")
        }
        .onAppear {
            createLink(archivePath)
        }
    }

    private func linkView(_ link: String, _ archivePath: URL) -> some View {
        List {
            Section {
                SimpleXLinkQRCode(uri: link)
                shareLinkButton(link)
            } header: {
                Text("Link to uploaded archive")
            }
        }
    }

    private func shareLinkButton(_ link: String) -> some View {
        Button {
            showShareSheet(items: [simplexChatLink(link)])
        } label: {
            Label("Share", systemImage: "square.and.arrow.up")
        }
    }

    private func largeProgressView(_ value: Float, _ text: LocalizedStringKey) -> some View {
        ZStack {
            Text(text)
                .font(.title3)
                .multilineTextAlignment(.center)

            Circle()
                .trim(from: 0, to: CGFloat(value))
                .stroke(
                    Color.accentColor,
                    style: StrokeStyle(lineWidth: 5)
                )
                .rotationEffect(.degrees(-90))
                .animation(.linear, value: value)
                .frame(width: 250, height: 250)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity )
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

    private func exportArchive() {
        Task {
            do {
                let archivePath = try await exportChatArchive()
                if let attrs = try? FileManager.default.attributesOfItem(atPath: archivePath.path),
                    let totalBytes = attrs[.size] as? Int64 {
                    await MainActor.run {
                        migrationState = .uploadProgress(uploadedBytes: 0, totalBytes: totalBytes, archivePath: archivePath)
                    }
                } else {
                    await MainActor.run {
                        alert = .error(title: "Exported file doesn't exist")
                        migrationState = .uploadConfirmation
                    }
                }
            } catch let error {
                await MainActor.run {
                    alert = .error(title: "Error exporting chat database", error: responseError(error))
                    migrationState = .uploadConfirmation
                }
            }
        }
    }

    private func startUploading(_ totalBytes: Int64, _ archivePath: URL) {
        Task {
            for i in 1...20 {
                try? await Task.sleep(nanoseconds: 50_000000)
                await MainActor.run {
                    migrationState = .uploadProgress(uploadedBytes: Int64(100_000 * i), totalBytes: 2_000_000, archivePath: archivePath)
                    if i == 20 {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            migrationState = .linkCreation(totalBytes: totalBytes, archivePath: archivePath)
                        }
                    }
                }
            }
        }
    }

    private func createLink(_ archivePath: URL) {
        migrationState = .linkShown(link: "https://simplex.chat", archivePath: archivePath)
    }
}

private struct PassphraseConfirmationView: View {
    @Binding var migrationState: MigrationState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var currentKey: String = ""
    @State private var verifyingPassphrase: Bool = false
    @Binding var alert: PassphraseConfirmationViewAlert?

    public enum PassphraseConfirmationViewAlert: Identifiable {
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
        ZStack {
            List {
                chatStoppedView()
                Section {
                    PassphraseField(key: $currentKey, placeholder: "Current passphrase…", valid: validKey(currentKey))
                    Button(action: {
                        verifyingPassphrase = true
                        hideKeyboard()
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.1) {
                            verifyDatabasePassphrase(currentKey, $verifyingPassphrase)
                        }
                    }) {
                        settingsRow(useKeychain ? "key" : "lock", color: .secondary) {
                            Text("Verify passphrase")
                        }
                    }
                    .disabled(currentKey.isEmpty)
                } header: {
                    Text("Verify database passphrase to migrate it")
                } footer: {
                    Text("Make sure you remember database passphrase before migrating")
                }
                .font(.callout)
            }
            if verifyingPassphrase {
                progressView()
            }
        }
    }

    private func verifyDatabasePassphrase(_ dbKey: String, _ verifyingPassphrase: Binding<Bool>) {
        let m = ChatModel.shared
        resetChatCtrl()
        m.ctrlInitInProgress = true
        defer { 
            m.ctrlInitInProgress = false
            verifyingPassphrase.wrappedValue = false
        }
        let (_, status) = chatMigrateInit(dbKey, confirmMigrations: .error)
        switch status {
        case .invalidConfirmation:
            alert = .invalidConfirmation()
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
}

private func progressView() -> some View {
    VStack {
        ProgressView().scaleEffect(2)
    }
    .frame(maxWidth: .infinity, maxHeight: .infinity )
}

func chatStoppedView() -> some View {
    settingsRow("exclamationmark.octagon.fill", color: .red) {
        Text("Chat is stopped")
    }
}

struct MigrateToAnotherDevice_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAnotherDevice()
    }
}
