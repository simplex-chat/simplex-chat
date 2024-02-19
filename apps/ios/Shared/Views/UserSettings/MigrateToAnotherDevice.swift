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
    case linkCreation(totalBytes: Int64, fileId: Int64, archivePath: URL)
    case linkShown(fileId: Int64, link: String, archivePath: URL)
    case finished
}

enum MigrateToAnotherDeviceViewAlert: Identifiable {
    case deleteChat(_ title: LocalizedStringKey = "Delete chat profile?", _ text: LocalizedStringKey = "This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost.")
    case startChat(_ title: LocalizedStringKey = "Start chat?", _ text: LocalizedStringKey = "Warning: starting chat on multiple devices is not supported and will cause message delivery failures")

    case wrongPassphrase(title: LocalizedStringKey = "Wrong passphrase!", message: LocalizedStringKey = "Enter correct passphrase.")
    case invalidConfirmation(title: LocalizedStringKey = "Invalid migration confirmation")
    case keychainError(_ title: LocalizedStringKey = "Keychain error")
    case databaseError(_ title: LocalizedStringKey = "Database error", message: String)
    case unknownError(_ title: LocalizedStringKey = "Unknown error", message: String)

    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case let .deleteChat(title, text): return "\(title) \(text)"
        case let .startChat(title, text): return "\(title) \(text)"

        case .wrongPassphrase: return "wrongPassphrase"
        case .invalidConfirmation: return "invalidConfirmation"
        case .keychainError: return "keychainError"
        case let .databaseError(title, message): return "\(title) \(message)"
        case let .unknownError(title, message): return "\(title) \(message)"

        case let .error(title, _): return "error \(title)"
        }
    }
}

struct MigrateToAnotherDevice: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @Binding var showSettings: Bool
    @State private var migrationState: MigrationState = .initial
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @AppStorage(GROUP_DEFAULT_INITIAL_RANDOM_DB_PASSPHRASE, store: groupDefaults) private var initialRandomDBPassphrase: Bool = false
    @State private var alert: MigrateToAnotherDeviceViewAlert?
    @State private var authorized = !UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    private let chatWasStoppedInitially: Bool = AppChatState.shared.value == .stopped

    var body: some View {
        if authorized {
            migrateView()
        } else {
            Button(action: runAuth) { Label("Unlock", systemImage: "lock") }
                .onAppear(perform: runAuth)
        }
    }

    private func runAuth() { authorize(NSLocalizedString("Open migration to another device", comment: "authentication reason"), $authorized) }

    func migrateView() -> some View {
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
                PassphraseConfirmationView(migrationState: $migrationState, alert: $alert)
            case .uploadConfirmation:
                uploadConfirmationView()
            case .archiving:
                archivingView()
            case let .uploadProgress(uploaded, total, archivePath):
                uploadProgressView(uploaded, totalBytes: total, archivePath)
            case let .linkCreation(totalBytes, fileId, archivePath):
                linkCreationView(totalBytes, fileId, archivePath)
            case let .linkShown(fileId, link, archivePath):
                linkView(fileId, link, archivePath)
            case .finished:
                finishedView()
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
            if case .linkShown = migrationState {} else if case .finished = migrationState {} else if !chatWasStoppedInitially {
                Task {
                    try? startChat(refreshInvitations: true)
                }
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .startChat(title, text):
                return Alert(
                    title: Text(title),
                    message: Text(text),
                    primaryButton: .destructive(Text("Start chat")) {
                        startChatAndDismiss()
                    },
                    secondaryButton: .cancel()
                )
            case let .deleteChat(title, text):
                return Alert(
                    title: Text(title),
                    message: Text(text),
                    primaryButton: .destructive(Text("Delete")) {
                        deleteChatAndDismiss()
                    },
                    secondaryButton: .cancel()
                )
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
                settingsRow("stop.fill") {
                    Text("Stop chat").foregroundColor(.red)
                }
            }
        } header: {
            Text("Error stopping chat")
        } footer: {
            Text("In order to continue, chat should be stopped")
                .font(.callout)
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
                    settingsRow("tray.and.arrow.up") {
                        Text("Archive and upload").foregroundColor(.accentColor)
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
            largeProgressView(ratio, "\(Int(ratio * 100))%", "\(ByteCountFormatter.string(fromByteCount: uploadedBytes, countStyle: .binary)) uploaded")
        }
        .onAppear {
            startUploading(totalBytes, archivePath)
        }
    }

    private func linkCreationView(_ totalBytes: Int64, _ fileId: Int64, _ archivePath: URL) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Creating archive link…")
                }
            }
            largeProgressView(1, "100%", "\(ByteCountFormatter.string(fromByteCount: totalBytes, countStyle: .binary)) uploaded")
        }
        .onAppear {
            createLink(fileId, archivePath)
        }
    }

    private func linkView(_ fileId: Int64, _ link: String, _ archivePath: URL) -> some View {
        List {
            Section {
                Text("Choose Migrate from another device on your new device and scan QR code")
                HStack {
                    SimpleXLinkQRCode(uri: link)
                        .frame(maxWidth: 200, maxHeight: 200)
                }
                .frame(maxWidth: .infinity)
                shareLinkButton(link)
            } header: {
                Text("Link to uploaded archive")
            }

            Section {
                Button(action: { cancelMigration(fileId) }) {
                    settingsRow("multiply") {
                        Text("Cancel migration").foregroundColor(.red)
                    }
                }
                Button(action: { finishMigration(fileId) }) {
                    settingsRow("checkmark") {
                        Text("Finish migration").foregroundColor(.accentColor)
                    }
                }
            }
        }
    }

    private func finishedView() -> some View {
        List {
            Section {
                Text("You should not use the same database on two devices")
                Button(action: showDeleteChatAlert) {
                    settingsRow("trash.fill") {
                        Text("Delete database from this device").foregroundColor(.accentColor)
                    }
                }
                Button(action: showStartChatAlert) {
                    settingsRow("play.fill") {
                        Text("Start chat").foregroundColor(.red)
                    }
                }
            } header: {
                Text("Migration complete")
            }
        }
    }

    private func shareLinkButton(_ link: String) -> some View {
        Button {
            showShareSheet(items: [simplexChatLink(link)])
        } label: {
            Label("Share link", systemImage: "square.and.arrow.up")
        }
    }

    private func largeProgressView(_ value: Float, _ title: String, _ description: LocalizedStringKey) -> some View {
        ZStack {
            VStack {
                Text(title)
                    .font(.title)
                Text(description)
                    .font(.title3)
            }

            Circle()
                .trim(from: 0, to: CGFloat(value))
                .stroke(
                    Color.accentColor,
                    style: StrokeStyle(lineWidth: 5)
                )
                .rotation3DEffect(.degrees(180), axis: (x: 1, y: 0, z: 0))
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
                    migrationState = .uploadProgress(uploadedBytes: Int64(Int(totalBytes) * i / 20), totalBytes: totalBytes, archivePath: archivePath)
                    if i == 20 {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            let fileId = Int64(1)
                            migrationState = .linkCreation(totalBytes: totalBytes, fileId: fileId, archivePath: archivePath)
                        }
                    }
                }
            }
        }
    }

    private func createLink(_ fileId: Int64, _ archivePath: URL) {
        migrationState = .linkShown(fileId: fileId, link: "https://simplex.chat", archivePath: archivePath)
    }

    private func cancelUploadedAchive(_ fileId: Int64) {
        if let user = m.currentUser {
            Task {
                await cancelFile(user: user, fileId: fileId)
            }
        }
    }

    private func cancelMigration(_ fileId: Int64) {
        cancelUploadedAchive(fileId)
        if !chatWasStoppedInitially {
            startChatAndDismiss()
        } else {
            dismiss()
        }
    }

    private func finishMigration(_ fileId: Int64) {
        cancelUploadedAchive(fileId)
        migrationState = .finished
    }

    private func showDeleteChatAlert() {
        alert = .deleteChat()
    }

    private func showStartChatAlert() {
        alert = .startChat()
    }

    private func deleteChatAndDismiss() {
        Task {
            do {
                try await deleteChatAsync()
                m.chatDbChanged = true
                m.chatInitialized = false
                showSettings = false
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    resetChatCtrl()
                    do {
                        try initializeChat(start: false)
                        m.chatDbChanged = false
                        AppChatState.shared.set(.active)
                    } catch let error {
                        fatalError("Error starting chat \(responseError(error))")
                    }
                }
                dismiss()
            } catch let error {
                alert = .error(title: "Error deleting database", error: responseError(error))
            }
        }
    }

    private func startChatAndDismiss() {
        Task {
            try? startChat(refreshInvitations: true)
            dismiss()
        }
    }
}

private struct PassphraseConfirmationView: View {
    @Binding var migrationState: MigrationState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var currentKey: String = ""
    @State private var verifyingPassphrase: Bool = false
    @Binding var alert: MigrateToAnotherDeviceViewAlert?

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
                        .font(.callout)
                }
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
        MigrateToAnotherDevice(showSettings: Binding.constant(true))
    }
}
