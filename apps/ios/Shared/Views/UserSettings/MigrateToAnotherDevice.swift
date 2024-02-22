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
    case uploadProgress(uploadedBytes: Int64, totalBytes: Int64, fileId: Int64, archivePath: URL, ctrl: chat_ctrl?)
    case uploadFailed(totalBytes: Int64, archivePath: URL)
    case linkCreation(totalBytes: Int64)
    case linkShown(fileId: Int64, link: String, archivePath: URL, ctrl: chat_ctrl)
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
    @State private var chatWasStoppedInitially: Bool = true
    @State private var tempDatabaseUrl = urlForTemporaryDatabase()
    @State private var chatReceiver: MigrationChatReceiver? = nil
    @State private var backDisabled: Bool = false

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
            case let .uploadProgress(uploaded, total, _, archivePath, _):
                uploadProgressView(uploaded, totalBytes: total, archivePath)
            case let .uploadFailed(total, archivePath):
                uploadFailedView(totalBytes: total, archivePath)
            case let .linkCreation(totalBytes):
                linkCreationView(totalBytes)
            case let .linkShown(fileId, link, archivePath, ctrl):
                linkView(fileId, link, archivePath, ctrl)
            case .finished:
                finishedView()
            }
        }
        .modifier(BackButton(label: "Back") {
            if !backDisabled {
                dismiss()
            }
        })
        .onChange(of: migrationState) { state in
            backDisabled = switch migrationState {
            case .linkCreation: true
            case .linkShown: true
            default: false
            }
        }
        .onAppear {
            if case .initial = migrationState {
                if m.chatRunning == false {
                    migrationState = initialRandomDBPassphrase ? .passphraseNotSet : .passphraseConfirmation
                    chatWasStoppedInitially = true
                } else {
                    migrationState = .chatStopInProgress
                    chatWasStoppedInitially = false
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
            Task {
                if case let .uploadProgress(_, _, fileId, _, ctrl) = migrationState, let ctrl {
                    await cancelUploadedAchive(fileId, ctrl)
                }
                chatReceiver?.stop()
                try? FileManager.default.removeItem(at: tempDatabaseUrl)
                try? FileManager.default.removeItem(at: getMigrationTempFilesDirectory())
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
        .interactiveDismissDisabled(backDisabled)
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
                Button(action: { migrationState = .archiving }) {
                    settingsRow("tray.and.arrow.up") {
                        Text("Archive and upload").foregroundColor(.accentColor)
                    }
                }
            } header: {
                Text("Confirm upload")
            } footer: {
                Text("All your contacts, conversations and files will be archived and uploaded as encrypted file to configured XFTP relays")
                    .font(.callout)
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

    private func uploadFailedView(totalBytes: Int64, _ archivePath: URL) -> some View {
        List {
            Section {
                Button(action: {
                    migrationState = .uploadProgress(uploadedBytes: 0, totalBytes: totalBytes, fileId: 0, archivePath: archivePath, ctrl: nil)
                }) {
                    settingsRow("tray.and.arrow.up") {
                        Text("Repeat upload").foregroundColor(.accentColor)
                    }
                }
            } header: {
                Text("Upload failed")
            } footer: {
                Text("You can give another try")
                    .font(.callout)
            }
        }
        .onAppear {
            chatReceiver?.stop()
        }
    }

    private func linkCreationView(_ totalBytes: Int64) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Creating archive link…")
                }
            }
            progressView()
        }
    }

    private func linkView(_ fileId: Int64, _ link: String, _ archivePath: URL, _ ctrl: chat_ctrl) -> some View {
        List {
            Section {
                Button(action: { cancelMigration(fileId, ctrl) }) {
                    settingsRow("multiply") {
                        Text("Cancel migration").foregroundColor(.red)
                    }
                }
                Button(action: { finishMigration(fileId, ctrl) }) {
                    settingsRow("checkmark") {
                        Text("Finalize migration").foregroundColor(.accentColor)
                    }
                }
            } footer: {
                Text("Make sure you made the migration before going forward")
                    .font(.callout)
            }
            Section {
                SimpleXLinkQRCode(uri: link)
                    .frame(maxWidth: .infinity)
                shareLinkButton(link)
            } header: {
                Text("Link to uploaded archive")
            } footer: {
                Text("Choose Migrate from another device on your new device and scan QR code")
                    .font(.callout)
            }
        }
    }

    private func finishedView() -> some View {
        List {
            Section {
                Button(action: { alert = .deleteChat() }) {
                    settingsRow("trash.fill") {
                        Text("Delete database from this device").foregroundColor(.accentColor)
                    }
                }
                Button(action: { alert = .startChat() }) {
                    settingsRow("play.fill") {
                        Text("Start chat").foregroundColor(.red)
                    }
                }
            } header: {
                Text("Migration complete")
            } footer: {
                Text("You should not use the same database on two devices")
                    .font(.callout)
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
                Text(description)
                    .font(.title3)
                    .hidden()

                Text(title)
                    .font(.system(size: 60))
                    .foregroundColor(.accentColor)

                Text(description)
                    .font(.title3)
            }

            Circle()
                .trim(from: 0, to: CGFloat(value))
                .stroke(
                    Color.accentColor,
                    style: StrokeStyle(lineWidth: 30)
                )
                .rotationEffect(.degrees(-90))
                .animation(.linear, value: value)
                .frame(maxWidth: .infinity)
                .padding(.horizontal)
                .padding(.horizontal)
        }
        .frame(maxWidth: .infinity)
    }

    private func stopChat() {
        Task {
            do {
                try await stopChatAsync()
                await MainActor.run {
                    migrationState = initialRandomDBPassphraseGroupDefault.get() ? .passphraseNotSet : .passphraseConfirmation
                }
            } catch let e {
                await MainActor.run {
                    migrationState = .chatStopFailed(reason: e.localizedDescription)
                }
            }
        }
    }

    private func exportArchive() {
        Task {
            do {
                try? FileManager.default.createDirectory(at: getMigrationTempFilesDirectory(), withIntermediateDirectories: true)
                let archivePath = try await exportChatArchive(getMigrationTempFilesDirectory())
                if let attrs = try? FileManager.default.attributesOfItem(atPath: archivePath.path),
                    let totalBytes = attrs[.size] as? Int64 {
                    await MainActor.run {
                        migrationState = .uploadProgress(uploadedBytes: 0, totalBytes: totalBytes, fileId: 0, archivePath: archivePath, ctrl: nil)
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

    private func initDatabaseIfNeeded() -> (chat_ctrl, User)? {
        // Remove previous one if this isn't a first try
        try? FileManager.default.removeItem(at: tempDatabaseUrl)
        tempDatabaseUrl = MigrateToAnotherDevice.urlForTemporaryDatabase()
        let (status, ctrl) = chatInitTemporaryDatabase(url: tempDatabaseUrl)
        showErrorOnMigrationIfNeeded(status, $alert)
        do {
            if let ctrl, let user = try startChatWithTemporaryDatabase(ctrl: ctrl) {
                return (ctrl, user)
            }
        } catch let error {
            logger.error("Error while starting chat in temporary database: \(error.localizedDescription)")
        }
        return nil
    }

    private func startUploading(_ totalBytes: Int64, _ archivePath: URL) {
        Task {
            guard let ctrlAndUser = initDatabaseIfNeeded() else {
                return migrationState = .uploadFailed(totalBytes: totalBytes, archivePath: archivePath)
            }
            let (ctrl, user) = ctrlAndUser
            chatReceiver = MigrationChatReceiver(ctrl: ctrl) { msg in
                Task {
                    await TerminalItems.shared.add(.resp(.now, msg))
                }
                logger.debug("processReceivedMsg: \(msg.responseType)")
                await MainActor.run {
                    switch msg {
                    case let .sndFileProgressXFTP(_, _, fileTransferMeta, sentSize, totalSize):
                        if case .uploadProgress = migrationState {
                            migrationState = .uploadProgress(uploadedBytes: sentSize, totalBytes: totalSize, fileId: fileTransferMeta.fileId, archivePath: archivePath, ctrl: ctrl)
                        }
                    case let .sndFileRedirectStartXFTP(_, fileTransferMeta, _):
                        migrationState = .linkCreation(totalBytes: fileTransferMeta.fileSize)
                    case let .sndStandaloneFileComplete(_, fileTransferMeta, rcvURIs):
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            migrationState = .linkShown(fileId: fileTransferMeta.fileId, link: rcvURIs[0], archivePath: archivePath, ctrl: ctrl)
                        }
                    default:
                        logger.debug("unsupported event: \(msg.responseType)")
                    }
                }
            }
            chatReceiver?.start()

            let (res, error) = await uploadStandaloneFile(user: user, file: CryptoFile.plain(archivePath.lastPathComponent), ctrl: ctrl)
            guard let res = res else {
                migrationState = .uploadFailed(totalBytes: totalBytes, archivePath: archivePath)
                return alert = .error(title: "Error uploading the archive", error: error ?? "")
            }
            migrationState = .uploadProgress(uploadedBytes: 0, totalBytes: res.fileSize, fileId: res.fileId, archivePath: archivePath, ctrl: ctrl)
        }
    }

    private func cancelUploadedAchive(_ fileId: Int64, _ ctrl: chat_ctrl) async {
        _ = await apiCancelFile(fileId: fileId, ctrl: ctrl)
    }

    private func cancelMigration(_ fileId: Int64, _ ctrl: chat_ctrl) {
        Task {
            await cancelUploadedAchive(fileId, ctrl)
            await MainActor.run {
                if !chatWasStoppedInitially {
                    startChatAndDismiss()
                } else {
                    dismiss()
                }
            }
        }
    }

    private func finishMigration(_ fileId: Int64, _ ctrl: chat_ctrl) {
        Task {
            await cancelUploadedAchive(fileId, ctrl)
            await MainActor.run {
                migrationState = .finished
            }
        }
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

    private static func urlForTemporaryDatabase() -> URL {
        URL(fileURLWithPath: generateNewFileName(getMigrationTempFilesDirectory().path + "/" + "migration", "db", fullPath: true))
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
                        Task {
                            await verifyDatabasePassphrase(currentKey)
                            verifyingPassphrase = false
                        }
                    }) {
                        settingsRow(useKeychain ? "key" : "lock", color: .secondary) {
                            Text("Verify passphrase")
                        }
                    }
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

    private func verifyDatabasePassphrase(_ dbKey: String) async {
        do {
            try await testStorageEncryption(key: dbKey)
            migrationState = .uploadConfirmation
        } catch {
            showErrorOnMigrationIfNeeded(.errorNotADatabase(dbFile: ""), $alert)
        }
    }
}

private func showErrorOnMigrationIfNeeded(_ status: DBMigrationResult, _ alert: Binding<MigrateToAnotherDeviceViewAlert?>) {
    switch status {
    case .invalidConfirmation:
        alert.wrappedValue = .invalidConfirmation()
    case .errorNotADatabase:
        alert.wrappedValue = .wrongPassphrase()
    case .errorKeychain:
        alert.wrappedValue = .keychainError()
    case let .errorSQL(_, error):
        alert.wrappedValue = .databaseError(message: error)
    case let .unknown(error):
        alert.wrappedValue = .unknownError(message: error)
    case .errorMigration: ()
    case .ok: ()
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

class MigrationChatReceiver {
    let ctrl: chat_ctrl
    let processReceivedMsg: (ChatResponse) async -> Void
    private var receiveLoop: Task<Void, Never>?
    private var receiveMessages = true

    init(ctrl: chat_ctrl, _ processReceivedMsg: @escaping (ChatResponse) async -> Void) {
        self.ctrl = ctrl
        self.processReceivedMsg = processReceivedMsg
    }

    func start() {
        logger.debug("MigrationChatReceiver.start")
        receiveMessages = true
        if receiveLoop != nil { return }
        receiveLoop = Task { await receiveMsgLoop() }
    }

    func receiveMsgLoop() async {
        // TODO use function that has timeout
        if let msg = await chatRecvMsg(ctrl) {
            await processReceivedMsg(msg)
        }
        if self.receiveMessages {
            _ = try? await Task.sleep(nanoseconds: 7_500_000)
            await receiveMsgLoop()
        }
    }

    func stop() {
        logger.debug("MigrationChatReceiver.stop")
        receiveMessages = false
        receiveLoop?.cancel()
        receiveLoop = nil
        //chat_close_store(ctrl)
    }
}

struct MigrateToAnotherDevice_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAnotherDevice(showSettings: Binding.constant(true))
    }
}
