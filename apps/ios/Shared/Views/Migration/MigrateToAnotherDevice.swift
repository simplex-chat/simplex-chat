//
//  MigrateToAnotherDevice.swift
//  SimpleX (iOS)
//
//  Created by Avently on 14.02.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private enum MigrationToState: Equatable {
    case chatStopInProgress
    case chatStopFailed(reason: String)
    case passphraseNotSet
    case passphraseConfirmation
    case uploadConfirmation
    case archiving
    case uploadProgress(uploadedBytes: Int64, totalBytes: Int64, fileId: Int64, archivePath: URL, ctrl: chat_ctrl?)
    case uploadFailed(totalBytes: Int64, archivePath: URL)
    case linkCreation
    case linkShown(fileId: Int64, link: String, archivePath: URL, ctrl: chat_ctrl)
    case finished(chatDeletion: Bool)
}

private enum MigrateToAnotherDeviceViewAlert: Identifiable {
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
    @Binding var showProgressOnSettings: Bool
    @State private var migrationState: MigrationToState = .chatStopInProgress
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @AppStorage(GROUP_DEFAULT_INITIAL_RANDOM_DB_PASSPHRASE, store: groupDefaults) private var initialRandomDBPassphrase: Bool = false
    @State private var alert: MigrateToAnotherDeviceViewAlert?
    @State private var authorized = !UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA)
    private let tempDatabaseUrl = urlForTemporaryDatabase()
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
            case .linkCreation:
                linkCreationView()
            case let .linkShown(fileId, link, archivePath, ctrl):
                linkShownView(fileId, link, archivePath, ctrl)
            case let .finished(chatDeletion):
                finishedView(chatDeletion)
            }
        }
        .modifier(BackButton(label: "Back", disabled: $backDisabled) {
            dismiss()
        })
        .onChange(of: migrationState) { state in
            backDisabled = switch migrationState {
            case .archiving: true
            case .linkCreation: true
            case .linkShown: true
            case .finished: true
            default: false
            }
        }
        .onAppear {
            stopChat()
        }
        .onDisappear {
            Task {
                if case .linkCreation = migrationState {} else if case .linkShown = migrationState {} else if case .finished = migrationState {} else {
                    await MainActor.run {
                        showProgressOnSettings = true
                    }
                    await startChatAndDismiss(false)
                    await MainActor.run {
                        showProgressOnSettings = false
                    }
                }
                if case let .uploadProgress(_, _, fileId, _, ctrl) = migrationState, let ctrl {
                    await cancelUploadedArchive(fileId, ctrl)
                }
                chatReceiver?.stopAndCleanUp()
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
                        Task {
                            await startChatAndDismiss()
                        }
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
        List {
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
                Text("In order to continue, chat should be stopped.")
                    .font(.callout)
            }
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
                Text("All your contacts, conversations and files will be securely encrypted and uploaded in chunks to configured XFTP relays.")
                    .font(.callout)
            }
        }
    }

    private func archivingView() -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Archiving database")
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
                    Text("Uploading archive")
                }
            }
            let ratio = Float(uploadedBytes) / Float(totalBytes)
            MigrateToAnotherDevice.largeProgressView(ratio, "\(Int(ratio * 100))%", "\(ByteCountFormatter.string(fromByteCount: uploadedBytes, countStyle: .binary)) uploaded")
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
                Text("You can give another try.")
                    .font(.callout)
            }
        }
        .onAppear {
            chatReceiver?.stopAndCleanUp()
        }
    }

    private func linkCreationView() -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Creating archive link")
                }
            }
            progressView()
        }
    }

    private func linkShownView(_ fileId: Int64, _ link: String, _ archivePath: URL, _ ctrl: chat_ctrl) -> some View {
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
                Text("Choose _Migrate from another device_ on the new device and scan QR code.")
                .font(.callout)
            }
            Section("Show QR code") {
                SimpleXLinkQRCode(uri: link)
                    .padding()
                    .background(
                        RoundedRectangle(cornerRadius: 12, style: .continuous)
                            .fill(Color(uiColor: .secondarySystemGroupedBackground))
                    )
                    .padding(.horizontal)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            }

            Section("Or securely share this file link") {
                shareLinkView(link)
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 20, bottom: 0, trailing: 10))
        }
    }

    private func finishedView(_ chatDeletion: Bool) -> some View {
        ZStack {
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
                    VStack(alignment: .leading, spacing: 16) {
                        Text("You **must not** use the same database on two devices.")
                        Text("**Please note**: using the same database on two devices will break the decryption of messages from your connections, as a security protection.")
                    }
                    .font(.callout)
                }
            }
            if chatDeletion {
                progressView()
            }
        }
    }

    private func shareLinkView(_ link: String) -> some View {
        HStack {
            linkTextView(link)
            Button {
                showShareSheet(items: [link])
            } label: {
                Image(systemName: "square.and.arrow.up")
                    .padding(.top, -7)
            }
        }
        .frame(maxWidth: .infinity)
    }

    private func linkTextView(_ link: String) -> some View {
        Text(link)
            .lineLimit(1)
            .font(.caption)
            .truncationMode(.middle)
    }

    static func largeProgressView(_ value: Float, _ title: String, _ description: LocalizedStringKey) -> some View {
        ZStack {
            VStack {
                Text(description)
                    .font(.title3)
                    .hidden()

                Text(title)
                    .font(.system(size: 54))
                    .bold()
                    .foregroundColor(.accentColor)

                Text(description)
                    .font(.title3)
            }

            Circle()
                .trim(from: 0, to: CGFloat(value))
                .stroke(
                    Color.accentColor,
                    style: StrokeStyle(lineWidth: 27)
                )
                .rotationEffect(.degrees(180))
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
                do {
                    try apiSaveAppSettings(settings: AppSettings.current.prepareForExport())
                    await MainActor.run {
                        migrationState = initialRandomDBPassphraseGroupDefault.get() ? .passphraseNotSet :  .passphraseConfirmation
                    }
                } catch let error {
                    alert = .error(title: "Error saving settings", error: error.localizedDescription)
                    migrationState = .chatStopFailed(reason: NSLocalizedString("Error saving settings", comment: "when migrating"))
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

    private func initTemporaryDatabase() -> (chat_ctrl, User)? {
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
            guard let ctrlAndUser = initTemporaryDatabase() else {
                return migrationState = .uploadFailed(totalBytes: totalBytes, archivePath: archivePath)
            }
            let (ctrl, user) = ctrlAndUser
            chatReceiver = MigrationChatReceiver(ctrl: ctrl, databaseUrl: tempDatabaseUrl) { msg in
                await MainActor.run {
                    switch msg {
                    case let .sndFileProgressXFTP(_, _, fileTransferMeta, sentSize, totalSize):
                        if case let .uploadProgress(uploaded, total, _, _, _) = migrationState, uploaded != total {
                            migrationState = .uploadProgress(uploadedBytes: sentSize, totalBytes: totalSize, fileId: fileTransferMeta.fileId, archivePath: archivePath, ctrl: ctrl)
                        }
                    case .sndFileRedirectStartXFTP:
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            migrationState = .linkCreation
                        }
                    case let .sndStandaloneFileComplete(_, fileTransferMeta, rcvURIs):
                        let cfg = getNetCfg()
                        let data = MigrationFileLinkData.init(
                            networkConfig: MigrationFileLinkData.NetworkConfig(
                                socksProxy: cfg.socksProxy,
                                hostMode: cfg.hostMode,
                                requiredHostMode: cfg.requiredHostMode
                            )
                        )
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            migrationState = .linkShown(fileId: fileTransferMeta.fileId, link: data.addToLink(link: rcvURIs[0]), archivePath: archivePath, ctrl: ctrl)
                        }
                    default:
                        logger.debug("unsupported event: \(msg.responseType)")
                    }
                }
            }
            chatReceiver?.start()

            let (res, error) = await uploadStandaloneFile(user: user, file: CryptoFile.plain(archivePath.lastPathComponent), ctrl: ctrl)
            await MainActor.run {
                guard let res = res else {
                    migrationState = .uploadFailed(totalBytes: totalBytes, archivePath: archivePath)
                    return alert = .error(title: "Error uploading the archive", error: error ?? "")
                }
                migrationState = .uploadProgress(uploadedBytes: 0, totalBytes: res.fileSize, fileId: res.fileId, archivePath: archivePath, ctrl: ctrl)
            }
        }
    }

    private func cancelUploadedArchive(_ fileId: Int64, _ ctrl: chat_ctrl) async {
        _ = await apiCancelFile(fileId: fileId, ctrl: ctrl)
    }

    private func cancelMigration(_ fileId: Int64, _ ctrl: chat_ctrl) {
        Task {
            await cancelUploadedArchive(fileId, ctrl)
            await startChatAndDismiss()
        }
    }

    private func finishMigration(_ fileId: Int64, _ ctrl: chat_ctrl) {
        Task {
            await cancelUploadedArchive(fileId, ctrl)
            await MainActor.run {
                migrationState = .finished(chatDeletion: false)
            }
        }
    }

    private func deleteChatAndDismiss() {
        Task {
            do {
                try await deleteChatAsync()
                m.chatDbChanged = true
                m.chatInitialized = false
                migrationState = .finished(chatDeletion: true)
                DispatchQueue.main.asyncAfter(deadline: .now()) {
                    resetChatCtrl()
                    do {
                        try initializeChat(start: false)
                        m.chatDbChanged = false
                        AppChatState.shared.set(.active)
                    } catch let error {
                        fatalError("Error starting chat \(responseError(error))")
                    }
                    showSettings = false
                }
            } catch let error {
                alert = .error(title: "Error deleting database", error: responseError(error))
            }
        }
    }

    private func startChatAndDismiss(_ dismiss: Bool = true) async {
        AppChatState.shared.set(.active)
        do {
            if m.chatDbChanged {
                resetChatCtrl()
                try initializeChat(start: true)
                m.chatDbChanged = false
            } else {
                try startChat(refreshInvitations: true)
            }
        } catch let error {
            alert = .error(title: "Error starting chat", error: responseError(error))
        }
        // Hide settings anyway if chatDbStatus is not ok, probably passphrase needs to be entered
        if dismiss || m.chatDbStatus != .ok {
            await MainActor.run {
                showSettings = false
            }
        }
    }

    private static func urlForTemporaryDatabase() -> URL {
        URL(fileURLWithPath: generateNewFileName(getMigrationTempFilesDirectory().path + "/" + "migration", "db", fullPath: true))
    }
}

private struct PassphraseConfirmationView: View {
    @Binding var migrationState: MigrationToState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var currentKey: String = ""
    @State private var verifyingPassphrase: Bool = false
    @FocusState private var keyboardVisible: Bool
    @Binding var alert: MigrateToAnotherDeviceViewAlert?

    var body: some View {
        ZStack {
            List {
                chatStoppedView()
                Section {
                    PassphraseField(key: $currentKey, placeholder: "Current passphrase…", valid: validKey(currentKey))
                        .focused($keyboardVisible)
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
                    .disabled(verifyingPassphrase || currentKey.isEmpty)
                } header: {
                    Text("Verify database passphrase")
                } footer: {
                    Text("Confirm that you remember database passphrase to migrate it.")
                        .font(.callout)
                }
                .onAppear {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                        keyboardVisible = true
                    }
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
            await MainActor.run {
                migrationState = .uploadConfirmation
            }
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

private class MigrationChatReceiver {
    let ctrl: chat_ctrl
    let databaseUrl: URL
    let processReceivedMsg: (ChatResponse) async -> Void
    private var receiveLoop: Task<Void, Never>?
    private var receiveMessages = true

    init(ctrl: chat_ctrl, databaseUrl: URL, _ processReceivedMsg: @escaping (ChatResponse) async -> Void) {
        self.ctrl = ctrl
        self.databaseUrl = databaseUrl
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
            Task {
                await TerminalItems.shared.add(.resp(.now, msg))
            }
            logger.debug("processReceivedMsg: \(msg.responseType)")
            await processReceivedMsg(msg)
        }
        if self.receiveMessages {
            _ = try? await Task.sleep(nanoseconds: 7_500_000)
            await receiveMsgLoop()
        }
    }

    func stopAndCleanUp() {
        logger.debug("MigrationChatReceiver.stop")
        receiveMessages = false
        receiveLoop?.cancel()
        receiveLoop = nil
        chat_close_store(ctrl)
        try? FileManager.default.removeItem(atPath: "\(databaseUrl.path)_chat.db")
        try? FileManager.default.removeItem(atPath: "\(databaseUrl.path)_agent.db")
    }
}

struct MigrateToAnotherDevice_Previews: PreviewProvider {
    static var previews: some View {
        MigrateToAnotherDevice(showSettings: Binding.constant(true), showProgressOnSettings: Binding.constant(false))
    }
}
