//
//  MigrateFromAnotherDevice.swift
//  SimpleX (iOS)
//
//  Created by Avently on 23.02.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum MigrationFromAnotherDeviceState: Codable, Equatable {
    case downloadProgress(link: String, archiveName: String)
    case archiveImport(archiveName: String)
    case passphrase

    // Here we check whether it's needed to show migration process after app restart or not
    // It's important to NOT show the process when archive was corrupted/not fully downloaded
    static func transform() -> MigrationFromAnotherDeviceState? {
        let state: MigrationFromAnotherDeviceState? = UserDefaults.standard.string(forKey: DEFAULT_MIGRATION_STAGE) != nil ? decodeJSON(UserDefaults.standard.string(forKey: DEFAULT_MIGRATION_STAGE)!) : nil

        if case let .downloadProgress(_, archiveName) = state {
            // iOS changes absolute directory every launch, check this way
            let archivePath = getMigrationTempFilesDirectory().path + "/" + archiveName
            try? FileManager.default.removeItem(atPath: archivePath)
            UserDefaults.standard.removeObject(forKey: DEFAULT_MIGRATION_STAGE)
            // No migration happens at the moment actually since archive were not downloaded fully
            logger.debug("MigrateFromDevice: archive wasn't fully downloaded, removed broken file")
            return nil
        }
        return state
    }

    static func save(_ state: MigrationFromAnotherDeviceState?, save: (MigrationFromAnotherDeviceState?) -> Void) {
        if let state {
            UserDefaults.standard.setValue(encodeJSON(state), forKey: DEFAULT_MIGRATION_STAGE)
        } else {
            UserDefaults.standard.removeObject(forKey: DEFAULT_MIGRATION_STAGE)
        }
        save(state)
    }
}

private enum MigrationState: Equatable {
    case pasteOrScanLink(link: String)
    case linkDownloading(link: String)
    case downloadProgress(downloadedBytes: Int64, totalBytes: Int64, fileId: Int64, link: String, archivePath: String, ctrl: chat_ctrl?)
    case downloadFailed(totalBytes: Int64, link: String, archivePath: String)
    case archiveImport(archivePath: String)
    case archiveImportFailed(archivePath: String)
    case passphrase(passphrase: String)
    case migration(passphrase: String)
    case onion(appSettings: AppSettings)
}

private enum MigrateFromAnotherDeviceViewAlert: Identifiable {
    case chatImportedWithErrors(title: LocalizedStringKey = "Chat database imported",
                                text: LocalizedStringKey = "Some non-fatal errors occurred during import - you may see Chat console for more details.")

    case wrongPassphrase(title: LocalizedStringKey = "Wrong passphrase!", message: LocalizedStringKey = "Enter correct passphrase.")
    case invalidConfirmation(title: LocalizedStringKey = "Invalid migration confirmation")
    case keychainError(_ title: LocalizedStringKey = "Keychain error")
    case databaseError(_ title: LocalizedStringKey = "Database error", message: String)
    case unknownError(_ title: LocalizedStringKey = "Unknown error", message: String)

    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case .chatImportedWithErrors: return "chatImportedWithErrors"

        case .wrongPassphrase: return "wrongPassphrase"
        case .invalidConfirmation: return "invalidConfirmation"
        case .keychainError: return "keychainError"
        case let .databaseError(title, message): return "\(title) \(message)"
        case let .unknownError(title, message): return "\(title) \(message)"

        case let .error(title, _): return "error \(title)"
        }
    }
}

struct MigrateFromAnotherDevice: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var migrationState: MigrationState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var alert: MigrateFromAnotherDeviceViewAlert?
    private let tempDatabaseUrl = urlForTemporaryDatabase()
    @State private var chatReceiver: MigrationChatReceiver? = nil
    @State private var backDisabled: Bool = false
    @State private var showQRCodeScanner: Bool = true

    init(state: MigrationFromAnotherDeviceState? = nil) {
        guard let state else {
            self._migrationState = State(initialValue: .pasteOrScanLink(link: ""))
            return
        }

        // Prevent from hiding the view until migration is finished or app deleted
        self._backDisabled = State(initialValue: true)

        var initial: MigrationState = .pasteOrScanLink(link: "")
//        logger.debug("Inited with migrationState: \(String(describing: state))")
        switch state {
        case let .downloadProgress(link, archiveName):
            // iOS changes absolute directory every launch, check this way
            let archivePath = getMigrationTempFilesDirectory().path + "/" + archiveName
            try? FileManager.default.removeItem(atPath: archivePath)
            // SHOULDN'T BE HERE because the app checks this before opening migration screen and will not open it in this case.
            // See MigrationFromAnotherDeviceState.transform()
            initial = .downloadFailed(totalBytes: 0, link: link, archivePath: archivePath)
        case let .archiveImport(archiveName):
            let archivePath = getMigrationTempFilesDirectory().path + "/" + archiveName
            self._backDisabled = State(initialValue: false)
            initial = .archiveImportFailed(archivePath: archivePath)
        case .passphrase:
            initial = .passphrase(passphrase: "")
        }
        self._migrationState = State(initialValue: initial)
    }

    var body: some View {
        VStack {
            switch migrationState {
            case let .pasteOrScanLink(link):
                pasteOrScanLinkView(link)
            case let .linkDownloading(link):
                linkDownloadingView(link)
            case let .downloadProgress(downloaded, total, _, link, archivePath, _):
                downloadProgressView(downloaded, totalBytes: total, link, archivePath)
            case let .downloadFailed(total, link, archivePath):
                downloadFailedView(totalBytes: total, link, archivePath)
            case let .archiveImport(archivePath):
                archiveImportView(archivePath)
            case let .archiveImportFailed(archivePath):
                archiveImportFailedView(archivePath)
            case let .passphrase(passphrase):
                PassphraseEnteringView(migrationState: $migrationState, currentKey: passphrase, alert: $alert)
            case let .migration(passphrase):
                migrationView(passphrase)
            case let .onion(appSettings):
                OnionView(appSettings: appSettings, finishMigration: finishMigration)
            }
        }
        .onChange(of: migrationState) { state in
            backDisabled = switch state {
            case .archiveImportFailed: false
            default: m.migrationState != nil
            }
        }
        .onDisappear {
            Task {
                if case .archiveImportFailed = migrationState {
                    // Original database is not exist, nothing is setup correctly for showing to a user yet. Return to clean state
                    deleteAppDatabaseAndFiles()
                    initChatAndMigrate()
                } else if case let .downloadProgress(_, _, fileId, _, _, ctrl) = migrationState, let ctrl {
                    await stopArchiveDownloading(fileId, ctrl)
                }
                chatReceiver?.stopAndCleanUp()
                if !backDisabled {
                    try? FileManager.default.removeItem(at: getMigrationTempFilesDirectory())
                    MigrationFromAnotherDeviceState.save(nil) { m.migrationState = $0 }
                }
            }
        }
        .alert(item: $alert) { alert in
            switch alert {
            case let .chatImportedWithErrors(title, text): 
                return Alert(title: Text(title), message: Text(text))
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

    private func pasteOrScanLinkView(_ link: String) -> some View {
        ZStack {
            List {
                Section("Scan QR code") {
                    ScannerInView(showQRCodeScanner: $showQRCodeScanner) { resp in
                        switch resp {
                        case let .success(r):
                            let link = r.string
                            if strHasSimplexFileLink(link.trimmingCharacters(in: .whitespaces)) {
                                migrationState = .linkDownloading(link: link.trimmingCharacters(in: .whitespaces))
                            } else {
                                alert = .error(title: "Invalid link", error: "The text you pasted is not a SimpleX link.")
                            }
                        case let .failure(e):
                            logger.error("processQRCode QR code error: \(e.localizedDescription)")
                            alert = .error(title: "Invalid link", error: "The text you pasted is not a SimpleX link.")
                        }
                    }
                }
                if developerTools {
                    Section("Or paste archive link") {
                        pasteLinkView()
                    }
                }
            }
        }
    }

    private func pasteLinkView() -> some View {
        Button {
            if let str = UIPasteboard.general.string {
                if strHasSimplexFileLink(str.trimmingCharacters(in: .whitespaces)) {
                    migrationState = .linkDownloading(link: str.trimmingCharacters(in: .whitespaces))
                } else {
                    alert = .error(title: "Invalid link", error: "The text you pasted is not a SimpleX link.")
                }
            }
        } label: {
            Text("Tap to paste link")
        }
        .disabled(!ChatModel.shared.pasteboardHasStrings)
        .frame(maxWidth: .infinity, alignment: .center)
    }

    private func linkDownloadingView(_ link: String) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Downloading link details…")
                }
            }
            progressView()
        }
        .onAppear {
            downloadLinkDetails(link)
        }
    }

    private func downloadProgressView(_ downloadedBytes: Int64, totalBytes: Int64, _ link: String, _ archivePath: String) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Downloading archive…")
                }
            }
            let ratio = Float(downloadedBytes) / Float(max(totalBytes, 1))
            largeProgressView(ratio, "\(Int(ratio * 100))%", "\(ByteCountFormatter.string(fromByteCount: downloadedBytes, countStyle: .binary)) downloaded")
        }
    }

    private func downloadFailedView(totalBytes: Int64, _ link: String, _ archivePath: String) -> some View {
        List {
            Section {
                Button(action: {
                    try? FileManager.default.removeItem(atPath: archivePath)
                    migrationState = .linkDownloading(link: link)
                }) {
                    settingsRow("tray.and.arrow.down") {
                        Text("Repeat download").foregroundColor(.accentColor)
                    }
                }
            } header: {
                Text("Download failed")
            } footer: {
                Text("You can give another try")
                    .font(.callout)
            }
        }
        .onAppear {
            chatReceiver?.stopAndCleanUp()
            try? FileManager.default.removeItem(atPath: archivePath)
            MigrationFromAnotherDeviceState.save(nil) { m.migrationState = $0 }
        }
    }

    private func archiveImportView(_ archivePath: String) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Importing archive…")
                }
            }
            progressView()
        }
        .onAppear {
            importArchive(archivePath)
        }
    }

    private func archiveImportFailedView(_ archivePath: String) -> some View {
        List {
            Section {
                Button(action: {
                    migrationState = .archiveImport(archivePath: archivePath)
                }) {
                    settingsRow("square.and.arrow.down") {
                        Text("Repeat import").foregroundColor(.accentColor)
                    }
                }
            } header: {
                Text("Import failed")
            } footer: {
                Text("You can give another try")
                    .font(.callout)
            }
        }
    }

    private func migrationView(_ passphrase: String) -> some View {
        ZStack {
            List {
                Section {} header: {
                    Text("Migrating…")
                }
            }
            progressView()
        }
        .onAppear {
            startChat(passphrase)
        }
    }

    struct OnionView: View {
        @State var appSettings: AppSettings
        @State private var onionHosts: OnionHosts = .no
        var finishMigration: (AppSettings) -> Void

        var body: some View {
            List {
                Section {
                    Button(action: {
                        var updated = appSettings.networkConfig!
                        let (hostMode, requiredHostMode) = onionHosts.hostMode
                        updated.hostMode = hostMode
                        updated.requiredHostMode = requiredHostMode
                        appSettings.networkConfig = updated
                        finishMigration(appSettings)
                    }) {
                        settingsRow("checkmark") {
                            Text("Apply").foregroundColor(.accentColor)
                        }
                    }
                } header: {
                    Text("Review .onion settings")
                } footer: {
                    Text("Since you migrated the database between platforms, make sure settings for .onion hosts are correct")
                        .font(.callout)
                }

                Section {
                    Picker("Use .onion hosts", selection: $onionHosts) {
                        ForEach(OnionHosts.values, id: \.self) { Text($0.text) }
                    }
                    .frame(height: 36)
                } footer: {
                    let text: LocalizedStringKey = switch onionHosts {
                    case .no:
                        "Selected No"
                    case .prefer:
                        "Selected Prefer"
                    case .require:
                        "Selected Require"
                    }
                    Text(text).font(.callout)
                }
            }
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

    private func downloadLinkDetails(_ link: String) {
        let archiveTime = Date.now
        let ts = archiveTime.ISO8601Format(Date.ISO8601FormatStyle(timeSeparator: .omitted))
        let archiveName = "simplex-chat.\(ts).zip"
        let archivePath = getMigrationTempFilesDirectory().appendingPathComponent(archiveName)

        startDownloading(0, link, archivePath.path)
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

    private func startDownloading(_ totalBytes: Int64, _ link: String, _ archivePath: String) {
        Task {
            guard let ctrlAndUser = initTemporaryDatabase() else {
                return migrationState = .downloadFailed(totalBytes: totalBytes, link: link, archivePath: archivePath)
            }
            let (ctrl, user) = ctrlAndUser
            chatReceiver = MigrationChatReceiver(ctrl: ctrl, databaseUrl: tempDatabaseUrl) { msg in
                Task {
                    await TerminalItems.shared.add(.resp(.now, msg))
                }
                logger.debug("processReceivedMsg: \(msg.responseType)")
                await MainActor.run {
                    switch msg {
                    case let .rcvFileProgressXFTP(_, _, receivedSize, totalSize, rcvFileTransfer):
                        migrationState = .downloadProgress(downloadedBytes: receivedSize, totalBytes: totalSize, fileId: rcvFileTransfer.fileId, link: link, archivePath: archivePath, ctrl: ctrl)
                        MigrationFromAnotherDeviceState.save(.downloadProgress(link: link, archiveName: URL(fileURLWithPath: archivePath).lastPathComponent)) { m.migrationState = $0 }
                    case .rcvStandaloneFileComplete:
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            migrationState = .archiveImport(archivePath: archivePath)
                            MigrationFromAnotherDeviceState.save(.archiveImport(archiveName: URL(fileURLWithPath: archivePath).lastPathComponent)) { m.migrationState = $0 }
                        }
                    case .rcvFileError:
                        alert = .error(title: "Download failed", error: "File was deleted or link is invalid")
                        migrationState = .downloadFailed(totalBytes: totalBytes, link: link, archivePath: archivePath)
                    default:
                        logger.debug("unsupported event: \(msg.responseType)")
                    }
                }
            }
            chatReceiver?.start()

            let (res, error) = await downloadStandaloneFile(user: user, url: link, file: CryptoFile.plain(URL(fileURLWithPath: archivePath).lastPathComponent), ctrl: ctrl)
            if res == nil {
                await MainActor.run {
                    migrationState = .downloadFailed(totalBytes: totalBytes, link: link, archivePath: archivePath)
                }
                return alert = .error(title: "Error downloading the archive", error: error ?? "")
            }
        }
    }

    private func importArchive(_ archivePath: String) {
        Task {
            do {
                if !hasChatCtrl() {
                    chatInitControllerRemovingDatabases()
                }
                try await apiDeleteStorage()
                do {
                    let config = ArchiveConfig(archivePath: archivePath)
                    let archiveErrors = try await apiImportArchive(config: config)
                    if !archiveErrors.isEmpty {
                        alert = .chatImportedWithErrors()
                    }
                    await MainActor.run {
                        migrationState = .passphrase(passphrase: "")
                        MigrationFromAnotherDeviceState.save(.passphrase) { m.migrationState = $0 }
                    }
                } catch let error {
                    await MainActor.run {
                        migrationState = .archiveImportFailed(archivePath: archivePath)
                    }
                    alert = .error(title: "Error importing chat database", error: responseError(error))
                }
            } catch let error {
                await MainActor.run {
                    migrationState = .archiveImportFailed(archivePath: archivePath)
                }
                alert = .error(title: "Error deleting chat database", error: responseError(error))
            }
        }
    }


    private func stopArchiveDownloading(_ fileId: Int64, _ ctrl: chat_ctrl) async {
        _ = await apiCancelFile(fileId: fileId, ctrl: ctrl)
    }

    private func cancelMigration(_ fileId: Int64, _ ctrl: chat_ctrl) {
        Task {
            await stopArchiveDownloading(fileId, ctrl)
            await MainActor.run {
                dismiss()
            }
        }
    }

    private func startChat(_ passphrase: String) {
        _ = kcDatabasePassword.set(passphrase)
        storeDBPassphraseGroupDefault.set(true)
        initialRandomDBPassphraseGroupDefault.set(false)
        AppChatState.shared.set(.active)
        Task {
            do {
//                resetChatCtrl()
                try initializeChat(start: false, confirmStart: false, dbKey: passphrase, refreshInvitations: true)
                var appSettings = try apiGetAppSettings(settings: AppSettings.current)
                await MainActor.run {
                    // LALAL
                    if true/*appSettings.networkConfig?.socksProxy != nil*/ {
                        appSettings.networkConfig?.socksProxy = nil
                        appSettings.networkConfig?.hostMode = .publicHost
                        appSettings.networkConfig?.requiredHostMode = true
                        migrationState = .onion(appSettings: appSettings)
                    } else {
                        finishMigration(appSettings)
                    }
                }
            } catch let error {
                hideView()
                AlertManager.shared.showAlert(Alert(title: Text("Error starting chat"), message: Text(responseError(error))))
            }
        }
    }

    private func finishMigration(_ appSettings: AppSettings) {
        do {
            try? FileManager.default.removeItem(at: getMigrationTempFilesDirectory())
            MigrationFromAnotherDeviceState.save(nil) { m.migrationState = $0 }
            try SimpleX.startChat(refreshInvitations: true)
            appSettings.importIntoApp()
            AlertManager.shared.showAlertMsg(title: "Chat migrated!", message: "Finalize migration on another device.")
        } catch let error {
            AlertManager.shared.showAlert(Alert(title: Text("Error starting chat"), message: Text(responseError(error))))
        }
        hideView()
    }

    private func hideView() {
        onboardingStageDefault.set(.onboardingComplete)
        m.onboardingStage = .onboardingComplete
        dismiss()
    }

    private func strHasSimplexFileLink(_ text: String) -> Bool {
        text.starts(with: "simplex:/file") || text.starts(with: "https://simplex.chat/file")
    }

    private static func urlForTemporaryDatabase() -> URL {
        URL(fileURLWithPath: generateNewFileName(getMigrationTempFilesDirectory().path + "/" + "migration", "db", fullPath: true))
    }
}

private struct PassphraseEnteringView: View {
    @Binding var migrationState: MigrationState
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State var currentKey: String
    @State private var verifyingPassphrase: Bool = false
    @Binding var alert: MigrateFromAnotherDeviceViewAlert?

    var body: some View {
        ZStack {
            List {
                Section {
                    PassphraseField(key: $currentKey, placeholder: "Current passphrase…", valid: validKey(currentKey))
                    Button(action: {
                        verifyingPassphrase = true
                        hideKeyboard()
                        Task {
                            let (status, ctrl) = chatInitTemporaryDatabase(url: getAppDatabasePath(), key: currentKey)
                            let success = switch status {
                            case .ok, .invalidConfirmation: true
                            default: false
                            }
                            if success {
//                                if let ctrl {
//                                    chat_close_store(ctrl)
//                                }
                                applyChatCtrl(ctrl: ctrl, result: (currentKey != "", status))
                                await MainActor.run {
                                    migrationState = .migration(passphrase: currentKey)
                                }
                            } else {
                                showErrorOnMigrationIfNeeded(status, $alert)
                            }
                            verifyingPassphrase = false
                        }
                    }) {
                        settingsRow("key", color: .secondary) {
                            Text("Open chat")
                        }
                    }
                } header: {
                    Text("Enter passphrase")
                } footer: {
                    Text("Passphrase will be stored on device in Keychain. It's required for notifications to work. You can change it later in settings.")
                        .font(.callout)
                }
            }
            if verifyingPassphrase {
                progressView()
            }
        }
    }
}

private func showErrorOnMigrationIfNeeded(_ status: DBMigrationResult, _ alert: Binding<MigrateFromAnotherDeviceViewAlert?>) {
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

struct MigrateFromAnotherDevice_Previews: PreviewProvider {
    static var previews: some View {
        MigrateFromAnotherDevice()
    }
}
