//
//  DatabaseView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 19/06/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum DatabaseAlert: Identifiable {
    case stopChat
    case exportProhibited
    case importArchive
    case archiveImported
    case archiveImportedWithErrors(archiveErrors: [ArchiveError])
    case archiveExportedWithErrors(archivePath: URL, archiveErrors: [ArchiveError])
    case deleteChat
    case chatDeleted
    case deleteLegacyDatabase
    case deleteFilesAndMedia
    case setChatItemTTL(ttl: ChatItemTTL)
    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case .stopChat: return "stopChat"
        case .exportProhibited: return "exportProhibited"
        case .importArchive: return "importArchive"
        case .archiveImported: return "archiveImported"
        case .archiveImportedWithErrors: return "archiveImportedWithErrors"
        case .archiveExportedWithErrors: return "archiveExportedWithErrors"
        case .deleteChat: return "deleteChat"
        case .chatDeleted: return "chatDeleted"
        case .deleteLegacyDatabase: return "deleteLegacyDatabase"
        case .deleteFilesAndMedia: return "deleteFilesAndMedia"
        case .setChatItemTTL: return "setChatItemTTL"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct DatabaseView: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    let dismissSettingsSheet: DismissAction
    @State private var runChat = false
    @State private var showRunChatToggle = false
    @State private var alert: DatabaseAlert? = nil
    @State private var showFileImporter = false
    @State private var importedArchivePath: URL?
    @State private var progressIndicator = false
    @AppStorage(DEFAULT_CHAT_ARCHIVE_NAME) private var chatArchiveName: String?
    @AppStorage(DEFAULT_CHAT_ARCHIVE_TIME) private var chatArchiveTime: Double = 0
    @State private var dbContainer = dbContainerGroupDefault.get()
    @State private var legacyDatabase = hasLegacyDatabase()
    @State private var useKeychain = storeDBPassphraseGroupDefault.get()
    @State private var appFilesCountAndSize: (Int, Int)?

    @State private var showDatabaseEncryptionView = false

    @State var chatItemTTL: ChatItemTTL
    @State private var currentChatItemTTL: ChatItemTTL = .none

    var body: some View {
        ZStack {
            chatDatabaseView()
            if progressIndicator {
                ProgressView().scaleEffect(2)
            }
        }
    }

    @ViewBuilder
    private func chatDatabaseView() -> some View {
        NavigationLink(isActive: $showDatabaseEncryptionView) {
            DatabaseEncryptionView(useKeychain: $useKeychain, migration: false, stopChatRunBlockStartChat: { progressIndicator, block in
                stopChatRunBlockStartChat(false, progressIndicator, Binding.constant(false), block)
            })
                .navigationTitle("Database passphrase")
                .modifier(ThemedBackground(grouped: true))
        } label: {
            EmptyView()
        }
        .frame(width: 1, height: 1)
        .hidden()

        List {
            let stopped = m.chatRunning == false
            Section {
                Picker("Delete messages after", selection: $chatItemTTL) {
                    ForEach(ChatItemTTL.values) { ttl in
                        Text(ttl.deleteAfterText).tag(ttl)
                    }
                    if case .seconds = chatItemTTL {
                        Text(chatItemTTL.deleteAfterText).tag(chatItemTTL)
                    }
                }
                .frame(height: 36)
                .disabled(stopped || progressIndicator)
            } header: {
                Text("Messages")
                    .foregroundColor(theme.colors.secondary)
            } footer: {
                Text("This setting applies to messages in your current chat profile **\(m.currentUser?.displayName ?? "")**.")
                    .foregroundColor(theme.colors.secondary)
            }

            // still show the toggle in case database was stopped when the user opened this screen because it can be in the following situations:
            // - database was stopped after migration and the app relaunched
            // - something wrong happened with database operations and the database couldn't be launched when it should
            if showRunChatToggle {
                Section {
                    settingsRow(
                        stopped ? "exclamationmark.octagon.fill" : "play.fill",
                        color: stopped ? .red : .green
                    ) {
                        Toggle(
                            stopped ? "Chat is stopped" : "Chat is running",
                            isOn: $runChat
                        )
                        .onChange(of: runChat) { _ in
                            if (runChat) {
                                DatabaseView.startChat($runChat, $showRunChatToggle)
                            } else {
                                alert = .stopChat
                            }
                        }
                    }
                } header: {
                    Text("Run chat")
                        .foregroundColor(theme.colors.secondary)
                } footer: {
                    if case .documents = dbContainer {
                        Text("Database will be migrated when the app restarts")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
            }

            Section {
                let unencrypted = m.chatDbEncrypted == false
                let color: Color = unencrypted ? .orange : theme.colors.secondary
                settingsRow(unencrypted ? "lock.open" : useKeychain ? "key" : "lock", color: color) {
                    NavigationLink {
                        DatabaseEncryptionView(useKeychain: $useKeychain, migration: false, stopChatRunBlockStartChat: { progressIndicator, block in
                            stopChatRunBlockStartChat(false, progressIndicator, Binding.constant(false), block)
                        })
                            .navigationTitle("Database passphrase")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        Text("Database passphrase")
                    }
                }
                settingsRow("square.and.arrow.up", color: theme.colors.secondary) {
                    Button("Export database") {
                        if initialRandomDBPassphraseGroupDefault.get() && !unencrypted {
                            showDatabaseEncryptionView = true
                            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                                alert = .exportProhibited
                            }
                        } else {
                            stopChatRunBlockStartChat(stopped, $progressIndicator, $showRunChatToggle) {
                                await exportArchive()
                            }
                        }
                    }
                }
                settingsRow("square.and.arrow.down", color: theme.colors.secondary) {
                    Button("Import database", role: .destructive) {
                        showFileImporter = true
                    }
                }
                settingsRow("trash.slash", color: theme.colors.secondary) {
                    Button("Delete database", role: .destructive) {
                        alert = .deleteChat
                    }
                }
            } header: {
                Text("Chat database")
                    .foregroundColor(theme.colors.secondary)
            } footer: {
                Text("You must use the most recent version of your chat database on one device ONLY, otherwise you may stop receiving the messages from some contacts.")
                .foregroundColor(theme.colors.secondary)
            }
            .disabled(progressIndicator)

            if case .group = dbContainer, legacyDatabase {
                Section(header: Text("Old database").foregroundColor(theme.colors.secondary)) {
                    settingsRow("trash", color: theme.colors.secondary) {
                        Button("Delete old database") {
                            alert = .deleteLegacyDatabase
                        }
                    }
                }
            }

            Section {
                Button(m.users.count > 1 ? "Delete files for all chat profiles" : "Delete all files", role: .destructive) {
                    alert = .deleteFilesAndMedia
                }
                .disabled(progressIndicator || appFilesCountAndSize?.0 == 0)
            } header: {
                Text("Files & media")
                    .foregroundColor(theme.colors.secondary)
            } footer: {
                if let (fileCount, size) = appFilesCountAndSize {
                    if fileCount == 0 {
                        Text("No received or sent files")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        Text("\(fileCount) file(s) with total size of \(ByteCountFormatter.string(fromByteCount: Int64(size), countStyle: .binary))")
                            .foregroundColor(theme.colors.secondary)
                    }
                }
            }
        }
        .onAppear {
            runChat = m.chatRunning ?? true
            showRunChatToggle = ChatModel.shared.chatRunning == false || UserDefaults.standard.bool(forKey: DEFAULT_DEVELOPER_TOOLS)
            appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
            currentChatItemTTL = chatItemTTL
        }
        .onChange(of: chatItemTTL) { ttl in
            if ttl < currentChatItemTTL {
                alert = .setChatItemTTL(ttl: ttl)
            } else if ttl != currentChatItemTTL {
                setCiTTL(ttl)
            }
        }
        .alert(item: $alert) { item in databaseAlert(item) }
        .fileImporter(
            isPresented: $showFileImporter,
            allowedContentTypes: [.zip],
            allowsMultipleSelection: false
        ) { result in
            if case let .success(files) = result, let fileURL = files.first {
                importedArchivePath = fileURL
                alert = .importArchive
            }
        }
    }

    private func databaseAlert(_ alertItem: DatabaseAlert) -> Alert {
        switch alertItem {
        case .stopChat:
            return Alert(
                title: Text("Stop chat?"),
                message: Text("Stop chat to export, import or delete chat database. You will not be able to receive and send messages while the chat is stopped."),
                primaryButton: .destructive(Text("Stop")) {
                    authStopChat()
                },
                secondaryButton: .cancel {
                    withAnimation { runChat = true }
                }
            )
        case .exportProhibited:
            return Alert(
                title: Text("Set passphrase to export"),
                message: Text("Database is encrypted using a random passphrase. Please change it before exporting.")
            )
        case .importArchive:
            if let fileURL = importedArchivePath {
                return Alert(
                    title: Text("Import chat database?"),
                    message: Text("Your current chat database will be DELETED and REPLACED with the imported one.") + Text("This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost."),
                    primaryButton: .destructive(Text("Import")) {
                        stopChatRunBlockStartChat(m.chatRunning == false, $progressIndicator, $showRunChatToggle) {
                            _ = await DatabaseView.importArchive(fileURL, $progressIndicator, $alert)
                            return true
                        }
                    },
                    secondaryButton: .cancel()
                )
            } else {
                return Alert(title: Text("Error: no database file"))
            }
        case .archiveImported:
            let (title, message) = archiveImported()
            return Alert(
                title: Text(title),
                message: Text(message)
            )
        case let .archiveImportedWithErrors(errs):
            let (title, message) = archiveImportedWithErrors(errs: errs)
            return Alert(
                title: Text(title),
                message: Text(message)
            )
        case let .archiveExportedWithErrors(archivePath, errs):
            return Alert(
                title: Text("Chat database exported"),
                message: Text("You may save the exported archive.") + Text(verbatim: "\n") + Text("Some file(s) were not exported:") + Text(archiveErrorsText(errs)),
                dismissButton: .default(Text("Continue")) {
                    showShareSheet(items: [archivePath])
                }
            )
        case .deleteChat:
            return Alert(
                title: Text("Delete chat profile?"),
                message: Text("This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost."),
                primaryButton: .destructive(Text("Delete")) {
                    let wasStopped = m.chatRunning == false
                    stopChatRunBlockStartChat(wasStopped, $progressIndicator, $showRunChatToggle) {
                        let success = await deleteChat()
                        if success && !wasStopped {
                            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                                dismissAllSheets(animated: true)
                            }
                        }
                        return true
                    }
                },
                secondaryButton: .cancel()
            )
        case .chatDeleted:
            return Alert(
                title: Text("Chat database deleted"),
                message: Text("Restart the app to create a new chat profile")
            )
        case .deleteLegacyDatabase:
            return Alert(
                title: Text("Delete old database?"),
                message: Text("The old database was not removed during the migration, it can be deleted."),
                primaryButton: .destructive(Text("Delete")) {
                    deleteLegacyDatabase()
                },
                secondaryButton: .cancel()
            )
        case .deleteFilesAndMedia:
            return Alert(
                title: Text("Delete files and media?"),
                message: Text("This action cannot be undone - all received and sent files and media will be deleted. Low resolution pictures will remain."),
                primaryButton: .destructive(Text("Delete")) {
                    stopChatRunBlockStartChat(m.chatRunning == false, $progressIndicator, $showRunChatToggle) {
                        deleteFiles()
                        return true
                    }
                },
                secondaryButton: .cancel()
            )
        case let .setChatItemTTL(ttl):
            return Alert(
                title: Text("Enable automatic message deletion?"),
                message: Text("This action cannot be undone - the messages sent and received earlier than selected will be deleted. It may take several minutes."),
                primaryButton: .destructive(Text("Delete messages")) {
                    setCiTTL(ttl)
                },
                secondaryButton: .cancel() {
                    chatItemTTL = currentChatItemTTL
                }
            )
        case let .error(title, error):
            return Alert(title: Text(title), message: Text(error))
        }
    }

    private func authStopChat(_ onStop: (() -> Void)? = nil) {
        if UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA) {
            authenticate(reason: NSLocalizedString("Stop SimpleX", comment: "authentication reason")) { laResult in
                switch laResult {
                case .success: stopChat(onStop)
                case .unavailable: stopChat(onStop)
                case .failed: withAnimation { runChat = true }
                }
            }
        } else {
            stopChat(onStop)
        }
    }

    private func stopChat(_ onStop: (() -> Void)? = nil) {
        Task {
            do {
                try await stopChatAsync()
                onStop?()
            } catch let error {
                await MainActor.run {
                    runChat = true
                    showAlert("Error stopping chat", message: responseError(error))
                }
            }
        }
    }

    func stopChatRunBlockStartChat(
        _ stopped: Bool,
        _ progressIndicator: Binding<Bool>,
        _ showRunChatToggle: Binding<Bool>,
        _ block: @escaping () async throws -> Bool
    ) {
        // if the chat was running, the sequence is: stop chat, run block, start chat.
        // Otherwise, just run block and do nothing - the toggle will be visible anyway and the user can start the chat or not
        if stopped {
            Task {
                do {
                    _ = try await block()
                } catch {
                    logger.error("Error while executing block: \(error)")
                }
            }
        } else {
            authStopChat {
                Task {
                    // if it throws, let's start chat again anyway
                    var canStart = false
                    do {
                        canStart = try await block()
                    } catch {
                        logger.error("Error executing block: \(error)")
                        canStart = true
                    }
                    if canStart {
                        await MainActor.run {
                            DatabaseView.startChat($runChat, $showRunChatToggle)
                        }
                    }
                }
            }
        }
    }

    static func startChat(_ runChat: Binding<Bool>, _ showRunChatToggle: Binding<Bool>) {
        let m = ChatModel.shared
        if m.chatDbChanged {
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                resetChatCtrl()
                do {
                    try initializeChat(start: true)
                    m.chatDbChanged = false
                    AppChatState.shared.set(.active)
                    showRunChatToggle.wrappedValue = false || UserDefaults.standard.bool(forKey: DEFAULT_DEVELOPER_TOOLS)
                    if m.chatDbStatus != .ok {
                        // Hide current view and show `DatabaseErrorView`
                        dismissAllSheets(animated: true)
                    }
                } catch let error {
                    showRunChatToggle.wrappedValue = true
                    fatalError("Error starting chat \(responseError(error))")
                }
            }
        } else {
            do {
                _ = try apiStartChat()
                runChat.wrappedValue = true
                m.chatRunning = true
                ChatReceiver.shared.start()
                chatLastStartGroupDefault.set(Date.now)
                AppChatState.shared.set(.active)
                showRunChatToggle.wrappedValue = false || UserDefaults.standard.bool(forKey: DEFAULT_DEVELOPER_TOOLS)
            } catch let error {
                runChat.wrappedValue = false
                showRunChatToggle.wrappedValue = true
                showAlert(NSLocalizedString("Error starting chat", comment: ""), message: responseError(error))
            }
        }
    }

    private func exportArchive() async -> Bool {
        await MainActor.run {
            progressIndicator = true
        }
        do {
            let (archivePath, archiveErrors) = try await exportChatArchive()
            if archiveErrors.isEmpty {
                showShareSheet(items: [archivePath])
                await MainActor.run { progressIndicator = false }
            } else {
                await MainActor.run {
                    alert = .archiveExportedWithErrors(archivePath: archivePath, archiveErrors: archiveErrors)
                    progressIndicator = false
                }
            }
        } catch let error {
            await MainActor.run {
                alert = .error(title: "Error exporting chat database", error: responseError(error))
                progressIndicator = false
            }
        }
        return true
    }

    static func importArchive(
        _ archivePath: URL,
        _ progressIndicator: Binding<Bool>,
        _ alert: Binding<DatabaseAlert?>
    ) async -> Bool {
        if archivePath.startAccessingSecurityScopedResource() {
            await MainActor.run {
                progressIndicator.wrappedValue = true
            }
            do {
                try await apiDeleteStorage()
                try? FileManager.default.createDirectory(at: getWallpaperDirectory(), withIntermediateDirectories: true)
                do {
                    let config = ArchiveConfig(archivePath: archivePath.path)
                    let archiveErrors = try await apiImportArchive(config: config)
                    shouldImportAppSettingsDefault.set(true)
                    _ = kcDatabasePassword.remove()
                    if archiveErrors.isEmpty {
                        await operationEnded(.archiveImported, progressIndicator, alert)
                    } else {
                        await operationEnded(.archiveImportedWithErrors(archiveErrors: archiveErrors), progressIndicator, alert)
                    }
                    return true
                } catch let error {
                    await operationEnded(.error(title: "Error importing chat database", error: responseError(error)), progressIndicator, alert)
                }
            } catch let error {
                await operationEnded(.error(title: "Error deleting chat database", error: responseError(error)), progressIndicator, alert)
            }
            archivePath.stopAccessingSecurityScopedResource()
        } else {
            showAlert("Error accessing database file")
        }
        return false
    }

    private func deleteChat() async -> Bool {
        await MainActor.run {
            progressIndicator = true
        }
        do {
            try await deleteChatAsync()
            await DatabaseView.operationEnded(.chatDeleted, $progressIndicator, $alert)
            appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
            return true
        } catch let error {
            await DatabaseView.operationEnded(.error(title: "Error deleting database", error: responseError(error)), $progressIndicator, $alert)
            return false
        }
    }

    private func deleteLegacyDatabase() {
        if removeLegacyDatabaseAndFiles() {
            legacyDatabase = false
        } else {
            alert = .error(title: "Error deleting old database")
        }
    }

    private static func operationEnded(_ dbAlert: DatabaseAlert, _ progressIndicator: Binding<Bool>, _ alert: Binding<DatabaseAlert?>) async {
        await MainActor.run {
            let m = ChatModel.shared
            m.chatDbChanged = true
            m.chatInitialized = false
            progressIndicator.wrappedValue = false
            // show these alerts globally so they are visible when all sheets will be hidden
            if case .archiveImported = dbAlert {
                let (title, message) = archiveImported()
                showAlert(title, message: message)
            } else if case .archiveImportedWithErrors(let errs) = dbAlert {
                let (title, message) = archiveImportedWithErrors(errs: errs)
                showAlert(title, message: message)
            } else {
                alert.wrappedValue = dbAlert
            }
        }
    }

    private func setCiTTL(_ ttl: ChatItemTTL) {
        logger.debug("DatabaseView setChatItemTTL \(ttl.seconds ?? -1)")
        progressIndicator = true
        Task {
            do {
                try await setChatItemTTL(ttl)
                await MainActor.run {
                    m.chatItemTTL = ttl
                    currentChatItemTTL = ttl
                    afterSetCiTTL()
                }
            } catch {
                await MainActor.run {
                    alert = .error(title: "Error changing setting", error: responseError(error))
                    chatItemTTL = currentChatItemTTL
                    afterSetCiTTL()
                }
            }
        }
    }

    private func afterSetCiTTL() {
        progressIndicator = false
        appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
        do {
            let chats = try apiGetChats()
            m.updateChats(chats)
        } catch let error {
            logger.error("apiGetChats: cannot update chats \(responseError(error))")
        }
    }

    private func deleteFiles() {
        deleteAppFiles()
        appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
    }
}

private func archiveImported() -> (String, String) {
    (
        NSLocalizedString("Chat database imported", comment: ""),
        NSLocalizedString("Restart the app to use imported chat database", comment: "")
    )
}
private func archiveImportedWithErrors(errs: [ArchiveError]) -> (String, String) {
    (
        NSLocalizedString("Chat database imported", comment: ""),
        NSLocalizedString("Restart the app to use imported chat database", comment: "") + "\n" + NSLocalizedString("Some non-fatal errors occurred during import:", comment: "") + archiveErrorsText(errs)
    )
}

func archiveErrorsText(_ errs: [ArchiveError]) -> String {
    return "\n" + errs.map(showArchiveError).joined(separator: "\n")
    
    func showArchiveError(_ err: ArchiveError) -> String {
        switch err {
        case let .import(importError): importError
        case let .fileError(file, fileError): "\(file): \(fileError)"
        }
    }
}

func stopChatAsync() async throws {
    try await apiStopChat()
    ChatReceiver.shared.stop()
    await MainActor.run { ChatModel.shared.chatRunning = false }
    AppChatState.shared.set(.stopped)
}

func deleteChatAsync() async throws {
    try await apiDeleteStorage()
    _ = kcDatabasePassword.remove()
    storeDBPassphraseGroupDefault.set(true)
    deleteAppDatabaseAndFiles()
    // Clean state so when creating new user the app will start chat automatically (see CreateProfile:createProfile())
    DispatchQueue.main.async {
        ChatModel.shared.users = []
    }
}

struct DatabaseView_Previews: PreviewProvider {
    @Environment(\.dismiss) static var mockDismiss

    static var previews: some View {
        DatabaseView(dismissSettingsSheet: mockDismiss, chatItemTTL: .none)
    }
}
