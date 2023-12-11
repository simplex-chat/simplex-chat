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
    @Binding var showSettings: Bool
    @State private var runChat = false
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

    private func chatDatabaseView() -> some View {
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
            } footer: {
                Text("This setting applies to messages in your current chat profile **\(m.currentUser?.displayName ?? "")**.")
            }

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
                            startChat()
                        } else {
                            alert = .stopChat
                        }
                    }
                }
            } header: {
                Text("Run chat")
            } footer: {
                if case .documents = dbContainer {
                    Text("Database will be migrated when the app restarts")
                }
            }

            Section {
                let unencrypted = m.chatDbEncrypted == false
                let color: Color = unencrypted ? .orange : .secondary
                settingsRow(unencrypted ? "lock.open" : useKeychain ? "key" : "lock", color: color) {
                    NavigationLink {
                        DatabaseEncryptionView(useKeychain: $useKeychain)
                            .navigationTitle("Database passphrase")
                    } label: {
                        Text("Database passphrase")
                    }
                }
                settingsRow("square.and.arrow.up") {
                    Button("Export database") {
                        if initialRandomDBPassphraseGroupDefault.get() && !unencrypted {
                            alert = .exportProhibited
                        } else {
                            exportArchive()
                        }
                    }
                }
                settingsRow("square.and.arrow.down") {
                    Button("Import database", role: .destructive) {
                        showFileImporter = true
                    }
                }
                if let archiveName = chatArchiveName {
                    let title: LocalizedStringKey = chatArchiveTimeDefault.get() < chatLastStartGroupDefault.get()
                        ? "Old database archive"
                        : "New database archive"
                    settingsRow("archivebox") {
                        NavigationLink {
                            ChatArchiveView(archiveName: archiveName)
                                .navigationTitle(title)
                        } label: {
                            Text(title)
                        }
                    }
                }
                settingsRow("trash.slash") {
                    Button("Delete database", role: .destructive) {
                        alert = .deleteChat
                    }
                }
            } header: {
                Text("Chat database")
            } footer: {
                Text(
                    stopped
                     ? "You must use the most recent version of your chat database on one device ONLY, otherwise you may stop receiving the messages from some contacts."
                     : "Stop chat to enable database actions"
                )
            }
            .disabled(!stopped)

            if case .group = dbContainer, legacyDatabase {
                Section("Old database") {
                    settingsRow("trash") {
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
                .disabled(!stopped || appFilesCountAndSize?.0 == 0)
            } header: {
                Text("Files & media")
            } footer: {
                if let (fileCount, size) = appFilesCountAndSize {
                    if fileCount == 0 {
                        Text("No received or sent files")
                    } else {
                        Text("\(fileCount) file(s) with total size of \(ByteCountFormatter.string(fromByteCount: Int64(size), countStyle: .binary))")
                    }
                }
            }
        }
        .onAppear {
            runChat = m.chatRunning ?? true
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
                        importArchive(fileURL)
                    },
                    secondaryButton: .cancel()
                )
            } else {
                return Alert(title: Text("Error: no database file"))
            }
        case .archiveImported:
            return Alert(
                title: Text("Chat database imported"),
                message: Text("Restart the app to use imported chat database")
            )
        case .archiveImportedWithErrors:
            return Alert(
                title: Text("Chat database imported"),
                message: Text("Restart the app to use imported chat database") + Text("\n") + Text("Some non-fatal errors occurred during import - you may see Chat console for more details.")
            )
        case .deleteChat:
            return Alert(
                title: Text("Delete chat profile?"),
                message: Text("This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost."),
                primaryButton: .destructive(Text("Delete")) {
                    deleteChat()
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
                    deleteFiles()
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

    private func authStopChat() {
        if UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA) {
            authenticate(reason: NSLocalizedString("Stop SimpleX", comment: "authentication reason")) { laResult in
                switch laResult {
                case .success: stopChat()
                case .unavailable: stopChat()
                case .failed: withAnimation { runChat = true }
                }
            }
        } else {
            stopChat()
        }
    }

    private func stopChat() {
        Task {
            do {
                try await stopChatAsync()
            } catch let error {
                await MainActor.run {
                    runChat = true
                    alert = .error(title: "Error stopping chat", error: responseError(error))
                }
            }
        }
    }

    private func exportArchive() {
        progressIndicator = true
        Task {
            do {
                let archivePath = try await exportChatArchive()
                showShareSheet(items: [archivePath])
                await MainActor.run { progressIndicator = false }
            } catch let error {
                await MainActor.run {
                    alert = .error(title: "Error exporting chat database", error: responseError(error))
                    progressIndicator = false
                }
            }
        }
    }

    private func importArchive(_ archivePath: URL) {
        if archivePath.startAccessingSecurityScopedResource() {
            progressIndicator = true
            Task {
                do {
                    try await apiDeleteStorage()
                    do {
                        let config = ArchiveConfig(archivePath: archivePath.path)
                        let archiveErrors = try await apiImportArchive(config: config)
                        _ = kcDatabasePassword.remove()
                        if archiveErrors.isEmpty {
                            await operationEnded(.archiveImported)
                        } else {
                            await operationEnded(.archiveImportedWithErrors(archiveErrors: archiveErrors))
                        }
                    } catch let error {
                        await operationEnded(.error(title: "Error importing chat database", error: responseError(error)))
                    }
                } catch let error {
                    await operationEnded(.error(title: "Error deleting chat database", error: responseError(error)))
                }
                archivePath.stopAccessingSecurityScopedResource()
            }
        } else {
            alert = .error(title: "Error accessing database file")
        }
    }

    private func deleteChat() {
        progressIndicator = true
        Task {
            do {
                try await deleteChatAsync()
                await operationEnded(.chatDeleted)
                appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
            } catch let error {
                await operationEnded(.error(title: "Error deleting database", error: responseError(error)))
            }
        }
    }

    private func deleteLegacyDatabase() {
        if removeLegacyDatabaseAndFiles() {
            legacyDatabase = false
        } else {
            alert = .error(title: "Error deleting old database")
        }
    }

    private func operationEnded(_ dbAlert: DatabaseAlert) async {
        await MainActor.run {
            m.chatDbChanged = true
            m.chatInitialized = false
            progressIndicator = false
            alert = dbAlert
        }
    }

    private func startChat() {
        if m.chatDbChanged {
            showSettings = false
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                resetChatCtrl()
                do {
                    try initializeChat(start: true)
                    m.chatDbChanged = false
                    AppChatState.shared.set(.active)
                } catch let error {
                    fatalError("Error starting chat \(responseError(error))")
                }
            }
        } else {
            do {
                _ = try apiStartChat()
                runChat = true
                m.chatRunning = true
                ChatReceiver.shared.start()
                chatLastStartGroupDefault.set(Date.now)
                AppChatState.shared.set(.active)
            } catch let error {
                runChat = false
                alert = .error(title: "Error starting chat", error: responseError(error))
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
            m.updateChats(with: chats)
        } catch let error {
            logger.error("apiGetChats: cannot update chats \(responseError(error))")
        }
    }

    private func deleteFiles() {
        deleteAppFiles()
        appFilesCountAndSize = directoryFileCountAndSize(getAppFilesDirectory())
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
}

struct DatabaseView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseView(showSettings: Binding.constant(false), chatItemTTL: .none)
    }
}
