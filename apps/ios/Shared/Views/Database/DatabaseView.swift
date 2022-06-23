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
    case importArchive
    case archiveImported
    case deleteChat
    case chatDeleted
    case error(title: LocalizedStringKey, error: String = "")

    var id: String {
        switch self {
        case .stopChat: return "stopChat"
        case .importArchive: return "importArchive"
        case .archiveImported: return "archiveImported"
        case .deleteChat: return "deleteChat"
        case .chatDeleted: return "chatDeleted"
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
            Section("Run chat") {
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
            }
            Section {
                settingsRow("square.and.arrow.up") {
                    Button {
                        exportArchive()
                    } label: {
                        Text("Export database")
                    }
                }
                settingsRow("square.and.arrow.down") {
                    Button(role: .destructive) {
                        showFileImporter = true
                    } label: {
                        Text("Import database")
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
                    Button(role: .destructive) {
                        alert = .deleteChat
                    } label: {
                        Text("Delete database")
                    }
                }
            } header: {
                Text("Chat database (BETA)")
            } footer: {
                Text(
                    stopped
                     ? "You must use the most recent version of your chat database on one device ONLY, otherwise you may stop receiving the messages from some contacts."
                     : "Stop chat to enable database actions"
                )
            }
            .disabled(!stopped)
        }
        .onAppear { runChat = m.chatRunning ?? true }
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
                    stopChat()
                },
                secondaryButton: .cancel {
                    runChat = false
                }
            )
        case .importArchive:
            if let fileURL = importedArchivePath {
                return Alert(
                    title: Text("Import chat database?"),
                    message: Text("Your current chat database will be DELETED and REPLACED with the imported one.\n") + Text("This action cannot be undone - your profile, contacts, messages and files will be irreversibly lost."),
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
                message: Text("Restart the app to use imported chat database"),
                primaryButton: .default(Text("Ok")),
                secondaryButton: .cancel()
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
                message: Text("Restart the app to create a new chat profile"),
                primaryButton: .default(Text("Ok")),
                secondaryButton: .cancel()
            )
        case let .error(title, error):
            return Alert(title: Text(title), message: Text("\(error)"))
        }
    }

    private func stopChat() {
        Task {
            do {
                try await apiStopChat()
                await MainActor.run { m.chatRunning = false }
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
                        try await apiImportArchive(config: config)
                        await operationEnded(.archiveImported)
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
                try await apiDeleteStorage()
                await operationEnded(.chatDeleted)
            } catch let error {
                await operationEnded(.error(title: "Error deleting database", error: responseError(error)))
            }
        }
    }

    private func operationEnded(_ dbAlert: DatabaseAlert) async {
        await MainActor.run {
            m.chatDbChanged = true
            progressIndicator = false
            alert = dbAlert
        }
    }

    private func startChat() {
        if m.chatDbChanged {
            showSettings = false
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                resetChatCtrl()
                initializeChat(start: true)
                m.chatDbChanged = false
            }
        } else {
            do {
                _ = try apiStartChat()
                m.chatRunning = true
                chatLastStartGroupDefault.set(Date.now)
            } catch let error {
                runChat = false
                alert = .error(title: "Error starting chat", error: responseError(error))
            }
        }
    }
}

struct DatabaseView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseView(showSettings: Binding.constant(false))
    }
}
