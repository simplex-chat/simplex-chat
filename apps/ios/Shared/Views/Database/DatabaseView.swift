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
    case deleteChat
    case chatDeleted
    case error(title: LocalizedStringKey, error: Error)

    var id: String {
        switch self {
        case .stopChat: return "stopChat"
        case .deleteChat: return "deleteChat"
        case .chatDeleted: return "chatDeleted"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct DatabaseView: View {
    @EnvironmentObject var m: ChatModel
    @Binding var showSettings: Bool
    @State private var runChat: Bool = false
    @State private var alert: DatabaseAlert? = nil
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
                        Text("Export archive")
                    }
                }
                settingsRow("square.and.arrow.down") {
                    NavigationLink {
                        ImportArchiveView()
                            .navigationTitle("Import chat archive")
                    } label: {
                        Text("Import archive")
                    }
                }
                if let archiveName = chatArchiveName {
                    let title: LocalizedStringKey = chatArchiveTimeDefault.get() < chatLastStartGroupDefault.get()
                        ? "Old chat archive"
                        : "Current chat archive"
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
                    Button {
                        alert = .deleteChat
                    } label: {
                        let v = Text("Delete chat database")
                        if (stopped) {
                            v.foregroundColor(.red)
                        } else {
                            v
                        }
                    }
                }
            } header: {
                Text("Chat database")
            } footer: {
                if (!stopped) {
                    Text("Stop chat to enable database actions")
                }
            }
            .disabled(!stopped)
        }
        .onAppear { runChat = m.chatRunning ?? true }
        .alert(item: $alert) {alertItem in
            switch alertItem {
            case .stopChat:
                return Alert(
                    title: Text("Stop chat?"),
                    message: Text("Stop chat to export, import or delete chat database. You will not be able to send and receive messages while the chat is stopped"),
                    primaryButton: .destructive(Text("Stop")) {
                        stopChat()
                    },
                    secondaryButton: .cancel {
                        runChat = false
                    }
                )
            case .deleteChat:
                return Alert(
                    title: Text("Delete chat profile?"),
                    message: Text("Tap delete if you want to delete chat profile. This action cannot be undone - your profile, contacts, messages and files will be irreversibly deleted"),
                    primaryButton: .destructive(Text("Delete")) {
                        deleteChat()
                    },
                    secondaryButton: .cancel()
                )
            case .chatDeleted:
                return Alert(
                    title: Text("Chat profile deleted"),
                    message: Text("Start chat or restart the app to create your new profile"),
                    primaryButton: .default(Text("Ok")),
                    secondaryButton: .cancel()
                )
            case let .error(title, error):
                return Alert(
                    title: Text(title),
                    message: Text("\(responseError(error))")
                )
            }
        }
    }

    private func stopChat() {
        Task {
            do {
                try await apiStopChat()
                await MainActor.run { m.chatRunning = false }
            } catch let error {
                await MainActor.run { runChat = true }
                alert = .error(title: "Error stopping chat", error: error)
            }
        }
    }

    private func exportArchive() {
        progressIndicator = true
        Task {
            do {
                let archivePath = try await exportChatArchive()
                showShareSheet(items: [archivePath])
            } catch let error {
                alert = .error(title: "Error exporting archive database", error: error)
            }
            progressIndicator = false
        }
    }

    private func deleteChat() {
        Task {
            do {
                try await apiDeleteStorage()
                m.chatDbChanged = true
                alert = .chatDeleted
            } catch let error {
                alert = .error(title: "Error deleting database", error: error)
            }
        }
    }

    private func startChat() {
        if m.chatDbChanged {
            showSettings = false
            DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                resetChatCtrl()
                initializeChat(start: true)
            }
            return
        }
        do {
            _ = try apiStartChat()
            m.chatRunning = true
        } catch let error {
            runChat = false
            alert = .error(title: "Error starting chat", error: error)
        }
    }
}

struct DatabaseView_Previews: PreviewProvider {
    static var previews: some View {
        DatabaseView(showSettings: Binding.constant(false))
    }
}
