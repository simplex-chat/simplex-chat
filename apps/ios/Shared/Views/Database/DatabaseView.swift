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
    case error(title: LocalizedStringKey, error: Error)

    var id: String {
        switch self {
        case .stopChat: return "stopChat"
        case .deleteChat: return "deleteChat"
        case let .error(title, _): return "error \(title)"
        }
    }
}

struct DatabaseView: View {
    @EnvironmentObject var m: ChatModel
    @State var runChat: Bool = false
    @State var alert: DatabaseAlert? = nil

    var body: some View {
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
                            stopChat()
                        } else {
                            alert = .stopChat
                        }
                    }
                }
            }
            Section {
                settingsRow("square.and.arrow.up") {
                    NavigationLink {
                        ExportArchiveView()
                            .navigationTitle("Export chat archive")
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
                settingsRow("trash.slash") {
                    Button {

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

                    },
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

    private func deleteChat() {
        
    }

    private func startChat() {
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
        DatabaseView()
    }
}
