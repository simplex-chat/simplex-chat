//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func infoRow(_ title: LocalizedStringKey, _ value: String) -> some View {
    HStack {
        Text(title)
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

func localizedInfoRow(_ title: LocalizedStringKey, _ value: LocalizedStringKey) -> some View {
    HStack {
        Text(title)
        Spacer()
        Text(value)
            .foregroundStyle(.secondary)
    }
}

@ViewBuilder func smpServers(_ title: LocalizedStringKey, _ servers: [String]?) -> some View {
    if let servers = servers,
       servers.count > 0 {
        infoRow(title, serverHost(servers[0]))
    }
}

private func serverHost(_ s: String) -> String {
    if let i = s.range(of: "@")?.lowerBound {
        return String(s[i...].dropFirst())
    } else {
        return s
    }
}

struct ChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    var connectionStats: ConnectionStats?
    @State private var alert: ChatInfoViewAlert? = nil
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum ChatInfoViewAlert: Identifiable {
        case deleteContactAlert
        case clearChatAlert
        case networkStatusAlert

        var id: ChatInfoViewAlert { get { self } }
    }

    var body: some View {
        NavigationView {
            List {
                contactInfoHeader()
                    .listRowBackground(Color.clear)

                if let connStats = connectionStats {
                    Section("Servers") {
                        networkStatusRow()
                            .onTapGesture {
                                alert = .networkStatusAlert
                            }
                        smpServers("Receiving via", connStats.rcvServers)
                        smpServers("Sending via", connStats.sndServers)
                    }
                }

                Section {
                    clearChatButton()
                    deleteContactButton()
                }

                if developerTools {
                    Section(header: Text("For console")) {
                        infoRow("Local name", chat.chatInfo.localDisplayName)
                        infoRow("Database ID", "\(chat.chatInfo.apiId)")
                    }
                }
            }
            .navigationBarHidden(true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteContactAlert: return deleteContactAlert()
            case .clearChatAlert: return clearChatAlert()
            case .networkStatusAlert: return networkStatusAlert()
            }
        }
    }

    func contactInfoHeader() -> some View {
        VStack {
            let cInfo = chat.chatInfo
            ChatInfoImage(chat: chat, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
                .padding(.top, 12)
                .padding()
            Text(cInfo.displayName)
                .font(.largeTitle)
                .lineLimit(1)
                .padding(.bottom, 2)
            if cInfo.fullName != "" && cInfo.fullName != cInfo.displayName {
                Text(cInfo.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    func networkStatusRow() -> some View {
        HStack {
            Text("Network status")
            Image(systemName: "info.circle")
                .foregroundColor(.accentColor)
                .font(.system(size: 14))
            Spacer()
            Text(chat.serverInfo.networkStatus.statusString)
                .foregroundColor(.secondary)
            serverImage()
        }
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
            .font(.system(size: 12))
    }

    func deleteContactButton() -> some View {
        Button(role: .destructive) {
            alert = .deleteContactAlert
        } label: {
            Label("Delete contact", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }

    private func deleteContactAlert() -> Alert {
        Alert(
            title: Text("Delete contact?"),
            message: Text("Contact and all messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        await MainActor.run {
                            chatModel.removeChat(chat.chatInfo.id)
                            dismiss()
                        }
                    } catch let error {
                        logger.error("deleteContactAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func networkStatusAlert() -> Alert {
        Alert(
            title: Text("Network status"),
            message: Text(chat.serverInfo.networkStatus.statusExplanation)
        )
    }
}

struct ChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        ChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []))
    }
}
