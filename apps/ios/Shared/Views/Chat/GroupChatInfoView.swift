//
//  DirectChatInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 14.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var chat: Chat
    @Binding var showChatInfo: Bool
    @State var alert: ChatInfoViewAlert? = nil
    @State var deletingContact: Contact?

    enum ChatInfoViewAlert: Identifiable {
        case deleteContactAlert
        case clearChatAlert

        var id: ChatInfoViewAlert { get { self } }
    }

    var body: some View {
        VStack{
            ChatInfoImage(chat: chat)
                .frame(width: 192, height: 192)
                .padding(.top, 48)
                .padding()
            Text(chat.chatInfo.localDisplayName).font(.largeTitle)
                .padding(.bottom, 2)
            Text(chat.chatInfo.fullName).font(.title)
                .padding(.bottom)

            Spacer()
            Button() {
                alert = .clearChatAlert
            } label: {
                Label("Clear conversation", systemImage: "gobackward")
            }
            .tint(Color.orange)
            .padding()
        }
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteContactAlert: return deleteContactAlert(deletingContact!)
            case .clearChatAlert: return clearChatAlert()
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
    }

    private func deleteContactAlert(_ contact: Contact) -> Alert {
        Alert(
            title: Text("Delete contact?"),
            message: Text("Contact and all messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: .direct, id: contact.apiId)
                        DispatchQueue.main.async {
                            chatModel.removeChat(contact.id)
                            showChatInfo = false
                        }
                    } catch let error {
                        logger.error("ChatInfoView.deleteContactAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    // TODO reuse between this and ChatInfoView
    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    DispatchQueue.main.async {
                        showChatInfo = false
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        @State var showChatInfo = true
        return GroupChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []), showChatInfo: $showChatInfo)
    }
}
