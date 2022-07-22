//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var alertManager = AlertManager.shared
    @ObservedObject var chat: Chat
    @Binding var chatViewSheet: ChatViewSheet?
    @State var alert: ChatInfoViewAlert? = nil
    @State var deletingContact: Contact?
    var contact: Contact

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

            HStack {
                serverImage()
                Text(chat.serverInfo.networkStatus.statusString)
                    .foregroundColor(.primary)
            }
            Text(chat.serverInfo.networkStatus.statusExplanation)
                .font(.subheadline)
                .multilineTextAlignment(.center)
                .padding(.horizontal, 64)
                .padding(.vertical, 8)

            Spacer()
            Button() {
                alert = .clearChatAlert
            } label: {
                Label("Clear conversation", systemImage: "gobackward")
            }
            .tint(Color.orange)
            Button(role: .destructive) {
                deletingContact = contact
                alert = .deleteContactAlert
            } label: {
                Label("Delete contact", systemImage: "trash")
            }
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
                            chatViewSheet = nil
                        }
                    } catch let error {
                        logger.error("ChatInfoView.deleteContactAlert apiDeleteChat error: \(error.localizedDescription)")
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
                    DispatchQueue.main.async {
                        chatViewSheet = nil
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct ChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatViewSheet = ChatViewSheet.chatInfo
        return ChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []), chatViewSheet: Binding($chatViewSheet), contact: Contact.sampleData)
    }
}
