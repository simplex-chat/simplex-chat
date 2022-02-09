//
//  ChatInfoView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 05/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var chat: Chat
    @Binding var showChatInfo: Bool
    @State private var showDeleteContactAlert = false
    @State private var alertContact: Contact?
    @State private var showNetworkStatusInfo = false

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

            if case let .direct(contact) = chat.chatInfo {
                VStack {
                    HStack {
                        Button {
                            showNetworkStatusInfo.toggle()
                        } label: {
                            serverImage()
                            Text(chat.serverInfo.networkStatus.statusString)
                                .foregroundColor(.primary)
                        }
                    }
                    if showNetworkStatusInfo {
                        Text(chat.serverInfo.networkStatus.statusExplanation)
                            .font(.subheadline)
                            .multilineTextAlignment(.center)
                            .padding(.horizontal, 64)
                            .padding(.vertical, 8)
                    }

                    Spacer()
                    Button(role: .destructive) {
                        alertContact = contact
                        showDeleteContactAlert = true
                    } label: {
                        Label("Delete contact", systemImage: "trash")
                    }
                    .padding()
                    .alert(isPresented: $showDeleteContactAlert) {
                        deleteContactAlert(alertContact!)
                    }
                }
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
            message: Text("Contact and all messages will be deleted"),
            primaryButton: .destructive(Text("Delete")) {
                do {
                    try apiDeleteChat(type: .direct, id: contact.apiId)
                    chatModel.removeChat(contact.id)
                    showChatInfo = false
                } catch let error {
                    print("apiDeleteChat error: \(error)")
                }
                alertContact = nil
            }, secondaryButton: .cancel() {
                alertContact = nil
            }
        )
    }
}

struct ChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        @State var showChatInfo = true
        return ChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: []), showChatInfo: $showChatInfo)
    }
}
