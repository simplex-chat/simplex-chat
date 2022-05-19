//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatListView: View {
    @EnvironmentObject var chatModel: ChatModel
    // not really used in this view
    @State private var showSettings = false
    @State private var searchText = ""
    @State private var showCallView = false
    @AppStorage(DEFAULT_PENDING_CONNECTIONS) private var pendingConnections = true

    var user: User

    var body: some View {
        let v = NavigationView {
            List {
                ForEach(filteredChats()) { chat in
                    ChatListNavLink(chat: chat, showCallView: $showCallView)
                        .padding(.trailing, -16)
                }
            }
            .onChange(of: chatModel.chatId) { _ in
                if chatModel.chatId == nil, let chatId = chatModel.chatToTop {
                    chatModel.chatToTop = nil
                    chatModel.popChat(chatId)
                }
            }
            .onChange(of: chatModel.chats.isEmpty) { empty in
                if !empty { return }
                withAnimation { chatModel.onboardingStage = .step3_MakeConnection }
            }
            .onChange(of: chatModel.appOpenUrl) { _ in connectViaUrl() }
            .onAppear() { connectViaUrl() }
            .offset(x: -8)
            .listStyle(.plain)
            .navigationTitle("Your chats")
            .navigationBarTitleDisplayMode(chatModel.chats.count > 8 ? .inline : .large)
            .toolbar {
                ToolbarItem(placement: .navigationBarLeading) {
                    SettingsButton()
                }
                ToolbarItem(placement: .navigationBarTrailing) {
                    NewChatButton()
                }
            }
            .fullScreenCover(isPresented: $showCallView) {
                ActiveCallView(showCallView: $showCallView)
            }
            .onChange(of: showCallView) { _ in
                if (showCallView) { return }
                if let call = chatModel.activeCall {
                    Task {
                        do {
                            try await apiEndCall(call.contact)
                        } catch {
                            logger.error("ChatListView apiEndCall error: \(error.localizedDescription)")
                        }
                    }
                }
                chatModel.callCommand = .end
            }
            .onChange(of: chatModel.activeCallInvitation) { _ in
                if let contactRef = chatModel.activeCallInvitation,
                   case let .direct(contact) = chatModel.getChat(contactRef.id)?.chatInfo,
                   let invitation = chatModel.callInvitations.removeValue(forKey: contactRef.id) {
                    answerCallAlert(contact, invitation)
                }
            }
        }
        .navigationViewStyle(.stack)

        if chatModel.chats.count > 8 {
            v.searchable(text: $searchText)
        } else {
            v
        }
    }

    private func filteredChats() -> [Chat] {
        let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
        return s == "" && pendingConnections
            ? chatModel.chats
            : s == ""
            ? chatModel.chats.filter {
                pendingConnections || $0.chatInfo.chatType != .contactConnection
            }
            : chatModel.chats.filter {
                (pendingConnections || $0.chatInfo.chatType != .contactConnection) &&
                $0.chatInfo.chatViewName.localizedLowercase.contains(s)
            }
    }

    private func answerCallAlert(_ contact: Contact, _ invitation: CallInvitation) {
        return AlertManager.shared.showAlert(Alert(
            title: Text(invitation.callTitle),
            message: Text(contact.profile.displayName).bold() +
                Text(" wants to connect with you via ") +
                Text(invitation.callTypeText) +
                Text("\nIf you accept this call and you don't use relay, your IP address might be visible to your contact."),
            primaryButton: .default(Text("Answer")) {
                if chatModel.activeCallInvitation == nil {
                    DispatchQueue.main.async {
                        AlertManager.shared.showAlertMsg(title: "Call already ended!")
                    }
                } else {
                    chatModel.activeCallInvitation = nil
                    chatModel.activeCall = Call(
                        contact: contact,
                        callState: .invitationReceived,
                        localMedia: invitation.peerMedia,
                        sharedKey: invitation.sharedKey
                    )
                    showCallView = true
                    chatModel.callCommand = .start(media: invitation.peerMedia, aesKey: invitation.sharedKey, useWorker: true)
                }
            },
            secondaryButton: .cancel()
        ))
    }
}

struct ChatListView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.chats = [
            Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ),
            Chat(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ),
            Chat(
                chatInfo: ChatInfo.sampleData.contactRequest,
                chatItems: []
            )

        ]
        return Group {
            ChatListView(user: User.sampleData)
                .environmentObject(chatModel)
            ChatListView(user: User.sampleData)
                .environmentObject(ChatModel())
        }
    }
}
