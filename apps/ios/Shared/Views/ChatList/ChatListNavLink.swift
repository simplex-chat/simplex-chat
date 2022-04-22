//
//  ChatListNavLink.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 01/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatListNavLink: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var chat: Chat
    @State private var showContactRequestDialog = false
    @State private var showPendingContactAlert = false

    var body: some View {
        switch chat.chatInfo {
        case let .direct(contact):
            if contact.ready { contactNavLink(contact) } else { pendingContactNavLink(contact) }
        case let .group(groupInfo):
            groupNavLink(groupInfo)
        case let .contactRequest(cReq):
            contactRequestNavLink(cReq)
        }
    }

    private func chatView() -> some View {
        ChatView(chat: chat)
        .onAppear {
            do {
                let cInfo = chat.chatInfo
                let chat = try apiGetChat(type: cInfo.chatType, id: cInfo.apiId)
                chatModel.updateChatInfo(chat.chatInfo)
                chatModel.chatItems = chat.chatItems
            } catch {
                logger.error("ChatListNavLink.chatView apiGetChatItems error: \(error.localizedDescription)")
            }
        }
    }

    private func contactNavLink(_ contact: Contact) -> some View {
        NavLinkPlain(
            tag: chat.chatInfo.id,
            selection: $chatModel.chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chat: chat) },
            disabled: !contact.ready
        )
        .swipeActions(edge: .leading) {
            if chat.chatStats.unreadCount > 0 {
                markReadButton()
            }
        }
        .swipeActions(edge: .trailing) {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(deleteContactAlert(contact))
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .frame(height: 80)
    }

    private func groupNavLink(_ groupInfo: GroupInfo) -> some View {
        NavLinkPlain(
            tag: chat.chatInfo.id,
            selection: $chatModel.chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chat: chat) },
            disabled: !groupInfo.ready
        )
        .swipeActions(edge: .leading) {
            if chat.chatStats.unreadCount > 0 {
                markReadButton()
            }
        }
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(deleteGroupAlert(groupInfo))
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .frame(height: 80)
    }

    private func markReadButton() -> some View {
        Button {
            Task { await markChatRead(chat) }
        } label: {
            Label("Read", systemImage: "checkmark")
        }
        .tint(Color.accentColor)
    }

    private func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        ContactRequestView(contactRequest: contactRequest)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button { Task { await acceptContactRequest(contactRequest) } }
                label: { Label("Accept", systemImage: "checkmark") }
                .tint(Color.accentColor)
            Button(role: .destructive) {
                AlertManager.shared.showAlert(rejectContactRequestAlert(contactRequest))
            } label: {
                Label("Reject", systemImage: "multiply")
            }
        }
        .frame(height: 80)
        .onTapGesture { showContactRequestDialog = true }
        .confirmationDialog("Connection request", isPresented: $showContactRequestDialog, titleVisibility: .visible) {
            Button("Accept contact") { Task { await acceptContactRequest(contactRequest) } }
            Button("Reject contact (sender NOT notified)") { Task { await rejectContactRequest(contactRequest) } }
        }
    }

    private func pendingContactNavLink(_ contact: Contact) -> some View {
        PendingConnectionView(chat: chat)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(deletePendingContactAlert(chat, contact))
            } label: {
                Label("Reject", systemImage: "multiply")
            }
        }
        .frame(height: 80)
        .onTapGesture { showPendingContactAlert = true }
        .alert(isPresented: $showPendingContactAlert) { pendingContactAlert(chat, contact) }
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
                        }
                    } catch let error {
                        logger.error("ChatListNavLink.deleteContactAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func deleteGroupAlert(_ groupInfo: GroupInfo) -> Alert {
        Alert(
            title: Text("Delete group"),
            message: Text("Group deletion is not supported")
        )
    }

    private func rejectContactRequestAlert(_ contactRequest: UserContactRequest) -> Alert {
        Alert(
            title: Text("Reject contact request"),
            message: Text("The sender will NOT be notified"),
            primaryButton: .destructive(Text("Reject")) {
                Task { await rejectContactRequest(contactRequest) }
            },
            secondaryButton: .cancel()
        )
    }

    private func pendingContactAlert(_ chat: Chat, _ contact: Contact) -> Alert {
        Alert(
            title: Text("Pending connection"),
            message: Text("Your connection to this contact is pending. They need to be online for the connection to become active. You can cancel this connection and remove the contact (and later retry with a new link)."),
            primaryButton: .cancel(),
            secondaryButton: .destructive(Text("Delete Contact")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        DispatchQueue.main.async {
                            chatModel.removeChat(contact.id)
                        }
                    } catch let error {
                        logger.error("ChatListNavLink.deletePendingContactAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            }
        )
    }

    private func deletePendingContactAlert(_ chat: Chat, _ contact: Contact) -> Alert {
        Alert(
            title: Text("Delete pending connection"),
            message: Text("Your connection to this contact is pending. They need to be online for the connection to become active. You can cancel this connection and remove the contact (and later retry with a new link)."),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        DispatchQueue.main.async {
                            chatModel.removeChat(contact.id)
                        }
                    } catch let error {
                        logger.error("ChatListNavLink.deletePendingContactAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

struct ChatListNavLink_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatId: String? = "@1"
        return Group {
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ))
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ))
            ChatListNavLink(chat: Chat(
                chatInfo: ChatInfo.sampleData.contactRequest,
                chatItems: []
            ))
        }
        .previewLayout(.fixed(width: 360, height: 80))
    }
}
