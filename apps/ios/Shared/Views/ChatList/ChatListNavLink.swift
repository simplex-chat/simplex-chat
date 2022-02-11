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

    @State private var showDeleteContactAlert = false
    @State private var showDeleteGroupAlert = false
    @State private var showContactRequestAlert = false
    @State private var showContactRequestDialog = false
    @State private var alertContact: Contact?
    @State private var alertGroupInfo: GroupInfo?
    @State private var alertContactRequest: UserContactRequest?

    var body: some View {
        switch chat.chatInfo {
        case let .direct(contact):
            contactNavLink(contact)
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
                alertContact = contact
                showDeleteContactAlert = true
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .alert(isPresented: $showDeleteContactAlert) {
            deleteContactAlert(alertContact!)
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
                alertGroupInfo = groupInfo
                showDeleteGroupAlert = true
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .alert(isPresented: $showDeleteGroupAlert) {
            deleteGroupAlert(alertGroupInfo!)
        }
        .frame(height: 80)
    }

    private func markReadButton() -> some View {
        Button {
            markChatRead(chat)
        } label: {
            Label("Read", systemImage: "checkmark")
        }
        .tint(Color.accentColor)
    }

    private func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        ContactRequestView(contactRequest: contactRequest)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button { acceptContactRequest(contactRequest) }
                label: { Label("Accept", systemImage: "checkmark") }
                .tint(Color.accentColor)
            Button(role: .destructive) {
                alertContactRequest = contactRequest
                showContactRequestAlert = true
            } label: {
                Label("Reject", systemImage: "multiply")
            }
        }
        .alert(isPresented: $showContactRequestAlert) {
            contactRequestAlert(alertContactRequest!)
        }
        .frame(height: 80)
        .onTapGesture { showContactRequestDialog = true }
        .confirmationDialog("Connection request", isPresented: $showContactRequestDialog, titleVisibility: .visible) {
            Button("Accept contact") { acceptContactRequest(contactRequest) }
            Button("Reject contact (sender NOT notified)") { rejectContactRequest(contactRequest) }
        }
    }

    private func deleteContactAlert(_ contact: Contact) -> Alert {
        Alert(
            title: Text("Delete contact?"),
            message: Text("Contact and all messages will be deleted"),
            primaryButton: .destructive(Text("Delete")) {
                do {
                    try apiDeleteChat(type: .direct, id: contact.apiId)
                    chatModel.removeChat(contact.id)
                } catch let error {
                    logger.error("ChatListNavLink.deleteContactAlert apiDeleteChat error: \(error.localizedDescription)")
                }
                alertContact = nil
            }, secondaryButton: .cancel() {
                alertContact = nil
            }
        )
    }

    private func deleteGroupAlert(_ groupInfo: GroupInfo) -> Alert {
        Alert(
            title: Text("Delete group"),
            message: Text("Group deletion is not supported")
        )
    }

    private func contactRequestAlert(_ contactRequest: UserContactRequest) -> Alert {
        Alert(
            title: Text("Reject contact request"),
            message: Text("The sender will NOT be notified"),
            primaryButton: .destructive(Text("Reject")) {
                rejectContactRequest(contactRequest)
                alertContactRequest = nil
            }, secondaryButton: .cancel {
                alertContactRequest = nil
            }
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
