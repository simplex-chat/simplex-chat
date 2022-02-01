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

    @Binding var chatId: String?
    @State var chatPreview: Chat
    var width: CGFloat

    @State private var showDeleteContactAlert = false
    @State private var showDeleteGroupAlert = false
    @State private var showContactRequestAlert = false
    @State private var showContactRequestDialog = false
    @State private var alertContact: Contact?
    @State private var alertGroupInfo: GroupInfo?
    @State private var alertContactRequest: UserContactRequest?

    var body: some View {
        switch chatPreview.chatInfo {
        case let .direct(contact):
            contactNavLink(contact)
        case let .group(groupInfo):
            groupNavLink(groupInfo)
        case let .contactRequest(cReq):
            contactRequestNavLink(cReq)
        }
    }

    private func chatView() -> some View {
        ChatView(
            chatId: $chatId,
            chatInfo: chatPreview.chatInfo,
            width: width
        )
        .onAppear {
            do {
                let ci = chatPreview.chatInfo
                let chat = try apiGetChat(type: ci.chatType, id: ci.apiId)
                chatModel.chats[ci.id] = chat
            } catch {
                print("apiGetChatItems", error)
            }
        }
    }

    private func contactNavLink(_ contact: Contact) -> some View {
        NavigationLink(
            tag: chatPreview.chatInfo.id,
            selection: $chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chatPreview: chatPreview) }
        )
        .disabled(!contact.connected)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
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
        NavigationLink(
            tag: chatPreview.chatInfo.id,
            selection: $chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chatPreview: chatPreview) }
        )
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

    private func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        ChatPreviewView(chatPreview: chatPreview)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button { acceptContactRequest(contactRequest) }
                label: { Label("Accept", systemImage: "checkmark") }
                .tint(.blue)
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
        .background(Color(uiColor: .systemBackground))
        .frame(width: width, height: 80)
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
                    try apiDeleteChat(type: .direct, id: contact.contactId)
                    chatModel.chats.removeValue(forKey: contact.id)
                    chatModel.chatPreviews.removeAll(where: { $0.id == contact.id })
                } catch let error {
                    print("Error: \(error)")
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

    private func acceptContactRequest(_ contactRequest: UserContactRequest) {
        do {
            let contact = try apiAcceptContactRequest(contactReqId: contactRequest.contactRequestId)
            chatModel.chats.removeValue(forKey: contactRequest.id)
            let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
            chatModel.chats[contact.id] = chat
            if let i = chatModel.chatPreviews.firstIndex(where: { $0.id == contactRequest.id }) {
                chatModel.chatPreviews[i] = chat
            } else {
                chatModel.chatPreviews.insert(chat, at: 0)
            }
        } catch let error {
            print("Error: \(error)")
        }
    }

    private func rejectContactRequest(_ contactRequest: UserContactRequest) {
        do {
            try apiRejectContactRequest(contactReqId: contactRequest.contactRequestId)
            chatModel.chats.removeValue(forKey: contactRequest.id)
            chatModel.chatPreviews.removeAll(where: { $0.id == contactRequest.id })
        } catch let error {
            print("Error: \(error)")
        }
    }
}

struct ChatListNavLink_Previews: PreviewProvider {
    static var previews: some View {
        @State var chatId: String? = "@1"
        return Group {
            ChatListNavLink(chatId: $chatId, chatPreview: Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            ), width: 300)
            ChatListNavLink(chatId: $chatId, chatPreview: Chat(
                chatInfo: sampleDirectChatInfo,
                chatItems: [chatItemSample(1, .directSnd, Date.now, "hello")]
            ), width: 300)
            ChatListNavLink(chatId: $chatId, chatPreview: Chat(
                chatInfo: sampleContactRequestChatInfo,
                chatItems: []
            ), width: 300)
        }
        .previewLayout(.fixed(width: 360, height: 80))
    }
}
