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
                print("apiGetChatItems", error)
            }
        }
    }

    private func contactNavLink(_ contact: Contact) -> some View {
        NavigationLink(
            tag: chat.chatInfo.id,
            selection: $chatModel.chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chat: chat) }
        )
        .disabled(!contact.ready)
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
            tag: chat.chatInfo.id,
            selection: $chatModel.chatId,
            destination: { chatView() },
            label: { ChatPreviewView(chat: chat) }
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
        ContactRequestView(contactRequest: contactRequest)
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
            let contact = try apiAcceptContactRequest(contactReqId: contactRequest.apiId)
            let chat = Chat(chatInfo: ChatInfo.direct(contact: contact), chatItems: [])
            chatModel.replaceChat(contactRequest.id, chat)
        } catch let error {
            print("Error: \(error)")
        }
    }

    private func rejectContactRequest(_ contactRequest: UserContactRequest) {
        do {
            try apiRejectContactRequest(contactReqId: contactRequest.apiId)
            chatModel.removeChat(contactRequest.id)
        } catch let error {
            print("Error: \(error)")
        }
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
