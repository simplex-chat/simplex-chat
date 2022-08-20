//
//  ChatListNavLink.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 01/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatListNavLink: View {
    @EnvironmentObject var chatModel: ChatModel
    @State var chat: Chat
    @State private var showContactRequestDialog = false
    @State private var showJoinGroupDialog = false

    var body: some View {
        switch chat.chatInfo {
        case let .direct(contact):
            contactNavLink(contact)
        case let .group(groupInfo):
            groupNavLink(groupInfo)
        case let .contactRequest(cReq):
            contactRequestNavLink(cReq)
        case let .contactConnection(cConn):
            contactConnectionNavLink(cConn)
        }
    }

    private func chatView() -> some View {
        ChatView(chat: chat)
            .onAppear { loadChat(chat: chat) }
    }

    @ViewBuilder private func contactNavLink(_ contact: Contact) -> some View {
        let v = NavLinkPlain(
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
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            clearChatButton()
        }
        .swipeActions(edge: .trailing) {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(
                    contact.ready
                    ? deleteContactAlert(chat.chatInfo)
                    : deletePendingContactAlert(chat, contact)
                )
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .frame(height: 80)

        if contact.ready {
            v
        } else {
            v.onTapGesture {
                AlertManager.shared.showAlert(pendingContactAlert(chat, contact))
            }
        }
    }

    @ViewBuilder private func groupNavLink(_ groupInfo: GroupInfo) -> some View {
        switch (groupInfo.membership.memberStatus) {
        case .memInvited:
            ChatPreviewView(chat: chat)
                .frame(height: 80)
                .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                    joinGroupButton()
                }
                .swipeActions(edge: .trailing) {
                    if groupInfo.canDelete {
                        deleteGroupChatButton(groupInfo)
                    }
                }
                .onTapGesture { showJoinGroupDialog = true }
                .confirmationDialog("Group invitation", isPresented: $showJoinGroupDialog, titleVisibility: .visible) {
                    Button(joinGroupIncognito ? "Join incognito" : "Join group") { Task { await joinGroup(groupInfo.groupId) } }
                    Button("Delete invitation", role: .destructive) { Task { await deleteChat(chat) } }
                }
        case .memAccepted:
            ChatPreviewView(chat: chat)
                .frame(height: 80)
                .onTapGesture {
                    AlertManager.shared.showAlert(groupInvitationAcceptedAlert())
                }
        default:
            NavLinkPlain(
                tag: chat.chatInfo.id,
                selection: $chatModel.chatId,
                destination: { chatView() },
                label: { ChatPreviewView(chat: chat) },
                disabled: !groupInfo.ready
            )
            .frame(height: 80)
            .swipeActions(edge: .leading) {
                if chat.chatStats.unreadCount > 0 {
                    markReadButton()
                }
            }
            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                clearChatButton()
            }
            .swipeActions(edge: .trailing) {
                if (groupInfo.membership.memberCurrent) {
                    Button {
                        AlertManager.shared.showAlert(leaveGroupAlert(groupInfo))
                    } label: {
                        Label("Leave", systemImage: "rectangle.portrait.and.arrow.right")
                    }
                    .tint(Color.indigo)
                }
            }
            .swipeActions(edge: .trailing) {
                if groupInfo.canDelete {
                    deleteGroupChatButton(groupInfo)
                }
            }
        }
    }

    private var joinGroupIncognito: Bool {
        // TODO there is a third condition where we would join incognito - if direct connection with host is incognito,
        // though we don't have this information easily accessible in types
        chat.chatInfo.incognito || chatModel.incognito
    }

    private func joinGroupButton() -> some View {
        Button {
            Task { await joinGroup(chat.chatInfo.apiId) }
        } label: {
            Label("Join", systemImage: joinGroupIncognito ? "theatermasks" : "ipad.and.arrow.forward")
        }
        .tint(Color.accentColor)
    }

    private func markReadButton() -> some View {
        Button {
            Task { await markChatRead(chat) }
        } label: {
            Label("Read", systemImage: "checkmark")
        }
        .tint(Color.accentColor)
    }

    private func clearChatButton() -> some View {
        Button {
            AlertManager.shared.showAlert(clearChatAlert())
        } label: {
            Label("Clear", systemImage: "gobackward")
        }
        .tint(Color.orange)
    }

    @ViewBuilder private func deleteGroupChatButton(_ groupInfo: GroupInfo) -> some View {
        Button(role: .destructive) {
            AlertManager.shared.showAlert(deleteGroupAlert(.group(groupInfo: groupInfo)))
        } label: {
            Label("Delete", systemImage: "trash")
        }
    }

    private func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        ContactRequestView(contactRequest: contactRequest, chat: chat)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button {
                Task { await acceptContactRequest(contactRequest) }
            } label: { Label("Accept", systemImage: chatModel.incognito ? "theatermasks" : "checkmark") }
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
            Button(chatModel.incognito ? "Accept incognito" : "Accept contact") { Task { await acceptContactRequest(contactRequest) } }
            Button("Reject contact (sender NOT notified)", role: .destructive) { Task { await rejectContactRequest(contactRequest) } }
        }
    }

    private func contactConnectionNavLink(_ contactConnection: PendingContactConnection) -> some View {
        ContactConnectionView(contactConnection: contactConnection)
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button(role: .destructive) {
                AlertManager.shared.showAlert(deleteContactConnectionAlert(contactConnection))
            } label: {
                Label("Delete", systemImage: "trash")
            }
        }
        .frame(height: 80)
        .onTapGesture {
            AlertManager.shared.showAlertMsg(
                title:
                    contactConnection.initiated
                    ? "You invited your contact"
                    : "You accepted connection",
                // below are the same messages that are shown in alert
                message:
                    contactConnection.viaContactUri
                    ? "You will be connected when your connection request is accepted, please wait or check later!"
                    : "You will be connected when your contact's device is online, please wait or check later!"
            )
        }
    }

    private func deleteContactAlert(_ chatInfo: ChatInfo) -> Alert {
        Alert(
            title: Text("Delete contact?"),
            message: Text("Contact and all messages will be deleted - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task { await deleteChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func deleteGroupAlert(_ chatInfo: ChatInfo) -> Alert {
        Alert(
            title: Text("Delete group?"),
            message: Text("Group will be deleted for all members - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task { await deleteChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task { await clearChat(chat) }
            },
            secondaryButton: .cancel()
        )
    }

    private func leaveGroupAlert(_ groupInfo: GroupInfo) -> Alert {
        Alert(
            title: Text("Leave group?"),
            message: Text("You will stop receiving messages from this group. Chat history will be preserved."),
            primaryButton: .destructive(Text("Leave")) {
                Task { await leaveGroup(groupInfo.groupId) }
            },
            secondaryButton: .cancel()
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

    private func deleteContactConnectionAlert(_ contactConnection: PendingContactConnection) -> Alert {
        Alert(
            title: Text("Delete pending connection?"),
            message:
                contactConnection.initiated
                ? Text("The contact you shared this link with will NOT be able to connect!")
                : Text("The connection you accepted will be cancelled!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: .contactConnection, id: contactConnection.apiId)
                        DispatchQueue.main.async {
                            chatModel.removeChat(contactConnection.id)
                        }
                    } catch let error {
                        logger.error("ChatListNavLink.deleteContactConnectionAlert apiDeleteChat error: \(responseError(error))")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func pendingContactAlert(_ chat: Chat, _ contact: Contact) -> Alert {
        Alert(
            title: Text("Contact is not connected yet!"),
            message: Text("Your contact needs to be online for the connection to complete.\nYou can cancel this connection and remove the contact (and try later with a new link)."),
            primaryButton: .cancel(),
            secondaryButton: .destructive(Text("Delete Contact")) {
                removePendingContact(chat, contact)
            }
        )
    }

    private func groupInvitationAcceptedAlert() -> Alert {
        Alert(
            title: Text("Joining group"),
            message: Text("You joined this group. Connecting to inviting group member.")
        )
    }

    private func deletePendingContactAlert(_ chat: Chat, _ contact: Contact) -> Alert {
        Alert(
            title: Text("Delete pending connection"),
            message: Text("Your contact needs to be online for the connection to complete.\nYou can cancel this connection and remove the contact (and try later with a new link)."),
            primaryButton: .destructive(Text("Delete")) {
                removePendingContact(chat, contact)
            },
            secondaryButton: .cancel()
        )
    }

    private func removePendingContact(_ chat: Chat, _ contact: Contact) {
        Task {
            do {
                try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                DispatchQueue.main.async {
                    chatModel.removeChat(contact.id)
                }
            } catch let error {
                logger.error("ChatListNavLink.removePendingContact apiDeleteChat error: \(responseError(error))")
            }
        }
    }
}

func joinGroup(_ groupId: Int64) async {
    do {
        let r = try await apiJoinGroup(groupId)
        switch r {
        case let .joined(groupInfo):
            await MainActor.run { ChatModel.shared.updateGroup(groupInfo) }
        case .invitationRemoved:
            AlertManager.shared.showAlertMsg(title: "Invitation expired!", message: "Group invitation is no longer valid, it was removed by sender.")
            await deleteGroup()
        case .groupNotFound:
            AlertManager.shared.showAlertMsg(title: "No group!", message: "This group no longer exists.")
            await deleteGroup()
        }
    } catch let error {
        let err = responseError(error)
        AlertManager.shared.showAlert(Alert(title: Text("Error joining group"), message: Text(err)))
        logger.error("apiJoinGroup error: \(err)")
    }

    func deleteGroup() async {
        do {
            // TODO this API should update chat item with the invitation as well
            try await apiDeleteChat(type: .group, id: groupId)
            await MainActor.run { ChatModel.shared.removeChat("#\(groupId)") }
        } catch {
            logger.error("apiDeleteChat error: \(responseError(error))")
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
