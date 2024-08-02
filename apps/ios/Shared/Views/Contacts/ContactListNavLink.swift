//
//  ContactListNavLink.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 01/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactListNavLink: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    @State private var alert: SomeAlert? = nil
    @State private var actionSheet: SomeActionSheet? = nil
    @State private var sheet: SomeSheet<AnyView>? = nil
    @State private var showConnectContactViaAddressDialog = false
    @State private var showContactRequestDialog = false

    var body: some View {
        let contactType = chatContactType(chat: chat)

        Group {
            switch (chat.chatInfo) {
            case let .direct(contact):
                switch contactType {
                case .recent:
                    recentContactNavLink(contact)
                case .chatDeleted:
                    deletedChatNavLink(contact)
                case .card:
                    contactCardNavLink(contact)
                default:
                    EmptyView()
                }
            case let .contactRequest(contactRequest):
                contactRequestNavLink(contactRequest)
            default:
                EmptyView()
            }
        }
        .alert(item: $alert) { $0.alert }
        .actionSheet(item: $actionSheet) { $0.actionSheet }
        .sheet(item: $sheet) {
            if #available(iOS 16.0, *) {
                $0.content
                    .presentationDetents([.fraction(0.4)])
            } else {
                $0.content
            }
        }
    }

    // regular active contact
    func recentContactNavLink(_ contact: Contact) -> some View {
        contactPreview(contact, titleColor: theme.colors.onBackground)
    }

    func deletedChatNavLink(_ contact: Contact) -> some View {
        contactPreview(contact, titleColor: theme.colors.onBackground)
    }

    func contactPreview(_ contact: Contact, titleColor: Color) -> some View {
        HStack{
            ProfileImage(imageStr: contact.image, size: 30)

            previewTitle(contact, titleColor: titleColor)

            Spacer()

            HStack {
                if chat.chatInfo.chatSettings?.favorite ?? false {
                    Image(systemName: "star.fill")
                        .resizable()
                        .scaledToFill()
                        .frame(width: 18, height: 18)
                        .foregroundColor(.secondary.opacity(0.65))
                }
                if contact.contactConnIncognito {
                    Image(systemName: "theatermasks")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 22, height: 22)
                        .foregroundColor(.secondary)
                }
            }
        }
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button {
                deleteContactDialog(
                    chat,
                    contact,
                    dismissToChatList: false,
                    showAlert: { alert = $0 },
                    showActionSheet: { actionSheet = $0 },
                    showSheetContent: { sheet = $0 }
                )
            } label: {
                Label("Delete", systemImage: "trash")
            }
            .tint(.red)
        }
    }

    @ViewBuilder private func previewTitle(_ contact: Contact, titleColor: Color) -> some View {
        let t = Text(chat.chatInfo.chatViewName).foregroundColor(titleColor)
        (
            contact.verified == true
            ? verifiedIcon + t
            : t
        )
        .lineLimit(1)
    }

    private var verifiedIcon: Text {
        (Text(Image(systemName: "checkmark.shield")) + Text(" "))
            .foregroundColor(.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }

    func contactCardNavLink(_ contact: Contact) -> some View {
        contactCardPreview(contact)
            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                Button {
                    deleteContactDialog(
                        chat,
                        contact,
                        dismissToChatList: false,
                        showAlert: { alert = $0 },
                        showActionSheet: { actionSheet = $0 },
                        showSheetContent: { sheet = $0 }
                    )
                } label: {
                    Label("Delete", systemImage: "trash")
                }
                .tint(.red)
            }
            .onTapGesture { showConnectContactViaAddressDialog = true }
            .confirmationDialog("Connect with \(contact.chatViewName)", isPresented: $showConnectContactViaAddressDialog, titleVisibility: .visible) {
                Button("Use current profile") { connectContactViaAddress_(contact, false) }
                Button("Use new incognito profile") { connectContactViaAddress_(contact, true) }
            }
    }

    private func connectContactViaAddress_(_ contact: Contact, _ incognito: Bool) {
        Task {
            let ok = await connectContactViaAddress(contact.contactId, incognito, showAlert: { alert = SomeAlert(alert: $0, id: "ContactListNavLink connectContactViaAddress") })
            if ok {
                await MainActor.run {
                    ChatModel.shared.chatId = contact.id
                }
                DispatchQueue.main.async {
                    dismissAllSheets(animated: true) {
                        AlertManager.shared.showAlert(connReqSentAlert(.contact))
                    }
                }
            }
        }
    }

    func contactCardPreview(_ contact: Contact) -> some View {
        HStack{
            ProfileImage(imageStr: contact.image, size: 30)

            Text(chat.chatInfo.chatViewName)
                .foregroundColor(.accentColor)
                .lineLimit(1)

            Spacer()

            Image(systemName: "envelope")
                .resizable()
                .scaledToFill()
                .frame(width: 14, height: 14)
                .foregroundColor(.accentColor)
        }
    }

    func contactRequestNavLink(_ contactRequest: UserContactRequest) -> some View {
        contactRequestPreview(contactRequest)
            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                Button {
                    Task { await acceptContactRequest(incognito: false, contactRequest: contactRequest) }
                } label: { Label("Accept", systemImage: "checkmark") }
                    .tint(theme.colors.primary)
                Button {
                    Task { await acceptContactRequest(incognito: true, contactRequest: contactRequest) }
                } label: {
                    Label("Accept incognito", systemImage: "theatermasks")
                }
                .tint(.indigo)
                Button {
                    alert = SomeAlert(alert: rejectContactRequestAlert(contactRequest), id: "rejectContactRequestAlert")
                } label: {
                    Label("Reject", systemImage: "multiply")
                }
                .tint(.red)
            }
            .onTapGesture { showContactRequestDialog = true }
            .confirmationDialog("Accept connection request?", isPresented: $showContactRequestDialog, titleVisibility: .visible) {
                Button("Accept") { Task { await acceptContactRequest(incognito: false, contactRequest: contactRequest) } }
                Button("Accept incognito") { Task { await acceptContactRequest(incognito: true, contactRequest: contactRequest) } }
                Button("Reject (sender NOT notified)", role: .destructive) { Task { await rejectContactRequest(contactRequest) } }
            }
    }

    func contactRequestPreview(_ contactRequest: UserContactRequest) -> some View {
        HStack{
            ProfileImage(imageStr: contactRequest.image, size: 30)

            Text(chat.chatInfo.chatViewName)
                .foregroundColor(.accentColor)
                .lineLimit(1)

            Spacer()

            Image(systemName: "checkmark")
                .resizable()
                .scaledToFill()
                .frame(width: 14, height: 14)
                .foregroundColor(.accentColor)
        }
    }
}
