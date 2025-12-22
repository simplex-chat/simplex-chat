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
    var showDeletedChatIcon: Bool
    @State private var alert: SomeAlert? = nil
    @State private var actionSheet: SomeActionSheet? = nil
    @State private var sheet: SomeSheet<AnyView>? = nil
    @State private var showConnectContactViaAddressDialog = false
    @State private var showContactRequestDialog = false

    var body: some View {
        Group {
            switch (chat.chatInfo) {
            case let .direct(contact):
                if contact.nextAcceptContactRequest {
                    contactWithRequestNavLink(contact)
                } else if contact.isContactCard {
                    contactCardNavLink(contact)
                } else if contact.chatDeleted {
                    deletedChatNavLink(contact)
                } else if contact.active {
                    recentContactNavLink(contact)
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

    func recentContactNavLink(_ contact: Contact) -> some View {
        Button {
            dismissAllSheets(animated: true) {
                ItemsModel.shared.loadOpenChat(contact.id)
            }
        } label: {
            contactPreview(contact, titleColor: contact.sendMsgToConnect ? theme.colors.primary : theme.colors.onBackground)
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

    func contactWithRequestNavLink(_ contact: Contact) -> some View {
        Button {
            dismissAllSheets(animated: true) {
                ItemsModel.shared.loadOpenChat(contact.id)
            }
        } label: {
            contactRequestPreview(color: contact.groupDirectInv?.memberRemoved == true ? theme.colors.secondary : theme.colors.primary)
        }
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            if let contactRequestId = contact.contactRequestId {
                Button {
                    Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequestId) }
                } label: {
                    Label("Accept", systemImage: "checkmark")
                }
                .tint(theme.colors.primary)
                if !ChatModel.shared.addressShortLinkDataSet {
                    Button {
                        Task { await acceptContactRequest(incognito: true, contactRequestId: contactRequestId) }
                    } label: {
                        Label("Accept incognito", systemImage: "theatermasks")
                    }
                    .tint(.indigo)
                }
                Button {
                    alert = SomeAlert(alert: rejectContactRequestAlert(contactRequestId), id: "rejectContactRequestAlert")
                } label: {
                    Label("Reject", systemImage: "multiply")
                }
                .tint(.red)
            } else if let groupDirectInv = contact.groupDirectInv, !groupDirectInv.memberRemoved {
                Button {
                    acceptMemberContactRequest(contact)
                } label: {
                    Label("Accept", systemImage: "checkmark")
                }
                .tint(theme.colors.primary)
                Button {
                    showRejectMemberContactRequestAlert(contact)
                } label: {
                    Label("Reject", systemImage: "multiply")
                }
                .tint(.red)
            } else {
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
    }

    func deletedChatNavLink(_ contact: Contact) -> some View {
        Button {
            Task {
                await MainActor.run {
                    dismissAllSheets(animated: true) {
                        ItemsModel.shared.loadOpenChat(contact.id)
                    }
                }
            }
        } label: {
            contactPreview(contact, titleColor: theme.colors.onBackground)
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

    func contactPreview(_ contact: Contact, titleColor: Color) -> some View {
        HStack{
            ProfileImage(imageStr: contact.image, size: 30)

            previewTitle(contact, titleColor: titleColor)

            Spacer()

            HStack {
                if showDeletedChatIcon && contact.chatDeleted {
                    Image(systemName: "archivebox")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 18, height: 18)
                        .foregroundColor(.secondary.opacity(0.65))
                } else if chat.chatInfo.chatSettings?.favorite ?? false {
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
    }

    private func previewTitle(_ contact: Contact, titleColor: Color) -> some View {
        let t = Text(chat.chatInfo.chatViewName).foregroundColor(titleColor)
        return (
            contact.verified == true
            ? verifiedIcon + t
            : t
        )
        .lineLimit(1)
    }

    private var verifiedIcon: Text {
        (Text(Image(systemName: "checkmark.shield")) + textSpace)
            .foregroundColor(.secondary)
            .baselineOffset(1)
            .kerning(-2)
    }

    func contactCardNavLink(_ contact: Contact) -> some View {
        Button {
            showConnectContactViaAddressDialog = true
        } label: {
            contactCardPreview(contact)
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
        .confirmationDialog("Connect with \(contact.chatViewName)", isPresented: $showConnectContactViaAddressDialog, titleVisibility: .visible) {
            if !contact.profileChangeProhibited {
                Button("Use current profile") { connectContactViaAddress_(contact, false) }
                Button("Use new incognito profile") { connectContactViaAddress_(contact, true) }
            } else if !contact.contactConnIncognito {
                Button("Use current profile") { connectContactViaAddress_(contact, false) }
            } else {
                Button("Use incognito profile") { connectContactViaAddress_(contact, true) }
            }
        }
    }

    private func connectContactViaAddress_(_ contact: Contact, _ incognito: Bool) {
        Task {
            let ok = await connectContactViaAddress(contact.contactId, incognito, showAlert: { alert = SomeAlert(alert: $0, id: "ContactListNavLink connectContactViaAddress") })
            if ok {
                ItemsModel.shared.loadOpenChat(contact.id) {
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
        Button {
            showContactRequestDialog = true
        } label: {
            contactRequestPreview(color: theme.colors.primary)
        }
        .swipeActions(edge: .trailing, allowsFullSwipe: true) {
            Button {
                Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequest.apiId) }
            } label: { Label("Accept", systemImage: "checkmark") }
                .tint(theme.colors.primary)
            if !ChatModel.shared.addressShortLinkDataSet {
                Button {
                    Task { await acceptContactRequest(incognito: true, contactRequestId: contactRequest.apiId) }
                } label: {
                    Label("Accept incognito", systemImage: "theatermasks")
                }
                .tint(.indigo)
            }
            Button {
                alert = SomeAlert(alert: rejectContactRequestAlert(contactRequest.apiId), id: "rejectContactRequestAlert")
            } label: {
                Label("Reject", systemImage: "multiply")
            }
            .tint(.red)
        }
        .confirmationDialog("Accept connection request?", isPresented: $showContactRequestDialog, titleVisibility: .visible) {
            Button("Accept") { Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequest.apiId) } }
            if !ChatModel.shared.addressShortLinkDataSet {
                Button("Accept incognito") { Task { await acceptContactRequest(incognito: true, contactRequestId: contactRequest.apiId) } }
            }
            Button("Reject (sender NOT notified)", role: .destructive) { Task { await rejectContactRequest(contactRequest.apiId) } }
        }
    }

    func contactRequestPreview(color: Color) -> some View {
        HStack{
            ProfileImage(imageStr: chat.chatInfo.image, size: 30)

            Text(chat.chatInfo.chatViewName)
                .foregroundColor(color)
                .lineLimit(1)

            Spacer()

            Image(systemName: "checkmark")
                .resizable()
                .scaledToFill()
                .frame(width: 14, height: 14)
                .foregroundColor(color)
        }
    }
}
