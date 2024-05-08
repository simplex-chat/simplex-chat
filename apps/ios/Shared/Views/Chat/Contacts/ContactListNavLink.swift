//
//  ContactListNavLink.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 06.05.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContactListNavLink: View {
    @ObservedObject var chat: Chat
    @State private var contactNavLinkSheet: ContactNavLinkActionSheet? = nil

    enum ContactNavLinkActionSheet: Identifiable {
        case deleteContactActionSheet
        case notifyDeleteContactActionSheet(contactDeleteMode: ContactDeleteMode)

        var id: String {
            switch self {
            case .deleteContactActionSheet: return "deleteContactActionSheet"
            case .notifyDeleteContactActionSheet: return "notifyDeleteContactActionSheet"
            }
        }
    }

    var body: some View {
        // TODO keep bottom bar?
        switch chat.chatInfo {
        case let .direct(contact):
            NavigationLink {
                ChatInfoView(
                    openedFromChatView: false,
                    chat: chat,
                    contact: contact,
                    localAlias: chat.chatInfo.localAlias
                )
            } label: {
                HStack{
                    ZStack(alignment: .bottomTrailing) {
                        ProfileImage(imageStr: contact.image, size: 38)
                        chatPreviewImageOverlayIcon(contact)
                            .padding([.bottom, .trailing], 1)
                    }
                    .padding(.trailing, 2)

                    previewTitle(contact)

                    if contact.contactConnIncognito {
                        Spacer()
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
                    contactNavLinkSheet = .deleteContactActionSheet
                } label: {
                    Label("Delete", systemImage: "person.badge.minus")
                }
                .tint(.red)
            }
            .actionSheet(item: $contactNavLinkSheet) { sheet in
                switch(sheet) {
                case .deleteContactActionSheet:
                    return ActionSheet(
                        title: Text("Delete contact?"),
                        buttons: [
                            .destructive(Text("Delete contact, keep conversation")) { contactNavLinkSheet = .notifyDeleteContactActionSheet(contactDeleteMode: .entity) },
                            .destructive(Text("Delete contact and conversation")) { contactNavLinkSheet = .notifyDeleteContactActionSheet(contactDeleteMode: .full) },
                            .cancel()
                        ]
                    )
                case let .notifyDeleteContactActionSheet(contactDeleteMode):
                    if contact.ready && contact.active {
                        return ActionSheet(
                            title: Text("Notify contact?\nThis cannot be undone!"),
                            buttons: [
                                .destructive(Text("Delete and notify contact")) { Task { await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: true)) } },
                                .destructive(Text("Delete without notification")) { Task { await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: false)) } },
                                .cancel()
                            ]
                        )
                    } else {
                        return ActionSheet(
                            title: Text("Confirm contact deletion.\nThis cannot be undone!"),
                            buttons: [
                                .destructive(Text("Delete")) { Task { await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: false)) } },
                                .cancel()
                            ]
                        )
                    }
                }
            }
        default:
            EmptyView()
        }
    }

    @ViewBuilder private func previewTitle(_ contact: Contact) -> some View {
        let t = Text(chat.chatInfo.chatViewName)
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

    @ViewBuilder private func chatPreviewImageOverlayIcon(_ contact: Contact) -> some View {
        if !contact.active {
            inactiveIcon()
        } else {
            EmptyView()
        }
    }

    // TODO smaller
    private func inactiveIcon() -> some View {
        Image(systemName: "multiply.circle.fill")
            .foregroundColor(.secondary.opacity(0.65))
            .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
    }
}

#Preview {
    ContactListNavLink(chat: Chat.sampleData)
}
