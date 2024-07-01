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

    @AppStorage(DEFAULT_SHOW_DELETE_CONTACT_NOTICE) private var showDeleteContactNotice = true

    enum ContactNavLinkActionSheet: Identifiable {
        case deleteContactActionSheet
        case confirmDeleteContactActionSheet(contactDeleteMode: ContactDeleteMode)

        var id: String {
            switch self {
            case .deleteContactActionSheet: return "deleteContactActionSheet"
            case .confirmDeleteContactActionSheet: return "confirmDeleteContactActionSheet"
            }
        }
    }

    var body: some View {
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

                    Spacer()

                    HStack {
                        if chat.chatInfo.chatSettings?.favorite ?? false {
                            Image(systemName: "star.fill")
                                .resizable()
                                .scaledToFill()
                                .frame(width: 18, height: 18)
                                .padding(.trailing, 1)
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
            .swipeActions(edge: .leading, allowsFullSwipe: true) {
                toggleFavoriteButton()
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
                    var sheetButtons: [ActionSheet.Button] = []
                    sheetButtons.append(
                        .destructive(Text("Delete contact")) { contactNavLinkSheet = .confirmDeleteContactActionSheet(contactDeleteMode: .full) }
                    )
                    if !contact.chatDeleted {
                        sheetButtons.append(
                            .destructive(Text("Delete contact, keep conversation")) { contactNavLinkSheet = .confirmDeleteContactActionSheet(contactDeleteMode: .entity) }
                        )
                    }
                    sheetButtons.append(.cancel())
                    return ActionSheet(
                        title: Text("Delete contact?"),
                        buttons: sheetButtons
                    )
                case let .confirmDeleteContactActionSheet(contactDeleteMode):
                    if contact.ready && contact.active {
                        return ActionSheet(
                            title: Text("Notify contact?\nThis cannot be undone!"),
                            buttons: [
                                .destructive(Text("Delete and notify contact")) {
                                    Task {
                                        await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: true))
                                        if contactDeleteMode == .entity && showDeleteContactNotice {
                                            AlertManager.shared.showAlert(deleteContactNotice(contact))
                                        }
                                    }
                                },
                                .destructive(Text("Delete without notification")) {
                                    Task {
                                        await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: false))
                                        if contactDeleteMode == .entity && showDeleteContactNotice {
                                            AlertManager.shared.showAlert(deleteContactNotice(contact))
                                        }
                                    }
                                },
                                .cancel()
                            ]
                        )
                    } else {
                        return ActionSheet(
                            title: Text("Confirm contact deletion.\nThis cannot be undone!"),
                            buttons: [
                                .destructive(Text("Delete")) {
                                    Task {
                                        await deleteChatContact(chat, chatDeleteMode: contactDeleteMode.toChatDeleteMode(notify: false))
                                        if contactDeleteMode == .entity && showDeleteContactNotice {
                                            AlertManager.shared.showAlert(deleteContactNotice(contact))
                                        }
                                    }
                                },
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

    private func inactiveIcon() -> some View {
        Image(systemName: "multiply.circle.fill")
            .foregroundColor(.secondary.opacity(0.65))
            .background(Circle().foregroundColor(Color(uiColor: .systemBackground)))
    }

    private func deleteContactNotice(_ contact: Contact) -> Alert {
        return Alert(
            title: Text("Contact deleted!"),
            message: Text("You can still view conversation with \(contact.displayName) in the Chats tab."),
            primaryButton: .default(Text("Don't show again")) {
                showDeleteContactNotice = false
            },
            secondaryButton: .default(Text("Ok"))
        )
    }

    @ViewBuilder private func toggleFavoriteButton() -> some View {
        if chat.chatInfo.chatSettings?.favorite == true {
            Button {
                toggleChatFavorite(chat, favorite: false)
            } label: {
                Label("Unfav.", systemImage: "star.slash")
            }
            .tint(.green)
        } else {
            Button {
                toggleChatFavorite(chat, favorite: true)
            } label: {
                Label("Favorite", systemImage: "star.fill")
            }
            .tint(.green)
        }
    }
}

#Preview {
    ContactListNavLink(chat: Chat.sampleData)
}
