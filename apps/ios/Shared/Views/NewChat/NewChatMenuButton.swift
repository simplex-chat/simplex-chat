//
//  NewChatMenuButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum NewChatMenuOption: Identifiable {
    case newContact
    case scanPaste
    case newGroup

    var id: Self { self }
}

enum ContactType: Int {
    case card, request, recent, chatDeleted, unlisted
}

struct NewChatMenuButton: View {
    @State private var showNewChatSheet = false

    var body: some View {
            Button {
            showNewChatSheet = true
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(isPresented: $showNewChatSheet) {
            NewChatSheet()
        }
    }
}

private var indent: CGFloat = 36

struct NewChatSheet: View {
    @State var newChatMenuOption: NewChatMenuOption?
    @EnvironmentObject var theme: AppTheme
    @State private var baseContactTypes: [ContactType] = [.card, .request, .recent]

    var body: some View {
        NavigationView {
            viewBody()
                .navigationTitle("New Chat")
                .navigationBarTitleDisplayMode(.inline)
                .modifier(ThemedBackground(grouped: true))
        }
    }
    
    @ViewBuilder private func viewBody() -> some View {
        List {
            Section {
                Button {
                    newChatMenuOption = .newContact
                } label: {
                    newChatActionButton("link.badge.plus", color: theme.colors.secondary) { Text("Add contact") }
                }
                Button {
                    newChatMenuOption = .scanPaste
                } label: {
                    newChatActionButton("qrcode", color: theme.colors.secondary) { Text("Scan / Paste link") }
                }
                Button {
                    newChatMenuOption = .newGroup
                } label: {
                    newChatActionButton("person.2", color: theme.colors.secondary) { Text("Create group") }
                }
            }
            Section(header: Text("Your contacts").textCase(.uppercase).foregroundColor(theme.colors.secondary)) {
                ContactsList(baseContactTypes: $baseContactTypes)
            }
        }
        .sheet(item: $newChatMenuOption) { opt in
            switch opt {
            case .newContact: NewChatView(selection: .invite)
            case .scanPaste: NewChatView(selection: .connect, showQRCodeScanner: true)
            case .newGroup: AddGroupView()
            }
        }
    }
    
    func newChatActionButton<Content : View>(_ icon: String, color: Color/* = .secondary*/, content: @escaping () -> Content) -> some View {
        ZStack(alignment: .leading) {
            Image(systemName: icon).frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .symbolRenderingMode(.monochrome)
                .foregroundColor(color)
            content().foregroundColor(theme.colors.onBackground).padding(.leading, indent)
        }
    }
}

func chatContactType(chat: Chat) -> ContactType {
    switch chat.chatInfo {
    case .contactRequest:
        return .request
    case let .direct(contact):
        if contact.activeConn == nil && contact.profile.contactLink != nil {
            return .card
        } else if contact.chatDeleted {
            return .chatDeleted
        } else if contact.contactStatus == .active {
            return .recent
        } else {
            return .unlisted
        }
    default:
        return .unlisted
    }
}


struct ContactsList: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var baseContactTypes: [ContactType]
    @State private var searchText = ""
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    
    var body: some View {
        let contactTypes = contactTypesSearchTargets(baseContactTypes: baseContactTypes, searchEmpty: searchText.isEmpty)
        let contactChats = filterContactTypes(chats: chatModel.chats, contactTypes: contactTypes)
        let filteredContactChats = filteredContactChats(
            showUnreadAndFavorites: showUnreadAndFavorites,
            searchShowingSimplexLink: false,
            searchChatFilteredBySimplexLink: searchChatFilteredBySimplexLink,
            searchText: searchText,
            contactChats: contactChats
        )
        
        ForEach(filteredContactChats, id: \.viewId) { chat in
            ContactListNavLink(chat: chat)
                .disabled(chatModel.chatRunning != true)
        }
    }
    
    private func contactTypesSearchTargets(baseContactTypes: [ContactType], searchEmpty: Bool) -> [ContactType] {
        if baseContactTypes.contains(.chatDeleted) || searchEmpty {
            return baseContactTypes
        } else {
            return baseContactTypes + [.chatDeleted]
        }
    }
    
    private func filterContactTypes(chats: [Chat], contactTypes: [ContactType]) -> [Chat] {
        return chats.filter { chat in
            contactTypes.contains(chatContactType(chat: chat))
        }
    }
    
    private func chatsByTypeComparator(chat1: Chat, chat2: Chat) -> Bool {
        let chat1Type = chatContactType(chat: chat1)
        let chat2Type = chatContactType(chat: chat2)

        if chat1Type.rawValue < chat2Type.rawValue {
            return true
        } else if chat1Type.rawValue > chat2Type.rawValue {
            return false
        } else {
            return chat2.chatInfo.chatTs < chat1.chatInfo.chatTs
        }
    }
        
    private func filterChat(chat: Chat, searchText: String, showUnreadAndFavorites: Bool) -> Bool {
        var meetsPredicate = true
        let s = searchText.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let cInfo = chat.chatInfo

        if !searchText.isEmpty {
            if (!cInfo.chatViewName.lowercased().contains(searchText.lowercased())) {
                if case let .direct(contact) = cInfo {
                    meetsPredicate = contact.profile.displayName.lowercased().contains(s) || contact.fullName.lowercased().contains(s)
                } else {
                    meetsPredicate = false
                }
            }
        }

        if showUnreadAndFavorites {
            meetsPredicate = meetsPredicate && (cInfo.chatSettings?.favorite ?? false)
        }

        return meetsPredicate
    }
    
    func filteredContactChats(
        showUnreadAndFavorites: Bool,
        searchShowingSimplexLink: Bool,
        searchChatFilteredBySimplexLink: String?,
        searchText: String,
        contactChats: [Chat]
    ) -> [Chat] {
        let linkChatId = searchChatFilteredBySimplexLink
        let s = searchShowingSimplexLink ? "" : searchText.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()

        let filteredChats: [Chat]

        if let linkChatId = linkChatId {
            filteredChats = contactChats.filter { $0.id == linkChatId }
        } else {
            filteredChats = contactChats.filter { chat in
                filterChat(chat: chat, searchText: s, showUnreadAndFavorites: showUnreadAndFavorites)
            }
        }

        return filteredChats.sorted(by: chatsByTypeComparator)
    }
}

#Preview {
    NewChatMenuButton()
}
