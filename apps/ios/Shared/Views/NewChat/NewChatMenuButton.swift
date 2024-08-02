//
//  NewChatMenuButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

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
    @EnvironmentObject var chatModel: ChatModel
    @State private var searchMode = false
    @FocusState var searchFocussed: Bool
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    
    var body: some View {
        NavigationView {
            viewBody()
                .navigationTitle("New Chat")
                .navigationBarTitleDisplayMode(.inline)
                .navigationBarHidden(searchMode)
                .modifier(ThemedBackground(grouped: true))
        }
    }
    
    @ViewBuilder private func viewBody() -> some View {
        List {
            ContactsListSearchBar(
                searchMode: $searchMode,
                searchFocussed: $searchFocussed,
                searchText: $searchText,
                searchShowingSimplexLink: $searchShowingSimplexLink,
                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
            )
            .listRowSeparator(.hidden)
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .frame(maxWidth: .infinity)
            
            if (searchText.isEmpty) {
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
                
                if (!filterContactTypes(chats: chatModel.chats, contactTypes: [.chatDeleted]).isEmpty) {
                    Section {
                        NavigationLink {
                            DeletedChats()
                        } label: {
                            newChatActionButton("folder", color: theme.colors.secondary) { Text("Deleted chats") }
                        }
                    }
                }
            }
            
            Section(header: Text("Your contacts").textCase(.uppercase).foregroundColor(theme.colors.secondary)) {
                ContactsList(
                    baseContactTypes: $baseContactTypes,
                    searchMode: $searchMode,
                    searchText: $searchText,
                    searchFocussed: $searchFocussed,
                    searchShowingSimplexLink: $searchShowingSimplexLink,
                    searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
                )
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

private func filterContactTypes(chats: [Chat], contactTypes: [ContactType]) -> [Chat] {
    return chats.filter { chat in
        contactTypes.contains(chatContactType(chat: chat))
    }
}

struct ContactsList: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var baseContactTypes: [ContactType]
    @Binding var searchMode: Bool
    @Binding var searchText: String
    @FocusState.Binding var searchFocussed: Bool
    @Binding var searchShowingSimplexLink: Bool
    @Binding var searchChatFilteredBySimplexLink: String?
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    
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

struct ContactsListSearchBar: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var searchMode: Bool
    @FocusState.Binding var searchFocussed: Bool
    @Binding var searchText: String
    @Binding var searchShowingSimplexLink: Bool
    @Binding var searchChatFilteredBySimplexLink: String?
    @State private var ignoreSearchTextChange = false
    @State private var alert: PlanAndConnectAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false

    var body: some View {
        VStack {
            HStack {
                HStack {
                    Image(systemName: "magnifyingglass")
                    TextField("Search or paste SimpleX link", text: $searchText)
                        .foregroundColor(searchShowingSimplexLink ? theme.colors.secondary : theme.colors.onBackground)
                        .disabled(searchShowingSimplexLink)
                        .focused($searchFocussed)
                        .frame(maxWidth: .infinity)
                    if !searchText.isEmpty {
                        Image(systemName: "xmark.circle.fill")
                            .onTapGesture {
                                searchText = ""
                            }
                    }
                }
                .padding(EdgeInsets(top: 7, leading: 7, bottom: 7, trailing: 7))
                .foregroundColor(theme.colors.secondary)
                .background(Color(.tertiarySystemFill))
                .cornerRadius(10.0)

                if searchFocussed {
                    Text("Cancel")
                        .foregroundColor(theme.colors.primary)
                        .onTapGesture {
                            searchText = ""
                            searchFocussed = false
                        }
                } else if m.chats.count > 0 {
                    toggleFilterButton()
                }
            }
            Divider()
        }
        .onChange(of: searchFocussed) { sf in
            withAnimation { searchMode = sf }
        }
        .onChange(of: searchText) { t in
            if ignoreSearchTextChange {
                ignoreSearchTextChange = false
            } else {
                if let link = strHasSingleSimplexLink(t.trimmingCharacters(in: .whitespaces)) { // if SimpleX link is pasted, show connection dialogue
                    searchFocussed = false
                    if case let .simplexLink(linkType, _, smpHosts) = link.format {
                        ignoreSearchTextChange = true
                        searchText = simplexLinkText(linkType, smpHosts)
                    }
                    searchShowingSimplexLink = true
                    searchChatFilteredBySimplexLink = nil
                    connect(link.text)
                } else {
                    if t != "" { // if some other text is pasted, enter search mode
                        searchFocussed = true
                    }
                    searchShowingSimplexLink = false
                    searchChatFilteredBySimplexLink = nil
                }
            }
        }
        .alert(item: $alert) { a in
            planAndConnectAlert(a, dismiss: true, cleanup: { searchText = "" })
        }
        .actionSheet(item: $sheet) { s in
            planAndConnectActionSheet(s, dismiss: true, cleanup: { searchText = "" })
        }
    }

    private func toggleFilterButton() -> some View {
        ZStack {
            Color.clear
                .frame(width: 22, height: 22)
            Image(systemName: showUnreadAndFavorites ? "line.3.horizontal.decrease.circle.fill" : "line.3.horizontal.decrease")
                .resizable()
                .scaledToFit()
                .foregroundColor(showUnreadAndFavorites ? theme.colors.primary : theme.colors.secondary)
                .frame(width: showUnreadAndFavorites ? 22 : 16, height: showUnreadAndFavorites ? 22 : 16)
                .onTapGesture {
                    showUnreadAndFavorites = !showUnreadAndFavorites
                }
        }
    }

    private func connect(_ link: String) {
        planAndConnect(
            link,
            showAlert: { alert = $0 },
            showActionSheet: { sheet = $0 },
            dismiss: false,
            incognito: nil,
            filterKnownContact: { searchChatFilteredBySimplexLink = $0.id },
            filterKnownGroup: { searchChatFilteredBySimplexLink = $0.id }
        )
    }
}


struct DeletedChats: View {
    @State private var baseContactTypes: [ContactType] = [.chatDeleted]
    @State private var searchMode = false
    @FocusState var searchFocussed: Bool
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    
    var body: some View {
        List {
            ContactsList(
                baseContactTypes: $baseContactTypes,
                searchMode: $searchMode,
                searchText: $searchText,
                searchFocussed: $searchFocussed,
                searchShowingSimplexLink: $searchShowingSimplexLink,
                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
            )
        }
        .modifier(ThemedBackground(grouped: true))
    }
}

#Preview {
    NewChatMenuButton()
}
