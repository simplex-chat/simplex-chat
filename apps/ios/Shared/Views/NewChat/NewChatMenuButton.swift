//
//  NewChatMenuButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum ContactType: Int {
    case card, request, recent, chatDeleted, unlisted
}

struct NewChatMenuButton: View {
    // do not use chatModel here because it prevents showing AddGroupMembersView after group creation and QR code after link creation on iOS 16
//    @EnvironmentObject var chatModel: ChatModel
    @State private var showNewChatSheet = false
    @State private var alert: SomeAlert? = nil
    @State private var pendingConnection: PendingContactConnection? = nil

    var body: some View {
            Button {
            showNewChatSheet = true
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .appSheet(isPresented: $showNewChatSheet) {
            NewChatSheet(pendingConnection: $pendingConnection)
                .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil)
                .onDisappear {
                    alert = cleanupPendingConnection(contactConnection: pendingConnection)
                    pendingConnection = nil
                }
        }
        .alert(item: $alert) { a in
            return a.alert
        }
    }
}

private var indent: CGFloat = 36

struct NewChatSheet: View {
    @EnvironmentObject var theme: AppTheme
    @State private var baseContactTypes: [ContactType] = [.card, .request, .recent]
    @EnvironmentObject var chatModel: ChatModel
    @State private var searchMode = false
    @FocusState var searchFocussed: Bool
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    @State private var alert: SomeAlert?
    @Binding var pendingConnection: PendingContactConnection?

    // Sheet height management
    @State private var isAddContactActive = false
    @State private var isScanPasteLinkActive = false
    @State private var isLargeSheet = false
    @State private var allowSmallSheet = true

    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true

    var body: some View {
        let showArchive = !filterContactTypes(chats: chatModel.chats, contactTypes: [.chatDeleted]).isEmpty
        let v = NavigationView {
            viewBody(showArchive)
                .navigationTitle("New message")
                .navigationBarTitleDisplayMode(.large)
                .navigationBarHidden(searchMode)
                .modifier(ThemedBackground(grouped: true))
                .alert(item: $alert) { a in
                    return a.alert
                }
        }
        if #available(iOS 16.0, *), oneHandUI {
            let sheetHeight: CGFloat = showArchive ? 575 : 500
            v.presentationDetents(
                allowSmallSheet ? [.height(sheetHeight), .large] : [.large],
                selection: Binding(
                    get: { isLargeSheet || !allowSmallSheet ? .large : .height(sheetHeight) },
                    set: { isLargeSheet = $0 == .large }
                )
            )
        } else {
            v
        }
    }

    @ViewBuilder private func viewBody(_ showArchive: Bool) -> some View {
        List {
            HStack {
                ContactsListSearchBar(
                    searchMode: $searchMode,
                    searchFocussed: $searchFocussed,
                    searchText: $searchText,
                    searchShowingSimplexLink: $searchShowingSimplexLink,
                    searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
                )
                .frame(maxWidth: .infinity)
            }
            .listRowSeparator(.hidden)
            .listRowBackground(Color.clear)
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            if (searchText.isEmpty) {
                Section {
                    NavigationLink(isActive: $isAddContactActive) {
                        NewChatView(selection: .invite, parentAlert: $alert, contactConnection: $pendingConnection)
                            .navigationTitle("New chat")
                            .modifier(ThemedBackground(grouped: true))
                            .navigationBarTitleDisplayMode(.large)
                    } label: {
                        navigateOnTap(Label("Add contact", systemImage: "link.badge.plus")) {
                            isAddContactActive = true
                        }
                    }
                    NavigationLink(isActive: $isScanPasteLinkActive) {
                        NewChatView(selection: .connect, showQRCodeScanner: true, parentAlert: $alert, contactConnection: $pendingConnection)
                            .navigationTitle("New chat")
                            .modifier(ThemedBackground(grouped: true))
                            .navigationBarTitleDisplayMode(.large)
                    } label: {
                        navigateOnTap(Label("Scan / Paste link", systemImage: "qrcode")) {
                            isScanPasteLinkActive = true
                        }
                    }
                    NavigationLink {
                        AddGroupView()
                            .navigationTitle("Create secret group")
                            .modifier(ThemedBackground(grouped: true))
                            .navigationBarTitleDisplayMode(.large)
                    } label: {
                        Label("Create group", systemImage: "person.2.circle.fill")
                    }
                }
                
                if (showArchive) {
                    Section {
                        NavigationLink {
                            DeletedChats()
                        } label: {
                            newChatActionButton("archivebox", color: theme.colors.secondary) { Text("Archived contacts") }
                        }
                    }
                }
            }
            
            ContactsList(
                baseContactTypes: $baseContactTypes,
                searchMode: $searchMode,
                searchText: $searchText,
                header: "Your Contacts",
                searchFocussed: $searchFocussed,
                searchShowingSimplexLink: $searchShowingSimplexLink,
                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink,
                showDeletedChatIcon: true
            )
        }
    }
    
    /// Extends label's tap area to match `.insetGrouped` list row insets
    private func navigateOnTap<L: View>(_ label: L, setActive: @escaping () -> Void) -> some View {
        label
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.leading, 16).padding(.vertical, 8).padding(.trailing, 32)
            .contentShape(Rectangle())
            .onTapGesture {
                isLargeSheet = true
                DispatchQueue.main.async {
                    allowSmallSheet = false
                    setActive()
                }
            }
            .padding(.leading, -16).padding(.vertical, -8).padding(.trailing, -32)
    }

    func newChatActionButton<Content : View>(_ icon: String, color: Color/* = .secondary*/, content: @escaping () -> Content) -> some View {
        ZStack(alignment: .leading) {
            Image(systemName: icon)
                .resizable()
                .scaledToFit()
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
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
        if contact.activeConn == nil && contact.profile.contactLink != nil && contact.active {
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
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatModel: ChatModel
    @Binding var baseContactTypes: [ContactType]
    @Binding var searchMode: Bool
    @Binding var searchText: String
    var header: String? = nil
    @FocusState.Binding var searchFocussed: Bool
    @Binding var searchShowingSimplexLink: Bool
    @Binding var searchChatFilteredBySimplexLink: String?
    var showDeletedChatIcon: Bool
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    
    var body: some View {
        let contactTypes = contactTypesSearchTargets(baseContactTypes: baseContactTypes, searchEmpty: searchText.isEmpty)
        let contactChats = filterContactTypes(chats: chatModel.chats, contactTypes: contactTypes)
        let filteredContactChats = filteredContactChats(
            showUnreadAndFavorites: showUnreadAndFavorites,
            searchShowingSimplexLink: searchShowingSimplexLink,
            searchChatFilteredBySimplexLink: searchChatFilteredBySimplexLink,
            searchText: searchText,
            contactChats: contactChats
        )
        
        if !filteredContactChats.isEmpty {
            Section(header: Group {
                if let header = header {
                    Text(header)
                        .textCase(.uppercase)
                        .foregroundColor(theme.colors.secondary)
                    }
                }
            ) {
                ForEach(filteredContactChats, id: \.viewId) { chat in
                    ContactListNavLink(chat: chat, showDeletedChatIcon: showDeletedChatIcon)
                        .disabled(chatModel.chatRunning != true)
                }
            }
        }
        
        if filteredContactChats.isEmpty && !contactChats.isEmpty {
            noResultSection(text: "No filtered contacts")
        } else if contactChats.isEmpty {
            noResultSection(text: "No contacts")
        }
    }
    
    @ViewBuilder private func noResultSection(text: String) -> some View {
        Section {
            Text(text)
                .foregroundColor(theme.colors.secondary)
                .frame(maxWidth: .infinity, alignment: .center)

        }
        .listRowSeparator(.hidden)
        .listRowBackground(Color.clear)
        .listRowInsets(EdgeInsets(top: 7, leading: 0, bottom: 7, trailing: 0))
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
        HStack(spacing: 12) {
            HStack(spacing: 4) {
                Spacer()
                    .frame(width: 8)
                Image(systemName: "magnifyingglass")
                    .resizable()
                    .scaledToFit()
                    .frame(width: 16, height: 16)
                TextField("Search or paste SimpleX link", text: $searchText)
                    .foregroundColor(searchShowingSimplexLink ? theme.colors.secondary : theme.colors.onBackground)
                    .disabled(searchShowingSimplexLink)
                    .focused($searchFocussed)
                    .frame(maxWidth: .infinity)
                if !searchText.isEmpty {
                    Image(systemName: "xmark.circle.fill")
                        .resizable()
                        .scaledToFit()
                        .frame(width: 16, height: 16)
                        .onTapGesture {
                            searchText = ""
                        }
                }
            }
            .padding(EdgeInsets(top: 7, leading: 7, bottom: 7, trailing: 7))
            .foregroundColor(theme.colors.secondary)
            .background(Color(uiColor: .secondarySystemGroupedBackground))
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
        .padding(.top, 24)
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
            dismiss: true,
            incognito: nil,
            filterKnownContact: { searchChatFilteredBySimplexLink = $0.id }
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
            
            ContactsList(
                baseContactTypes: $baseContactTypes,
                searchMode: $searchMode,
                searchText: $searchText,
                searchFocussed: $searchFocussed,
                searchShowingSimplexLink: $searchShowingSimplexLink,
                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink,
                showDeletedChatIcon: false
            )
        }
        .navigationTitle("Archived contacts")
        .navigationBarTitleDisplayMode(.large)
        .navigationBarHidden(searchMode)
        .modifier(ThemedBackground(grouped: true))

    }
}

#Preview {
    NewChatMenuButton()
}
