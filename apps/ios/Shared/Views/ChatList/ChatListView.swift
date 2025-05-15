//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum UserPickerSheet: Identifiable {
    case address
    case chatPreferences
    case chatProfiles
    case currentProfile
    case useFromDesktop
    case settings

    var id: Self { self }

    var navigationTitle: LocalizedStringKey {
        switch self {
        case .address: "SimpleX address"
        case .chatPreferences: "Your preferences"
        case .chatProfiles: "Your chat profiles"
        case .currentProfile: "Your current profile"
        case .useFromDesktop: "Connect to desktop"
        case .settings: "Your settings"
        }
    }
}

enum PresetTag: Int, Identifiable, CaseIterable, Equatable {
    case groupReports = 0
    case favorites = 1
    case contacts = 2
    case groups = 3
    case business = 4
    case notes = 5

    var id: Int { rawValue }
    
    var сollapse: Bool {
        self != .groupReports
    }
}

enum ActiveFilter: Identifiable, Equatable {
    case presetTag(PresetTag)
    case userTag(ChatTag)
    case unread
    
    var id: String {
        switch self {
        case let .presetTag(tag): "preset \(tag.id)"
        case let .userTag(tag): "user \(tag.chatTagId)"
        case .unread: "unread"
        }
    }
}

class SaveableSettings: ObservableObject {
    @Published var servers: ServerSettings = ServerSettings(currUserServers: [], userServers: [], serverErrors: [])
}

struct ServerSettings {
    public var currUserServers: [UserOperatorServers]
    public var userServers: [UserOperatorServers]
    public var serverErrors: [UserServersError]
}

struct UserPickerSheetView: View {
    let sheet: UserPickerSheet
    @EnvironmentObject var chatModel: ChatModel
    @StateObject private var ss = SaveableSettings()

    @State private var loaded = false

    var body: some View {
        NavigationView {
            ZStack {
                if loaded, let currentUser = chatModel.currentUser {
                    switch sheet {
                    case .address:
                        UserAddressView(shareViaProfile: currentUser.addressShared)
                    case .chatPreferences:
                        PreferencesView(
                            profile: currentUser.profile,
                            preferences: currentUser.fullPreferences,
                            currentPreferences: currentUser.fullPreferences
                        )
                    case .chatProfiles:
                        UserProfilesView()
                    case .currentProfile:
                        UserProfile()
                    case .useFromDesktop:
                        ConnectDesktopView()
                    case .settings:
                        SettingsView()
                    }
                }
                Color.clear // Required for list background to be rendered during loading
            }
            .navigationTitle(sheet.navigationTitle)
            .navigationBarTitleDisplayMode(.large)
            .modifier(ThemedBackground(grouped: true))
        }
        .overlay {
            if let la = chatModel.laRequest {
                LocalAuthView(authRequest: la)
            }
        }
        .task {
            withAnimation(
                .easeOut(duration: 0.1),
                { loaded = true }
            )
        }
        .onDisappear {
            if serversCanBeSaved(
                ss.servers.currUserServers,
                ss.servers.userServers,
                ss.servers.serverErrors
            ) {
                showAlert(
                    title: NSLocalizedString("Save servers?", comment: "alert title"),
                    buttonTitle: NSLocalizedString("Save", comment: "alert button"),
                    buttonAction: { saveServers($ss.servers.currUserServers, $ss.servers.userServers) },
                    cancelButton: true
                )
            }
        }
        .environmentObject(ss)
    }
}

struct ChatListView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var activeUserPickerSheet: UserPickerSheet?
    @State private var searchMode = false
    @FocusState private var searchFocussed
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    @State private var scrollToSearchBar = false
    @State private var userPickerShown: Bool = false
    @State private var sheet: SomeSheet<AnyView>? = nil
    @StateObject private var chatTagsModel = ChatTagsModel.shared

    // iOS 15 is required it to show/hide toolbar while chat is hidden/visible
    @State private var viewOnScreen = true

    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @AppStorage(DEFAULT_ONE_HAND_UI_CARD_SHOWN) private var oneHandUICardShown = false
    @AppStorage(DEFAULT_ADDRESS_CREATION_CARD_SHOWN) private var addressCreationCardShown = false
    @AppStorage(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    
    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody.scrollDismissesKeyboard(.immediately)
        } else {
            viewBody
        }
    }
    
    private var viewBody: some View {
        ZStack(alignment: oneHandUI ? .bottomLeading : .topLeading) {
            NavStackCompat(
                isActive: Binding(
                    get: { chatModel.chatId != nil },
                    set: { active in
                        if !active { chatModel.chatId = nil }
                    }
                ),
                destination: chatView
            ) { chatListView }
        }
        .modifier(
            Sheet(isPresented: $userPickerShown) {
                UserPicker(userPickerShown: $userPickerShown, activeSheet: $activeUserPickerSheet)
            }
        )
        .appSheet(
            item: $activeUserPickerSheet,
            onDismiss: { chatModel.laRequest = nil },
            content: { UserPickerSheetView(sheet: $0) }
        )
        .onChange(of: activeUserPickerSheet) {
            if $0 != nil {
                DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
                    userPickerShown = false
                }
            }
        }
        .environmentObject(chatTagsModel)
    }
    
    private var chatListView: some View {
        let tm = ToolbarMaterial.material(toolbarMaterial)
        return withToolbar(tm) {
            chatList
                .background(theme.colors.background)
                .navigationBarTitleDisplayMode(.inline)
                .navigationBarHidden(searchMode || oneHandUI)
        }
        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
        .onAppear {
            if #unavailable(iOS 16.0), !viewOnScreen {
                viewOnScreen = true
            }
        }
        .onDisappear {
            activeUserPickerSheet = nil
            if #unavailable(iOS 16.0) {
                viewOnScreen = false
            }
        }
        .refreshable {
            AlertManager.shared.showAlert(Alert(
                title: Text("Reconnect servers?"),
                message: Text("Reconnect all connected servers to force message delivery. It uses additional traffic."),
                primaryButton: .default(Text("Ok")) {
                    Task {
                        do {
                            try await reconnectAllServers()
                        } catch let error {
                            AlertManager.shared.showAlertMsg(title: "Error", message: "\(responseError(error))")
                        }
                    }
                },
                secondaryButton: .cancel()
            ))
        }
        .safeAreaInset(edge: .top) {
            if oneHandUI { Divider().background(tm) }
        }
        .safeAreaInset(edge: .bottom) {
            if oneHandUI {
                Divider().padding(.bottom, Self.hasHomeIndicator ? 0 : 8).background(tm)
            }
        }
        .sheet(item: $sheet) { sheet in
            if #available(iOS 16.0, *) {
                sheet.content.presentationDetents([.fraction(sheet.fraction)])
            } else {
                sheet.content
            }
        }
    }
    
    static var hasHomeIndicator: Bool = {
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
           let window = windowScene.windows.first {
            window.safeAreaInsets.bottom > 0
        } else { false }
    }()
    
    @ViewBuilder func withToolbar(_ material: Material, content: () -> some View) -> some View {
        if #available(iOS 16.0, *) {
            if oneHandUI {
                content()
                    .toolbarBackground(.hidden, for: .bottomBar)
                    .toolbar { bottomToolbar }
            } else {
                content()
                    .toolbarBackground(.automatic, for: .navigationBar)
                    .toolbarBackground(material)
                    .toolbar { topToolbar }
            }
        } else {
            if oneHandUI {
                content().toolbar { bottomToolbarGroup() }
            } else {
                content().toolbar { topToolbar }
            }
        }
    }
    
    @ToolbarContentBuilder var topToolbar: some ToolbarContent {
        ToolbarItem(placement: .topBarLeading) { leadingToolbarItem }
        ToolbarItem(placement: .principal) { SubsStatusIndicator() }
        ToolbarItem(placement: .topBarTrailing) { trailingToolbarItem }
    }
    
    @ToolbarContentBuilder var bottomToolbar: some ToolbarContent {
        let padding: Double = Self.hasHomeIndicator ? 0 : 14
        ToolbarItem(placement: .bottomBar) {
            HStack {
                leadingToolbarItem.padding(.bottom, padding)
                Spacer()
                SubsStatusIndicator().padding(.bottom, padding)
                Spacer()
                trailingToolbarItem.padding(.bottom, padding)
            }
            .contentShape(Rectangle())
            .onTapGesture { scrollToSearchBar = true }
        }
    }
    
    @ToolbarContentBuilder func bottomToolbarGroup() -> some ToolbarContent {
        let padding: Double = Self.hasHomeIndicator ? 0 : 14
        ToolbarItemGroup(placement: viewOnScreen ? .bottomBar : .principal) {
            leadingToolbarItem.padding(.bottom, padding)
            Spacer()
            SubsStatusIndicator().padding(.bottom, padding)
            Spacer()
            trailingToolbarItem.padding(.bottom, padding)
        }
    }
    
    @ViewBuilder var leadingToolbarItem: some View {
        let user = chatModel.currentUser ?? User.sampleData
        ZStack(alignment: .topTrailing) {
            ProfileImage(imageStr: user.image, size: 32, color: Color(uiColor: .quaternaryLabel))
                .padding([.top, .trailing], 3)
            let allRead = chatModel.users
                .filter { u in !u.user.activeUser && !u.user.hidden }
                .allSatisfy { u in u.unreadCount == 0 }
            if !allRead {
                unreadBadge(size: 12)
            }
        }
        .onTapGesture {
            userPickerShown = true
        }
    }
    
    @ViewBuilder var trailingToolbarItem: some View {
        switch chatModel.chatRunning {
        case .some(true): NewChatMenuButton()
        case .some(false): chatStoppedIcon()
        case .none: EmptyView()
        }
    }
    
    private var chatList: some View {
        let cs = filteredChats()
        return ZStack {
            ScrollViewReader { scrollProxy in
                List {
                    if !chatModel.chats.isEmpty {
                        ChatListSearchBar(
                            searchMode: $searchMode,
                            searchFocussed: $searchFocussed,
                            searchText: $searchText,
                            searchShowingSimplexLink: $searchShowingSimplexLink,
                            searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink,
                            parentSheet: $sheet
                        )
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .listRowSeparator(.hidden)
                        .listRowBackground(Color.clear)
                        .frame(maxWidth: .infinity)
                        .padding(.top, oneHandUI ? 8 : 0)
                        .id("searchBar")
                    }
                    if #available(iOS 16.0, *) {
                        ForEach(cs, id: \.viewId) { chat in
                            ChatListNavLink(chat: chat, parentSheet: $sheet)
                                .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                                .padding(.trailing, -16)
                                .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                                .listRowBackground(Color.clear)
                        }
                        .offset(x: -8)
                    } else {
                        ForEach(cs, id: \.viewId) { chat in
                            ChatListNavLink(chat: chat,  parentSheet: $sheet)
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .listRowSeparator(.hidden)
                            .listRowInsets(EdgeInsets())
                            .background { theme.colors.background } // Hides default list selection colour
                            .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                        }
                    }
                    if !oneHandUICardShown {
                        OneHandUICard()
                            .padding(.vertical, 6)
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .listRowSeparator(.hidden)
                            .listRowBackground(Color.clear)
                    }
                    if !addressCreationCardShown {
                        AddressCreationCard()
                            .padding(.vertical, 6)
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .listRowSeparator(.hidden)
                            .listRowBackground(Color.clear)
                    }
                }
                .listStyle(.plain)
                .onChange(of: chatModel.chatId) { currentChatId in
                    if let chatId = chatModel.chatToTop, currentChatId != chatId {
                        chatModel.chatToTop = nil
                        chatModel.popChat(chatId)
                    }
                    stopAudioPlayer()
                }
                .onChange(of: chatModel.currentUser?.userId) { _ in
                    stopAudioPlayer()
                }
                .onChange(of: scrollToSearchBar) { scrollToSearchBar in
                    if scrollToSearchBar {
                        Task { self.scrollToSearchBar = false }
                        withAnimation { scrollProxy.scrollTo("searchBar") }
                    }
                }
            }
            if cs.isEmpty && !chatModel.chats.isEmpty {
                noChatsView()
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            }
        }
    }
    
    @ViewBuilder private func noChatsView() -> some View {
        if searchString().isEmpty {
            switch chatTagsModel.activeFilter {
            case .presetTag: Text("No filtered chats") // this should not happen
            case let .userTag(tag): Text("No chats in list \(tag.chatTagText)")
            case .unread:
                Button {
                    chatTagsModel.activeFilter = nil
                } label: {
                    HStack {
                        Image(systemName: "line.3.horizontal.decrease")
                        Text("No unread chats")
                    }
                }
            case .none: Text("No chats")
            }
        } else {
            Text("No chats found")
        }
    }

    
    private func unreadBadge(size: CGFloat = 18) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(theme.colors.primary)
    }
    
    @ViewBuilder private func chatView() -> some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            ChatView(chat: chat)
        }
    }
    
    func stopAudioPlayer() {
        VoiceItemState.smallView.values.forEach { $0.audioPlayer?.stop() }
        VoiceItemState.smallView = [:]
    }
    
    private func filteredChats() -> [Chat] {
        if let linkChatId = searchChatFilteredBySimplexLink {
            return chatModel.chats.filter { $0.id == linkChatId }
        } else {
            let s = searchString()
            return s == ""
            ? chatModel.chats.filter { chat in
                !chat.chatInfo.chatDeleted && !chat.chatInfo.contactCard && filtered(chat)
            }
            : chatModel.chats.filter { chat in
                let cInfo = chat.chatInfo
                return switch cInfo {
                case let .direct(contact):
                    !contact.chatDeleted && !chat.chatInfo.contactCard && (
                        ( viewNameContains(cInfo, s) ||
                          contact.profile.displayName.localizedLowercase.contains(s) ||
                          contact.fullName.localizedLowercase.contains(s)
                        )
                    )
                case .group: viewNameContains(cInfo, s)
                case .local: viewNameContains(cInfo, s)
                case .contactRequest: viewNameContains(cInfo, s)
                case let .contactConnection(conn): conn.localAlias.localizedLowercase.contains(s)
                case .invalidJSON: false
                }
            }
        }
        
        func filtered(_ chat: Chat) -> Bool {
            switch chatTagsModel.activeFilter {
            case let .presetTag(tag): presetTagMatchesChat(tag, chat.chatInfo, chat.chatStats)
            case let .userTag(tag): chat.chatInfo.chatTags?.contains(tag.chatTagId) == true
            case .unread: chat.unreadTag
            case .none: true
            }
        }
        
        func viewNameContains(_ cInfo: ChatInfo, _ s: String) -> Bool {
            cInfo.chatViewName.localizedLowercase.contains(s)
        }
    }
    
    func searchString() -> String {
        searchShowingSimplexLink ? "" : searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
    }
}

struct SubsStatusIndicator: View {
    @State private var subs: SMPServerSubs = SMPServerSubs.newSMPServerSubs
    @State private var hasSess: Bool = false
    @State private var task: Task<Void, Never>?
    @State private var showServersSummary = false

    @AppStorage(DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE) private var showSubscriptionPercentage = false

    var body: some View {
        Button {
            showServersSummary = true
        } label: {
            HStack(spacing: 4) {
                Text("Chats").foregroundStyle(Color.primary).fixedSize().font(.headline)
                SubscriptionStatusIndicatorView(subs: subs, hasSess: hasSess)
                if showSubscriptionPercentage {
                    SubscriptionStatusPercentageView(subs: subs, hasSess: hasSess)
                }
            }
        }
        .disabled(ChatModel.shared.chatRunning != true)
        .onAppear {
            startTask()
        }
        .onDisappear {
            stopTask()
        }
        .appSheet(isPresented: $showServersSummary) {
            ServersSummaryView()
                .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil)
        }
    }

    private func startTask() {
        task = Task {
            while !Task.isCancelled {
                if AppChatState.shared.value == .active, ChatModel.shared.chatRunning == true {
                    do {
                        let (subs, hasSess) = try await getAgentSubsTotal()
                        await MainActor.run {
                            self.subs = subs
                            self.hasSess = hasSess
                        }
                    } catch let error {
                        logger.error("getSubsTotal error: \(responseError(error))")
                    }
                }
                try? await Task.sleep(nanoseconds: 1_000_000_000) // Sleep for 1 second
            }
        }
    }

    func stopTask() {
        task?.cancel()
        task = nil
    }
}

struct ChatListSearchBar: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @EnvironmentObject var chatTagsModel: ChatTagsModel
    @Binding var searchMode: Bool
    @FocusState.Binding var searchFocussed: Bool
    @Binding var searchText: String
    @Binding var searchShowingSimplexLink: Bool
    @Binding var searchChatFilteredBySimplexLink: String?
    @Binding var parentSheet: SomeSheet<AnyView>?
    @State private var ignoreSearchTextChange = false
    @State private var alert: PlanAndConnectAlert?
    @State private var sheet: PlanAndConnectActionSheet?

    var body: some View {
        VStack(spacing: 12) {
            ScrollView([.horizontal], showsIndicators: false) { TagsView(parentSheet: $parentSheet, searchText: $searchText) }
            HStack(spacing: 12) {
                HStack(spacing: 4) {
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
        .onChange(of: chatTagsModel.activeFilter) { _ in
            searchText = ""
        }
        .alert(item: $alert) { a in
            planAndConnectAlert(a, dismiss: true, cleanup: { searchText = "" })
        }
        .actionSheet(item: $sheet) { s in
            planAndConnectActionSheet(s, dismiss: true, cleanup: { searchText = "" })
        }
    }

    private func toggleFilterButton() -> some View {
        let showUnread = chatTagsModel.activeFilter == .unread
        return ZStack {
            Color.clear
                .frame(width: 22, height: 22)
            Image(systemName: showUnread ? "line.3.horizontal.decrease.circle.fill" : "line.3.horizontal.decrease")
                .resizable()
                .scaledToFit()
                .foregroundColor(showUnread ? theme.colors.primary : theme.colors.secondary)
                .frame(width: showUnread ? 22 : 16, height: showUnread ? 22 : 16)
                .onTapGesture {
                    if chatTagsModel.activeFilter == .unread {
                        chatTagsModel.activeFilter = nil
                    } else {
                        chatTagsModel.activeFilter = .unread
                    }
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

struct TagsView: View {
    @EnvironmentObject var chatTagsModel: ChatTagsModel
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var parentSheet: SomeSheet<AnyView>?
    @Binding var searchText: String

    var body: some View {
        HStack {
            tagsView()
        }
    }
    
    @ViewBuilder private func tagsView() -> some View {
        if chatTagsModel.presetTags.count > 1 {
            if chatTagsModel.presetTags.count + chatTagsModel.userTags.count <= 3 {
                expandedPresetTagsFiltersView()
            } else {
                collapsedTagsFilterView()
                ForEach(PresetTag.allCases, id: \.id) { (tag: PresetTag) in
                    if !tag.сollapse && (chatTagsModel.presetTags[tag] ?? 0) > 0 {
                        expandedTagFilterView(tag)
                    }
                }
            }
        }
        let selectedTag: ChatTag? = if case let .userTag(tag) = chatTagsModel.activeFilter {
            tag
        } else {
            nil
        }
        ForEach(chatTagsModel.userTags, id: \.id) { tag in
            let current = tag == selectedTag
            let color: Color = current ? .accentColor : .secondary
            ZStack {
                HStack(spacing: 4) {
                    if let emoji = tag.chatTagEmoji {
                        Text(emoji)
                    } else {
                        Image(systemName: current ? "tag.fill" : "tag")
                            .foregroundColor(color)
                    }
                    ZStack {
                        let badge = Text(verbatim: (chatTagsModel.unreadTags[tag.chatTagId] ?? 0) > 0 ? " ●" : "").font(.footnote)
                        (Text(tag.chatTagText).fontWeight(.semibold) + badge).foregroundColor(.clear)
                        Text(tag.chatTagText).fontWeight(current ? .semibold : .regular).foregroundColor(color) + badge.foregroundColor(theme.colors.primary)
                    }
                }
                .onTapGesture {
                    setActiveFilter(filter: .userTag(tag))
                }
                .onLongPressGesture {
                    let screenHeight = UIScreen.main.bounds.height
                    let reservedSpace: Double = 4 * 44 // 2 for padding, 1 for "Create list" and another for extra tag
                    let tagsSpace = Double(max(chatTagsModel.userTags.count, 3)) * 44
                    let fraction = min((reservedSpace + tagsSpace) / screenHeight, 0.62)

                    parentSheet = SomeSheet(
                        content: {
                            AnyView(
                                NavigationView {
                                    TagListView(chat: nil)
                                        .modifier(ThemedBackground(grouped: true))
                                }
                            )
                        },
                        id: "tag list",
                        fraction: fraction
                    )
                }
            }
        }
        
        Button {
            parentSheet = SomeSheet(
                content: {
                    AnyView(
                        NavigationView {
                            TagListEditor()
                        }
                    )
                },
                id: "tag create"
            )
        } label: {
            if chatTagsModel.userTags.isEmpty {
                HStack(spacing: 4) {
                    Image(systemName: "plus")
                    Text("Add list")
                }
            } else {
                Image(systemName: "plus")
            }
        }
        .foregroundColor(.secondary)
    }

    @ViewBuilder private func expandedTagFilterView(_ tag: PresetTag) -> some View {
        let selectedPresetTag: PresetTag? = if case let .presetTag(tag) = chatTagsModel.activeFilter {
            tag
        } else {
            nil
        }
        let active = tag == selectedPresetTag
        let (icon, text) = presetTagLabel(tag: tag, active: active)
        let color: Color = active ? .accentColor : .secondary

        HStack(spacing: 4) {
            Image(systemName: icon)
                .foregroundColor(color)
            ZStack {
                Text(text).fontWeight(.semibold).foregroundColor(.clear)
                Text(text).fontWeight(active ? .semibold : .regular).foregroundColor(color)
            }
        }
        .onTapGesture {
            setActiveFilter(filter: .presetTag(tag))
        }
    }

    private func expandedPresetTagsFiltersView() -> some View {
        ForEach(PresetTag.allCases, id: \.id) { tag in
            if (chatTagsModel.presetTags[tag] ?? 0) > 0 {
                expandedTagFilterView(tag)
            }
        }
    }
    
    @ViewBuilder private func collapsedTagsFilterView() -> some View {
        let selectedPresetTag: PresetTag? = if case let .presetTag(tag) = chatTagsModel.activeFilter {
            tag
        } else {
            nil
        }
        Menu {
            if chatTagsModel.activeFilter != nil || !searchText.isEmpty {
                Button {
                    chatTagsModel.activeFilter = nil
                    searchText = ""
                } label: {
                    HStack {
                        Image(systemName: "list.bullet")
                        Text("All")
                    }
                }
            }
            ForEach(PresetTag.allCases, id: \.id) { tag in
                if (chatTagsModel.presetTags[tag] ?? 0) > 0 && tag.сollapse {
                    Button {
                        setActiveFilter(filter: .presetTag(tag))
                    } label: {
                        let (systemName, text) = presetTagLabel(tag: tag, active: tag == selectedPresetTag)
                        HStack {
                            Image(systemName: systemName)
                            Text(text)
                        }
                    }
                }
            }
        } label: {
            if let tag = selectedPresetTag, tag.сollapse {
                let (systemName, _) = presetTagLabel(tag: tag, active: true)
                Image(systemName: systemName)
                    .foregroundColor(.accentColor)
            } else {
                Image(systemName: "list.bullet")
                    .foregroundColor(.secondary)
            }
        }
        .frame(minWidth: 28)
    }
    
    private func presetTagLabel(tag: PresetTag, active: Bool) -> (String, LocalizedStringKey) {
        switch tag {
        case .groupReports: (active ? "flag.fill" : "flag", "Reports")
        case .favorites: (active ? "star.fill" : "star", "Favorites")
        case .contacts: (active ? "person.fill" : "person", "Contacts")
        case .groups: (active ? "person.2.fill" : "person.2", "Groups")
        case .business: (active ? "briefcase.fill" : "briefcase", "Businesses")
        case .notes: (active ? "folder.fill" : "folder", "Notes")
        }
    }

    private func setActiveFilter(filter: ActiveFilter) {
        if filter != chatTagsModel.activeFilter {
            chatTagsModel.activeFilter = filter
        } else {
            chatTagsModel.activeFilter = nil
        }
    }
}

func chatStoppedIcon() -> some View {
    Button {
        AlertManager.shared.showAlertMsg(
            title: "Chat is stopped",
            message: "You can start chat via app Settings / Database or by restarting the app"
        )
    } label: {
        Image(systemName: "exclamationmark.octagon.fill").foregroundColor(.red)
    }
}

func presetTagMatchesChat(_ tag: PresetTag, _ chatInfo: ChatInfo, _ chatStats: ChatStats) -> Bool {
    switch tag {
    case .groupReports:
        chatStats.reportsCount > 0
    case .favorites:
        chatInfo.chatSettings?.favorite == true
    case .contacts:
        switch chatInfo {
        case let .direct(contact): !(contact.activeConn == nil && contact.profile.contactLink != nil && contact.active) && !contact.chatDeleted
        case .contactRequest: true
        case .contactConnection: true
        case let .group(groupInfo): groupInfo.businessChat?.chatType == .customer
        default: false
        }
    case .groups:
        switch chatInfo {
        case let .group(groupInfo): groupInfo.businessChat == nil
        default: false
        }
    case .business:
        chatInfo.groupInfo?.businessChat?.chatType == .business
    case .notes:
        switch chatInfo {
        case .local: true
        default: false
        }
    }
}

struct ChatListView_Previews: PreviewProvider {
    @State static var userPickerSheet: UserPickerSheet? = .none

    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.updateChats([
            ChatData(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ),
            ChatData(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ),
            ChatData(
                chatInfo: ChatInfo.sampleData.contactRequest,
                chatItems: []
            )

        ])
        return Group {
            ChatListView(activeUserPickerSheet: $userPickerSheet)
                .environmentObject(chatModel)
            ChatListView(activeUserPickerSheet: $userPickerSheet)
                .environmentObject(ChatModel())
        }
    }
}
