//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatListView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Binding var showSettings: Bool
    @State private var searchMode = false
    @FocusState private var searchFocussed
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    @State private var userPickerVisible = false
    @State private var showConnectDesktop = false
    @State private var scrollToSearchBar = false

    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @AppStorage(DEFAULT_ONE_HAND_UI_CARD_SHOWN) private var oneHandUICardShown = false
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
            if userPickerVisible {
                Rectangle().fill(.white.opacity(0.001)).onTapGesture {
                    withAnimation {
                        userPickerVisible.toggle()
                    }
                }
            }
            UserPicker(
                showSettings: $showSettings,
                showConnectDesktop: $showConnectDesktop,
                userPickerVisible: $userPickerVisible
            )
        }
        .sheet(isPresented: $showConnectDesktop) {
            ConnectDesktopView()
        }
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
        .onDisappear() { withAnimation { userPickerVisible = false } }
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
                content().toolbar { bottomToolbarGroup }
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

    @ToolbarContentBuilder var bottomToolbarGroup: some ToolbarContent {
        let padding: Double = Self.hasHomeIndicator ? 0 : 14
        ToolbarItemGroup(placement: .bottomBar) {
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
                .padding(.trailing, 4)
            let allRead = chatModel.users
                .filter { u in !u.user.activeUser && !u.user.hidden }
                .allSatisfy { u in u.unreadCount == 0 }
            if !allRead {
                unreadBadge(size: 12)
            }
        }
        .onTapGesture {
            if chatModel.users.filter({ u in u.user.activeUser || !u.user.hidden }).count > 1 {
                withAnimation {
                    userPickerVisible.toggle()
                }
            } else {
                showSettings = true
            }
        }
    }

    @ViewBuilder var trailingToolbarItem: some View {
        switch chatModel.chatRunning {
        case .some(true): NewChatMenuButton()
        case .some(false): chatStoppedIcon()
        case .none: EmptyView()
        }
    }

    @ViewBuilder private var chatList: some View {
        let cs = filteredChats()
        ZStack {
            ScrollViewReader { scrollProxy in
                List {
                    if !chatModel.chats.isEmpty {
                        ChatListSearchBar(
                            searchMode: $searchMode,
                            searchFocussed: $searchFocussed,
                            searchText: $searchText,
                            searchShowingSimplexLink: $searchShowingSimplexLink,
                            searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
                        )
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .listRowSeparator(.hidden)
                        .listRowBackground(Color.clear)
                        .frame(maxWidth: .infinity)
                        .padding(.top, oneHandUI ? 8 : 0)
                        .id("searchBar")
                    }
                    if !oneHandUICardShown {
                        OneHandUICard()
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .listRowSeparator(.hidden)
                            .listRowBackground(Color.clear)
                    }
                    if #available(iOS 16.0, *) {
                        ForEach(cs, id: \.viewId) { chat in
                            ChatListNavLink(chat: chat)
                                .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                                .padding(.trailing, -16)
                                .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                                .listRowBackground(Color.clear)
                        }
                        .offset(x: -8)
                    } else {
                        ForEach(cs, id: \.viewId) { chat in
                            VStack(spacing: .zero) {
                                Divider()
                                    .padding(.leading, 16)
                                ChatListNavLink(chat: chat)
                                    .padding(.horizontal, 8)
                                    .padding(.vertical, 6)
                            }
                            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                            .listRowSeparator(.hidden)
                            .listRowInsets(EdgeInsets())
                            .background { theme.colors.background } // Hides default list selection colour
                            .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                        }
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
                Text("No filtered chats")
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            }
        }
    }

    private func unreadBadge(_ text: Text? = Text(" "), size: CGFloat = 18) -> some View {
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
            return s == "" && !showUnreadAndFavorites
            ? chatModel.chats.filter { chat in
                !chat.chatInfo.chatDeleted && chatContactType(chat: chat) != ContactType.card
            }
            : chatModel.chats.filter { chat in
                let cInfo = chat.chatInfo
                switch cInfo {
                case let .direct(contact):
                    return !contact.chatDeleted && chatContactType(chat: chat) != ContactType.card && (
                        s == ""
                        ? filtered(chat)
                        : (viewNameContains(cInfo, s) ||
                           contact.profile.displayName.localizedLowercase.contains(s) ||
                           contact.fullName.localizedLowercase.contains(s))
                    )
                case let .group(gInfo):
                    return s == ""
                    ? (filtered(chat) || gInfo.membership.memberStatus == .memInvited)
                    : viewNameContains(cInfo, s)
                case .local:
                    return s == "" || viewNameContains(cInfo, s)
                case .contactRequest:
                    return s == "" || viewNameContains(cInfo, s)
                case let .contactConnection(conn):
                    return s != "" && conn.localAlias.localizedLowercase.contains(s)
                case .invalidJSON:
                    return false
                }
            }
        }

        func searchString() -> String {
            searchShowingSimplexLink ? "" : searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
        }

        func filtered(_ chat: Chat) -> Bool {
            (chat.chatInfo.chatSettings?.favorite ?? false) ||
            chat.chatStats.unreadChat ||
            (chat.chatInfo.ntfsEnabled && chat.chatStats.unreadCount > 0)
        }

        func viewNameContains(_ cInfo: ChatInfo, _ s: String) -> Bool {
            cInfo.chatViewName.localizedLowercase.contains(s)
        }
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
                if AppChatState.shared.value == .active {
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
        VStack(spacing: 12) {
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

struct ChatListView_Previews: PreviewProvider {
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
            ChatListView(showSettings: Binding.constant(false))
                .environmentObject(chatModel)
            ChatListView(showSettings: Binding.constant(false))
                .environmentObject(ChatModel())
        }
    }
}
