//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import SwiftUIIntrospect

private weak var collectionView: UICollectionView?

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

    @State private var isSearchExpanded = true
    @State private var contentOffsetObservation: NSKeyValueObservation?

    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @AppStorage(DEFAULT_ONE_HAND_UI_CARD_SHOWN) private var oneHandUICardShown = false

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
                    set: { _ in }
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
        withToolbar {
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
            if oneHandUI {
                Divider().background(Material.thin)
            } else {
                searchBar
            }
        }
        .safeAreaInset(edge: .bottom) {
            if oneHandUI { searchBar }
        }

    }
    
    @ViewBuilder
    var searchBar: some View {
        // TODO: Preserve height, without hardcoding it. Remove `.font(.system(size: 18))` after done.
        let height: Double = 56
        let isVisible = isSearchExpanded || searchFocussed
        VStack(spacing: 0) {
            if oneHandUI { Divider() }
            ChatListSearchBar(
                searchMode: $searchMode,
                searchFocussed: $searchFocussed,
                searchText: $searchText,
                searchShowingSimplexLink: $searchShowingSimplexLink,
                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
            )
            .padding(8)
            .frame(height: isVisible ? height : 0)
            .opacity(isVisible ? 1 : 0)
            if !oneHandUI { Divider() }
        }
        .background(Material.thin)
        .padding(oneHandUI ? .top : .bottom, isVisible ? 0 : height)
    }

    @ViewBuilder
    func withToolbar(content: () -> some View) -> some View {
        if #available(iOS 16.0, *) {
            content()
                .toolbarBackground(.hidden, for: .navigationBar)
                .toolbarBackground(.hidden, for: .bottomBar)
                .toolbar {
                    if oneHandUI {
                        bottomToolbar
                    } else {
                        topToolbar
                    }
                }
        } else {
            content().toolbar { topToolbar }
        }
    }

    @ToolbarContentBuilder
    var topToolbar: some ToolbarContent {
        ToolbarItem(placement: .topBarLeading) { leadingToolbarItem }
        ToolbarItem(placement: .principal) { principalToolbarItem }
        ToolbarItem(placement: .topBarTrailing) { trailingToolbarItem }
    }

    @ToolbarContentBuilder
    var bottomToolbar: some ToolbarContent {
        ToolbarItem(placement: .bottomBar) {
            HStack {
                leadingToolbarItem
                principalToolbarItem
                trailingToolbarItem
            }
        }
    }

    @ViewBuilder
    var leadingToolbarItem: some View {
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

    @ViewBuilder
    var principalToolbarItem: some View {
        HStack(spacing: 4) {
            Text("Chats").font(.headline)
            SubsStatusIndicator()
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    @ViewBuilder
    var trailingToolbarItem: some View {
        switch chatModel.chatRunning {
        case .some(true): NewChatMenuButton()
        case .some(false): chatStoppedIcon()
        case .none: EmptyView()
        }
    }

    @ViewBuilder private var chatList: some View {
        let cs = filteredChats()
        ZStack {
            List {
                if !oneHandUICardShown {
                    OneHandUICard()
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .listRowSeparator(.hidden)
                        .listRowBackground(Color.clear)
                }
                ForEach(cs, id: \.viewId) { chat in
                    ChatListNavLink(chat: chat)
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .padding(.trailing, -16)
                        .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                        .listRowBackground(Color.clear)
                }
                .offset(x: -8)
            }
            .introspect(.list, on: .iOS(.v16, .v17, .v18)) { setObservations(for: $0) }
            .listStyle(.plain)
            .onChange(of: chatModel.chatId) { chId in
                if chId == nil, let chatId = chatModel.chatToTop {
                    chatModel.chatToTop = nil
                    chatModel.popChat(chatId)
                }
                stopAudioPlayer()
            }
            .onChange(of: chatModel.currentUser?.userId) { _ in
                stopAudioPlayer()
            }
//            .onAppear {
//                oneHandUICardShown = false
//            }
            if cs.isEmpty && !chatModel.chats.isEmpty {
                Text("No filtered chats")
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            }
        }
    }

    private func setObservations(for cv: UICollectionView) {
        if collectionView != cv {
            collectionView = cv
            var scrollDistance: CGFloat = 0
            contentOffsetObservation?.invalidate()
            contentOffsetObservation = cv.observe(
                \.contentOffset,
                 options: [.new, .old]
            ) { (cv, change) in
                if let newOffset = change.newValue?.y,
                   let oldOffset = change.oldValue?.y {
                    let bottomOffset = cv.contentSize.height - cv.visibleSize.height - newOffset + cv.safeAreaInsets.bottom
                    // Show/Hide search bar when scrolled for more than `MAX` amount
                    if newOffset > .zero,
                       bottomOffset > 0 {
                        let MAX: CGFloat = 64
                        scrollDistance = min(max(scrollDistance + oldOffset - newOffset, -MAX), +MAX)
                        if (isSearchExpanded && scrollDistance == -MAX) ||
                          (!isSearchExpanded && scrollDistance == +MAX) {
                            withAnimation(.easeOut(duration: 0.15)) { isSearchExpanded.toggle() }
                        }
                    }
                }
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
    @State private var timer: Timer? = nil
    @State private var showServersSummary = false

    @AppStorage(DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE) private var showSubscriptionPercentage = false

    var body: some View {
        Button {
            showServersSummary = true
        } label: {
            HStack(spacing: 4) {
                SubscriptionStatusIndicatorView(subs: subs, hasSess: hasSess)
                if showSubscriptionPercentage {
                    SubscriptionStatusPercentageView(subs: subs, hasSess: hasSess)
                }
            }
        }
        .onAppear {
            startTimer()
        }
        .onDisappear {
            stopTimer()
        }
        .appSheet(isPresented: $showServersSummary) {
            ServersSummaryView()
                .environment(\EnvironmentValues.refresh as! WritableKeyPath<EnvironmentValues, RefreshAction?>, nil)
        }
    }

    private func startTimer() {
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { _ in
            if AppChatState.shared.value == .active {
                getSubsTotal()
            }
        }
    }

    func stopTimer() {
        timer?.invalidate()
        timer = nil
    }

    private func getSubsTotal() {
        do {
            (subs, hasSess) = try getAgentSubsTotal()
        } catch let error {
            logger.error("getSubsTotal error: \(responseError(error))")
        }
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
        HStack(spacing: 12) {
            HStack(spacing: 4) {
                Image(systemName: "magnifyingglass")
                TextField("Search or paste SimpleX link", text: $searchText)
                    .font(.system(size: 18))
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
        chatModel.chats = [
            Chat(
                chatInfo: ChatInfo.sampleData.direct,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "hello")]
            ),
            Chat(
                chatInfo: ChatInfo.sampleData.group,
                chatItems: [ChatItem.getSample(1, .directSnd, .now, "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")]
            ),
            Chat(
                chatInfo: ChatInfo.sampleData.contactRequest,
                chatItems: []
            )

        ]
        return Group {
            ChatListView(showSettings: Binding.constant(false))
                .environmentObject(chatModel)
            ChatListView(showSettings: Binding.constant(false))
                .environmentObject(ChatModel())
        }
    }
}
