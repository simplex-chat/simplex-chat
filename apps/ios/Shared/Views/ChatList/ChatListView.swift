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
    @State private var newChatMenuOption: NewChatMenuOption? = nil
    @State private var userPickerVisible = false
    @State private var showConnectDesktop = false
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false

    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody.scrollDismissesKeyboard(.immediately)
        } else {
            viewBody
        }
    }

    private var viewBody: some View {
        ZStack(alignment: .topLeading) {
            NavStackCompat(
                isActive: Binding(
                    get: { chatModel.chatId != nil },
                    set: { _ in }
                ),
                destination: chatView
            ) {
                VStack {
                    if chatModel.chats.isEmpty {
                        onboardingButtons()
                    }
                    chatListView
                }
            }
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
        VStack {
            chatList
        }
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
        .listStyle(.plain)
        .background(theme.colors.background)
        .navigationBarTitleDisplayMode(.inline)
        .navigationBarHidden(searchMode)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
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
            ToolbarItem(placement: .principal) {
                HStack(spacing: 4) {
                    Text("Chats")
                        .font(.headline)
                    SubsStatusIndicator()
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                switch chatModel.chatRunning {
                case .some(true): NewChatMenuButton(newChatMenuOption: $newChatMenuOption)
                case .some(false): chatStoppedIcon()
                case .none: EmptyView()
                }
            }
        }
    }

    @ViewBuilder private var chatList: some View {
        let cs = filteredChats()
        ZStack {
            VStack {
                List {
                    if !chatModel.chats.isEmpty {
                        ChatListSearchBar(
                            searchMode: $searchMode,
                            searchFocussed: $searchFocussed,
                            searchText: $searchText,
                            searchShowingSimplexLink: $searchShowingSimplexLink,
                            searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
                        )
                        .listRowSeparator(.hidden)
                        .listRowBackground(Color.clear)
                        .frame(maxWidth: .infinity)
                    }
                    ForEach(cs, id: \.viewId) { chat in
                        ChatListNavLink(chat: chat)
                            .padding(.trailing, -16)
                            .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(chat.chatInfo.id))
                            .listRowBackground(Color.clear)
                    }
                    .offset(x: -8)
                }
            }
            .onChange(of: chatModel.chatId) { _ in
                if chatModel.chatId == nil, let chatId = chatModel.chatToTop {
                    chatModel.chatToTop = nil
                    chatModel.popChat(chatId)
                }
            }
            if cs.isEmpty && !chatModel.chats.isEmpty {
                Text("No filtered chats").foregroundColor(theme.colors.secondary)
            }
        }
    }

    private func unreadBadge(_ text: Text? = Text(" "), size: CGFloat = 18) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(theme.colors.primary)
    }

    private func onboardingButtons() -> some View {
        VStack(alignment: .trailing, spacing: 0) {
            Path { p in
                p.move(to: CGPoint(x: 8, y: 0))
                p.addLine(to: CGPoint(x: 16, y: 10))
                p.addLine(to: CGPoint(x: 0, y: 10))
                p.addLine(to: CGPoint(x: 8, y: 0))
            }
            .fill(theme.colors.primary)
            .frame(width: 20, height: 10)
            .padding(.trailing, 12)

            connectButton("Tap to start a new chat") {
                newChatMenuOption = .newContact
            }

            Spacer()
            Text("You have no chats")
                .foregroundColor(theme.colors.secondary)
                .frame(maxWidth: .infinity)
        }
        .padding(.trailing, 6)
        .frame(maxHeight: .infinity)
    }

    private func connectButton(_ label: LocalizedStringKey, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            Text(label)
                .padding(.vertical, 10)
                .padding(.horizontal, 20)
        }
        .background(theme.colors.primary)
        .foregroundColor(.white)
        .clipShape(RoundedRectangle(cornerRadius: 16))
    }

    @ViewBuilder private func chatView() -> some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            ChatView(chat: chat)
        }
    }

    private func filteredChats() -> [Chat] {
        if let linkChatId = searchChatFilteredBySimplexLink {
            return chatModel.chats.filter { $0.id == linkChatId }
        } else {
            let s = searchString()
            return s == "" && !showUnreadAndFavorites
            ? chatModel.chats
            : chatModel.chats.filter { chat in
                let cInfo = chat.chatInfo
                switch cInfo {
                case let .direct(contact):
                    return s == ""
                    ? filtered(chat)
                    : (viewNameContains(cInfo, s) ||
                       contact.profile.displayName.localizedLowercase.contains(s) ||
                       contact.fullName.localizedLowercase.contains(s))
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
    @State private var sess: ServerSessions = ServerSessions.newServerSessions
    @State private var timer: Timer? = nil
    @State private var showServersSummary = false

    @AppStorage(DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE) private var showSubscriptionPercentage = false

    var body: some View {
        Button {
            showServersSummary = true
        } label: {
            HStack(spacing: 4) {
                SubscriptionStatusIndicatorView(subs: subs, sess: sess)
                if showSubscriptionPercentage {
                    SubscriptionStatusPercentageView(subs: subs, sess: sess)
                }
            }
        }
        .onAppear {
            startTimer()
        }
        .onDisappear {
            stopTimer()
        }
        .sheet(isPresented: $showServersSummary) {
            ServersSummaryView()
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
            (subs, sess) = try getAgentSubsTotal()
        } catch let error {
            logger.error("getAgentServersSummary error: \(responseError(error))")
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
