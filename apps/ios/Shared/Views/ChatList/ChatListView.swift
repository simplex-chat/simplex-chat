//
//  ChatListView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 27/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatListView<ToolbarContent: View>: View {
    @EnvironmentObject var chatModel: ChatModel

    @State private var searchMode = false
    @FocusState private var searchFocussed
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil
    @State var topVisibleRowIndex: Int? = nil;
    @State private var searchVisible: Bool = true;
    @AppStorage(DEFAULT_SHOW_UNREAD_AND_FAVORITES) private var showUnreadAndFavorites = false
    @AppStorage(DEFAULT_ONE_HAND_UI) private var oneHandUI = false
    let toolbarContent: ToolbarContent?

    var body: some View {
        if #available(iOS 16.0, *) {
            viewBody.scrollDismissesKeyboard(.immediately)
        } else {
            viewBody
        }
    }
    
    init(@ViewBuilder toolbarContent: () -> ToolbarContent?) {
        self.toolbarContent = toolbarContent()
    }

    private var viewBody: some View {
        VStack {
            chatList
        }
        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
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
    }

    @ViewBuilder private var chatList: some View {
        let cs = filteredChats()
        ZStack(alignment: .top) {
            VStack {
                ScrollViewReader { scrollViewProxy in
                    List {
                        Color.clear
                            .frame(height: oneHandUI ? 80 : 30)
                        ForEach(cs.indices, id: \.self) { index in
                            ChatListNavLink(chat: cs[index])
                                .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                                .padding(.trailing, -16)
                                .disabled(chatModel.chatRunning != true || chatModel.deletedChats.contains(cs[index].chatInfo.id))
                                .background(GeometryReader { proxy in
                                    Color.clear
                                        .onAppear {
                                            updateTopVisibleRowIndex(proxy: proxy, index: index)
                                        }
                                        .onChange(of: proxy.frame(in: .named("SCROLL")).minY) { _ in
                                            updateTopVisibleRowIndex(proxy: proxy, index: index)
                                        }
                                })
                        }
                        .offset(x: -8)
                    }
                    .coordinateSpace(name: "SCROLL")
                }
            }
            .onChange(of: chatModel.chatId) { _ in
                if chatModel.chatId == nil, let chatId = chatModel.chatToTop {
                    chatModel.chatToTop = nil
                    chatModel.popChat(chatId)
                }
            }
            if cs.isEmpty && !chatModel.chats.isEmpty {
                Text("No filtered chats")
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .foregroundColor(.secondary)
            }
            
            VStack {
                if let tbcontent = toolbarContent, oneHandUI, #available(iOS 16.0, *), !searchFocussed {
                    tbcontent
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                        .frame(maxWidth: .infinity)
                        .padding(.horizontal, 25)
                        .padding(.bottom, 5)
                        .toolbar(.hidden, for: .bottomBar)
                }
                
                if !chatModel.chats.isEmpty && searchVisible {
                    ChatListSearchBar(
                        searchMode: $searchMode,
                        searchFocussed: $searchFocussed,
                        searchText: $searchText,
                        searchShowingSimplexLink: $searchShowingSimplexLink,
                        searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
                    )
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    .listRowSeparator(.hidden)
                    .frame(maxWidth: .infinity)
                    .padding(10)
                    .transition(.move(edge: .top))
                }
            }
            .background(.bar)
        }
    }
    

    
    private func updateTopVisibleRowIndex(proxy: GeometryProxy, index: Int) {
        let frame = proxy.frame(in: .named("SCROLL"))
        
        if (oneHandUI) {
            let screenHeight = UIScreen.main.bounds.height

            if frame.maxY <= screenHeight && frame.maxY > screenHeight - frame.height / 2 {
                if topVisibleRowIndex != index {
                    withAnimation {
                        searchVisible = if let topVisibleRowIndex  {
                            topVisibleRowIndex > index || index == 0
                        } else {
                            true
                        }
                    }
                    
                    topVisibleRowIndex = index
                }
            }
        } else {
            if frame.minY >= 0 && frame.minY < frame.height / 2 {
                if topVisibleRowIndex != index {
                    withAnimation {
                        searchVisible = if let topVisibleRowIndex {
                            topVisibleRowIndex > index
                        } else {
                            true
                        }
                    }
                    
                    topVisibleRowIndex = index
                }
            }
        }
    }

    private func unreadBadge(_ text: Text? = Text(" "), size: CGFloat = 18) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(.accentColor)
    }

    private func filteredChats() -> [Chat] {
        if let linkChatId = searchChatFilteredBySimplexLink {
            return chatModel.chats.filter { $0.id == linkChatId }
        } else {
            let s = searchString()
            return s == "" && !showUnreadAndFavorites
            ? chatModel.chats.filter { chat in !chat.chatInfo.chatDeleted }
            : chatModel.chats.filter { chat in
                let cInfo = chat.chatInfo
                switch cInfo {
                case let .direct(contact):
                    return !contact.chatDeleted && (
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

struct ChatListSearchBar: View {
    @EnvironmentObject var m: ChatModel
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
                        .foregroundColor(searchShowingSimplexLink ? .secondary : .primary)
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
                .foregroundColor(.secondary)
                .background(Color(.tertiarySystemFill))
                .cornerRadius(10.0)

                if searchFocussed {
                    Text("Cancel")
                        .foregroundColor(.accentColor)
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
                .foregroundColor(showUnreadAndFavorites ? .accentColor : .secondary)
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
            ChatListView {
                EmptyView()
            }.environmentObject(chatModel)
            ChatListView {
                EmptyView()
            }.environmentObject(ChatModel())
        }
    }
}
