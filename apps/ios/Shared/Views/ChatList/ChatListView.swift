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
    @Binding var showSettings: Bool
    @State private var searchText = ""
    @State private var showAddChat = false
    @State private var userPickerVisible = false
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
                    get: { ChatModel.shared.chatId != nil },
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
            UserPicker(showSettings: $showSettings, userPickerVisible: $userPickerVisible)
        }
    }

    private var chatListView: some View {
        VStack {
            if chatModel.chats.count > 0 {
                chatList.searchable(text: $searchText)
            } else {
                chatList
            }
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
        .offset(x: -8)
        .listStyle(.plain)
        .navigationBarTitleDisplayMode(.inline)
        .toolbar {
            ToolbarItem(placement: .navigationBarLeading) {
                let user = chatModel.currentUser ?? User.sampleData
                ZStack(alignment: .topTrailing) {
                    ProfileImage(imageStr: user.image, color: Color(uiColor: .quaternaryLabel))
                        .frame(width: 32, height: 32)
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
                    if chatModel.chats.count > 0 {
                        toggleFilterButton()
                    }
                }
                .frame(maxWidth: .infinity, alignment: .center)
            }
            ToolbarItem(placement: .navigationBarTrailing) {
                switch chatModel.chatRunning {
                case .some(true): NewChatButton(showAddChat: $showAddChat)
                case .some(false): chatStoppedIcon()
                case .none: EmptyView()
                }
            }
        }
    }

    private func toggleFilterButton() -> some View {
        Button {
            showUnreadAndFavorites = !showUnreadAndFavorites
        } label: {
            Image(systemName: "line.3.horizontal.decrease.circle" + (showUnreadAndFavorites ? ".fill" : ""))
                .foregroundColor(.accentColor)
        }
    }

    @ViewBuilder private var chatList: some View {
        let cs = filteredChats()
        ZStack {
            List {
                ForEach(cs, id: \.viewId) { chat in
                    ChatListNavLink(chat: chat)
                        .padding(.trailing, -16)
                        .disabled(chatModel.chatRunning != true)
                }
            }
            .onChange(of: chatModel.chatId) { _ in
                if chatModel.chatId == nil, let chatId = chatModel.chatToTop {
                    chatModel.chatToTop = nil
                    chatModel.popChat(chatId)
                }
            }
            if cs.isEmpty && !chatModel.chats.isEmpty {
                Text("No filtered chats").foregroundColor(.secondary)
            }
        }
    }

    private func unreadBadge(_ text: Text? = Text(" "), size: CGFloat = 18) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(.accentColor)
    }

    private func onboardingButtons() -> some View {
        VStack(alignment: .trailing, spacing: 0) {
            Path { p in
                p.move(to: CGPoint(x: 8, y: 0))
                p.addLine(to: CGPoint(x: 16, y: 10))
                p.addLine(to: CGPoint(x: 0, y: 10))
                p.addLine(to: CGPoint(x: 8, y: 0))
            }
            .fill(Color.accentColor)
            .frame(width: 20, height: 10)
            .padding(.trailing, 12)

            connectButton("Tap to start a new chat") {
                showAddChat = true
            }

            connectButton("or chat with the developers") {
                DispatchQueue.main.async {
                    UIApplication.shared.open(simplexTeamURL)
                }
            }
            .padding(.top, 10)

            Spacer()
            Text("You have no chats")
                .foregroundColor(.secondary)
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
        .background(Color.accentColor)
        .foregroundColor(.white)
        .clipShape(RoundedRectangle(cornerRadius: 16))
    }

    @ViewBuilder private func chatView() -> some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            ChatView(chat: chat).onAppear {
                loadChat(chat: chat)
            }
        }
    }

    private func filteredChats() -> [Chat] {
        let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
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
                case .contactRequest:
                    return s == "" || viewNameContains(cInfo, s)
                case let .contactConnection(conn):
                    return s != "" && conn.localAlias.localizedLowercase.contains(s)
                case .invalidJSON:
                    return false
                }
            }

        func filtered(_ chat: Chat) -> Bool {
            (chat.chatInfo.chatSettings?.favorite ?? false) || chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat
        }

        func viewNameContains(_ cInfo: ChatInfo, _ s: String) -> Bool {
            cInfo.chatViewName.localizedLowercase.contains(s)
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
