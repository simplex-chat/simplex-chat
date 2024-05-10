//
//  HomeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 01.05.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum HomeTab {
    case contacts
    case chats
}

struct HomeView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var showSettings: Bool
    @State private var homeTab: HomeTab = .chats
    @State private var userPickerVisible = false
    @State private var showConnectDesktop = false
    @State private var newChatMenuOption: NewChatMenuOption? = nil

    @AppStorage(DEFAULT_ONE_HAND_UI) private var oneHandUI = true

    var body: some View {
        ZStack(alignment: .bottomLeading) {
            NavStackCompat(
                isActive: Binding(
                    get: { chatModel.chatId != nil },
                    set: { _ in }
                ),
                destination: chatView
            ) {
                VStack {
                    switch homeTab {
                    case .contacts:
                        contactsView()
                            .navigationBarTitleDisplayMode(.inline)
                            .navigationTitle("Contacts")
                    case .chats:
                        chatListView()
                            .navigationBarTitleDisplayMode(.inline)
                            .navigationTitle("Chats")
                    }
                }
                .toolbar {
                    ToolbarItemGroup(placement: .bottomBar) {
                        settingsButton()
                        Spacer()
                        contactsButton()
                        Spacer()
                        chatsButton()
                        Spacer()
                        newChatButton()
                    }
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

    @ViewBuilder private func settingsButton() -> some View {
        let user = chatModel.currentUser ?? User.sampleData
        let multiUser = chatModel.users.filter({ u in u.user.activeUser || !u.user.hidden }).count > 1
        Button {
            if multiUser {
                withAnimation {
                    userPickerVisible.toggle()
                }
            } else {
                showSettings = true
            }
        } label: {
            VStack(spacing: 2) {
                ZStack(alignment: .topTrailing) {
                    ProfileImage(imageStr: user.image, size: 40, color: Color(uiColor: .quaternaryLabel))
                        .padding(.top, 2)
                        .padding(.trailing, 3)
                    let allRead = chatModel.users
                        .filter { u in !u.user.activeUser && !u.user.hidden }
                        .allSatisfy { u in u.unreadCount == 0 }
                    if !allRead {
                        userUnreadBadge(size: 12)
                    }
                }
            }
        }
        .foregroundColor(.secondary)
    }

    private func userUnreadBadge(_ text: Text? = Text(" "), size: CGFloat = 18) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(.accentColor)
    }

    private func contactsButton() -> some View {
        Button {
            homeTab = .contacts
        } label: {
            iconLabel("book.fill", "Contacts")
        }
        .foregroundColor(homeTab == .contacts ? .accentColor : .secondary)
    }

    private func chatsButton() -> some View {
        Button {
            homeTab = .chats
        } label: {
            iconLabel("message.fill", "Chats")
        }
        .foregroundColor(homeTab == .chats ? .accentColor : .secondary)
    }

    @ViewBuilder private func newChatButton() -> some View {
        if case .some(false) = chatModel.chatRunning {
            chatsStoppedButton()
        } else {
            Menu {
                Button {
                    newChatMenuOption = .newGroup
                } label: {
                    Text("Create group")
                }
                Button {
                    newChatMenuOption = .scanPaste
                } label: {
                    Text("Scan / Paste link")
                }
                Button {
                    newChatMenuOption = .newContact
                } label: {
                    Text("Add contact")
                }
            } label: {
                iconLabel("square.and.pencil", "New chat")
            }
            .sheet(item: $newChatMenuOption) { opt in
                switch opt {
                case .newContact: NewChatView(selection: .invite)
                case .scanPaste: NewChatView(selection: .connect, showQRCodeScanner: true)
                case .newGroup: AddGroupView()
                }
            }
        }
    }

    func chatsStoppedButton() -> some View {
        Button {
            AlertManager.shared.showAlertMsg(
                title: "Chat is stopped",
                message: "You can start chat via app Settings / Database or by restarting the app"
            )
        } label: {
            VStack(spacing: 4) {
                Image(systemName: "exclamationmark.octagon.fill")
                    .resizable()
                    .scaledToFit()
                    .foregroundColor(.red)
                    .frame(width: 24, height: 24)
                Text("Stopped")
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
        }
    }

    private func iconLabel(_ image: String, _ title: LocalizedStringKey) -> some View {
        VStack(spacing: 2) {
            Image(systemName: image)
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
            Text(title)
                .font(.caption2)
        }
        .padding(.top, 3)
    }

    @ViewBuilder private func contactsView() -> some View {
        ContactsView()
            .padding(.vertical, oneHandUI ? 1 : 0)
    }

    @ViewBuilder private func chatListView() -> some View {
        // TODO reverse scale effect for swipe actions
        ChatListView()
            .padding(.vertical, oneHandUI ? 1 : 0)
    }

    @ViewBuilder private func chatView() -> some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            ChatView(chat: chat).onAppear {
                loadChat(chat: chat)
            }
        }
    }
}

#Preview {
    HomeView(showSettings: Binding.constant(false))
}
