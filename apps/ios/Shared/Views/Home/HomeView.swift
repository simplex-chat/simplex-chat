//
//  HomeView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 01.05.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct HomeView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Binding var homeTab: HomeTab
    @State private var userPickerVisible = false
    @State private var showConnectDesktop = false
    @State private var newChatMenuOption: NewChatMenuOption? = nil

    var body: some View {
        ZStack(alignment: .bottomLeading) {
            switch homeTab {
            case .settings: settingsView()
            case .contacts: contactsView()
            case .chats: chatsView()
            case .newChat: newChatView()
            }
            if userPickerVisible {
                Rectangle().fill(.white.opacity(0.001)).onTapGesture {
                    withAnimation {
                        userPickerVisible.toggle()
                    }
                }
            }
            UserPicker(
                homeTab: $homeTab,
                showConnectDesktop: $showConnectDesktop,
                userPickerVisible: $userPickerVisible
            )
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
                homeTab = .settings
            }
        } label: {
            if user.image != nil {
                ZStack(alignment: .topTrailing) {
                    ProfileImage(imageStr: user.image, size: 32, color: Color(uiColor: .quaternaryLabel))
                        .padding(.trailing, 4)
                    let allRead = chatModel.users
                        .filter { u in !u.user.activeUser && !u.user.hidden }
                        .allSatisfy { u in u.unreadCount == 0 }
                    if !allRead {
                        userUnreadBadge(size: 12)
                    }
                }
            } else {
                VStack(spacing: 4) {
                    Image(systemName: multiUser ? "person.2.fill" : "gearshape.fill")
                    Text("Users")
                        .font(.caption)
                }
            }
        }
        .foregroundColor(homeTab == .settings ? .accentColor : .secondary)
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
            VStack(spacing: 4) {
                Image(systemName: "person.crop.circle.fill")
                Text("Contacts")
                    .font(.caption)
            }
        }
        .foregroundColor(homeTab == .contacts ? .accentColor : .secondary)
    }

    private func chatsButton() -> some View {
        Button {
            homeTab = .chats
        } label: {
            VStack(spacing: 4) {
                Image(systemName: "message.fill")
                Text("Chats")
                    .font(.caption)
            }
        }
        .foregroundColor(homeTab == .chats ? .accentColor : .secondary)
    }

    @ViewBuilder private func newChatButton() -> some View {
        if homeTab != .newChat {
            Menu {
                Button {
                    newChatMenuOption = .newContact
                    homeTab = .newChat
                } label: {
                    Text("Add contact")
                }
                Button {
                    newChatMenuOption = .newGroup
                    homeTab = .newChat
                } label: {
                    Text("Create group")
                }
            } label: {
                newChatButtonLabel()
            }
            .foregroundColor(.secondary)
        } else {
            Button {} label: {
                newChatButtonLabel()
            }
            .foregroundColor(.accentColor)
        }
    }

    private func newChatButtonLabel() -> some View {
        VStack(spacing: 4) {
            Image(systemName: "square.and.pencil")
            Text("New chat")
                .font(.caption)
        }
    }

    private func settingsView() -> some View {
        SettingsView(homeTab: $homeTab)
    }

    private func contactsView() -> some View {
        // TODO
        VStack {
            Text("Contacts")
        }
    }

    private func chatsView() -> some View {
        // TODO remove top bar, move search to bottom
        // TODO hide toolbar when in chat
        // TODO onboarding buttons (remove?)
        ChatsView()
    }

    @ViewBuilder private func newChatView() -> some View {
        // TODO doesn't fit
        // TODO alerts don't work
        // TODO dismiss on connect
        // TODO dismiss when creating group
        // TODO chat stopped (see chatsStoppedIcon in ChatsView)
        switch newChatMenuOption {
        case .newContact:
            NewChatView(selection: .invite)
        case .newGroup:
            AddGroupView()
        case nil:
            EmptyView()
        }
    }
}

#Preview {
    HomeView(homeTab: Binding.constant(.chats))
}
