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

    @State private var searchMode = false
    @FocusState private var searchFocussed
    @State private var searchText = ""
    @State private var searchShowingSimplexLink = false
    @State private var searchChatFilteredBySimplexLink: String? = nil

    @AppStorage(DEFAULT_SEARCH_IN_BOTTOM) private var searchInBottom = false

//    init(homeTab: Binding<HomeTab>) {
//        // Make the background color of the bottom toolbar fully transparent
//        let appearance = UIToolbarAppearance()
//        appearance.configureWithOpaqueBackground()
//        appearance.shadowColor = .clear
//        appearance.backgroundColor = .clear
//        appearance.backgroundImage = UIImage()
//        UIToolbar.appearance().standardAppearance = appearance
//        UIToolbar.appearance().compactAppearance = appearance
//        UIToolbar.appearance().scrollEdgeAppearance = appearance
//
//        self._homeTab = homeTab
//    }

    var body: some View {
        ZStack(alignment: .bottomLeading) {
            NavStackCompat(
                isActive: Binding(
                    get: { chatModel.chatId != nil },
                    set: { _ in }
                ),
                destination: chatView
            ) {
                ZStack {
                    switch homeTab {
                    case .settings: settingsView()
                    case .contacts: contactsView()
                    case .chats: chatsView()
                    case .newChat: newChatView()
                    }

                    VStack {
                        topToolbar()
                            .background(BlurView(style: .systemThinMaterial).ignoresSafeArea())
                        Spacer()
                    }

                    VStack {
                        Spacer()
                        bottomToolbar()
                            .background(BlurView(style: .systemThinMaterial).ignoresSafeArea())
                    }

//                    .toolbar {
//                        ToolbarItemGroup(placement: .bottomBar) {
//                            settingsButton()
//                            Spacer()
//                            contactsButton()
//                            Spacer()
//                            chatsButton()
//                            Spacer()
//                            newChatButton()
//                        }
//                    }
//
//                    if homeTab == .chats {
//                        VStack {
//                            Spacer()
//                            ChatsSearchBar(
//                                searchMode: $searchMode,
//                                searchFocussed: $searchFocussed,
//                                searchText: $searchText,
//                                searchShowingSimplexLink: $searchShowingSimplexLink,
//                                searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
//                            )
//                            .padding(.horizontal)
//                            .padding(.top, 8)
//                            .background(BlurView(style: .systemMaterial))
//                        }
//                    }

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
                homeTab: $homeTab,
                showConnectDesktop: $showConnectDesktop,
                userPickerVisible: $userPickerVisible
            )
        }
        .sheet(isPresented: $showConnectDesktop) {
            ConnectDesktopView()
        }
    }

    @ViewBuilder private func topToolbar() -> some View {
        if !searchInBottom, homeTab == .chats {
            chatsSearch()
                .padding(.horizontal)
                .padding(.vertical, 8)
        }
    }

    private func bottomToolbar() -> some View {
        VStack {
            if searchInBottom, homeTab == .chats {
                chatsSearch()
                    .padding(.horizontal)
                    .padding(.top, 8)
            }

            Spacer()
                .frame(height: 8)

            if !searchFocussed {
                HStack {
                    settingsButton()
                    Spacer()
                    contactsButton()
                    Spacer()
                    chatsButton()
                    Spacer()
                    newChatButton()
                }
                .padding(.horizontal, 12)
                .padding(.horizontal)
                .frame(maxWidth: .infinity)
            }
        }
    }

    private func chatsSearch() -> some View {
        ChatsSearchBar(
            searchMode: $searchMode,
            searchFocussed: $searchFocussed,
            searchText: $searchText,
            searchShowingSimplexLink: $searchShowingSimplexLink,
            searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
        )
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
                iconLabel(
                    multiUser ? "person.2.fill" : "gearshape.fill",
                    multiUser ? "Users" : "Settings"
                )
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
            iconLabel("person.crop.circle.fill", "Contacts")
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
                iconLabel("square.and.pencil", "New chat")
            }
            .foregroundColor(.secondary)
        } else {
            Button {} label: {
                iconLabel("square.and.pencil", "New chat")
            }
            .foregroundColor(.accentColor)
        }
    }

    private func iconLabel(_ image: String, _ title: LocalizedStringKey) -> some View {
        VStack(spacing: 4) {
            Image(systemName: image)
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
            Text(title)
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
        // TODO onboarding buttons (remove?)
        // TODO for reversed chat list start at bottom
        ChatsView(
            searchText: $searchText,
            searchShowingSimplexLink: $searchShowingSimplexLink,
            searchChatFilteredBySimplexLink: $searchChatFilteredBySimplexLink
        )
        .padding(.top, 5)
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

    @ViewBuilder private func chatView() -> some View {
        if let chatId = chatModel.chatId, let chat = chatModel.getChat(chatId) {
            ChatView(chat: chat).onAppear {
                loadChat(chat: chat)
            }
        }
    }
}

struct BlurView: UIViewRepresentable {
    let style: UIBlurEffect.Style

    func makeUIView(context: Context) -> UIVisualEffectView {
        let view = UIVisualEffectView(effect: UIBlurEffect(style: style))
        return view
    }

    func updateUIView(_ uiView: UIVisualEffectView, context: Context) {}
}

#Preview {
    HomeView(homeTab: Binding.constant(.chats))
}
