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
//                ZStack {
                VStack {
                    switch homeTab {
                    case .contacts: contactsView()
                    case .chats: chatsView()
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

//                    VStack {
//                        Spacer()
//                        bottomToolbar()
//                            .background(BlurView(style: .systemThinMaterial).ignoresSafeArea())
//                    }
//                }

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

    private func bottomToolbar() -> some View {
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

    private func newChatButton() -> some View {
        Menu {
            Button {
                newChatMenuOption = .newContact
            } label: {
                Text("Add contact")
            }
            Button {
                newChatMenuOption = .newGroup
            } label: {
                Text("Create group")
            }
        } label: {
            iconLabel("square.and.pencil", "New chat")
        }
        .foregroundColor(.secondary)
        .sheet(item: $newChatMenuOption) { opt in
            switch opt {
            case .newContact: NewChatView(selection: .invite)
            case .newGroup: AddGroupView()
            }
        }
//        NewChatView(selection: .connect, showQRCodeScanner: true)
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
        SettingsView(showSettings: $showSettings)
    }

    private func contactsView() -> some View {
        // TODO
        VStack {
            Text("Contacts")
        }
    }

    @ViewBuilder private func chatsView() -> some View {
        // TODO onboarding buttons (remove?)
        // TODO for reversed chat list start at bottom
        if oneHandUI {
            ChatsView()
                .padding(.vertical, 5)
        } else {
            ChatsView()
                .padding(.top, 5)
        }
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
    HomeView(showSettings: Binding.constant(false))
}
