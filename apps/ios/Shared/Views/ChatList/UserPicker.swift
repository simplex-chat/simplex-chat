//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let fillColorDark = Color(uiColor: UIColor(red: 0.11, green: 0.11, blue: 0.11, alpha: 255))
private let fillColorLight = Color(uiColor: UIColor(red: 0.99, green: 0.99, blue: 0.99, alpha: 255))

struct UserPicker: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @Binding var showSettings: Bool
    @Binding var userPickerVisible: Bool
    var manageUsers: () -> Void = {}
    @State var users: [UserInfo] = []
    @State var scrollViewContentSize: CGSize = .zero
    @State var disableScrolling: Bool = true
    private let menuButtonHeight: CGFloat = 68
    @State var chatViewNameWidth: CGFloat = 0

    var fillColor: Color {
        colorScheme == .dark ? fillColorDark : fillColorLight
    }

    var body: some View {
        VStack {
            Spacer().frame(height: 1)
            VStack(spacing: 0) {
                ScrollView {
                    LazyVStack(spacing: 0) {
                        ForEach(Array(users.enumerated()), id: \.0) { i, userInfo in
                            Button(action: {
                                // TODO: why the reference is not updating?
                                //if !userInfo.user.activeUser {
                                    changeActiveUser(toUser: userInfo)
                                //}
                            }, label: {
                                HStack(spacing: 0) {
                                    ProfileImage(imageStr: userInfo.user.image)
                                        .frame(width: 44, height: 44)
                                        .padding(.trailing, 12)
                                    Text(userInfo.user.chatViewName)
                                        .fontWeight(i == 0 ? .medium : .regular)
                                        .foregroundColor(.primary)
                                        .overlay(DetermineWidth())
                                    Spacer()
                                    if i == 0 {
                                        Image(systemName: "chevron.right")
                                            .frame(width: 24, alignment: .center)
                                    } else if userInfo.unreadCount > 0 {
                                        unreadCountText(Int(truncatingIfNeeded: userInfo.unreadCount))
                                            .font(.caption)
                                            .foregroundColor(.white)
                                            .padding(.horizontal, 4)
                                            .frame(minWidth: 18, minHeight: 18)
                                            .background(Color.accentColor)
                                            .cornerRadius(10)
                                    }
                                }
                                .padding(12)
                            })
                            .buttonStyle(PressedButtonStyle(defaultColor: fillColor, pressedColor: Color(uiColor: .secondarySystemFill)))
                            if i < users.count - 1 {
                                Divider()
                            }
                        }
                    }
                    .overlay {
                        GeometryReader { geo -> Color in
                            DispatchQueue.main.async {
                                scrollViewContentSize = geo.size
                                let layoutFrame = UIApplication.shared.windows[0].safeAreaLayoutGuide.layoutFrame
                                disableScrolling = scrollViewContentSize.height + menuButtonHeight * 2 + 10 < layoutFrame.height
                            }
                            return Color.clear
                        }
                    }
                }
                .simultaneousGesture(DragGesture(minimumDistance: disableScrolling ? 0 : 10000000))
                .frame(maxHeight: scrollViewContentSize.height)

                Divider()
                menuButton("Your user profiles", icon: "pencil") {
                    manageUsers()
                }
                Divider()
                menuButton("Settings", icon: "gearshape") {
                    showSettings = true
                    withAnimation {
                        userPickerVisible.toggle()
                    }
                }
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 16))
        .background(
            Rectangle()
                .fill(fillColor)
                .cornerRadius(16)
                .shadow(color: .black.opacity(0.12), radius: 24, x: 0, y: 0)
        )
        .onPreferenceChange(DetermineWidth.Key.self) { chatViewNameWidth = $0 }
        .frame(maxWidth: chatViewNameWidth > 0 ? min(300, chatViewNameWidth + 130) : 300)
        .padding(8)
        .onChange(of: [chatModel.currentUser?.chatViewName, chatModel.currentUser?.image] ) { _ in
            reloadCurrentUser()
        }
        .opacity(userPickerVisible ? 1.0 : 0.0)
        .onAppear {
            reloadUsers()
        }
    }

    private func reloadCurrentUser() {
        if let updatedUser = chatModel.currentUser, let index = users.firstIndex(where: { $0.user.userId == updatedUser.userId }) {
            let removed = users.remove(at: index)
            users.insert(UserInfo(user: updatedUser, unreadCount: removed.unreadCount), at: 0)
        }
    }

    private func changeActiveUser(toUser: UserInfo) {
        Task {
            do {
                let activeUser = try apiSetActiveUser(toUser.user.userId)
                chatModel.currentUser = activeUser
                let index = users.firstIndex(where: { $0.user.userId == activeUser.userId })!
                users.remove(at: index)
                users.insert(UserInfo(user: activeUser, unreadCount: toUser.unreadCount), at: 0)
                try retrieveUserSpecificData(chatModel)
                userPickerVisible = false
            } catch {
                logger.error("Unable to set active user: \(error.localizedDescription)")
            }
        }
    }

    private func reloadUsers() {
        Task {
            users = await listUsers().sorted { one, two -> Bool in one.user.activeUser }
        }
    }

    private func menuButton(_ title: LocalizedStringKey, icon: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            HStack(spacing: 0) {
                Text(title)
                    .overlay(DetermineWidth())
                Spacer()
                Image(systemName: icon)
                    .frame(width: 24, alignment: .center)
            }
            .padding(.horizontal)
            .padding(.vertical, 22)
            .frame(height: menuButtonHeight)
        }
        .buttonStyle(PressedButtonStyle(defaultColor: fillColor, pressedColor: Color(uiColor: .secondarySystemFill)))
    }
}

struct UserPicker_Previews: PreviewProvider {
    static var previews: some View {
        UserPicker(
            showSettings: Binding.constant(false),
            userPickerVisible: Binding.constant(true),
            users: [UserInfo.sampleData, UserInfo.sampleData]
        )
        .environmentObject(ChatModel())
    }
}
