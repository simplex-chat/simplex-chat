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
    @State var users: [User] = [ChatModel.shared.currentUser ?? User.sampleData, User.sampleData]
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
                        //let users: [User] = [chatModel.currentUser ?? User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData]
                        let unreadCount = 777
                        ForEach(Array(users.enumerated()), id: \.0) { i, user in
                            Button(action: {
                            }, label: {
                                HStack(spacing: 0) {
                                    ProfileImage(imageStr: user.image)
                                        .frame(width: 44, height: 44)
                                        .padding(.trailing, 12)
                                    Text(user.chatViewName)
                                        .fontWeight(i == 0 ? .medium : .regular)
                                        .foregroundColor(.primary)
                                        .overlay(DetermineWidth())
                                    Spacer()
                                    if i == 0 {
                                        Image(systemName: "chevron.right")
                                            .frame(width: 24, alignment: .center)
                                    } else if unreadCount > 0 {
                                        unreadCountText(unreadCount)
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
                menuButton("Your user profiles", icon: "plus") {
                    print("manage profiles page")
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
        .opacity(userPickerVisible ? 1.0 : 0.0)
    }

    private func menuButton(_ title: LocalizedStringKey, icon: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            HStack(spacing: 0) {
                Text(title)
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
            users: [User.sampleData, User.sampleData]
        )
        .environmentObject(ChatModel())
    }
}
