//
// Created by Dev on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserChooser: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @Binding var showSettings: Bool
    @Binding var userChooserVisible: Bool
    @State var users: [User] = []
    @State var scrollViewContentSize: CGSize = .zero

    var body: some View {
        let fillColor = colorScheme == .dark
            ? Color(uiColor: UIColor(red: 0.11, green: 0.11, blue: 0.11, alpha: 255))
            : Color(uiColor: UIColor(red: 0.99, green: 0.99, blue: 0.99, alpha: 255))
        VStack {
            Spacer().frame(height: 1)
            VStack(spacing: 0) {
                ScrollView {
                    LazyVStack(spacing: 0) {
                        //let users: [User] = [chatModel.currentUser ?? User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData, User.sampleData]
                        let users: [User] = [chatModel.currentUser ?? User.sampleData, User.sampleData]
                        let unreadCount = 777
                        ForEach(Array(users.enumerated()), id: \.0) { i, user in
                            Button(action: {
                            }, label: {
                                HStack(spacing: 0) {
                                    Label {
                                        Text(user.fullName).foregroundColor(.primary)
                                    } icon: {
                                        ProfileImage(imageStr: user.image)
                                        .frame(width: 20, height: 20)
                                    }
                                    Spacer()
                                    if unreadCount > 0 {
                                        unreadCountText(unreadCount)
                                        .font(.caption)
                                        .foregroundColor(.white)
                                        .padding(.horizontal, 4)
                                        .frame(minWidth: 18, minHeight: 18)
                                        .background(Color.accentColor)
                                        .cornerRadius(10)
                                    }
                                }
                                .padding(10)
                            })
                            .buttonStyle(PressedButtonStyle(defaultColor: fillColor, pressedColor: Color(uiColor: .secondarySystemFill)))
                            .overlay(Divider().background(fillColor).padding(.leading, i < users.count - 1 ? 40 : 0), alignment: .bottom)
                        }
                        Button {
                            showSettings = true; withAnimation {
                                userChooserVisible.toggle()
                            }
                        } label: {
                            HStack(spacing: 0) {
                                Text("Settings").foregroundColor(.primary)
                                Spacer()
                                Image(systemName: "gearshape")
                            }
                            .padding(10)
                        }
                        .buttonStyle(PressedButtonStyle(defaultColor: fillColor, pressedColor: Color(uiColor: .secondarySystemFill)))
                    }
                    .overlay {
                        GeometryReader { geo -> Color in
                            DispatchQueue.main.async {
                                scrollViewContentSize = geo.size
                            }
                            return Color.clear
                        }
                    }
                }
                .frame(maxWidth: 250)
                .clipShape(RoundedRectangle(cornerRadius: 10))
                .background(
                    Rectangle()
                    .fill(fillColor)
                    .cornerRadius(10)
                    .shadow(color: .black.opacity(0.6), radius: 100, x: 0, y: 0)
                )
                .frame(maxHeight: scrollViewContentSize.height)
                .transition(.opacity)
            }
        }
        .padding([.leading, .bottom], 5)
    }
}