//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserPicker: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.scenePhase) var scenePhase
    @Environment(\.colorScheme) var colorScheme
    @Binding var activeSheet: UserPickerSheet?
    @State private var activeUser: User? = nil
    
    var body: some View {
        if #available(iOS 16.0, *) {
            let v = viewBody.presentationDetents([.height(425)])
            if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
            }
        } else {
            viewBody
        }
    }
    
    private var viewBody: some View {
        let otherUsers = m.users.filter { u in !u.user.hidden && u.user.userId != m.currentUser?.userId }
        return List {
            Section {
                if otherUsers.isEmpty {
                    openSheetOnTap(title: "Create chat profile", icon: "person.crop.circle.fill.badge.plus") {
                        activeSheet = .chatProfiles
                    }
                } else {
                    userPickerRow(otherUsers)
                        .listRowInsets(EdgeInsets(top: 0, leading: 12, bottom: 0, trailing: 12))
                        .listRowBackground(Color.clear)
                        .listRowSeparator(.hidden)
                }
            }
                
            Section {
                if let user = m.currentUser {
                    openSheetOnTap(label: {
                        ProfilePreview(profileOf: user)
                            .foregroundColor(.primary)
                            .padding(.leading, -8)
                    }) {
                        activeSheet = .currentProfile
                    }

                    openSheetOnTap(title: m.userAddress == nil ? "Create public address" : "Your public address", icon: "qrcode") {
                        activeSheet = .address
                    }
                    
                    openSheetOnTap(title: "Chat preferences", icon: "switch.2") {
                        activeSheet = .chatPreferences
                    }
                }
            }
            
            Section {
                openSheetOnTap(title: "Use from desktop", icon: "desktopcomputer") {
                    activeSheet = .useFromDesktop
                }

                HStack {
                    openSheetOnTap(title: "Settings", icon: "gearshape") {
                        activeSheet = .settings
                    }
                    Label {} icon: {
                        Image(systemName: colorScheme == .light ? "sun.max" : "moon.fill")
                            .resizable()
                            .symbolRenderingMode(.monochrome)
                            .foregroundColor(theme.colors.secondary)
                            .frame(maxWidth: 20, maxHeight: 20)
                    }
                    .padding(.leading, 16).padding(.vertical, 8).padding(.trailing, 16)
                    .contentShape(Rectangle())
                    .onTapGesture {
                        if (colorScheme == .light) {
                            ThemeManager.applyTheme(systemDarkThemeDefault.get())
                        } else {
                            ThemeManager.applyTheme(DefaultTheme.LIGHT.themeName)
                        }
                    }
                    .onLongPressGesture {
                        ThemeManager.applyTheme(DefaultTheme.SYSTEM_THEME_NAME)
                    }
                    .padding(.leading, -16).padding(.vertical, -8).padding(.trailing, -16)
                }
                .padding(.horizontal, -3)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
        .onAppear {
            // This check prevents the call of listUsers after the app is suspended, and the database is closed.
            if case .active = scenePhase {
                Task {
                    do {
                        let users = try await listUsersAsync()
                        await MainActor.run { m.users = users }
                    } catch {
                        logger.error("Error loading users \(responseError(error))")
                    }
                }
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }
        
    private func userPickerRow(_ users: [UserInfo]) -> some View {
        HStack(spacing: 0) {
            let s = ScrollView(.horizontal) {
                HStack(spacing: 24) {
                    ForEach(users) { u in
                        if !u.user.hidden && u.user.userId != m.currentUser?.userId {
                            userView(u, size: 32)
                        }
                    }
                }
                .padding(.leading, 2)
                .padding(.trailing, 24)
            }
            ZStack(alignment: .topTrailing) {
                if #available(iOS 16.0, *) {
                    s.scrollIndicators(.hidden)
                } else {
                    s
                }
                LinearGradient(
                    colors: [.clear, theme.colors.background.asGroupedBackground(theme.base.mode)],
                    startPoint: .leading,
                    endPoint: .trailing
                )
                .frame(width: 24, height: 35)
                .allowsHitTesting(false)
            }
            .padding(.top, -3) // to fit unread badge
            Spacer()
            Image(systemName: "chevron.right.circle")
                .resizable()
                .scaledToFit()
                .foregroundColor(theme.colors.secondary.opacity(0.63))
                .frame(width: 31, height: 31)
                .padding(1)
                .onTapGesture {
                    activeSheet = .chatProfiles
                }
        }
    }

    private func userView(_ u: UserInfo, size: CGFloat) -> some View {
        ZStack(alignment: .topTrailing) {
            ProfileImage(imageStr: u.user.image, size: size, color: Color(uiColor: .quaternaryLabel))
                .padding([.top, .trailing], 3)
            if (u.unreadCount > 0) {
                unreadBadge(size: 10)
            }
        }
        .frame(width: size)
        .onTapGesture {
            activeUser = m.currentUser

            Task {
                do {
                    try await changeActiveUserAsync_(u.user.userId, viewPwd: nil)
                    await MainActor.run {
                        activeSheet = nil
                    }
                } catch {
                    await MainActor.run {
                        AlertManager.shared.showAlertMsg(
                            title: "Error switching profile!",
                            message: "Error: \(responseError(error))"
                        )
                    }
                }
            }
        }
    }
    
    private func openSheetOnTap(title: LocalizedStringKey, icon: String, action: @escaping () -> Void) -> some View {
        openSheetOnTap(label: {
            ZStack(alignment: .leading) {
                Image(systemName: icon).frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                    .symbolRenderingMode(.monochrome)
                    .foregroundColor(theme.colors.secondary)
                Text(title)
                    .foregroundColor(.primary)
                    .padding(.leading, 36)
            }
//            Label {
//                Text(title).foregroundColor(.primary)
//            } icon: {
//                Image(systemName: image)
//                    .resizable()
//                    .scaledToFit()
//                    .symbolRenderingMode(.monochrome)
//                    .foregroundColor(theme.colors.secondary)
//                    .frame(maxHeight: 20)
//            }
        }, action: action)
    }
    
    private func openSheetOnTap<V: View>(label: () -> V, action: @escaping () -> Void) -> some View {
        Button(action: action, label: label)
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }
    
    private func unreadBadge(size: CGFloat) -> some View {
        Circle()
            .frame(width: size, height: size)
            .foregroundColor(theme.colors.primary)
    }
}

struct UserPicker_Previews: PreviewProvider {
    static var previews: some View {
        @State var activeSheet: UserPickerSheet?

        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return UserPicker(
            activeSheet: $activeSheet
        )
        .environmentObject(m)
    }
}
