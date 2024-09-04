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
        let v = List {
//            VStack(alignment: .leading, spacing: 6) {
//                if let currentUser = activeUser ?? m.currentUser {
//                    HStack(alignment: .top) {
//                        ProfileImage(imageStr: currentUser.image, size: 52)
//                            .onTapGesture {
//                                activeSheet = .currentProfile
//                            }
//                        Spacer()
//                        let usersToPreview = m.users.filter({ u in !u.user.hidden && u.user.userId != currentUser.userId })
//                        ZStack(alignment: .leading) {
//                            ZStack(alignment: .trailing) {
//                                let ps = HStack(spacing: 20) {
//                                    Color.clear.frame(width: 48, height: 32)
//                                    ForEach(usersToPreview) { u in
//                                        userView(u)
//                                    }
//                                    Color.clear.frame(width: 32, height: 32)
//                                }
//                                
//                                if usersToPreview.count > 3 {
//                                    let s = ScrollView(.horizontal) { ps }.frame(width: 284)
//                                    if #available(iOS 16.0, *) {
//                                        s.scrollIndicators(.hidden)
//                                    } else {
//                                        s
//                                    }
//                                } else {
//                                    ps
//                                }
//                                HStack(spacing: 0) {
//                                    LinearGradient(
//                                        colors: [.clear, theme.colors.background.asGroupedBackground(theme.base.mode)],
//                                        startPoint: .leading,
//                                        endPoint: .trailing
//                                    )
//                                    .frame(width: 32, height: 35)
//                                    Button {
//                                        activeSheet = .chatProfiles
//                                    } label: {
//                                        Image(systemName: "ellipsis.circle.fill")
//                                            .resizable()
//                                            .scaledToFit()
//                                            .frame(width: 31, height: 31)
//                                            .padding(.top, 4)
//                                            .foregroundColor(Color(uiColor: .quaternaryLabel))
//                                            .modifier(ThemedBackground(grouped: true))
//                                    }
//                                }
//                            }
//                            .padding(.top, 10)
//                            
//                            LinearGradient(
//                                colors: [.clear, theme.colors.background.asGroupedBackground(theme.base.mode)],
//                                startPoint: .trailing,
//                                endPoint: .leading
//                            )
//                            .frame(width: 32, height: 35)
//                        }
//                    }
//                    
//                    Text(currentUser.displayName)
//                        .fontWeight(.bold)
//                        .font(.headline)
//                }
//            }
//            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
//            .listRowBackground(Color.clear)
//            .listRowSeparator(.hidden)
//            .padding(.horizontal, 12)

            Section {
                let otherUsers = m.users.filter { u in !u.user.hidden && u.user.userId != m.currentUser?.userId }
                if otherUsers.isEmpty {
                    openSheetOnTap(title: "Create chat profile", image: "person.crop.circle.fill.badge.plus") {
                        activeSheet = .chatProfiles
                    }
                } else {
                    userPickerRow(otherUsers)
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

                    openSheetOnTap(title: m.userAddress == nil ? "Create public address" : "Your public address", image: "qrcode") {
                        activeSheet = .address
                    }
                    
                    openSheetOnTap(title: "Chat preferences", image: "switch.2") {
                        activeSheet = .chatPreferences
                    }
                }
            }
            
            Section {
                openSheetOnTap(title: "Use from desktop", image: "desktopcomputer") {
                    activeSheet = .useFromDesktop
                }

                HStack {
                    openSheetOnTap(title: "Settings", image: "gearshape") {
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

        if #available(iOS 16.0, *) {
            v.presentationDetents([.height(425)])
        } else {
            v
        }
    }
        
    private func userPickerRow(_ users: [UserInfo]) -> some View {
        HStack(spacing: 6) {
            let s = ScrollView(.horizontal) {
                HStack(spacing: 20) {
                    ForEach(users) { u in
                        if !u.user.hidden && u.user.userId != m.currentUser?.userId {
                            userView(u, size: 28)
                        }
                    }
                }
                .padding(.trailing, 14)
            }
            ZStack(alignment: .topTrailing) {
                if #available(iOS 16.0, *) {
                    s.scrollIndicators(.hidden)
                } else {
                    s
                }
                LinearGradient(
                    colors: [.clear, theme.colors.background],
                    startPoint: .leading,
                    endPoint: .trailing
                )
                .frame(width: 28, height: 31)
                .allowsHitTesting(false)
            }
            .padding(.top, -3) // to fit unread badge
            Spacer()
            Image(systemName: "chevron.right")
                .foregroundColor(theme.colors.secondary)
                .onTapGesture {
                    activeSheet = .chatProfiles
                }
        }
        .padding(.leading, -4)
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
    
    private func openSheetOnTap(title: LocalizedStringKey, image: String, action: @escaping () -> Void) -> some View {
        openSheetOnTap(label: {
            Label {
                Text(title).foregroundColor(.primary)
            } icon: {
                Image(systemName: image)
                    .resizable()
                    .scaledToFit()
                    .symbolRenderingMode(.monochrome)
                    .foregroundColor(theme.colors.secondary)
                    .frame(maxWidth: 20, maxHeight: 20)
            }
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
