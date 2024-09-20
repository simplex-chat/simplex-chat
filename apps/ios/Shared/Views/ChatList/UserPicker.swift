//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct UserPicker: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
    @Environment(\.scenePhase) private var scenePhase: ScenePhase
    @Environment(\.colorScheme) private var colorScheme: ColorScheme
    @Environment(\.dismiss) private var dismiss: DismissAction
    @Binding var activeSheet: UserPickerSheet?
    @State private var currentUser: Int64?
    @State private var switchingProfile = false
    @State private var frameWidth: CGFloat?

    var body: some View {
        if #available(iOS 16.0, *) {
            let v = viewBody.presentationDetents([.height(420)])
            if #available(iOS 16.4, *) {
                v.scrollBounceBehavior(.basedOnSize)
            } else {
                v
            }
        } else {
            viewBody
        }
    }

    @ViewBuilder
    private var viewBody: some View {
        VStack(spacing: 0) {
            if !m.users.isEmpty {
                StickyScrollView { width in
                    HStack {
                        ForEach(m.users) { u in
                            userView(u, size: 44)
                                .padding(.vertical, 7)
                                .padding(.leading, 5)
                                .padding(.trailing, 9)
                                .frame(
                                    minWidth: u.user == m.currentUser ? width.map { max(0, $0 - 64) } : nil,
                                    alignment: .leading
                                )
                                .background(Color(.secondarySystemGroupedBackground))
                                .clipShape(RoundedRectangle(cornerRadius: 10, style: .continuous))
                                .onTapGesture {
                                    Task {
                                        do {
                                            try await changeActiveUserAsync_(u.user.userId, viewPwd: nil)
                                            await MainActor.run {
                                                switchingProfile = false
                                                dismiss()
                                            }
                                        } catch {
                                            await MainActor.run {
                                                switchingProfile = false
                                                AlertManager.shared.showAlertMsg(
                                                    title: "Error switching profile!",
                                                    message: "Error: \(responseError(error))"
                                                )
                                            }
                                        }
                                    }
                                }
                        }
                    }
                    .padding([.horizontal, .top], 16)
                    .padding(.bottom, 8)
                }
                .frame(height: 92)
                .zIndex(1) // Position above list
            }
            List {
                Section {
                    if let user = m.currentUser {
                        openSheetOnTap(label: {
                            ZStack {
                                let v = ProfilePreview(profileOf: user)
                                    .foregroundColor(.primary)
                                    .padding(.leading, -8)
                                if #available(iOS 16.0, *) {
                                    v
                                } else {
                                    v.padding(.vertical, 4)
                                }
                            }
                        }) {
                            activeSheet = .currentProfile
                        }
                        openSheetOnTap(title: m.userAddress == nil ? "Create SimpleX address" : "Your SimpleX address", icon: "qrcode") {
                            activeSheet = .address
                        }
                        openSheetOnTap(title: "Chat preferences", icon: "switch.2") {
                            activeSheet = .chatPreferences
                        }
                    }
                    openSheetOnTap(title: "Your chat profiles", icon: "person.crop.rectangle.stack") {
                        activeSheet = .chatProfiles
                    }
                    openSheetOnTap(title: "Use from desktop", icon: "desktopcomputer") {
                        activeSheet = .useFromDesktop
                    }

                    ZStack(alignment: .trailing) {
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
                    }
                }
            }
            .padding(.top, -24)
        }
        .onAppear {
            // This check prevents the call of listUsers after the app is suspended, and the database is closed.
            if case .active = scenePhase {
                currentUser = m.currentUser?.userId
                Task {
                    do {
                        let users = try await listUsersAsync()
                        await MainActor.run {
                            m.users = users
                            currentUser = m.currentUser?.userId
                        }
                    } catch {
                        logger.error("Error loading users \(responseError(error))")
                    }
                }
            }
        }
        .modifier(ThemedBackground(grouped: true))
        .disabled(switchingProfile)
    }
        
//    private func userPickerRow(_ users: [UserInfo], size: CGFloat) -> some View {
//        HStack(spacing: 6) {
//            let s = ScrollView(.horizontal) {
//                HStack(spacing: 27) {
//                    ForEach(users) { u in
//                        if !u.user.hidden && u.user.userId != m.currentUser?.userId {
//                            userView(u, size: size)
//                        }
//                    }
//                }
//                .padding(.leading, 4)
//                .padding(.trailing, 22)
//            }
//            ZStack(alignment: .trailing) {
//                if #available(iOS 16.0, *) {
//                    s.scrollIndicators(.hidden)
//                } else {
//                    s
//                }
//                LinearGradient(
//                    colors: [.clear, .black],
//                    startPoint: .leading,
//                    endPoint: .trailing
//                )
//                .frame(width: size, height: size + 3)
//                .blendMode(.destinationOut)
//                .allowsHitTesting(false)
//            }
//            .compositingGroup()
//            .padding(.top, -3) // to fit unread badge
//            Spacer()
//            Image(systemName: "chevron.right")
//                .foregroundColor(theme.colors.secondary)
//                .padding(.trailing, 4)
//                .onTapGesture {
//                    activeSheet = .chatProfiles
//                }
//        }
//    }
//
    private func userView(_ u: UserInfo, size: CGFloat) -> some View {
        HStack {
            ZStack(alignment: .topTrailing) {
                ProfileImage(imageStr: u.user.image, size: size, color: Color(uiColor: .tertiarySystemGroupedBackground))
                    .padding(3)
                if (u.unreadCount > 0) {
                    unreadBadge(u)
                }
            }
            profileName(u.user)
        }
        .onTapGesture {
            switchingProfile = true
            Task {
                do {
                    try await changeActiveUserAsync_(u.user.userId, viewPwd: nil)
                    await MainActor.run {
                        switchingProfile = false
                        dismiss()
                    }
                } catch {
                    await MainActor.run {
                        switchingProfile = false
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
        }, action: action)
    }
    
    private func openSheetOnTap<V: View>(label: () -> V, action: @escaping () -> Void) -> some View {
        Button(action: action, label: label)
        .frame(maxWidth: .infinity, alignment: .leading)
        .contentShape(Rectangle())
    }
    
    private func unreadBadge(_ u: UserInfo) -> some View {
        let size = dynamicSize(userFont).chatInfoSize
        return unreadCountText(u.unreadCount)
            .font(userFont <= .xxxLarge ? .caption  : .caption2)
            .foregroundColor(.white)
            .padding(.horizontal, dynamicSize(userFont).unreadPadding)
            .frame(minWidth: size, minHeight: size)
            .background(u.user.showNtfs ? theme.colors.primary : theme.colors.secondary)
            .cornerRadius(dynamicSize(userFont).unreadCorner)
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
