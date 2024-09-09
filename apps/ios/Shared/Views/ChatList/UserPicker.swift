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
    @State private var switchingProfile = false

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
    
    private var viewBody: some View {
        let otherUsers = m.users.filter { u in !u.user.hidden && u.user.userId != m.currentUser?.userId }
        return List {
            Section(header: Text("You").foregroundColor(theme.colors.secondary)) {
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

                    openSheetOnTap(title: m.userAddress == nil ? "Create public address" : "Your public address", icon: "qrcode") {
                        activeSheet = .address
                    }
                    
                    openSheetOnTap(title: "Chat preferences", icon: "switch.2") {
                        activeSheet = .chatPreferences
                    }
                }
            }

            Section {
                if otherUsers.isEmpty {
                    openSheetOnTap(title: "Your chat profiles", icon: "person.crop.rectangle.stack") {
                        activeSheet = .chatProfiles
                    }
                } else {
                    let v = userPickerRow(otherUsers, size: 44)
                        .padding(.leading, -8)
                    if #available(iOS 16.0, *) {
                        v
                    } else {
                        v.padding(.vertical, 4)
                    }
                }

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
        .disabled(switchingProfile)
    }
        
    private func userPickerRow(_ users: [UserInfo], size: CGFloat) -> some View {
        HStack(spacing: 6) {
            let s = ScrollView(.horizontal) {
                HStack(spacing: 27) {
//                    Image(systemName: "person.crop.rectangle.stack.fill")
//                        .resizable()
//                        .scaledToFit()
//                        .frame(height: size)
//                        .foregroundColor(Color(uiColor: .tertiarySystemGroupedBackground).asAnotherColorFromSecondaryVariant(theme))
//                        .padding([.top, .trailing], 3)
//                    Image(systemName: "theatermasks.fill")
//                        .resizable()
//                        .scaledToFit()
//                        .frame(width: size, height: size)
//                        .foregroundColor(.indigo)
//                        .padding([.top, .trailing], 3)
                    ForEach(users) { u in
                        if !u.user.hidden && u.user.userId != m.currentUser?.userId {
                            userView(u, size: size)
                        }
                    }
                }
                .padding(.leading, 4)
                .padding(.trailing, 22)
            }
            ZStack {
                if #available(iOS 16.0, *) {
                    s.scrollIndicators(.hidden)
                } else {
                    s
                }
                HStack(spacing: 0) {
                    LinearGradient(
                        colors: [.black, .clear],
                        startPoint: .leading,
                        endPoint: .trailing
                    )
                    .frame(width: 2)
                    Color.clear
                    LinearGradient(
                        colors: [.clear, .black],
                        startPoint: .leading,
                        endPoint: .trailing
                    )
                    .frame(width: size)
                }
                .frame(height: size + 3)
                .blendMode(.destinationOut)
                .allowsHitTesting(false)
            }
            .compositingGroup()
            .padding(.top, -3) // to fit unread badge
            Spacer()
            Image(systemName: "chevron.right")
                .foregroundColor(theme.colors.secondary)
                .padding(.trailing, 4)
                .onTapGesture {
                    activeSheet = .chatProfiles
                }
        }
    }

    private func userView(_ u: UserInfo, size: CGFloat) -> some View {
        ZStack(alignment: .topTrailing) {
            ProfileImage(imageStr: u.user.image, size: size, color: Color(uiColor: .tertiarySystemGroupedBackground))
                .padding([.top, .trailing], 3)
            if (u.unreadCount > 0) {
                unreadBadge(u)
            }
        }
        .frame(width: size)
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
