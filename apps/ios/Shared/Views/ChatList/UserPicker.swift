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
            VStack(alignment: .leading, spacing: 6) {
                if let currentUser = activeUser ?? m.currentUser {
                    HStack(alignment: .top) {
                        ProfileImage(imageStr: currentUser.image, size: 52)
                            .onTapGesture {
                                activeSheet = .currentProfile
                            }
                        Spacer()
                        let usersToPreview = m.users.filter({ u in !u.user.hidden && u.user.userId != currentUser.userId })
                        HStack(spacing: 0) {
                            usersRow(users: usersToPreview)
                            Button {
                                activeSheet = .chatProfiles
                            } label: {
                                Image(systemName: "ellipsis.circle.fill")
                                    .resizable()
                                    .scaledToFit()
                                    .frame(width: 32, height: 32)
                                    .foregroundColor(Color(uiColor: .quaternaryLabel))
                            }
                        }
                    }
                    Text(currentUser.displayName)
                        .fontWeight(.bold)
                        .font(.headline)
                }
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .listRowBackground(Color.clear)
            .listRowSeparator(.hidden)
            .padding(.horizontal, 12)

            Section {
                if (m.currentUser != nil) {
                    openSheetOnTap(title: m.userAddress == nil ? "Create public address" : "Your public address", image: "qrcode") {
                        activeSheet = .address
                    }
                    
                    openSheetOnTap(title: "Chat preferences", image: "switch.2") {
                        activeSheet = .chatPreferences
                    }
                    
                    openSheetOnTap(title: "Use from desktop", image: "desktopcomputer") {
                        activeSheet = .useFromDesktop
                    }
                }
            }
            
            Section {
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
            v.presentationDetents([.height(400)])
        } else {
            v
        }
    }

    @ViewBuilder
    private func usersRow(users: [UserInfo]) -> some View {
        let gradientInset: Double = 16
        let leadingPadding: Double = 32
        let scrollView = ScrollView(.horizontal) {
            HStack(spacing: 0) {
                ForEach(users) { userView($0) }
            }
            .padding(.leading, leadingPadding)
            .padding(.horizontal, gradientInset)

        }
        ZStack {
            if #available(iOS 16.4, *) {
                scrollView
                    .scrollBounceBehavior(.basedOnSize, axes: [.horizontal])
                    .scrollIndicators(.hidden)
            } else {
                scrollView
            }
            HStack(spacing: 0) {
                LinearGradient(
                    colors: [.black, .clear],
                    startPoint: .leading,
                    endPoint: .trailing
                ).frame(width: gradientInset)
                Color.clear
                LinearGradient(
                    colors: [.clear, .black],
                    startPoint: .leading,
                    endPoint: .trailing
                ).frame(width: gradientInset)
            }.blendMode(.destinationOut)
        }
        .compositingGroup()
        .frame(maxWidth: gradientInset + leadingPadding + Double(users.count) * 40 + gradientInset)
    }

    private func userView(_ u: UserInfo) -> some View {
        ZStack(alignment: .topTrailing) {
            ProfileImage(imageStr: u.user.image, size: 32, color: Color(uiColor: .quaternaryLabel))
                .padding(4)
            if (u.unreadCount > 0) {
                unreadBadge()
            }
        }
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
    
    private func openSheetOnTap(title: LocalizedStringKey, image: String, setActive: @escaping () -> Void) -> some View {
        Button(action: setActive) {
            Label {
                Text(title).foregroundColor(.primary)
            } icon: {
                Image(systemName: image)
                    .resizable()
                    .symbolRenderingMode(.monochrome)
                    .foregroundColor(theme.colors.secondary)
                    .frame(maxWidth: 20, maxHeight: 20)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(.leading, 16).padding(.vertical, 8).padding(.trailing, 32)
        .contentShape(Rectangle())
        .padding(.leading, -19).padding(.vertical, -8).padding(.trailing, -32)
    }
    
    private func unreadBadge() -> some View {
        Circle()
            .frame(width: 12, height: 12)
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
