//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum UserPickerSheet: Identifiable {
    case address
    case chatPreferences
    case migrateDevice
    case chatProfiles

    var id: Self { self }
}

struct UserPicker: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.scenePhase) var scenePhase
    @EnvironmentObject var theme: AppTheme
    @Binding var showSettings: Bool
    @Binding var showConnectDesktop: Bool
    @Binding var userPickerVisible: Bool
    @State var scrollViewContentSize: CGSize = .zero
    @State var disableScrolling: Bool = true
    @State var activeSheet: UserPickerSheet? = nil
    @State private var showProgress: Bool = false
    private let verticalSpaceDefault: CGFloat = 12
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true

    var body: some View {
        let users = m.users
            .filter({ u in !u.user.hidden && !u.user.activeUser })
            .prefix(3)
        
        VStack {
            VStack(alignment: .leading, spacing: 0) {
                if let currentUser = m.currentUser {
                    HStack(spacing: 19) {
                        ProfileImage(imageStr: currentUser.image, size: 44)
                        Spacer()
                        ForEach(users) { u in
                            userView(u)
                        }
                            Image(systemName: "list.bullet.circle")
                                .resizable()
                                .foregroundColor(theme.colors.secondary)
                                .accentColor(theme.colors.secondary)
                                .frame(width: 32, height: 32)
                                .onTapGesture {
                                    showSettings = false
                                    activeSheet = .chatProfiles
                                }
                    }
                    .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                    Text(currentUser.displayName)
                        .fontWeight(.bold)
                        .font(.title2)
                        .padding(.vertical, verticalSpaceDefault)
                        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
                }
                menuButton("Your SimpleX address", icon: "qrcode") {
                    activeSheet = .address
                    userPickerVisible.toggle()
                }
                menuButton("Chat preferences", icon: "switch.2") {
                    activeSheet = .chatPreferences
                    userPickerVisible.toggle()
                }
                menuButton("Use from desktop", icon: "desktopcomputer") {
                    showConnectDesktop = true
                    userPickerVisible.toggle()
                }
                menuButton("Migrate to another device", icon: "tray.and.arrow.up") {
                    activeSheet = .migrateDevice
                    userPickerVisible.toggle()
                }
                
                Divider().padding(.vertical, verticalSpaceDefault)
                
                menuButton("Settings", icon: "gearshape") {
                    showSettings = true
                    userPickerVisible.toggle()
                }
            }
            .padding(16)
        }
        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
        .background(
            Rectangle()
                .fill(theme.colors.surface)
                .ignoresSafeArea()
        )
        .frame(maxWidth: .infinity)
        .opacity(userPickerVisible ? 1.0 : 0.0)
        .offset(y: userPickerVisible ? 0 : oneHandUI ? 200 : -200)
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
        .sheet(item: $activeSheet) { sheet in
            NavigationView {
                switch sheet {
                case .chatProfiles:
                    UserProfilesView(showSettings: $showSettings)
                case .address:
                    UserAddressView(shareViaProfile: true)
                        .navigationTitle("SimpleX address")
                        .navigationBarTitleDisplayMode(.large)
                        .modifier(ThemedBackground(grouped: true))
                case .chatPreferences:
                    if let user = m.currentUser {
                        PreferencesView(profile: user.profile, preferences: user.fullPreferences, currentPreferences: user.fullPreferences)
                            .navigationTitle("Your preferences")
                            .navigationBarTitleDisplayMode(.large)
                            .modifier(ThemedBackground(grouped: true))
                    }
                case .migrateDevice:
                    MigrateFromDevice(showSettings: $showSettings, showProgressOnSettings: $showProgress)
                        .navigationTitle("Migrate device")
                        .modifier(ThemedBackground(grouped: true))
                        .navigationBarTitleDisplayMode(.large)
                }
            }
        }
        
        if showProgress {
            progressView()
        }
    }
    
    private func progressView() -> some View {
        VStack {
            ProgressView().scaleEffect(2)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity )
    }
    
    private func userView(_ u: UserInfo) -> some View {
        let user = u.user
        return Button(action: {
            Task {
                do {
                    try await changeActiveUserAsync_(user.userId, viewPwd: nil)
                    await MainActor.run { userPickerVisible = false }
                } catch {
                    await MainActor.run {
                        AlertManager.shared.showAlertMsg(
                            title: "Error switching profile!",
                            message: "Error: \(responseError(error))"
                        )
                    }
                }
            }
        }, label: {
            ProfileImage(imageStr: u.user.image, size: 32)
        })
        .buttonStyle(PressedButtonStyle(defaultColor: theme.colors.surface, pressedColor: Color(uiColor: .secondarySystemFill)))
    }


    private func menuButton(_ title: LocalizedStringKey, icon: String, action: @escaping () -> Void) -> some View {
        Button(action: action) {
            HStack(spacing: verticalSpaceDefault) {
                Image(systemName: icon)
                    .symbolRenderingMode(.monochrome)
                    .foregroundColor(theme.colors.secondary)
                    .frame(maxWidth: 20, maxHeight: 20)
                Text(title)
                    .overlay(DetermineWidth())
                Spacer()
            }
            .frame(height: 20)
            .padding(.vertical, verticalSpaceDefault)
        }
        .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
        .buttonStyle(PressedButtonStyle(defaultColor: theme.colors.surface, pressedColor: Color(uiColor: .secondarySystemFill)))
    }
}

private func unreadCounter(_ unread: Int, color: Color) -> some View {
    unreadCountText(unread)
    .font(.caption)
    .foregroundColor(.white)
    .padding(.horizontal, 4)
    .frame(minWidth: 18, minHeight: 18)
    .background(color)
    .cornerRadius(10)
}

struct UserPicker_Previews: PreviewProvider {
    static var previews: some View {
        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return UserPicker(
            showSettings: Binding.constant(false),
            showConnectDesktop: Binding.constant(false),
            userPickerVisible: Binding.constant(true)
        )
        .environmentObject(m)
    }
}
