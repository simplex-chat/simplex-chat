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
    @Binding var showSettings: Bool
    @Binding var userPickerVisible: Bool
    @State var scrollViewContentSize: CGSize = .zero
    @State var disableScrolling: Bool = true
    @State private var showProgress: Bool = false
    private let verticalSpaceDefault: CGFloat = 12
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @State private var usersToPreview: ArraySlice<UserInfo> = []
    @State private var activeUser: User? = nil

    // Sheet height management
    @State private var isLargeSheet = false
    @State private var allowSmallSheet = true
    @State private var isAddressActive = false
    @State private var isChatPreferencesActive = false
    @State private var isUseFromDesktopActive = false
    @State private var isMigrateToAnotherDeviceActice = false

    var body: some View {
        let v = NavigationView {
            VStack(alignment: .leading, spacing: 0) {
                if let currentUser = activeUser {
                    HStack(spacing: 16) {
                        NavigationLink {
                            UserProfile()
                                .navigationTitle("Your current profile")
                                .modifier(ThemedBackground())
                        } label: {
                            ProfileImage(imageStr: currentUser.image, size: 44)
                        }
                        Spacer()
                        ForEach(usersToPreview) { u in
                            userView(u)
                        }
                        NavigationLink {
                            UserProfilesView()
                                .navigationBarTitleDisplayMode(.large)
                        } label: {
                            let c = Color(uiColor: .tertiarySystemGroupedBackground).asAnotherColorFromSecondaryVariant(theme)

                            Image(systemName: "list.bullet.circle.fill")
                                .resizable()
                                .foregroundColor(c)
                                .font(.system(size: 32))
                                .frame(width: 32, height: 32)
                        }
                    }
                    .padding(.horizontal, 16)
                    
                    Text(currentUser.displayName)
                        .fontWeight(.bold)
                        .font(.title2)
                        .padding(.top, verticalSpaceDefault)
                        .padding(.horizontal, 16)
                }
                
                List {
                    Section {
                        if let currentUser = activeUser {
                            NavigationLink(isActive: $isAddressActive) {
                                UserAddressView(shareViaProfile: currentUser.addressShared)
                                    .navigationTitle("SimpleX address")
                                    .navigationBarTitleDisplayMode(.large)
                                    .modifier(ThemedBackground(grouped: true))
                            } label: {
                                navigateOnTap(title: "Your SimpleX address", image: "qrcode") {
                                    isAddressActive = true
                                }
                            }
                            
                            NavigationLink(isActive: $isChatPreferencesActive) {
                                PreferencesView(profile: currentUser.profile, preferences: currentUser.fullPreferences, currentPreferences: currentUser.fullPreferences)
                                    .navigationTitle("Your preferences")
                                    .navigationBarTitleDisplayMode(.large)
                                    .modifier(ThemedBackground(grouped: true))
                            } label: {
                                navigateOnTap(title: "Chat preferences", image: "switch.2") {
                                    isChatPreferencesActive = true
                                }
                            }
                            
                            NavigationLink(isActive: $isUseFromDesktopActive) {
                                ConnectDesktopView(viaSettings: true)
                            } label: {
                                navigateOnTap(title: "Use from desktop", image: "desktopcomputer") {
                                    isUseFromDesktopActive = true
                                }
                            }
                            
                            NavigationLink(isActive: $isMigrateToAnotherDeviceActice) {
                                MigrateFromDevice(showProgressOnSettings: $showProgress)
                                    .navigationTitle("Migrate device")
                                    .modifier(ThemedBackground(grouped: true))
                                    .navigationBarTitleDisplayMode(.large)
                            } label: {
                                navigateOnTap(title: "Migrate to another device", image: "tray.and.arrow.up") {
                                    isMigrateToAnotherDeviceActice = true
                                }
                            }
                        }
                    }
                    
                    Section {
                        HStack {
                            Button {
                                DispatchQueue.main.async {
                                    dismissAllSheets(animated: false) {
                                        showSettings = true
                                    }
                                }
                            } label: {
                                Label {
                                    Text("Settings")
                                } icon: {
                                    Image(systemName: "gearshape")
                                        .symbolRenderingMode(.monochrome)
                                        .foregroundColor(theme.colors.secondary)
                                        .frame(maxWidth: 20, maxHeight: 20)
                                }
                                Spacer()
                            }
                            .buttonStyle(PlainButtonStyle())
                            Label {} icon: {
                                Image(systemName: colorScheme == .light ? "sun.max" : "moon.stars")
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
            }
            .padding(.vertical, 19)
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
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
                
                usersToPreview = m.users
                    .filter({ u in !u.user.hidden && !u.user.activeUser })
                    .prefix(3)
                
                activeUser = m.currentUser
            }
            .modifier(ThemedBackground(grouped: true))
            
            if showProgress {
                progressView()
            }
        }
        
        if #available(iOS 16.0, *), oneHandUI {
            let sheetHeight: CGFloat = 425
            v.presentationDetents(
                allowSmallSheet ? [.height(sheetHeight), .large] : [.large],
                selection: Binding(
                    get: { isLargeSheet || !allowSmallSheet ? .large : .height(sheetHeight) },
                    set: { isLargeSheet = $0 == .large }
                )
            )
        } else {
            v
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
                    await MainActor.run {
                        withAnimation {
                            userPickerVisible.toggle()
                        }
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
        }, label: {
            ZStack(alignment: .topTrailing) {
                ProfileImage(imageStr: u.user.image, size: 32)
                if (u.unreadCount > 0) {
                    unreadCounter()
                }
            }
        })
    }
    
    private func navigateOnTap(title: String, image: String, setActive: @escaping () -> Void) -> some View {
        Label {
            Text(title)
        } icon: {
            Image(systemName: image)
                .symbolRenderingMode(.monochrome)
                .foregroundColor(theme.colors.secondary)
                .frame(maxWidth: 20, maxHeight: 20)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(.leading, 16).padding(.vertical, 8).padding(.trailing, 32)
        .contentShape(Rectangle())
        .onTapGesture {
            isLargeSheet = true
            DispatchQueue.main.async {
                allowSmallSheet = false
                setActive()
            }
        }
        .padding(.leading, -16).padding(.vertical, -8).padding(.trailing, -32)
    }
    
    private func unreadCounter() -> some View {
        Circle()
            .frame(width: 12, height: 12)
            .foregroundColor(theme.colors.primary)
    }
}

struct UserPicker_Previews: PreviewProvider {
    static var previews: some View {
        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return UserPicker(
            showSettings: Binding.constant(false),
            userPickerVisible: Binding.constant(true)
        )
        .environmentObject(m)
    }
}
