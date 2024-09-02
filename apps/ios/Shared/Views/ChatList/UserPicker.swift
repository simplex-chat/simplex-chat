//
// Created by Avently on 16.01.2023.
// Copyright (c) 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private enum UserPickerSheet: Identifiable {
    case chatPreferences
    case chatProfiles
    case currentProfile
    case useFromDesktop
    case settings

    var id: Self { self }
}

struct UserPicker: View {
    @EnvironmentObject var m: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.scenePhase) var scenePhase
    @Environment(\.colorScheme) var colorScheme
    @Binding var userPickerVisible: Bool
    @State var scrollViewContentSize: CGSize = .zero
    @State var disableScrolling: Bool = true
    private let verticalSpaceDefault: CGFloat = 12
    @AppStorage(GROUP_DEFAULT_ONE_HAND_UI, store: groupDefaults) private var oneHandUI = true
    @State private var activeUser: User? = nil
    @State private var activeSheet: UserPickerSheet? = nil
    @State private var showSettings = false
    
    // Address sheet state
    @State private var aas = AutoAcceptState()
    @State private var savedAAS = AutoAcceptState()
    @State private var showAASSaveDialogue = false
    @State private var showingAddressSheet = false
    
    var body: some View {
        let v = NavigationView {
            VStack(alignment: .leading) {
                let activeUser = getActiveUser()
                List {
                    Section {
                        if let currentUser = activeUser {
                            VStack(alignment: .leading, spacing: 6) {
                                HStack(alignment: .top) {
                                    Button {
                                        activeSheet = .currentProfile
                                    } label: {
                                        ProfileImage(imageStr: currentUser.image, size: 52)
                                    }
                                    Spacer()
                                    let usersToPreview = m.users.filter({ u in !u.user.hidden && u.user.userId != currentUser.userId })
                                    ZStack(alignment: .leading) {
                                        ZStack(alignment: .trailing) {
                                            let ps = HStack(spacing: 20) {
                                                Color.clear.frame(width: 48, height: 32)
                                                ForEach(usersToPreview) { u in
                                                    userView(u)
                                                }
                                                Color.clear.frame(width: 32, height: 32)
                                            }
                                            
                                            if usersToPreview.count > 3 {
                                                let s = ScrollView(.horizontal) { ps }.frame(width: 284)
                                                if #available(iOS 16.0, *) {
                                                    s.scrollIndicators(.hidden)
                                                } else {
                                                    s
                                                }
                                            } else {
                                                ps
                                            }
                                            HStack(spacing: 0) {
                                                LinearGradient(
                                                    colors: [.clear, theme.colors.background.asGroupedBackground(theme.base.mode)],
                                                    startPoint: .leading,
                                                    endPoint: .trailing
                                                )
                                                .frame(width: 32, height: 35)
                                                Button {
                                                    activeSheet = .chatProfiles
                                                } label: {
                                                    Image(systemName: "ellipsis.circle.fill")
                                                        .resizable()
                                                        .scaledToFit()
                                                        .frame(width: 31, height: 31)
                                                        .padding(.top, 4)
                                                        .foregroundColor(Color(uiColor: .quaternaryLabel))
                                                        .modifier(ThemedBackground(grouped: true))
                                                }
                                            }
                                        }
                                        .padding(.top, 10)
                                        
                                        LinearGradient(
                                            colors: [.clear, theme.colors.background.asGroupedBackground(theme.base.mode)],
                                            startPoint: .trailing,
                                            endPoint: .leading
                                        )
                                        .frame(width: 32, height: 35)
                                    }
                                }
                                
                                Text(currentUser.displayName)
                                    .fontWeight(.bold)
                                    .font(.headline)
                            }
                            .padding(.horizontal, 30)
                            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                            .listRowBackground(Color.clear)
                        }
                    }
                    
                    Section {
                        if (m.currentUser != nil) {
                            openSheetOnTap(title: m.userAddress == nil ? "Create public address" : "Your public address", image: "qrcode") {
                                showingAddressSheet = true
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
            .alert(isPresented: $showAASSaveDialogue) {
                Alert(
                    title: Text("Auto accept settings"),
                    message: Text("Settings were changed"),
                    primaryButton: .default(Text("Save")) {
                        saveAAS()
                    },
                    secondaryButton: .destructive(Text("Revert"))
                )
            }
            .sheet(isPresented: $showingAddressSheet, onDismiss: {
                if savedAAS != aas {
                    showAASSaveDialogue = true
                }
            }) {
                if let currentUser = m.currentUser {
                    NavigationView {
                        UserAddressView(shareViaProfile: currentUser.addressShared, aas: $aas, savedAAS: $savedAAS)
                            .navigationTitle("Public address")
                            .navigationBarTitleDisplayMode(.large)
                            .modifier(ThemedBackground(grouped: true))
                    }
                }
            }
            .sheet(item: $activeSheet) { sheet in
                    if let currentUser = m.currentUser {
                        NavigationView {
                            switch sheet {
                            case .chatProfiles:
                                UserProfilesView()
                            case .currentProfile:
                                UserProfile()
                                    .navigationTitle("Your current profile")
                                    .modifier(ThemedBackground())
                            case .chatPreferences:
                                PreferencesView(profile: currentUser.profile, preferences: currentUser.fullPreferences, currentPreferences: currentUser.fullPreferences)
                                    .navigationTitle("Your preferences")
                                    .navigationBarTitleDisplayMode(.large)
                                    .modifier(ThemedBackground(grouped: true))
                            case .useFromDesktop:
                                ConnectDesktopView(viaSettings: true)
                            case .settings:
                                SettingsView(showSettings: $showSettings, viaUserPicker: true)
                                    .navigationBarTitleDisplayMode(.large)
                            }
                        }
                    }
                }
                .modifier(ThemedBackground(grouped: true))
        }
        
        if #available(iOS 16.0, *) {
            v.presentationDetents([.height(400)])
        } else {
            v
        }
    }
    
    private func getActiveUser() -> User? {
        return activeUser ?? m.currentUser
    }
    
    private func saveAAS() {
        Task {
            do {
                if let address = try await userAddressAutoAccept(aas.autoAccept) {
                    m.userAddress = address
                    savedAAS = aas
                }
            } catch let error {
                logger.error("userAddressAutoAccept error: \(responseError(error))")
            }
        }
    }
    
    private func userView(_ u: UserInfo) -> some View {
        let user = u.user
        return Button(action: {
            activeUser = m.currentUser

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
                ProfileImage(imageStr: u.user.image, size: 32, color: Color(uiColor: .quaternaryLabel))
                    .padding([.top, .trailing], 3)
                if (u.unreadCount > 0) {
                    unreadBadge()
                }
            }
        })
    }
    
    private func openSheetOnTap(title: LocalizedStringKey, image: String, setActive: @escaping () -> Void) -> some View {
        Button {
            setActive()
        } label: {
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
        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return UserPicker(
            userPickerVisible: Binding.constant(true)
        )
        .environmentObject(m)
    }
}
