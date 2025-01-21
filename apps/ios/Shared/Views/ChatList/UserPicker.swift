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
    @Binding var userPickerShown: Bool
    @Binding var activeSheet: UserPickerSheet?
    @State private var currentUser: Int64?
    @State private var switchingProfile = false
    @State private var frameWidth: CGFloat = 0
    @State private var resetScroll = ResetScrollAction()

    // Inset grouped list dimensions
    private let imageSize: CGFloat = 44
    private let rowPadding: CGFloat = 16
    private let rowVerticalPadding: CGFloat = 11
    private let sectionSpacing: CGFloat = 35
    private var sectionHorizontalPadding: CGFloat { frameWidth > 375 ? 20 : 16 }
    private let sectionShape = RoundedRectangle(cornerRadius: 10, style: .continuous)

    var body: some View {
        let otherUsers: [UserInfo] = m.users
            .filter { u in !u.user.hidden && u.user.userId != m.currentUser?.userId }
            .sorted(using: KeyPathComparator<UserInfo>(\.user.activeOrder, order: .reverse))
        let sectionWidth = max(frameWidth - sectionHorizontalPadding * 2, 0)
        let currentUserWidth = max(frameWidth - sectionHorizontalPadding - rowPadding * 2 - 14 - imageSize, 0)
        let stopped = m.chatRunning != true
        VStack(spacing: sectionSpacing) {
            if let user = m.currentUser {
                StickyScrollView(resetScroll: $resetScroll) {
                    HStack(spacing: rowPadding) {
                        HStack {
                            ProfileImage(imageStr: user.image, size: imageSize, color: Color(uiColor: .tertiarySystemGroupedBackground))
                                .padding(.trailing, 6)
                            profileName(user).lineLimit(1)
                        }
                        .padding(rowPadding)
                        .frame(width: otherUsers.isEmpty ? sectionWidth : currentUserWidth, alignment: .leading)
                        .modifier(ListRow { activeSheet = .currentProfile })
                        .clipShape(sectionShape)
                        .disabled(stopped)
                        .opacity(stopped ? 0.4 : 1)
                        ForEach(otherUsers) { u in
                            userView(u, size: imageSize)
                                .frame(maxWidth: sectionWidth * 0.618)
                                .fixedSize()
                                .disabled(stopped)
                                .opacity(stopped ? 0.4 : 1)
                        }
                    }
                    .padding(.horizontal, sectionHorizontalPadding)
                }
                .frame(height: 2 * rowPadding + imageSize)
                .padding(.top, sectionSpacing)
                .overlay(DetermineWidth())
                .onPreferenceChange(DetermineWidth.Key.self) { frameWidth = $0 }
            }
            VStack(spacing: 0) {
                openSheetOnTap("qrcode", title: m.userAddress == nil ? "Create SimpleX address" : "Your SimpleX address", sheet: .address, disabled: stopped)
                openSheetOnTap("switch.2", title: "Chat preferences", sheet: .chatPreferences, disabled: stopped)
                openSheetOnTap("person.crop.rectangle.stack", title: "Your chat profiles", sheet: .chatProfiles, disabled: stopped)
                openSheetOnTap("desktopcomputer", title: "Use from desktop", sheet: .useFromDesktop, disabled: stopped)
                ZStack(alignment: .trailing) {
                    openSheetOnTap("gearshape", title: "Settings", sheet: .settings, showDivider: false)
                    Image(systemName: colorScheme == .light ? "sun.max" : "moon.fill")
                        .resizable()
                        .scaledToFit()
                        .symbolRenderingMode(.monochrome)
                        .foregroundColor(theme.colors.secondary)
                        .frame(maxWidth: 20, maxHeight: .infinity)
                        .padding(.horizontal, rowPadding)
                        .background(Color(.systemBackground).opacity(0.01))
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
            .clipShape(sectionShape)
            .padding(.horizontal, sectionHorizontalPadding)
            .padding(.bottom, sectionSpacing)
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
        .onChange(of: userPickerShown) {
            if !$0 { resetScroll() }
        }
        .modifier(ThemedBackground(grouped: true))
        .disabled(switchingProfile)
    }

    private func userView(_ u: UserInfo, size: CGFloat) -> some View {
        HStack {
            ZStack(alignment: .topTrailing) {
                ProfileImage(imageStr: u.user.image, size: size, color: Color(uiColor: .tertiarySystemGroupedBackground))
                if (u.unreadCount > 0) {
                    UnreadBadge(userInfo: u).offset(x: 4, y: -4)
                }
            }
            .padding(.trailing, 6)
            Text(u.user.displayName).font(.title2).lineLimit(1)
        }
        .padding(rowPadding)
        .modifier(ListRow {
            switchingProfile = true
            Task {
                do {
                    try await changeActiveUserAsync_(u.user.userId, viewPwd: nil)
                    await MainActor.run {
                        switchingProfile = false
                        userPickerShown = false
                    }
                } catch {
                    await MainActor.run {
                        switchingProfile = false
                        showAlert(
                            NSLocalizedString("Error switching profile!", comment: "alertTitle"),
                            message: String.localizedStringWithFormat(NSLocalizedString("Error: %@", comment: "alert message"), responseError(error))
                        )
                    }
                }
            }
        })
        .clipShape(sectionShape)
    }

    private func openSheetOnTap(_ icon: String, title: LocalizedStringKey, sheet: UserPickerSheet, showDivider: Bool = true, disabled: Bool = false) -> some View {
        ZStack(alignment: .bottom) {
            settingsRow(icon, color: theme.colors.secondary) {
                Text(title).foregroundColor(.primary).opacity(disabled ? 0.4 : 1)
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, rowPadding)
            .padding(.vertical, rowVerticalPadding)
            .modifier(ListRow { activeSheet = sheet })
            .disabled(disabled)
            if showDivider {
                Divider().padding(.leading, 52)
            }
        }
    }
}

struct UnreadBadge: View {
    var userInfo: UserInfo
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize

    var body: some View {
        let size = dynamicSize(userFont).chatInfoSize
        unreadCountText(userInfo.unreadCount)
            .font(userFont <= .xxxLarge ? .caption : .caption2)
            .foregroundColor(.white)
            .padding(.horizontal, dynamicSize(userFont).unreadPadding)
            .frame(minWidth: size, minHeight: size)
            .background(userInfo.user.showNtfs ? theme.colors.primary : theme.colors.secondary)
            .cornerRadius(dynamicSize(userFont).unreadCorner)
    }
}

struct ListRow: ViewModifier {
    @Environment(\.colorScheme) private var colorScheme: ColorScheme
    @State private var touchDown = false
    let action: () -> Void

    func body(content: Content) -> some View {
        ZStack {
            elevatedSecondarySystemGroupedBackground
            Color(.systemGray4).opacity(touchDown ? 1 : 0)
            content
            TouchOverlay(touchDown: $touchDown, action: action)
        }
    }

    var elevatedSecondarySystemGroupedBackground: Color {
        switch colorScheme {
        case .dark: Color(0xFF2C2C2E)
        default:    Color(0xFFFFFFFF)
        }
    }

    struct TouchOverlay: UIViewRepresentable {
        @Binding var touchDown: Bool
        let action: () -> Void

        func makeUIView(context: Context) -> TouchView {
            let touchView = TouchView()
            let gesture = UILongPressGestureRecognizer(
                target: touchView,
                action: #selector(touchView.longPress(gesture:))
            )
            gesture.delegate = touchView
            gesture.minimumPressDuration = 0
            touchView.addGestureRecognizer(gesture)
            return touchView
        }

        func updateUIView(_ touchView: TouchView, context: Context) {
            touchView.representer = self
        }

        class TouchView: UIView, UIGestureRecognizerDelegate {
            var representer: TouchOverlay?
            private var startLocation: CGPoint?
            private var task: Task<Void, Never>?

            @objc
            func longPress(gesture: UILongPressGestureRecognizer) {
                switch gesture.state {
                case .began:
                    startLocation = gesture.location(in: nil)
                    task = Task {
                        do {
                            try await Task.sleep(nanoseconds: 200_000000)
                            await MainActor.run { representer?.touchDown = true }
                        } catch { }
                    }
                case .ended:
                    if hitTest(gesture.location(in: self), with: nil) == self {
                        representer?.action()
                    }
                    task?.cancel()
                    representer?.touchDown = false
                case .changed:
                    if let startLocation {
                        let location = gesture.location(in: nil)
                        let dx = location.x - startLocation.x
                        let dy = location.y - startLocation.y
                        if sqrt(pow(dx, 2) + pow(dy, 2)) > 10 { gesture.state = .failed }
                    }
                case .cancelled, .failed:
                    task?.cancel()
                    representer?.touchDown = false
                default: break
                }
            }

            func gestureRecognizer(
                _: UIGestureRecognizer,
                shouldRecognizeSimultaneouslyWith: UIGestureRecognizer
            ) -> Bool { true }
        }
    }
}


struct UserPicker_Previews: PreviewProvider {
    static var previews: some View {
        @State var activeSheet: UserPickerSheet?

        let m = ChatModel()
        m.users = [UserInfo.sampleData, UserInfo.sampleData]
        return UserPicker(
            userPickerShown: .constant(true),
            activeSheet: $activeSheet
        )
        .environmentObject(m)
    }
}
