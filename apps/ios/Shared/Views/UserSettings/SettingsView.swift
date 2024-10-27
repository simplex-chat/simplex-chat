//
//  SettingsView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import StoreKit
import SimpleXChat

let simplexTeamURL = URL(string: "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")!

let appVersion = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String

let appBuild = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion")  as? String

let DEFAULT_SHOW_LA_NOTICE = "showLocalAuthenticationNotice"
let DEFAULT_LA_NOTICE_SHOWN = "localAuthenticationNoticeShown"
let DEFAULT_PERFORM_LA = "performLocalAuthentication" // deprecated, moved to app group
let DEFAULT_LA_MODE = "localAuthenticationMode"
let DEFAULT_LA_LOCK_DELAY = "localAuthenticationLockDelay"
let DEFAULT_LA_SELF_DESTRUCT = "localAuthenticationSelfDestruct"
let DEFAULT_LA_SELF_DESTRUCT_DISPLAY_NAME = "localAuthenticationSelfDestructDisplayName"
let DEFAULT_NOTIFICATION_ALERT_SHOWN = "notificationAlertShown"
let DEFAULT_WEBRTC_POLICY_RELAY = "webrtcPolicyRelay"
let DEFAULT_WEBRTC_ICE_SERVERS = "webrtcICEServers"
let DEFAULT_CALL_KIT_CALLS_IN_RECENTS = "callKitCallsInRecents"
let DEFAULT_PRIVACY_ACCEPT_IMAGES = "privacyAcceptImages" // unused. Use GROUP_DEFAULT_PRIVACY_ACCEPT_IMAGES instead
let DEFAULT_PRIVACY_LINK_PREVIEWS = "privacyLinkPreviews" // deprecated, moved to app group
let DEFAULT_PRIVACY_SIMPLEX_LINK_MODE = "privacySimplexLinkMode"
let DEFAULT_PRIVACY_SHOW_CHAT_PREVIEWS = "privacyShowChatPreviews"
let DEFAULT_PRIVACY_SAVE_LAST_DRAFT = "privacySaveLastDraft"
let DEFAULT_PRIVACY_PROTECT_SCREEN = "privacyProtectScreen"
let DEFAULT_PRIVACY_DELIVERY_RECEIPTS_SET = "privacyDeliveryReceiptsSet"
let DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS = "privacyMediaBlurRadius"
let DEFAULT_EXPERIMENTAL_CALLS = "experimentalCalls"
let DEFAULT_CHAT_ARCHIVE_NAME = "chatArchiveName"
let DEFAULT_CHAT_ARCHIVE_TIME = "chatArchiveTime"
let DEFAULT_CHAT_V3_DB_MIGRATION = "chatV3DBMigration"
let DEFAULT_DEVELOPER_TOOLS = "developerTools"
let DEFAULT_ENCRYPTION_STARTED = "encryptionStarted"
let DEFAULT_ENCRYPTION_STARTED_AT = "encryptionStartedAt"
let DEFAULT_ACCENT_COLOR_RED = "accentColorRed" // deprecated, only used for migration
let DEFAULT_ACCENT_COLOR_GREEN = "accentColorGreen" // deprecated, only used for migration
let DEFAULT_ACCENT_COLOR_BLUE = "accentColorBlue" // deprecated, only used for migration
let DEFAULT_USER_INTERFACE_STYLE = "userInterfaceStyle" // deprecated, only used for migration
let DEFAULT_PROFILE_IMAGE_CORNER_RADIUS = "profileImageCornerRadius"
let DEFAULT_CHAT_ITEM_ROUNDNESS = "chatItemRoundness"
let DEFAULT_CHAT_ITEM_TAIL = "chatItemTail"
let DEFAULT_ONE_HAND_UI_CARD_SHOWN = "oneHandUICardShown"
let DEFAULT_TOOLBAR_MATERIAL = "toolbarMaterial"
let DEFAULT_CONNECT_VIA_LINK_TAB = "connectViaLinkTab"
let DEFAULT_LIVE_MESSAGE_ALERT_SHOWN = "liveMessageAlertShown"
let DEFAULT_SHOW_HIDDEN_PROFILES_NOTICE = "showHiddenProfilesNotice"
let DEFAULT_SHOW_MUTE_PROFILE_ALERT = "showMuteProfileAlert"
let DEFAULT_WHATS_NEW_VERSION = "defaultWhatsNewVersion"
let DEFAULT_ONBOARDING_STAGE = "onboardingStage"
let DEFAULT_MIGRATION_TO_STAGE = "migrationToStage"
let DEFAULT_MIGRATION_FROM_STAGE = "migrationFromStage"
let DEFAULT_CUSTOM_DISAPPEARING_MESSAGE_TIME = "customDisappearingMessageTime"
let DEFAULT_SHOW_UNREAD_AND_FAVORITES = "showUnreadAndFavorites"
let DEFAULT_DEVICE_NAME_FOR_REMOTE_ACCESS = "deviceNameForRemoteAccess"
let DEFAULT_CONFIRM_REMOTE_SESSIONS = "confirmRemoteSessions"
let DEFAULT_CONNECT_REMOTE_VIA_MULTICAST = "connectRemoteViaMulticast"
let DEFAULT_CONNECT_REMOTE_VIA_MULTICAST_AUTO = "connectRemoteViaMulticastAuto"
let DEFAULT_SHOW_DELETE_CONVERSATION_NOTICE = "showDeleteConversationNotice"
let DEFAULT_SHOW_DELETE_CONTACT_NOTICE = "showDeleteContactNotice"
let DEFAULT_SHOW_SENT_VIA_RPOXY = "showSentViaProxy"
let DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE = "showSubscriptionPercentage"

let DEFAULT_CURRENT_THEME = "currentTheme"
let DEFAULT_SYSTEM_DARK_THEME = "systemDarkTheme"
let DEFAULT_CURRENT_THEME_IDS = "currentThemeIds"
let DEFAULT_THEME_OVERRIDES = "themeOverrides"

let DEFAULT_NETWORK_PROXY = "networkProxy"

let ANDROID_DEFAULT_CALL_ON_LOCK_SCREEN = "androidCallOnLockScreen"

let defaultChatItemRoundness: Double = 0.75

let appDefaults: [String: Any] = [
    DEFAULT_SHOW_LA_NOTICE: false,
    DEFAULT_LA_NOTICE_SHOWN: false,
    DEFAULT_PERFORM_LA: false,
    DEFAULT_LA_MODE: LAMode.system.rawValue,
    DEFAULT_LA_LOCK_DELAY: 30,
    DEFAULT_LA_SELF_DESTRUCT: false,
    DEFAULT_NOTIFICATION_ALERT_SHOWN: false,
    DEFAULT_WEBRTC_POLICY_RELAY: true,
    DEFAULT_CALL_KIT_CALLS_IN_RECENTS: false,
    DEFAULT_PRIVACY_ACCEPT_IMAGES: true,
    DEFAULT_PRIVACY_LINK_PREVIEWS: true,
    DEFAULT_PRIVACY_SIMPLEX_LINK_MODE: SimpleXLinkMode.description.rawValue,
    DEFAULT_PRIVACY_SHOW_CHAT_PREVIEWS: true,
    DEFAULT_PRIVACY_SAVE_LAST_DRAFT: true,
    DEFAULT_PRIVACY_PROTECT_SCREEN: false,
    DEFAULT_PRIVACY_DELIVERY_RECEIPTS_SET: false,
    DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS: 0,
    DEFAULT_EXPERIMENTAL_CALLS: false,
    DEFAULT_CHAT_V3_DB_MIGRATION: V3DBMigrationState.offer.rawValue,
    DEFAULT_DEVELOPER_TOOLS: false,
    DEFAULT_ENCRYPTION_STARTED: false,
    DEFAULT_PROFILE_IMAGE_CORNER_RADIUS: defaultProfileImageCorner,
    DEFAULT_CHAT_ITEM_ROUNDNESS: defaultChatItemRoundness,
    DEFAULT_CHAT_ITEM_TAIL: true,
    DEFAULT_ONE_HAND_UI_CARD_SHOWN: false,
    DEFAULT_TOOLBAR_MATERIAL: ToolbarMaterial.defaultMaterial,
    DEFAULT_CONNECT_VIA_LINK_TAB: ConnectViaLinkTab.scan.rawValue,
    DEFAULT_LIVE_MESSAGE_ALERT_SHOWN: false,
    DEFAULT_SHOW_HIDDEN_PROFILES_NOTICE: true,
    DEFAULT_SHOW_MUTE_PROFILE_ALERT: true,
    DEFAULT_ONBOARDING_STAGE: OnboardingStage.onboardingComplete.rawValue,
    DEFAULT_CUSTOM_DISAPPEARING_MESSAGE_TIME: 300,
    DEFAULT_SHOW_UNREAD_AND_FAVORITES: false,
    DEFAULT_CONFIRM_REMOTE_SESSIONS: false,
    DEFAULT_CONNECT_REMOTE_VIA_MULTICAST: true,
    DEFAULT_CONNECT_REMOTE_VIA_MULTICAST_AUTO: true,
    DEFAULT_SHOW_DELETE_CONVERSATION_NOTICE: true,
    DEFAULT_SHOW_DELETE_CONTACT_NOTICE: true,
    DEFAULT_SHOW_SENT_VIA_RPOXY: false,
    DEFAULT_SHOW_SUBSCRIPTION_PERCENTAGE: false,
    ANDROID_DEFAULT_CALL_ON_LOCK_SCREEN: AppSettingsLockScreenCalls.show.rawValue,

    DEFAULT_THEME_OVERRIDES: "{}",
    DEFAULT_CURRENT_THEME: DefaultTheme.SYSTEM_THEME_NAME,
    DEFAULT_SYSTEM_DARK_THEME: DefaultTheme.DARK.themeName,
    DEFAULT_CURRENT_THEME_IDS: "{}"
]

// only Bool defaults can be used here,
// or hintDefaultsUnchanged and resetHintDefaults need to be changed
let hintDefaults = [
    DEFAULT_LA_NOTICE_SHOWN,
    DEFAULT_ONE_HAND_UI_CARD_SHOWN,
    DEFAULT_LIVE_MESSAGE_ALERT_SHOWN,
    DEFAULT_SHOW_HIDDEN_PROFILES_NOTICE,
    DEFAULT_SHOW_MUTE_PROFILE_ALERT,
    DEFAULT_SHOW_DELETE_CONVERSATION_NOTICE,
    DEFAULT_SHOW_DELETE_CONTACT_NOTICE
]

// not used anymore
enum ConnectViaLinkTab: String {
    case scan
    case paste
}

enum SimpleXLinkMode: String, Identifiable {
    case description
    case full
    case browser

    static var values: [SimpleXLinkMode] = [.description, .full]

    public var id: Self { self }

    var text: LocalizedStringKey {
        switch self {
        case .description: return "Description"
        case .full: return "Full link"
        case .browser: return "Via browser"
        }
    }
}

private var indent: CGFloat = 36

let chatArchiveTimeDefault = DateDefault(defaults: UserDefaults.standard, forKey: DEFAULT_CHAT_ARCHIVE_TIME)

let encryptionStartedDefault = BoolDefault(defaults: UserDefaults.standard, forKey: DEFAULT_ENCRYPTION_STARTED)

let encryptionStartedAtDefault = DateDefault(defaults: UserDefaults.standard, forKey: DEFAULT_ENCRYPTION_STARTED_AT)

let connectViaLinkTabDefault = EnumDefault<ConnectViaLinkTab>(defaults: UserDefaults.standard, forKey: DEFAULT_CONNECT_VIA_LINK_TAB, withDefault: .scan)

let privacySimplexLinkModeDefault = EnumDefault<SimpleXLinkMode>(defaults: UserDefaults.standard, forKey: DEFAULT_PRIVACY_SIMPLEX_LINK_MODE, withDefault: .description)

let privacyLocalAuthModeDefault = EnumDefault<LAMode>(defaults: UserDefaults.standard, forKey: DEFAULT_LA_MODE, withDefault: .system)

let privacyDeliveryReceiptsSet = BoolDefault(defaults: UserDefaults.standard, forKey: DEFAULT_PRIVACY_DELIVERY_RECEIPTS_SET)

let onboardingStageDefault = EnumDefault<OnboardingStage>(defaults: UserDefaults.standard, forKey: DEFAULT_ONBOARDING_STAGE, withDefault: .onboardingComplete)

let customDisappearingMessageTimeDefault = IntDefault(defaults: UserDefaults.standard, forKey: DEFAULT_CUSTOM_DISAPPEARING_MESSAGE_TIME)

let showDeleteConversationNoticeDefault = BoolDefault(defaults: UserDefaults.standard, forKey: DEFAULT_SHOW_DELETE_CONVERSATION_NOTICE)
let showDeleteContactNoticeDefault = BoolDefault(defaults: UserDefaults.standard, forKey: DEFAULT_SHOW_DELETE_CONTACT_NOTICE)

let currentThemeDefault = StringDefault(defaults: UserDefaults.standard, forKey: DEFAULT_CURRENT_THEME, withDefault: DefaultTheme.SYSTEM_THEME_NAME)
let systemDarkThemeDefault = StringDefault(defaults: UserDefaults.standard, forKey: DEFAULT_SYSTEM_DARK_THEME, withDefault: DefaultTheme.DARK.themeName)
let currentThemeIdsDefault = CodableDefault<[String: String]>(defaults: UserDefaults.standard, forKey: DEFAULT_CURRENT_THEME_IDS, withDefault: [:] )
let themeOverridesDefault: CodableDefault<[ThemeOverrides]> = CodableDefault(defaults: UserDefaults.standard, forKey: DEFAULT_THEME_OVERRIDES, withDefault: [])

func setGroupDefaults() {
    privacyAcceptImagesGroupDefault.set(UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_ACCEPT_IMAGES))
    appLocalAuthEnabledGroupDefault.set(UserDefaults.standard.bool(forKey: DEFAULT_PERFORM_LA))
    privacyLinkPreviewsGroupDefault.set(UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_LINK_PREVIEWS))
    profileImageCornerRadiusGroupDefault.set(UserDefaults.standard.double(forKey: DEFAULT_PROFILE_IMAGE_CORNER_RADIUS))
}

public class StringDefault {
    var defaults: UserDefaults
    var key: String
    var defaultValue: String

    public init(defaults: UserDefaults = UserDefaults.standard, forKey: String, withDefault: String) {
        self.defaults = defaults
        self.key = forKey
        self.defaultValue = withDefault
    }

    public func get() -> String {
        defaults.string(forKey: key) ?? defaultValue
    }

    public func set(_ value: String) {
        defaults.set(value, forKey: key)
        defaults.synchronize()
    }
}

public class CodableDefault<T: Codable> {
    var defaults: UserDefaults
    var key: String
    var defaultValue: T

    public init(defaults: UserDefaults = UserDefaults.standard, forKey: String, withDefault: T) {
        self.defaults = defaults
        self.key = forKey
        self.defaultValue = withDefault
    }

    var cache: T? = nil

    public func get() -> T {
        if let cache {
            return cache
        } else if let value = defaults.string(forKey: key) {
            let res = decodeJSON(value) ?? defaultValue
            cache = res
            return res
        }
        return defaultValue
    }

    public func set(_ value: T) {
        defaults.set(encodeJSON(value), forKey: key)
        cache = value
        //defaults.synchronize()
    }
}

let networkProxyDefault: CodableDefault<NetworkProxy> = CodableDefault(defaults: UserDefaults.standard, forKey: DEFAULT_NETWORK_PROXY, withDefault: NetworkProxy.def)

struct SettingsView: View {
    @Environment(\.colorScheme) var colorScheme
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var sceneDelegate: SceneDelegate
    @EnvironmentObject var theme: AppTheme
    @State private var showProgress: Bool = false

    var body: some View {
        ZStack {
            settingsView()
            if showProgress {
                progressView()
            }
        }
    }

    @ViewBuilder func settingsView() -> some View {
        let user = chatModel.currentUser
            List {
                Section(header: Text("Settings").foregroundColor(theme.colors.secondary)) {
                    NavigationLink {
                        NotificationsView()
                            .navigationTitle("Notifications")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        HStack {
                            notificationsIcon()
                            Text("Notifications")
                        }
                    }
                    .disabled(chatModel.chatRunning != true)
                    
                    NavigationLink {
                        NetworkAndServers()
                            .navigationTitle("Network & servers")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        settingsRow("externaldrive.connected.to.line.below", color: theme.colors.secondary) { Text("Network & servers") }
                    }
                    .disabled(chatModel.chatRunning != true)
                    
                    NavigationLink {
                        CallSettings()
                            .navigationTitle("Your calls")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        settingsRow("video", color: theme.colors.secondary) { Text("Audio & video calls") }
                    }
                    .disabled(chatModel.chatRunning != true)
                    
                    NavigationLink {
                        PrivacySettings()
                            .navigationTitle("Your privacy")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        settingsRow("lock", color: theme.colors.secondary) { Text("Privacy & security") }
                    }
                    .disabled(chatModel.chatRunning != true)
                    
                    if UIApplication.shared.supportsAlternateIcons {
                        NavigationLink {
                            AppearanceSettings()
                                .navigationTitle("Appearance")
                                .modifier(ThemedBackground(grouped: true))
                        } label: {
                            settingsRow("sun.max", color: theme.colors.secondary) { Text("Appearance") }
                        }
                        .disabled(chatModel.chatRunning != true)
                    }
                }

                Section(header: Text("Chat database").foregroundColor(theme.colors.secondary)) {
                    chatDatabaseRow()
                    NavigationLink {
                        MigrateFromDevice(showProgressOnSettings: $showProgress)
                            .toolbar {
                                // Redaction broken for `.navigationTitle` - using a toolbar item instead.
                                ToolbarItem(placement: .principal) {
                                    Text("Migrate device").font(.headline)
                                }
                            }
                            .modifier(ThemedBackground(grouped: true))
                            .navigationBarTitleDisplayMode(.large)
                    } label: {
                        settingsRow("tray.and.arrow.up", color: theme.colors.secondary) { Text("Migrate to another device") }
                    }
                }
                
                Section(header: Text("Help").foregroundColor(theme.colors.secondary)) {
                    if let user = user {
                        NavigationLink {
                            ChatHelp(dismissSettingsSheet: dismiss)
                                .navigationTitle("Welcome \(user.displayName)!")
                                .modifier(ThemedBackground())
                                .frame(maxHeight: .infinity, alignment: .top)
                        } label: {
                            settingsRow("questionmark", color: theme.colors.secondary) { Text("How to use it") }
                        }
                    }
                    NavigationLink {
                        WhatsNewView(viaSettings: true)
                            .modifier(ThemedBackground())
                            .navigationBarTitleDisplayMode(.inline)
                    } label: {
                        settingsRow("plus", color: theme.colors.secondary) { Text("What's new") }
                    }
                    NavigationLink {
                        SimpleXInfo(onboarding: false)
                            .navigationBarTitle("", displayMode: .inline)
                            .modifier(ThemedBackground())
                            .frame(maxHeight: .infinity, alignment: .top)
                    } label: {
                        settingsRow("info", color: theme.colors.secondary) { Text("About SimpleX Chat") }
                    }
                    settingsRow("number", color: theme.colors.secondary) {
                        Button("Send questions and ideas") {
                            dismiss()
                            DispatchQueue.main.async {
                                UIApplication.shared.open(simplexTeamURL)
                            }
                        }
                    }
                    .disabled(chatModel.chatRunning != true)
                    settingsRow("envelope", color: theme.colors.secondary) { Text("[Send us email](mailto:chat@simplex.chat)") }
                }

                Section(header: Text("Support SimpleX Chat").foregroundColor(theme.colors.secondary)) {
                    settingsRow("keyboard", color: theme.colors.secondary) { Text("[Contribute](https://github.com/simplex-chat/simplex-chat#contribute)") }
                    settingsRow("star", color: theme.colors.secondary) {
                        Button("Rate the app") {
                            if let scene = sceneDelegate.windowScene {
                                SKStoreReviewController.requestReview(in: scene)
                            }
                        }
                    }
                    ZStack(alignment: .leading) {
                        Image(colorScheme == .dark ? "github_light" : "github")
                            .resizable()
                            .frame(width: 24, height: 24)
                            .opacity(0.5)
                            .colorMultiply(theme.colors.secondary)
                        Text("[Star on GitHub](https://github.com/simplex-chat/simplex-chat)")
                            .padding(.leading, indent)
                    }
                }

                Section(header: Text("Develop").foregroundColor(theme.colors.secondary)) {
                    NavigationLink {
                        DeveloperView()
                            .navigationTitle("Developer tools")
                            .modifier(ThemedBackground(grouped: true))
                    } label: {
                        settingsRow("chevron.left.forwardslash.chevron.right", color: theme.colors.secondary) { Text("Developer tools") }
                    }
                    NavigationLink {
                        VersionView()
                            .navigationBarTitle("App version")
                            .modifier(ThemedBackground())
                    } label: {
                        Text("v\(appVersion ?? "?") (\(appBuild ?? "?"))")
                    }
                }
            }
            .navigationTitle("Your settings")
            .modifier(ThemedBackground(grouped: true))
            .onDisappear {
                chatModel.showingTerminal = false
                chatModel.terminalItems = []
            }
    }
    
    private func chatDatabaseRow() -> some View {
        NavigationLink {
            DatabaseView(dismissSettingsSheet: dismiss, chatItemTTL: chatModel.chatItemTTL)
                .navigationTitle("Your chat database")
                .modifier(ThemedBackground(grouped: true))
        } label: {
            let color: Color = chatModel.chatDbEncrypted == false ? .orange : theme.colors.secondary
            settingsRow("internaldrive", color: color) {
                HStack {
                    Text("Database passphrase & export")
                    Spacer()
                    if chatModel.chatRunning == false {
                        Image(systemName: "exclamationmark.octagon.fill").foregroundColor(.red)
                    }
                }
            }
        }
    }

    private func progressView() -> some View {
        VStack {
            ProgressView().scaleEffect(2)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity )
    }

    private enum NotificationAlert {
        case enable
        case error(LocalizedStringKey, String)
    }

    private func notificationsIcon() -> some View {
        let icon: String
        let color: Color
        switch (chatModel.tokenStatus) {
        case .new:
            icon = "bolt"
            color = theme.colors.secondary
        case .registered:
            icon = "bolt.fill"
            color = theme.colors.secondary
        case .invalid:
            icon = "bolt.slash"
            color = theme.colors.secondary
        case .confirmed:
            icon = "bolt.fill"
            color = .yellow
        case .active:
            icon = "bolt.fill"
            color = .green
        case .expired:
            icon = "bolt.slash.fill"
            color = theme.colors.secondary
        case .none:
            icon = "bolt"
            color = theme.colors.secondary
        }
        return Image(systemName: icon)
            .padding(.trailing, 9)
            .foregroundColor(color)
    }
}

func settingsRow<Content : View>(_ icon: String, color: Color/* = .secondary*/, content: @escaping () -> Content) -> some View {
    ZStack(alignment: .leading) {
        Image(systemName: icon).frame(maxWidth: 24, maxHeight: 24, alignment: .center)
            .symbolRenderingMode(.monochrome)
            .foregroundColor(color)
        content().padding(.leading, indent)
    }
}

struct ProfilePreview: View {
    var profileOf: NamedChat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)

    var body: some View {
        HStack {
            ProfileImage(imageStr: profileOf.image, size: 44, color: color)
                .padding(.trailing, 6)
            profileName(profileOf).lineLimit(1)
        }
    }
}

func profileName(_ profileOf: NamedChat) -> Text {
    var t = Text(profileOf.displayName).fontWeight(.semibold).font(.title2)
    if profileOf.fullName != "" && profileOf.fullName != profileOf.displayName {
        t = t + Text(" (" + profileOf.fullName + ")")
//                        .font(.callout)
        }
    return t
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        return SettingsView()
            .environmentObject(chatModel)
    }
}
