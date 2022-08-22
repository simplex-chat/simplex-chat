//
//  SettingsView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let simplexTeamURL = URL(string: "simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D")!

let appVersion = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String

let appBuild = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion")  as? String

let DEFAULT_SHOW_LA_NOTICE = "showLocalAuthenticationNotice"
let DEFAULT_LA_NOTICE_SHOWN = "localAuthenticationNoticeShown"
let DEFAULT_PERFORM_LA = "performLocalAuthentication"
let DEFAULT_WEBRTC_POLICY_RELAY = "webrtcPolicyRelay"
let DEFAULT_PRIVACY_ACCEPT_IMAGES = "privacyAcceptImages"
let DEFAULT_PRIVACY_LINK_PREVIEWS = "privacyLinkPreviews"
let DEFAULT_EXPERIMENTAL_CALLS = "experimentalCalls"
let DEFAULT_CHAT_ARCHIVE_NAME = "chatArchiveName"
let DEFAULT_CHAT_ARCHIVE_TIME = "chatArchiveTime"
let DEFAULT_CHAT_V3_DB_MIGRATION = "chatV3DBMigration"
let DEFAULT_DEVELOPER_TOOLS = "developerTools"
let DEFAULT_ACCENT_COLOR_RED = "accentColorRed"
let DEFAULT_ACCENT_COLOR_GREEN = "accentColorGreen"
let DEFAULT_ACCENT_COLOR_BLUE = "accentColorBlue"

let appDefaults: [String: Any] = [
    DEFAULT_SHOW_LA_NOTICE: false,
    DEFAULT_LA_NOTICE_SHOWN: false,
    DEFAULT_PERFORM_LA: false,
    DEFAULT_WEBRTC_POLICY_RELAY: true,
    DEFAULT_PRIVACY_ACCEPT_IMAGES: true,
    DEFAULT_PRIVACY_LINK_PREVIEWS: true,
    DEFAULT_EXPERIMENTAL_CALLS: false,
    DEFAULT_CHAT_V3_DB_MIGRATION: "offer",
    DEFAULT_DEVELOPER_TOOLS: false,
    DEFAULT_ACCENT_COLOR_RED: 0.000,
    DEFAULT_ACCENT_COLOR_GREEN: 0.533,
    DEFAULT_ACCENT_COLOR_BLUE: 1.000
]

private var indent: CGFloat = 36

let chatArchiveTimeDefault = DateDefault(defaults: UserDefaults.standard, forKey: DEFAULT_CHAT_ARCHIVE_TIME)

func setGroupDefaults() {
    privacyAcceptImagesGroupDefault.set(UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_ACCEPT_IMAGES))
}

struct SettingsView: View {
    @Environment(\.colorScheme) var colorScheme
    @EnvironmentObject var chatModel: ChatModel
    @Binding var showSettings: Bool
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var settingsSheet: SettingsSheet?

    var body: some View {
        let user: User = chatModel.currentUser!

        NavigationView {
            List {
                Section("You") {
                    NavigationLink {
                        UserProfile()
                            .navigationTitle("Your chat profile")
                    } label: {
                        ProfilePreview(profileOf: user)
                        .padding(.leading, -8)
                    }
                    .disabled(chatModel.chatRunning != true)

                    incognitoRow()

                    NavigationLink {
                        UserAddress()
                            .navigationTitle("Your chat address")
                    } label: {
                        settingsRow("qrcode") { Text("Your SimpleX contact address") }
                    }
                    .disabled(chatModel.chatRunning != true)

                    NavigationLink {
                        DatabaseView(showSettings: $showSettings)
                            .navigationTitle("Your chat database")
                    } label: {
                        settingsRow("internaldrive") {
                            HStack {
                                Text("Database export & import")
                                Spacer()
                                if chatModel.chatRunning == false {
                                    Image(systemName: "exclamationmark.octagon.fill").foregroundColor(.red)
                                }
                            }
                        }
                    }
                }
                
                Section("Settings") {
                    NavigationLink {
                        NotificationsView()
                            .navigationTitle("Notifications")
                    } label: {
                        HStack {
                            notificationsIcon()
                            Text("Notifications")
                        }
                    }
                    NavigationLink {
                        CallSettings()
                            .navigationTitle("Your calls")
                    } label: {
                        settingsRow("video") { Text("Audio & video calls") }
                    }
                    NavigationLink {
                        PrivacySettings()
                            .navigationTitle("Your privacy")
                    } label: {
                        settingsRow("lock") { Text("Privacy & security") }
                    }
                    if UIApplication.shared.supportsAlternateIcons {
                        NavigationLink {
                            AppearanceSettings()
                                .navigationTitle("Appearance")
                        } label: {
                            settingsRow("sun.max") { Text("Appearance") }
                        }
                    }
                    NavigationLink {
                        NetworkAndServers()
                            .navigationTitle("Network & servers")
                    } label: {
                        settingsRow("externaldrive.connected.to.line.below") { Text("Network & servers") }
                    }
                }
                .disabled(chatModel.chatRunning != true)

                Section("Help") {
                    NavigationLink {
                        ChatHelp(showSettings: $showSettings)
                            .navigationTitle("Welcome \(user.displayName)!")
                            .frame(maxHeight: .infinity, alignment: .top)
                    } label: {
                        settingsRow("questionmark") { Text("How to use it") }
                    }
                    NavigationLink {
                        SimpleXInfo(onboarding: false)
                            .navigationBarTitle("", displayMode: .inline)
                            .frame(maxHeight: .infinity, alignment: .top)
                    } label: {
                        settingsRow("info") { Text("About SimpleX Chat") }
                    }
                    NavigationLink {
                        MarkdownHelp()
                            .navigationTitle("How to use markdown")
                            .frame(maxHeight: .infinity, alignment: .top)
                    } label: {
                        settingsRow("textformat") { Text("Markdown in messages") }
                    }
                    settingsRow("number") {
                        Button {
                            showSettings = false
                            DispatchQueue.main.async {
                                UIApplication.shared.open(simplexTeamURL)
                            }
                        } label: {
                            Text("Chat with the developers")
                        }
                    }
                    .disabled(chatModel.chatRunning != true)
                    settingsRow("envelope") { Text("[Send us email](mailto:chat@simplex.chat)") }
                }

                Section("Develop") {
                    NavigationLink {
                        TerminalView()
                    } label: {
                        settingsRow("terminal") { Text("Chat console") }
                    }
                    .disabled(chatModel.chatRunning != true)
                    settingsRow("gear") {
                        Toggle("Developer tools", isOn: $developerTools)
                    }
                    ZStack(alignment: .leading) {
                        Image(colorScheme == .dark ? "github_light" : "github")
                            .resizable()
                            .frame(width: 24, height: 24)
                            .opacity(0.5)
                        Text("Install [SimpleX Chat for terminal](https://github.com/simplex-chat/simplex-chat)")
                            .padding(.leading, indent)
                    }
//                    NavigationLink {
//                        ExperimentalFeaturesView()
//                            .navigationTitle("Experimental features")
//                    } label: {
//                        settingsRow("gauge") { Text("Experimental features") }
//                    }
                    Text("v\(appVersion ?? "?") (\(appBuild ?? "?"))")
                }
            }
            .navigationTitle("Your settings")
        }
        .sheet(item: $settingsSheet) { sheet in
            switch sheet {
            case .incognitoInfo: IncognitoHelp()
            }
        }
    }

    @ViewBuilder private func incognitoRow() -> some View {
        ZStack(alignment: .leading) {
            Image(systemName: chatModel.incognito ? "theatermasks.fill" : "theatermasks")
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .foregroundColor(chatModel.incognito ? Color.indigo : .secondary)
            Toggle(isOn: $chatModel.incognito) {
                HStack {
                    Text("Incognito")
                    Spacer().frame(width: 4)
                    Image(systemName: "info.circle")
                        .foregroundColor(.accentColor)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    settingsSheet = .incognitoInfo
                }
            }
            .onChange(of: chatModel.incognito) { incognito in
                incognitoGroupDefault.set(incognito)
                do {
                    try apiSetIncognito(incognito: incognito)
                } catch {
                    logger.error("apiSetIncognito: cannot set incognito \(responseError(error))")
                }
            }
            .padding(.leading, indent)
        }
    }

    private enum SettingsSheet: Identifiable {
        case incognitoInfo

        var id: SettingsSheet { get { self } }
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
            color = .secondary
        case .registered:
            icon = "bolt.fill"
            color = .secondary
        case .invalid:
            icon = "bolt.slash"
            color = .secondary
        case .confirmed:
            icon = "bolt.fill"
            color = .yellow
        case .active:
            icon = "bolt.fill"
            color = .green
        case .expired:
            icon = "bolt.slash.fill"
            color = .secondary
        case .none:
            icon = "bolt"
            color = .secondary
        }
        return Image(systemName: icon)
            .padding(.trailing, 9)
            .foregroundColor(color)
    }
}

func settingsRow<Content : View>(_ icon: String, color: Color = .secondary, content: @escaping () -> Content) -> some View {
    ZStack(alignment: .leading) {
        Image(systemName: icon).frame(maxWidth: 24, maxHeight: 24, alignment: .center).foregroundColor(color)
        content().padding(.leading, indent)
    }
}

struct ProfilePreview: View {
    var profileOf: NamedChat
    var color = Color(uiColor: .tertiarySystemGroupedBackground)

    var body: some View {
        HStack {
            ProfileImage(imageStr: profileOf.image, color: color)
                .frame(width: 44, height: 44)
                .padding(.trailing, 6)
                .padding(.vertical, 6)
            VStack(alignment: .leading) {
                Text(profileOf.displayName)
                    .fontWeight(.bold)
                    .font(.title2)
                Text(profileOf.fullName)
            }
        }
    }
}

struct SettingsView_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        @State var showSettings = false

        return SettingsView(showSettings: $showSettings)
            .environmentObject(chatModel)
    }
}
