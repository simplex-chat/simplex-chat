//
//  WhatsNewView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private struct VersionDescription {
    var version: String
    var post: URL?
    var features: [Feature]
}

private enum Feature: Identifiable {
    case feature(Description)
    case view(FeatureView)
    
    var id: LocalizedStringKey {
        switch self {
        case let .feature(d): d.title
        case let .view(v): v.title
        }
    }
}

private struct Description {
    let icon: String?
    let title: LocalizedStringKey
    let description: LocalizedStringKey?
    var subfeatures: [(icon: String, description: LocalizedStringKey)] = []
}

private struct FeatureView {
    let icon: String?
    let title: LocalizedStringKey
    let view: () -> any View
}

private let versionDescriptions: [VersionDescription] = [
    VersionDescription(
        version: "v4.2",
        post: URL(string: "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"),
        features: [
            .feature(Description(
                icon: "checkmark.shield",
                title: "Security assessment",
                description: "SimpleX Chat security was audited by Trail of Bits."
            )),
            .feature(Description(
                icon: "person.2",
                title: "Group links",
                description: "Admins can create the links to join groups."
            )),
            .feature(Description(
                icon: "checkmark",
                title: "Auto-accept contact requests",
                description: "With optional welcome message."
            )),
        ]
    ),
    VersionDescription(
        version: "v4.3",
        post: URL(string: "https://simplex.chat/blog/20221206-simplex-chat-v4.3-voice-messages.html"),
        features: [
            .feature(Description(
                icon: "mic",
                title: "Voice messages",
                description: "Max 30 seconds, received instantly."
            )),
            .feature(Description(
                icon: "trash.slash",
                title: "Irreversible message deletion",
                description: "Your contacts can allow full message deletion."
            )),
            .feature(Description(
                icon: "externaldrive.connected.to.line.below",
                title: "Improved server configuration",
                description: "Add servers by scanning QR codes."
            )),
            .feature(Description(
                icon: "eye.slash",
                title: "Improved privacy and security",
                description: "Hide app screen in the recent apps."
            )),
        ]
    ),
    VersionDescription(
        version: "v4.4",
        post: URL(string: "https://simplex.chat/blog/20230103-simplex-chat-v4.4-disappearing-messages.html"),
        features: [
            .feature(Description(
                icon: "stopwatch",
                title: "Disappearing messages",
                description: "Sent messages will be deleted after set time."
            )),
            .feature(Description(
                icon: "ellipsis.circle",
                title: "Live messages",
                description: "Recipients see updates as you type them."
            )),
            .feature(Description(
                icon: "checkmark.shield",
                title: "Verify connection security",
                description: "Compare security codes with your contacts."
            )),
            .feature(Description(
                icon: "camera",
                title: "GIFs and stickers",
                description: "Send them from gallery or custom keyboards."
            )),
            .feature(Description(
                icon: "character",
                title: "French interface",
                description: "Thanks to the users – contribute via Weblate!"
            )),
        ]
    ),
    VersionDescription(
        version: "v4.5",
        post: URL(string: "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html"),
        features: [
            .feature(Description(
                icon: "person.crop.rectangle.stack",
                title: "Multiple chat profiles",
                description: "Different names, avatars and transport isolation."
            )),
            .feature(Description(
                icon: "rectangle.and.pencil.and.ellipsis",
                title: "Message draft",
                description: "Preserve the last message draft, with attachments."
            )),
            .feature(Description(
                icon: "network.badge.shield.half.filled",
                title: "Transport isolation",
                description: "By chat profile (default) or [by connection](https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation) (BETA)."
            )),
            .feature(Description(
                icon: "lock.doc",
                title: "Private filenames",
                description: "To protect timezone, image/voice files use UTC."
            )),
            .feature(Description(
                icon: "battery.25",
                title: "Reduced battery usage",
                description: "More improvements are coming soon!"
            )),
            .feature(Description(
                icon: "character",
                title: "Italian interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v4.6",
        post: URL(string: "https://simplex.chat/blog/20230328-simplex-chat-v4-6-hidden-profiles.html"),
        features: [
            .feature(Description(
                icon: "lock",
                title: "Hidden chat profiles",
                description: "Protect your chat profiles with a password!"
            )),
            .feature(Description(
                icon: "phone.arrow.up.right",
                title: "Audio and video calls",
                description: "Fully re-implemented - work in background!"
            )),
            .feature(Description(
                icon: "flag",
                title: "Group moderation",
                description: "Now admins can:\n- delete members' messages.\n- disable members (\"observer\" role)"
            )),
            .feature(Description(
                icon: "plus.message",
                title: "Group welcome message",
                description: "Set the message shown to new members!"
            )),
            .feature(Description(
                icon: "battery.50",
                title: "Further reduced battery usage",
                description: "More improvements are coming soon!"
            )),
            .feature(Description(
                icon: "character",
                title: "Chinese and Spanish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.0",
        post: URL(string: "https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html"),
        features: [
            .feature(Description(
                icon: "arrow.up.doc",
                title: "Videos and files up to 1gb",
                description: "Fast and no wait until the sender is online!"
            )),
            .feature(Description(
                icon: "lock",
                title: "App passcode",
                description: "Set it instead of system authentication."
            )),
            .feature(Description(
                icon: "character",
                title: "Polish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    // Also
    // preference to disable calls per contact
    // access welcome message via a group profile
    VersionDescription(
        version: "v5.1",
        post: URL(string: "https://simplex.chat/blog/20230523-simplex-chat-v5-1-message-reactions-self-destruct-passcode.html"),
        features: [
            .feature(Description(
                icon: "face.smiling",
                title: "Message reactions",
                description: "Finally, we have them! 🚀"
            )),
            .feature(Description(
                icon: "arrow.up.message",
                title: "Better messages",
                description: "- voice messages up to 5 minutes.\n- custom time to disappear.\n- editing history."
            )),
            .feature(Description(
                icon: "lock",
                title: "Self-destruct passcode",
                description: "All data is erased when it is entered."
            )),
            .feature(Description(
                icon: "character",
                title: "Japanese interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.2",
        post: URL(string: "https://simplex.chat/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html"),
        features: [
            .feature(Description(
                icon: "checkmark",
                title: "Message delivery receipts!",
                description: "The second tick we missed! ✅"
            )),
            .feature(Description(
                icon: "star",
                title: "Find chats faster",
                description: "Filter unread and favorite chats."
            )),
            .feature(Description(
                icon: "exclamationmark.arrow.triangle.2.circlepath",
                title: "Keep your connections",
                description: "Fix encryption after restoring backups."
            )),
            .feature(Description(
                icon: "stopwatch",
                title: "Make one message disappear",
                description: "Even when disabled in the conversation."
            )),
            .feature(Description(
                icon: "gift",
                title: "A few more things",
                description: "- more stable message delivery.\n- a bit better groups.\n- and more!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.3",
        post: URL(string: "https://simplex.chat/blog/20230925-simplex-chat-v5-3-desktop-app-local-file-encryption-directory-service.html"),
        features: [
            .feature(Description(
                icon: "desktopcomputer",
                title: "New desktop app!",
                description: "Create new profile in [desktop app](https://simplex.chat/downloads/). 💻"
            )),
            .feature(Description(
                icon: "lock",
                title: "Encrypt stored files & media",
                description: "App encrypts new local files (except videos)."
            )),
            .feature(Description(
                icon: "magnifyingglass",
                title: "Discover and join groups",
                description: "- connect to [directory service](simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion) (BETA)!\n- delivery receipts (up to 20 members).\n- faster and more stable."
            )),
            .feature(Description(
                icon: "theatermasks",
                title: "Simplified incognito mode",
                description: "Toggle incognito when connecting."
            )),
            .feature(Description(
                icon: "character",
                title: "\(4) new interface languages",
                description: "Bulgarian, Finnish, Thai and Ukrainian - thanks to the users and [Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.4",
        post: URL(string: "https://simplex.chat/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.html"),
        features: [
            .feature(Description(
                icon: "desktopcomputer",
                title: "Link mobile and desktop apps! 🔗",
                description: "Via secure quantum resistant protocol."
            )),
            .feature(Description(
                icon: "person.2",
                title: "Better groups",
                description: "Faster joining and more reliable messages."
            )),
            .feature(Description(
                icon: "theatermasks",
                title: "Incognito groups",
                description: "Create a group using a random profile."
            )),
            .feature(Description(
                icon: "hand.raised",
                title: "Block group members",
                description: "To hide unwanted messages."
            )),
            .feature(Description(
                icon: "gift",
                title: "A few more things",
                description: "- optionally notify deleted contacts.\n- profile names with spaces.\n- and more!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.5",
        post: URL(string: "https://simplex.chat/blog/20240124-simplex-chat-infrastructure-costs-v5-5-simplex-ux-private-notes-group-history.html"),
        features: [
            .feature(Description(
                icon: "folder",
                title: "Private notes",
                description: "With encrypted files and media."
            )),
            .feature(Description(
                icon: "link",
                title: "Paste link to connect!",
                description: "Search bar accepts invitation links."
            )),
            .feature(Description(
                icon: "bubble.left.and.bubble.right",
                title: "Join group conversations",
                description: "Recent history and improved [directory bot](simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion)."
            )),
            .feature(Description(
                icon: "battery.50",
                title: "Improved message delivery",
                description: "With reduced battery usage."
            )),
            .feature(Description(
                icon: "character",
                title: "Turkish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.6",
        post: URL(string: "https://simplex.chat/blog/20240323-simplex-network-privacy-non-profit-v5-6-quantum-resistant-e2e-encryption-simple-migration.html"),
        features: [
            .feature(Description(
                icon: "key",
                title: "Quantum resistant encryption",
                description: "Enable in direct chats (BETA)!"
            )),
            .feature(Description(
                icon: "tray.and.arrow.up",
                title: "App data migration",
                description: "Migrate to another device via QR code."
            )),
            .feature(Description(
                icon: "phone",
                title: "Picture-in-picture calls",
                description: "Use the app while in the call."
            )),
            .feature(Description(
                icon: "hand.raised",
                title: "Safer groups",
                description: "Admins can block a member for all."
            )),
            .feature(Description(
                icon: "character",
                title: "Hungarian interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )),
        ]
    ),
    VersionDescription(
        version: "v5.7",
        post: URL(string: "https://simplex.chat/blog/20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.html"),
        features: [
            .feature(Description(
                icon: "key",
                title: "Quantum resistant encryption",
                description: "Will be enabled in direct chats!"
            )),
            .feature(Description(
                icon: "arrowshape.turn.up.forward",
                title: "Forward and save messages",
                description: "Message source remains private."
            )),
            .feature(Description(
                icon: "music.note",
                title: "In-call sounds",
                description: "When connecting audio and video calls."
            )),
            .feature(Description(
                icon: "person.crop.square",
                title: "Shape profile images",
                description: "Square, circle, or anything in between."
            )),
            .feature(Description(
                icon: "antenna.radiowaves.left.and.right",
                title: "Network management",
                description: "More reliable network connection."
            )),
        ]
    ),
    VersionDescription(
        version: "v5.8",
        post: URL(string: "https://simplex.chat/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html"),
        features: [
            .feature(Description(
                icon: "arrow.forward",
                title: "Private message routing 🚀",
                description: "Protect your IP address from the messaging relays chosen by your contacts.\nEnable in *Network & servers* settings."
            )),
            .feature(Description(
                icon: "network.badge.shield.half.filled",
                title: "Safely receive files",
                description: "Confirm files from unknown servers."
            )),
            .feature(Description(
                icon: "battery.50",
                title: "Improved message delivery",
                description: "With reduced battery usage."
            )),
        ]
    ),
    VersionDescription(
        version: "v6.0",
        post: URL(string: "https://simplex.chat/blog/20240814-simplex-chat-vision-funding-v6-private-routing-new-user-experience.html"),
        features: [
            .feature(Description(
                icon: nil,
                title: "New chat experience 🎉",
                description: nil,
                subfeatures: [
                    ("link.badge.plus", "Connect to your friends faster."),
                    ("archivebox", "Archive contacts to chat later."),
                    ("trash", "Delete up to 20 messages at once."),
                    ("platter.filled.bottom.and.arrow.down.iphone", "Use the app with one hand."),
                    ("paintpalette", "Color chats with the new themes."),
                ]
            )),
            .feature(Description(
                icon: nil,
                title: "New media options",
                description: nil,
                subfeatures: [
                    ("square.and.arrow.up", "Share from other apps."),
                    ("play.circle", "Play from the chat list."),
                    ("circle.filled.pattern.diagonalline.rectangle", "Blur for better privacy.")
                ]
            )),
            .feature(Description(
                icon: "arrow.forward",
                title: "Private message routing 🚀",
                description: "It protects your IP address and connections."
            )),
            .feature(Description(
                icon: "network",
                title: "Better networking",
                description: "Connection and servers status."
            )),
        ]
    ),
    VersionDescription(
        version: "v6.1",
        post: URL(string: "https://simplex.chat/blog/20241014-simplex-network-v6-1-security-review-better-calls-user-experience.html"),
        features: [
            .feature(Description(
                icon: "checkmark.shield",
                title: "Better security ✅",
                description: "SimpleX protocols reviewed by Trail of Bits."
            )),
            .feature(Description(
                icon: "video",
                title: "Better calls",
                description: "Switch audio and video during the call."
            )),
            .feature(Description(
                icon: "bolt",
                title: "Better notifications",
                description: "Improved delivery, reduced traffic usage.\nMore improvements are coming soon!"
            )),
            .feature(Description(
                icon: nil,
                title: "Better user experience",
                description: nil,
                subfeatures: [
                    ("link", "Switch chat profile for 1-time invitations."),
                    ("message", "Customizable message shape."),
                    ("calendar", "Better message dates."),
                    ("arrowshape.turn.up.right", "Forward up to 20 messages at once."),
                    ("flag", "Delete or moderate up to 200 messages.")
                ]
            )),
        ]
    ),
    VersionDescription(
        version: "v6.2",
        post: URL(string: "https://simplex.chat/blog/20241210-simplex-network-v6-2-servers-by-flux-business-chats.html"),
        features: [
            .view(FeatureView(
                icon: nil,
                title: "Network decentralization",
                view: { NewOperatorsView() }
            )),
            .feature(Description(
                icon: "briefcase",
                title: "Business chats",
                description: "Privacy for your customers."
            )),
            .feature(Description(
                icon: "bolt",
                title: "More reliable notifications",
                description: "Delivered even when Apple drops them."
            )),
        ]
    ),
    VersionDescription(
        version: "v6.3",
        post: URL(string: "https://simplex.chat/blog/20250308-simplex-chat-v6-3-new-user-experience-safety-in-public-groups.html"),
        features: [
            .feature(Description(
                icon: "at",
                title: "Mention members 👋",
                description: "Get notified when mentioned."
            )),
            .feature(Description(
                icon: "flag",
                title: "Send private reports",
                description: "Help admins moderating their groups."
            )),
            .feature(Description(
                icon: "list.bullet",
                title: "Organize chats into lists",
                description: "Don't miss important messages."
            )),
            .feature(Description(
                icon: nil,
                title: "Better privacy and security",
                description: nil,
                subfeatures: [
                    ("eye.slash", "Private media file names."),
                    ("trash", "Set message expiration in chats.")
                ]
            )),
            .feature(Description(
                icon: nil,
                title: "Better groups performance",
                description: nil,
                subfeatures: [
                    ("bolt", "Faster sending messages."),
                    ("person.2.slash", "Faster deletion of groups.")
                ]
            )),
        ]
    ),
]

private let lastVersion = versionDescriptions.last!.version

func setLastVersionDefault() {
    UserDefaults.standard.set(lastVersion, forKey: DEFAULT_WHATS_NEW_VERSION)
}

func shouldShowWhatsNew() -> Bool {
    let v = UserDefaults.standard.string(forKey: DEFAULT_WHATS_NEW_VERSION)
    setLastVersionDefault()
    return v != lastVersion
}

fileprivate struct NewOperatorsView: View {
    var body: some View {
        VStack(alignment: .leading) {
            Image((operatorsInfo[.flux] ?? ServerOperator.dummyOperatorInfo).largeLogo)
                .resizable()
                .scaledToFit()
                .frame(height: 48)
            Text("The second preset operator in the app!")
                .multilineTextAlignment(.leading)
                .lineLimit(10)
            HStack {
                Text("Enable Flux in Network & servers settings for better metadata privacy.")
            }
        }
    }
}

private enum WhatsNewViewSheet: Identifiable {
    case showConditions

    var id: String {
        switch self {
        case .showConditions: return "showConditions"
        }
    }
}

struct WhatsNewView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @State var currentVersion = versionDescriptions.count - 1
    @State var currentVersionNav = versionDescriptions.count - 1
    var viaSettings = false
    var updatedConditions: Bool
    @State private var sheetItem: WhatsNewViewSheet? = nil

    var body: some View {
        whatsNewView()
            .sheet(item: $sheetItem) { item in
                switch item {
                case .showConditions:
                    UsageConditionsView(
                        currUserServers: Binding.constant([]),
                        userServers: Binding.constant([])
                    )
                    .modifier(ThemedBackground(grouped: true))
                }
            }
    }

    private func whatsNewView() -> some View {
        VStack {
            TabView(selection: $currentVersion) {
                ForEach(Array(versionDescriptions.enumerated()), id: \.0) { (i, v) in
                    ScrollView {
                        VStack(alignment: .leading, spacing: 16) {
                            Text("New in \(v.version)")
                                .font(.title)
                                .foregroundColor(theme.colors.secondary)
                                .frame(maxWidth: .infinity)
                                .padding(.vertical)
                            ForEach(v.features) { f in
                                switch f {
                                case let .feature(d): featureDescription(d).padding(.bottom, 8)
                                case let .view(v): AnyView(v.view()).padding(.bottom, 8)
                                }
                            }
                            if let post = v.post {
                                Link(destination: post) {
                                    HStack {
                                        Text("Read more")
                                        Image(systemName: "arrow.up.right.circle")
                                    }
                                }
                            }
                            if updatedConditions {
                                Button("View updated conditions") {
                                    sheetItem = .showConditions
                                }
                            }
                            if !viaSettings {
                                Spacer()

                                Button("Ok") {
                                    dismiss()
                                }
                                .font(.title3)
                                .frame(maxWidth: .infinity, alignment: .center)

                                Spacer()
                            }
                        }
                        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
                    }
                    .tag(i)
                }
            }
            .tabViewStyle(.page(indexDisplayMode: .never))
            Spacer()
            pagination()
        }
        .padding()
        .onChange(of: currentVersion) { _ in
            currentVersionNav = currentVersion
        }
    }
    
    @ViewBuilder private func featureHeader(_ icon: String?, _ title: LocalizedStringKey) -> some View {
        if let icon {
            HStack(alignment: .center, spacing: 4) {
                Image(systemName: icon)
                    .symbolRenderingMode(.monochrome)
                    .foregroundColor(theme.colors.secondary)
                    .frame(minWidth: 30, alignment: .center)
                Text(title).font(.title3).bold()
            }
        } else {
            Text(title).font(.title3).bold()
        }
    }

    private func featureDescription(_ f: Description) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            featureHeader(f.icon, f.title)
            if let d = f.description {
                Text(d)
                    .multilineTextAlignment(.leading)
                    .lineLimit(10)
            }
            if f.subfeatures.count > 0 {
                ForEach(f.subfeatures, id: \.icon) { s in
                    HStack(alignment: .center, spacing: 4) {
                        Image(systemName: s.icon)
                            .symbolRenderingMode(.monochrome)
                            .foregroundColor(theme.colors.secondary)
                            .frame(minWidth: 30, alignment: .center)
                        Text(s.description)
                            .multilineTextAlignment(.leading)
                            .lineLimit(3)
                    }
                }
            }
        }
    }

    private func pagination() -> some View {
        HStack {
            if currentVersionNav > 0 {
                let prev = currentVersionNav - 1
                Button {
                    currentVersionNav = prev
                    withAnimation { currentVersion = prev }
                } label: {
                    HStack {
                        Image(systemName: "chevron.left")
                        Text(versionDescriptions[prev].version)
                    }
                }
            }
            Spacer()
            if currentVersionNav < versionDescriptions.count - 1 {
                let next = currentVersionNav + 1
                Button {
                    currentVersionNav = next
                    withAnimation { currentVersion = next }
                } label: {
                    HStack {
                        Text(versionDescriptions[next].version)
                        Image(systemName: "chevron.right")
                    }
                }
            }
        }
    }
}

struct NewFeaturesView_Previews: PreviewProvider {
    static var previews: some View {
        WhatsNewView(updatedConditions: false)
    }
}
