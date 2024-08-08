//
//  WhatsNewView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 24/12/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private struct VersionDescription {
    var version: String
    var post: URL?
    var features: [FeatureDescription]
}

private struct FeatureDescription {
    var icon: String?
    var title: LocalizedStringKey
    var description: LocalizedStringKey?
    var subfeatures: [(icon: String, description: LocalizedStringKey)] = []
}

private let versionDescriptions: [VersionDescription] = [
    VersionDescription(
        version: "v4.2",
        post: URL(string: "https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"),
        features: [
            FeatureDescription(
                icon: "checkmark.shield",
                title: "Security assessment",
                description: "SimpleX Chat security was audited by Trail of Bits."
            ),
            FeatureDescription(
                icon: "person.2",
                title: "Group links",
                description: "Admins can create the links to join groups."
            ),
            FeatureDescription(
                icon: "checkmark",
                title: "Auto-accept contact requests",
                description: "With optional welcome message."
            ),
        ]
    ),
    VersionDescription(
        version: "v4.3",
        post: URL(string: "https://simplex.chat/blog/20221206-simplex-chat-v4.3-voice-messages.html"),
        features: [
            FeatureDescription(
                icon: "mic",
                title: "Voice messages",
                description: "Max 30 seconds, received instantly."
            ),
            FeatureDescription(
                icon: "trash.slash",
                title: "Irreversible message deletion",
                description: "Your contacts can allow full message deletion."
            ),
            FeatureDescription(
                icon: "externaldrive.connected.to.line.below",
                title: "Improved server configuration",
                description: "Add servers by scanning QR codes."
            ),
            FeatureDescription(
                icon: "eye.slash",
                title: "Improved privacy and security",
                description: "Hide app screen in the recent apps."
            ),
        ]
    ),
    VersionDescription(
        version: "v4.4",
        post: URL(string: "https://simplex.chat/blog/20230103-simplex-chat-v4.4-disappearing-messages.html"),
        features: [
            FeatureDescription(
                icon: "stopwatch",
                title: "Disappearing messages",
                description: "Sent messages will be deleted after set time."
            ),
            FeatureDescription(
                icon: "ellipsis.circle",
                title: "Live messages",
                description: "Recipients see updates as you type them."
            ),
            FeatureDescription(
                icon: "checkmark.shield",
                title: "Verify connection security",
                description: "Compare security codes with your contacts."
            ),
            FeatureDescription(
                icon: "camera",
                title: "GIFs and stickers",
                description: "Send them from gallery or custom keyboards."
            ),
            FeatureDescription(
                icon: "character",
                title: "French interface",
                description: "Thanks to the users – contribute via Weblate!"
            )
        ]
    ),
    VersionDescription(
        version: "v4.5",
        post: URL(string: "https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html"),
        features: [
            FeatureDescription(
                icon: "person.crop.rectangle.stack",
                title: "Multiple chat profiles",
                description: "Different names, avatars and transport isolation."
            ),
            FeatureDescription(
                icon: "rectangle.and.pencil.and.ellipsis",
                title: "Message draft",
                description: "Preserve the last message draft, with attachments."
            ),
            FeatureDescription(
                icon: "network.badge.shield.half.filled",
                title: "Transport isolation",
                description: "By chat profile (default) or [by connection](https://simplex.chat/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html#transport-isolation) (BETA)."
            ),
            FeatureDescription(
                icon: "lock.doc",
                title: "Private filenames",
                description: "To protect timezone, image/voice files use UTC."
            ),
            FeatureDescription(
                icon: "battery.25",
                title: "Reduced battery usage",
                description: "More improvements are coming soon!"
            ),
            FeatureDescription(
                icon: "character",
                title: "Italian interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            )
        ]
    ),
    VersionDescription(
        version: "v4.6",
        post: URL(string: "https://simplex.chat/blog/20230328-simplex-chat-v4-6-hidden-profiles.html"),
        features: [
            FeatureDescription(
                icon: "lock",
                title: "Hidden chat profiles",
                description: "Protect your chat profiles with a password!"
            ),
            FeatureDescription(
                icon: "phone.arrow.up.right",
                title: "Audio and video calls",
                description: "Fully re-implemented - work in background!"
            ),
            FeatureDescription(
                icon: "flag",
                title: "Group moderation",
                description: "Now admins can:\n- delete members' messages.\n- disable members (\"observer\" role)"
            ),
            FeatureDescription(
                icon: "plus.message",
                title: "Group welcome message",
                description: "Set the message shown to new members!"
            ),
            FeatureDescription(
                icon: "battery.50",
                title: "Further reduced battery usage",
                description: "More improvements are coming soon!"
            ),
            FeatureDescription(
                icon: "character",
                title: "Chinese and Spanish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.0",
        post: URL(string: "https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html"),
        features: [
            FeatureDescription(
                icon: "arrow.up.doc",
                title: "Videos and files up to 1gb",
                description: "Fast and no wait until the sender is online!"
            ),
            FeatureDescription(
                icon: "lock",
                title: "App passcode",
                description: "Set it instead of system authentication."
            ),
            FeatureDescription(
                icon: "character",
                title: "Polish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    // Also
    // preference to disable calls per contact
    // access welcome message via a group profile
    VersionDescription(
        version: "v5.1",
        post: URL(string: "https://simplex.chat/blog/20230523-simplex-chat-v5-1-message-reactions-self-destruct-passcode.html"),
        features: [
            FeatureDescription(
                icon: "face.smiling",
                title: "Message reactions",
                description: "Finally, we have them! 🚀"
            ),
            FeatureDescription(
                icon: "arrow.up.message",
                title: "Better messages",
                description: "- voice messages up to 5 minutes.\n- custom time to disappear.\n- editing history."
            ),
            FeatureDescription(
                icon: "lock",
                title: "Self-destruct passcode",
                description: "All data is erased when it is entered."
            ),
            FeatureDescription(
                icon: "character",
                title: "Japanese interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.2",
        post: URL(string: "https://simplex.chat/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html"),
        features: [
            FeatureDescription(
                icon: "checkmark",
                title: "Message delivery receipts!",
                description: "The second tick we missed! ✅"
            ),
            FeatureDescription(
                icon: "star",
                title: "Find chats faster",
                description: "Filter unread and favorite chats."
            ),
            FeatureDescription(
                icon: "exclamationmark.arrow.triangle.2.circlepath",
                title: "Keep your connections",
                description: "Fix encryption after restoring backups."
            ),
            FeatureDescription(
                icon: "stopwatch",
                title: "Make one message disappear",
                description: "Even when disabled in the conversation."
            ),
            FeatureDescription(
                icon: "gift",
                title: "A few more things",
                description: "- more stable message delivery.\n- a bit better groups.\n- and more!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.3",
        post: URL(string: "https://simplex.chat/blog/20230925-simplex-chat-v5-3-desktop-app-local-file-encryption-directory-service.html"),
        features: [
            FeatureDescription(
                icon: "desktopcomputer",
                title: "New desktop app!",
                description: "Create new profile in [desktop app](https://simplex.chat/downloads/). 💻"
            ),
            FeatureDescription(
                icon: "lock",
                title: "Encrypt stored files & media",
                description: "App encrypts new local files (except videos)."
            ),
            FeatureDescription(
                icon: "magnifyingglass",
                title: "Discover and join groups",
                description: "- connect to [directory service](simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion) (BETA)!\n- delivery receipts (up to 20 members).\n- faster and more stable."
            ),
            FeatureDescription(
                icon: "theatermasks",
                title: "Simplified incognito mode",
                description: "Toggle incognito when connecting."
            ),
            FeatureDescription(
                icon: "character",
                title: "\(4) new interface languages",
                description: "Bulgarian, Finnish, Thai and Ukrainian - thanks to the users and [Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.4",
        post: URL(string: "https://simplex.chat/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.html"),
        features: [
            FeatureDescription(
                icon: "desktopcomputer",
                title: "Link mobile and desktop apps! 🔗",
                description: "Via secure quantum resistant protocol."
            ),
            FeatureDescription(
                icon: "person.2",
                title: "Better groups",
                description: "Faster joining and more reliable messages."
            ),
            FeatureDescription(
                icon: "theatermasks",
                title: "Incognito groups",
                description: "Create a group using a random profile."
            ),
            FeatureDescription(
                icon: "hand.raised",
                title: "Block group members",
                description: "To hide unwanted messages."
            ),
            FeatureDescription(
                icon: "gift",
                title: "A few more things",
                description: "- optionally notify deleted contacts.\n- profile names with spaces.\n- and more!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.5",
        post: URL(string: "https://simplex.chat/blog/20240124-simplex-chat-infrastructure-costs-v5-5-simplex-ux-private-notes-group-history.html"),
        features: [
            FeatureDescription(
                icon: "folder",
                title: "Private notes",
                description: "With encrypted files and media."
            ),
            FeatureDescription(
                icon: "link",
                title: "Paste link to connect!",
                description: "Search bar accepts invitation links."
            ),
            FeatureDescription(
                icon: "bubble.left.and.bubble.right",
                title: "Join group conversations",
                description: "Recent history and improved [directory bot](simplex:/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion)."
            ),
            FeatureDescription(
                icon: "battery.50",
                title: "Improved message delivery",
                description: "With reduced battery usage."
            ),
            FeatureDescription(
                icon: "character",
                title: "Turkish interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.6",
        post: URL(string: "https://simplex.chat/blog/20240323-simplex-network-privacy-non-profit-v5-6-quantum-resistant-e2e-encryption-simple-migration.html"),
        features: [
            FeatureDescription(
                icon: "key",
                title: "Quantum resistant encryption",
                description: "Enable in direct chats (BETA)!"
            ),
            FeatureDescription(
                icon: "tray.and.arrow.up",
                title: "App data migration",
                description: "Migrate to another device via QR code."
            ),
            FeatureDescription(
                icon: "phone",
                title: "Picture-in-picture calls",
                description: "Use the app while in the call."
            ),
            FeatureDescription(
                icon: "hand.raised",
                title: "Safer groups",
                description: "Admins can block a member for all."
            ),
            FeatureDescription(
                icon: "character",
                title: "Hungarian interface",
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat)!"
            ),
        ]
    ),
    VersionDescription(
        version: "v5.7",
        post: URL(string: "https://simplex.chat/blog/20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.html"),
        features: [
            FeatureDescription(
                icon: "key",
                title: "Quantum resistant encryption",
                description: "Will be enabled in direct chats!"
            ),
            FeatureDescription(
                icon: "arrowshape.turn.up.forward",
                title: "Forward and save messages",
                description: "Message source remains private."
            ),
            FeatureDescription(
                icon: "music.note",
                title: "In-call sounds",
                description: "When connecting audio and video calls."
            ),
            FeatureDescription(
                icon: "person.crop.square",
                title: "Shape profile images",
                description: "Square, circle, or anything in between."
            ),
            FeatureDescription(
                icon: "antenna.radiowaves.left.and.right",
                title: "Network management",
                description: "More reliable network connection."
            )
        ]
    ),
    VersionDescription(
        version: "v5.8",
        post: URL(string: "https://simplex.chat/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html"),
        features: [
            FeatureDescription(
                icon: "arrow.forward",
                title: "Private message routing 🚀",
                description: "Protect your IP address from the messaging relays chosen by your contacts.\nEnable in *Network & servers* settings."
            ),
            FeatureDescription(
                icon: "network.badge.shield.half.filled",
                title: "Safely receive files",
                description: "Confirm files from unknown servers."
            ),
            FeatureDescription(
                icon: "battery.50",
                title: "Improved message delivery",
                description: "With reduced battery usage."
            )
        ]
    ),
    VersionDescription(
        version: "v6.0",
        post: URL(string: "https://simplex.chat/blog/20240814-simplex-chat-vision-funding-v6-private-routing-new-user-experience.html"),
        features: [
            FeatureDescription(
                icon: nil,
                title: "New chat experience",
                description: nil,
                subfeatures: [
                    ("link.badge.plus", "Connect to your friends faster."),
                    ("square.and.arrow.up", "Share media from other apps."),
                    ("play.circle", "Play media from the chat list."),
                    ("archivebox", "Archive contacts to chat later."),
                    ("trash", "Delete up to 20 messages at once."),
                    ("platter.filled.bottom.and.arrow.down.iphone", "Reachable chat toolbar."),
                ]
            ),
            FeatureDescription(
                icon: "paintpalette",
                title: "New chat themes",
                description: "Make your chats look different!"
            ),
            FeatureDescription(
                icon: "arrow.forward",
                title: "Private message routing 🚀",
                description: "It protects your IP address and connections."
            ),
            FeatureDescription(
                icon: "network",
                title: "Better networking",
                description: "Connection and servers status."
            )
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

struct WhatsNewView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @EnvironmentObject var theme: AppTheme
    @State var currentVersion = versionDescriptions.count - 1
    @State var currentVersionNav = versionDescriptions.count - 1
    var viaSettings = false

    var body: some View {
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
                            ForEach(v.features, id: \.icon) { f in
                                featureDescription(f)
                                    .padding(.bottom, 8)
                            }
                            if let post = v.post {
                                Link(destination: post) {
                                    HStack {
                                        Text("Read more")
                                        Image(systemName: "arrow.up.right.circle")
                                    }
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

    private func featureDescription(_ f: FeatureDescription) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            if let icon = f.icon {
                HStack(alignment: .center, spacing: 4) {
                    Image(systemName: icon)
                        .symbolRenderingMode(.monochrome)
                        .foregroundColor(theme.colors.secondary)
                        .frame(minWidth: 30, alignment: .center)
                    Text(f.title).font(.title3).bold()
                }
            } else {
                Text(f.title).font(.title3).bold()
            }
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
        WhatsNewView()
    }
}
