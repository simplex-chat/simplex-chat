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
    var features: [FeatureDescription]
}

private struct FeatureDescription {
    var icon: String
    var title: LocalizedStringKey
    var description: LocalizedStringKey
}

private let versionDescriptions: [VersionDescription] = [
    VersionDescription(
        version: "v4.2",
        features: [
            FeatureDescription(
                icon: "checkmark.shield",
                title: "Security assessment",
                description: "SimpleX Chat security was [audited by Trail of Bits](https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html)."
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
                description: "Thanks to the users – [contribute via Weblate](https://github.com/simplex-chat/simplex-chat/tree/stable#translate-the-apps)!"
            )
        ]
    )
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
    @State var currentVersion = versionDescriptions.count - 1
    @State var currentVersionNav = versionDescriptions.count - 1
    var viaSettings = false

    var body: some View {
        VStack {
            TabView(selection: $currentVersion) {
                ForEach(Array(versionDescriptions.enumerated()), id: \.0) { (i, v) in
                    VStack(alignment: .leading, spacing: 16) {
                        Text("New in \(v.version)")
                            .font(.title)
                            .foregroundColor(.secondary)
                            .frame(maxWidth: .infinity)
                            .padding(.vertical)
                        ForEach(v.features, id: \.icon) { f in
                            featureDescription(f.icon, f.title, f.description)
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
                    .tag(i)
                }
            }
            .tabViewStyle(.page(indexDisplayMode: .never))
            Spacer()
            pagination()
        }
        .padding()
    }

    private func featureDescription(_ icon: String, _ title: LocalizedStringKey, _ description: LocalizedStringKey) -> some View {
        VStack(alignment: .leading) {
            HStack(alignment: .center, spacing: 4) {
                Image(systemName: icon).foregroundColor(.secondary)
                    .frame(minWidth: 30, alignment: .center)
                Text(title).font(.title3).bold()
            }
            Text(description)
                .multilineTextAlignment(.leading)
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
