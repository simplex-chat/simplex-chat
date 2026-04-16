import SwiftUI
import SimpleXChat

struct CIChatLinkView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.showTimestamp) var showTimestamp: Bool
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    var chatLink: MsgChatLink
    var ownerSig: LinkOwnerSig?
    var chatIncognito: Bool = false

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false
    @State private var frameWidth: CGFloat = 0

    var body: some View {
        CICardView(chat: chat, chatItem: chatItem) {
            linkHeaderView()
                .padding(.horizontal, 2)
                .padding(.top, 8)
                .padding(.bottom, 6)
                .overlay(DetermineWidth())

            Divider().frame(width: frameWidth)

            VStack(alignment: .leading, spacing: 2) {
                if let text = strippedText, !text.isEmpty {
                    Text(text)
                        .font(.callout)
                        .overlay(DetermineWidth())
                }
                if ownerSig != nil {
                    Text("signed")
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                        .overlay(DetermineWidth())
                }
                (
                    Text("Tap to open")
                        .foregroundColor(chatIncognito ? .indigo : theme.colors.primary)
                        .font(.callout)
                    + Text(verbatim: "   ")
                    + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, colorMode: .transparent, showStatus: false, showEdited: false, showViaProxy: showSentViaProxy, showTimesamp: showTimestamp)
                )
                .overlay(DetermineWidth())
            }
        }
        .onPreferenceChange(DetermineWidth.Key.self) { frameWidth = $0 }
        .simultaneousGesture(TapGesture().onEnded {
            openChatLink()
        })
    }

    private func linkHeaderView() -> some View {
        HStack(alignment: .top) {
            ProfileImage(
                imageStr: linkImage,
                iconName: linkIconName,
                size: 44,
                color: theme.colors.primary
            )
            .padding(.trailing, 4)
            VStack(alignment: .leading) {
                Text(linkDisplayName).font(.headline).lineLimit(2)
                if let fullName = linkFullName, fullName != "" && fullName != linkDisplayName {
                    Text(fullName).font(.subheadline).lineLimit(2)
                }
            }
            .frame(minHeight: 44)
        }
    }

    private var linkImage: String? {
        switch chatLink {
        case let .group(_, groupProfile): groupProfile.image
        case let .contact(_, profile, _): profile.image
        case let .invitation(_, profile): profile.image
        }
    }

    private var linkDisplayName: String {
        switch chatLink {
        case let .group(_, groupProfile): groupProfile.displayName
        case let .contact(_, profile, _): profile.displayName
        case let .invitation(_, profile): profile.displayName
        }
    }

    private var linkFullName: String? {
        switch chatLink {
        case let .group(_, groupProfile): groupProfile.fullName
        case let .contact(_, profile, _): profile.fullName
        case let .invitation(_, profile): profile.fullName
        }
    }

    private var linkIconName: String {
        switch chatLink {
        case let .group(_, groupProfile):
            if groupProfile.publicGroup?.groupType == .channel {
                "antenna.radiowaves.left.and.right.circle.fill"
            } else {
                "person.2.circle.fill"
            }
        case let .contact(_, _, business):
            business ? "briefcase.circle.fill" : "person.crop.circle.fill"
        case .invitation:
            "person.crop.circle.fill"
        }
    }

    private var strippedText: String? {
        let text = chatItem.text
        let link = chatLinkStr
        if text.hasSuffix("\n" + link) {
            let stripped = String(text.dropLast(link.count + 1))
            return stripped.isEmpty ? nil : stripped
        }
        return text.isEmpty ? nil : text
    }

    private var chatLinkStr: String {
        switch chatLink {
        case let .group(connLink, _): connLink
        case let .contact(connLink, _, _): connLink
        case let .invitation(invLink, _): invLink
        }
    }

    private func openChatLink() {
        planAndConnect(
            chatLinkStr,
            linkOwnerSig: ownerSig,
            theme: theme,
            dismiss: false
        )
    }
}
