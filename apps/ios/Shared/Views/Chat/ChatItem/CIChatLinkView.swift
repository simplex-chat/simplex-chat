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
            planAndConnect(
                chatLinkStr(chatLink),
                linkOwnerSig: ownerSig,
                theme: theme,
                dismiss: false
            )
        })
    }

    private func linkHeaderView() -> some View {
        HStack(alignment: .top) {
            ProfileImage(
                imageStr: chatLinkImage(chatLink),
                iconName: chatLinkIconName(chatLink),
                size: 44,
                color: theme.colors.primary
            )
            .padding(.trailing, 4)
            VStack(alignment: .leading) {
                Text(chatLinkDisplayName(chatLink)).font(.headline).lineLimit(2)
                Text(chatLinkDescription(chatLink))
                    .font(.subheadline)
                    .foregroundColor(theme.colors.secondary)
                    .lineLimit(1)
            }
            .frame(minHeight: 44)
        }
    }

    private var strippedText: String? {
        let text = chatCardText(chatItem.text, chatLink)
        return text.isEmpty ? nil : text
    }
}
