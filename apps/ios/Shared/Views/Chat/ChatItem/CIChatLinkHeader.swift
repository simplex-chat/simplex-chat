import SwiftUI
import SimpleXChat

struct CIChatLinkHeader: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.showTimestamp) var showTimestamp: Bool
    var chatItem: ChatItem
    var chatLink: MsgChatLink
    var ownerSig: LinkOwnerSig?
    var hasText: Bool

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    var body: some View {
        VStack(alignment: .leading) {
            linkProfileView()
                .padding(.horizontal, 2)
                .padding(.top, 8)
                .padding(.bottom, 6)
                .overlay(DetermineWidth())
            Divider()
            VStack(alignment: .leading, spacing: 2) {
                if let descr = chatLink.shortDescription {
                    Text(descr)
                        .font(.footnote)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(2)
                        .padding(.bottom, 2)
                }
                Text(chatLink.infoLine(signed: ownerSig != nil))
                    .font(.footnote)
                    .foregroundColor(theme.colors.secondary)
                    .padding(.bottom, 2)
                let t = Text("Tap to open").foregroundColor(theme.colors.primary).font(.callout)
                if hasText {
                    t
                } else {
                    t
                    + Text(verbatim: "   ")
                    + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, colorMode: .transparent, showStatus: false, showEdited: false, showViaProxy: showSentViaProxy, showTimesamp: showTimestamp)
                }
            }
            .overlay(DetermineWidth())
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
    }

    private func linkProfileView() -> some View {
        HStack(alignment: .top) {
            ProfileImage(
                imageStr: chatLink.image,
                iconName: chatLink.iconName,
                size: 44,
                color: Color(uiColor: .tertiaryLabel)
            )
            .padding(.trailing, 4)
            VStack(alignment: .leading) {
                Text(chatLink.displayName).font(.headline).lineLimit(2)
                let fn = chatLink.fullName
                if fn != "" && fn != chatLink.displayName {
                    Text(fn).font(.subheadline).lineLimit(2)
                }
            }
            .frame(minHeight: 44)
        }
    }
}
