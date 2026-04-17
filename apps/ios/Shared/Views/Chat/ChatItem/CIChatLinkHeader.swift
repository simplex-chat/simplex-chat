import SwiftUI
import SimpleXChat

struct CIChatLinkHeader: View {
    @EnvironmentObject var theme: AppTheme
    var chatLink: MsgChatLink
    var ownerSig: LinkOwnerSig?

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(alignment: .top) {
                ProfileImage(
                    imageStr: chatLink.image,
                    iconName: chatLink.iconName,
                    size: 44,
                    color: theme.colors.primary
                )
                .padding(.trailing, 4)
                VStack(alignment: .leading) {
                    Text(chatLink.displayName).font(.headline).lineLimit(2)
                    Text(chatLink.description)
                        .font(.subheadline)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(1)
                }
                .frame(minHeight: 44)
            }
            if ownerSig != nil {
                Text("signed")
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Text("Tap to open")
                .foregroundColor(theme.colors.primary)
                .font(.callout)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
    }
}
