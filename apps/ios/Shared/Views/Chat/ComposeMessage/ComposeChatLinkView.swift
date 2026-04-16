import SwiftUI
import SimpleXChat

struct ComposeChatLinkView: View {
    @EnvironmentObject var theme: AppTheme
    var chatLink: MsgChatLink
    var cancelPreview: () -> Void
    let cancelEnabled: Bool

    var body: some View {
        HStack(alignment: .center, spacing: 8) {
            ProfileImage(
                imageStr: chatLinkImage(chatLink),
                iconName: chatLinkIconName(chatLink),
                size: 44
            )
            .padding(.leading, 12)
            VStack(alignment: .leading, spacing: 2) {
                Text(chatLinkDisplayName(chatLink))
                    .font(.headline)
                    .lineLimit(1)
                Text(chatLinkDescription(chatLink))
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
                    .lineLimit(1)
            }
            .padding(.vertical, 5)
            Spacer()
            if cancelEnabled {
                Button { cancelPreview() } label: {
                    Image(systemName: "multiply")
                }
            }
        }
        .padding(.vertical, 1)
        .padding(.trailing, 12)
        .background(theme.appColors.sentMessage)
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity)
    }
}
