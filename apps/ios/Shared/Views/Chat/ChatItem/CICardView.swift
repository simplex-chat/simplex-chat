import SwiftUI
import SimpleXChat

struct CICardView<Content: View>: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    @ViewBuilder var content: Content

    var body: some View {
        ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading) {
                content
            }
            .padding(.bottom, 2)

            CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary, showStatus: false, showEdited: false)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background { chatItemFrameColor(chatItem, theme).modifier(ChatTailPadding()) }
        .textSelection(.disabled)
    }
}
