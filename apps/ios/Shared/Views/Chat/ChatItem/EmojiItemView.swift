//
//  EmojiItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct EmojiItemView: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem

    var body: some View {
        VStack(spacing: 1) {
            emojiText(chatItem.content.text)
                .padding(.top, 8)
                .padding(.horizontal, 6)
            CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary)
                .padding(.bottom, 8)
                .padding(.horizontal, 12)
        }
    }
}

func emojiText(_ text: String) -> Text {
    let s = text.trimmingCharacters(in: .whitespaces)
    return Text(s).font(s.count < 4 ? largeEmojiFont : mediumEmojiFont)
}

struct EmojiItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            EmojiItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂", .sndSent(sndProgress: .complete)))
            EmojiItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directRcv, .now, "👍"))
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
