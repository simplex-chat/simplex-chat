//
//  ChatItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatItemView: View {
    var chatItem: ChatItem

    var body: some View {
        if (chatItem.quotedItem == nil && isShortEmoji(chatItem.content.text)) {
            EmojiItemView(chatItem: chatItem)
        } else {
            FramedItemView(chatItem: chatItem)
        }
    }
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"))
            ChatItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"))
            ChatItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"))
            ChatItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"))
            ChatItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"))
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
