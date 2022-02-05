//
//  ChatItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private var dateFormatter: DateFormatter?

struct ChatItemView: View {
    var chatItem: ChatItem
    var width: CGFloat

    var body: some View {
        if (isShortEmoji(chatItem.content.text)) {
            EmojiItemView(chatItem: chatItem)
        } else {
            TextItemView(chatItem: chatItem, width: width)
        }
    }
}

func getDateFormatter() -> DateFormatter {
    if let df = dateFormatter { return df }
    let df = DateFormatter()
    df.dateFormat = "HH:mm"
    dateFormatter = df
    return df
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatItem: chatItemSample(1, .directSnd, .now, "hello"), width: 360)
            ChatItemView(chatItem: chatItemSample(2, .directRcv, .now, "hello there too"), width: 360)
            ChatItemView(chatItem: chatItemSample(1, .directSnd, .now, "🙂"), width: 360)
            ChatItemView(chatItem: chatItemSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"), width: 360)
            ChatItemView(chatItem: chatItemSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"), width: 360)
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
