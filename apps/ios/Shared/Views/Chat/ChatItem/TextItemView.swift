//
//  TextItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct TextItemView: View {
    var chatItem: ChatItem
    var width: CGFloat

    var body: some View {
        let sent = chatItem.chatDir.sent
        let minWidth = min(200, width)
        let maxWidth = min(300, width * 0.78)

        return VStack {
            Text(chatItem.content.text)
                .padding(.top, 8)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .leading)
                .foregroundColor(sent ? .white : .primary)
                .textSelection(.enabled)
            Text(getDateFormatter().string(from: chatItem.meta.itemTs))
                .font(.caption)
                .foregroundColor(sent ? .white : .secondary)
                .padding(.bottom, 8)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .trailing)
        }
        .background(sent ? .blue : Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(10)
        .padding(.horizontal)
        .frame(
            minWidth: 200,
            maxWidth: .infinity,
            minHeight: 0,
            maxHeight: .infinity,
            alignment: sent ? .trailing : .leading
        )
    }
}

struct TextItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            TextItemView(chatItem: chatItemSample(1, .directSnd, .now, "hello"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "hello there too"), width: 360)
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
