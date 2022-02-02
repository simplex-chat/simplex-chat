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

    var body: some View {
        let sent = chatItem.chatDir.sent

        return VStack {
            Group {
                Text(chatItem.content.text)
                    .padding(.top, 8)
                    .padding(.horizontal, 12)
                    .frame(minWidth: 200, maxWidth: 300, alignment: .leading)
                    .foregroundColor(sent ? .white : .primary)
                    .textSelection(.enabled)
                Text(getDateFormatter().string(from: chatItem.meta.itemTs))
                    .font(.subheadline)
                    .foregroundColor(sent ? .white : .secondary)
                    .padding(.bottom, 8)
                    .padding(.horizontal, 12)
                    .frame(minWidth: 200, maxWidth: 300, alignment: .trailing)
            }
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
            ChatItemView(chatItem: chatItemSample(1, .directSnd, Date.now, "hello"))
            ChatItemView(chatItem: chatItemSample(2, .directRcv, Date.now, "hello there too"))
        }
        .previewLayout(.fixed(width: 300, height: 70))
    }
}
