//
//  DeletedItemView.swift
//  SimpleX
//
//  Created by JRoberts on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct DeletedItemView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            Text(chatItem.content.text)
                .foregroundColor(theme.colors.secondary)
                .italic()
            CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, theme))
        .textSelection(.disabled)
    }
}

struct DeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            DeletedItemView(chat: Chat.sampleData, chatItem: ChatItem.getDeletedContentSample())
            DeletedItemView(chat: Chat.sampleData, chatItem: ChatItem.getDeletedContentSample(dir: .groupRcv(groupMember: GroupMember.sampleData)))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
