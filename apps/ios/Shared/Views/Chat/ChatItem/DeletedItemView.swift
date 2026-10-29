//
//  DeletedItemView.swift
//  SimpleX
//
//  Created by JRoberts on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/chat-view.md

import SwiftUI
import SimpleXChat

// Spec: spec/client/chat-view.md#DeletedItemView
struct DeletedItemView: View {
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem

    var body: some View {
        ItemWithMeta {
            CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary)
        } content: { inlineMeta in
            HStack(alignment: .bottom, spacing: 0) {
                Text(chatItem.content.text)
                    .foregroundColor(theme.colors.secondary)
                    .italic()
                if let inlineMeta {
                    inlineMeta
                        .padding(.horizontal, 12)
                }
            }
            .padding(.leading, 12)
            .padding(.trailing, inlineMeta == nil ? 12 : 0)
            .padding(.vertical, 6)
            .background(chatItemFrameColor(chatItem, theme))
            .if(inlineMeta == nil) { $0.modifier(ChatItemClipped(chatItem, tailVisible: false)) }
            .textSelection(.disabled)
        }
    }
}

struct DeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            let ci1 = ChatItem.getDeletedContentSample(1, previewDate)
            DeletedItemView(chat: Chat.sampleData, chatItem: ci1).modifier(ChatItemClipped(ci1, tailVisible: true))
            let ci2 = ChatItem.getDeletedContentSample(dir: .groupRcv(groupMember: GroupMember.sampleData), previewDate)
            DeletedItemView(chat: Chat.sampleData, chatItem: ci2).modifier(ChatItemClipped(ci2, tailVisible: true))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
