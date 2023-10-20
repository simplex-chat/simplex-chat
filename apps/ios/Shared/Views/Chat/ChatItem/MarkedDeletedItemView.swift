//
//  MarkedDeletedItemView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 30.11.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MarkedDeletedItemView: View {
    @EnvironmentObject var m: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    @Binding var revealed: Bool

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            Text(mergedMarkedDeletedText)
                .font(.caption)
                .foregroundColor(.secondary)
                .italic()
                .lineLimit(1)
            CIMetaView(chat: chat, chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .textSelection(.disabled)
    }

    var mergedMarkedDeletedText: LocalizedStringKey {
        if !revealed, var i = m.getChatItemIndex(chatItem) {
            var moderated = 0
            var blocked = 0
            var deleted = 0
            var moderatedBy: [String] = []
            while i < m.reversedChatItems.count,
                  let itemDeleted = m.reversedChatItems[i].meta.itemDeleted {
                switch itemDeleted {
                case let .moderated(_, byGroupMember):
                    moderated += 1
                    moderatedBy.append(byGroupMember.displayName)
                case .blocked: blocked += 1
                case .deleted: deleted += 1
                }
                i += 1
            }
            let total = moderated + blocked + deleted
            return total <= 1
            ? markedDeletedText
            : total == moderated
            ? "\(total) messages moderated by \(moderatedBy.joined(separator: ", "))"
            : total == blocked
            ? "\(total) messages blocked"
            : "\(total) messages marked deleted"
        } else {
            return markedDeletedText
        }
    }

    var markedDeletedText: LocalizedStringKey {
        switch chatItem.meta.itemDeleted {
        case let .moderated(_, byGroupMember): "moderated by \(byGroupMember.displayName)"
        case .blocked: "blocked"
        default: "marked deleted"
        }
    }
}

struct MarkedDeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            MarkedDeletedItemView(chat: Chat.sampleData, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now)), revealed: Binding.constant(true))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
