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
        (Text(mergedMarkedDeletedText).italic() + Text(" ") + chatItem.timestampText)
        .font(.caption)
        .foregroundColor(.secondary)
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .textSelection(.disabled)
    }

    var mergedMarkedDeletedText: LocalizedStringKey {
        if !revealed,
           let ciCategory = chatItem.mergeCategory,
           var i = m.getChatItemIndex(chatItem) {
            var moderated = 0
            var blocked = 0
            var blockedByAdmin = 0
            var deleted = 0
            var moderatedBy: Set<String> = []
            while i < m.reversedChatItems.count,
                  let ci = .some(m.reversedChatItems[i]),
                  ci.mergeCategory == ciCategory,
                  let itemDeleted = ci.meta.itemDeleted {
                switch itemDeleted {
                case let .moderated(_, byGroupMember):
                    moderated += 1
                    moderatedBy.insert(byGroupMember.displayName)
                case .blocked: blocked += 1
                case .blockedByAdmin: blockedByAdmin += 1
                case .deleted: deleted += 1
                }
                i += 1
            }
            let total = moderated + blocked + blockedByAdmin + deleted
            return total <= 1
            ? markedDeletedText
            : total == moderated
            ? "\(total) messages moderated by \(moderatedBy.joined(separator: ", "))"
            : total == blockedByAdmin
            ? "\(total) messages blocked by admin"
            : total == blocked + blockedByAdmin
            ? "\(total) messages blocked"
            : "\(total) messages marked deleted"
        } else {
            return markedDeletedText
        }
    }

    // same texts are in markedDeletedText in ChatPreviewView, but it returns String;
    // can be refactored into a single function if functions calling these are changed to return same type
    var markedDeletedText: LocalizedStringKey {
        switch chatItem.meta.itemDeleted {
        case let .moderated(_, byGroupMember): "moderated by \(byGroupMember.displayName)"
        case .blocked: "blocked"
        case .blockedByAdmin: "blocked by admin"
        case .deleted, nil: "marked deleted"
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
