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
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    var mergedRange: CIMergedRange?

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            Text(markedDeletedText)
                .font(.caption)
                .foregroundColor(.secondary)
                .italic()
                .lineLimit(1)
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .textSelection(.disabled)
    }

    var markedDeletedText: LocalizedStringKey {
        if let merged = mergedRange, merged.many {
            var moderated = 0
            var blocked = 0
            var deleted = 0
            var moderatedBy: [String] = []
            for i in merged.range {
                let ci = ChatModel.shared.reversedChatItems[i]
                switch ci.meta.itemDeleted {
                case let .moderated(_, byGroupMember):
                    moderated += 1
                    moderatedBy.append(byGroupMember.chatViewName)
                case .blocked: blocked += 1
                default: deleted += 1
                }
            }
            let total = moderated + blocked + deleted
            return total == moderated
                    ? "\(total) messages moderated by \(moderatedBy.joined(separator: ", "))"
                    : total == blocked
                    ? "\(total) messages blocked"
                    : "\(total) messages marked deleted"
        } else {
            return switch chatItem.meta.itemDeleted {
            case let .moderated(_, byGroupMember): "moderated by \(byGroupMember.chatViewName)"
            case .blocked: "blocked"
            default: "marked deleted"
            }
        }
    }
}

struct MarkedDeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            MarkedDeletedItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent(sndProgress: .complete), itemDeleted: .deleted(deletedTs: .now)))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
