//
//  Timeline.Item.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 15/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SimpleXChat

extension Timeline {
    /// Represents a single item in the list
    struct Item: Hashable {
        /// The latest `chatItem`
        let chatItem: ChatItem
        /// Zero or more ``ChatItem``s which have been coalesced into this ``Timeline.Item``
        var mergedChatItems: Array<ChatItem>
        /// Different rendering for ``Item``s that have the same ``CIDirection`` as before
        let isDirectionPersisted: Bool
        /// Defines if merged chat items are expanded and visible
        let isExpanded: Bool
    }
}

extension Array<Timeline.Item> {
    /// Creates an array of ``Timeline.Item``s by coalescing similar ``chatItem``s
    init(reversedChatItems: Array<ChatItem>, expanded: Set<ChatItem.ID>) {
        self = reversedChatItems
            .enumerated()
            .reduce(into: Array<Timeline.Item>()) { items, enumerated in
                if let mergeCategory = enumerated.element.mergeCategory,
                   enumerated.offset > .zero,
                   mergeCategory == reversedChatItems[enumerated.offset - 1].mergeCategory,
                   var last = items.popLast() {
                    last.mergedChatItems.append(enumerated.element)
                    items.append(last)
                } else {
                    items.append(
                        Timeline.Item(
                            chatItem: enumerated.element,
                            mergedChatItems: Array<ChatItem>(),
                            isDirectionPersisted: enumerated.offset < reversedChatItems.count - 1
                            ? reversedChatItems[enumerated.offset + 1].chatDir == enumerated.element.chatDir
                            : false,
                            isExpanded: expanded.contains(enumerated.element.id)
                        )
                    )
                }
        }
    }
}
