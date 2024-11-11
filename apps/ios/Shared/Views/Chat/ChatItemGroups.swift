//
//  ChatItemGroups.swift
//  SimpleX (iOS)
//
//  Created by Diogo Cunha on 11/11/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import Foundation
import SwiftUI
import SimpleXChat

/// Represents an anchor  in a list of chat items, indicating where data is missing and should be loaded.
///
/// - Parameters:
///   - itemId: The unique identifier of the last item in the loaded list before the anchor.
///     This ID corresponds to an item in the chat history, ordered from older to newer items.
///     It is typically used when loading items via .around or .initial pagination when loading items
///   - indexRange: The range of indexes within `reversedChatItems` array that
///     represents the anchor. The first index in this range is the position of the anchor itself.
///     For instance, if the array `[0, 1, 2, -100-, 101]` has an anchor at index 3, `indexRange`
///     would be `3..<5`, indicating the anchor starts at index 3.
///   - indexRangeInParentItems: The range of indexes in the `ReverseList` or parent UI component
///     that considers revealed or hidden items, showing where the anchor appears in the visible list.
///     The first index in this range points to where the anchor starts in the UI.
struct AnchoredRange {
    let itemId: Int64
    let indexRange: Range<Int>
    let indexRangeInParentItems: Range<Int>
}
struct SectionGroups {
    let sections: [SectionItems]
    let anchoredRanges: [AnchoredRange]
}

struct ListItem: Hashable, Equatable {
    let item: ChatItem
    let separation: ItemSeparation
    let prevItemSeparationLargeGap: Bool
}

class SectionItems: ObservableObject {    
    var mergeCategory: CIMergeCategory?
    var items: [ListItem]
    @Published var revealed: Bool
    var showAvatar: Set<Int64>
    var startIndexInParentItems: Int

    init(mergeCategory: CIMergeCategory?, items: [ListItem], revealed: Bool, showAvatar: Set<Int64>, startIndexInParentItems: Int) {
        self.mergeCategory = mergeCategory
        self.items = items
        self.revealed = revealed
        self.showAvatar = showAvatar
        self.startIndexInParentItems = startIndexInParentItems
    }

    func reveal(_ reveal: Bool, revealedItems: inout Set<Int64>) {
        if reveal {
            for item in items {
                revealedItems.insert(item.item.id)
            }
        } else {
            for item in items {
                revealedItems.remove(item.item.id)
            }
        }
        self.revealed = reveal
    }
}

func putIntoGroups(chatItems: [ChatItem], revealedItems: Set<ChatItem.ID>, itemAnchors: Array<ChatItem.ID>) -> SectionGroups {
    guard !chatItems.isEmpty else { return SectionGroups(sections: [], anchoredRanges: []) }

    var groups: [SectionItems] = []
    var anchoredRanges: [AnchoredRange] = []
    var index = 0
    var unclosedAnchorIndex: Int?
    var unclosedAnchorIndexInParent: Int?
    var unclosedAnchorItemId: Int64?
    var visibleItemIndexInParent = -1
    var recent: SectionItems?

    while index < chatItems.count {
        let item = chatItems[index]
        let next = index + 1 < chatItems.count ? chatItems[index + 1] : nil
        let category = item.mergeCategory
        let itemIsAnchor = itemAnchors.contains(item.id)

        let itemSeparation: ItemSeparation
        let prevItemSeparationLargeGap: Bool

        if let recentSection = recent, index > 0, recentSection.mergeCategory == category, !itemIsAnchor {
            if recentSection.revealed {
                let prev = index > 0 ? chatItems[index - 1] : nil
                itemSeparation = getItemSeparation(item, at: index)
                let nextForGap = (category != nil && category == prev?.mergeCategory) || index + 1 == chatItems.count ? nil : next
                prevItemSeparationLargeGap = nextForGap == nil ? false : getItemSeparationLargeGap(item, at: index)
                visibleItemIndexInParent += 1
            } else {
                itemSeparation = getItemSeparation(item, at: index)
                prevItemSeparationLargeGap = false
            }

            let listItem = ListItem(item: item, separation: itemSeparation, prevItemSeparationLargeGap: prevItemSeparationLargeGap)
            recentSection.items.append(listItem)
            if shouldShowAvatar(current: item, older: next) {
                recentSection.showAvatar.insert(item.id)
            }
        } else {
            let revealed = item.mergeCategory == nil || revealedItems.contains(item.id)
            visibleItemIndexInParent += 1

            if revealed {
                let prev = index > 0 ? chatItems[index - 1] : nil
                itemSeparation = getItemSeparation(item, at: index)
                let nextForGap = (category != nil && category == prev?.mergeCategory) || index + 1 == chatItems.count ? nil : next
                prevItemSeparationLargeGap = nextForGap == nil ? false : getItemSeparationLargeGap(item, at: index)
            } else {
                itemSeparation = getItemSeparation(item, at: index)
                prevItemSeparationLargeGap = false
            }

            let listItem = ListItem(item: item, separation: itemSeparation, prevItemSeparationLargeGap: prevItemSeparationLargeGap)
            let newSection = SectionItems(
                mergeCategory: item.mergeCategory,
                items: [listItem],
                revealed: revealed,
                showAvatar: shouldShowAvatar(current: item, older: next) ? [item.id] : [],
                startIndexInParentItems: visibleItemIndexInParent
            )
            groups.append(newSection)
            recent = newSection
        }

        if itemIsAnchor {
            if let unclosedIndex = unclosedAnchorIndex, let unclosedId = unclosedAnchorItemId, let unclosedIndexInParent = unclosedAnchorIndexInParent {
                anchoredRanges.append(
                    AnchoredRange(
                        itemId: unclosedId,
                        indexRange: unclosedIndex..<index,
                        indexRangeInParentItems: unclosedIndexInParent..<visibleItemIndexInParent
                    )
                )
            }
            unclosedAnchorIndex = index
            unclosedAnchorIndexInParent = visibleItemIndexInParent
            unclosedAnchorItemId = item.id
        } else if index + 1 == chatItems.count, let unclosedIndex = unclosedAnchorIndex, let unclosedId = unclosedAnchorItemId, let unclosedIndexInParent = unclosedAnchorIndexInParent {
            anchoredRanges.append(
                AnchoredRange(
                    itemId: unclosedId,
                    indexRange: unclosedIndex..<index + 1,
                    indexRangeInParentItems: unclosedIndexInParent..<visibleItemIndexInParent + 1
                )
            )
        }
        index += 1
    }
    
    return SectionGroups(sections: groups, anchoredRanges: anchoredRanges)
}

func getItemSectionItems(sections: Array<SectionItems>, itemId: ChatItem.ID) -> SectionItems? {
    for sec in sections {
        if sec.items.firstIndex(where: { $0.item.id == itemId }) != nil {
            return sec
        }
    }
    return nil
}

func getIndexInParentItems(sections: Array<SectionItems>, itemId: ChatItem.ID) -> Int {
    for sec in sections {
        if let index = sec.items.firstIndex(where: { $0.item.id == itemId }) {
            return sec.startIndexInParentItems + (sec.revealed ? index : 0)
        }
    }
    return -1
}

func getNewestItemAtParentIndexOrNull(sections: [SectionItems], parentIndex: Int) -> ChatItem? {
    for group in sections {
        let range = group.startIndexInParentItems...(group.startIndexInParentItems + group.items.count - 1)
        if range.contains(parentIndex) {
            if group.revealed {
                return group.items[parentIndex - group.startIndexInParentItems].item
            } else {
                return group.items.first?.item
            }
        }
    }
    return nil
}


private func shouldShowAvatar(current: ChatItem, older: ChatItem?) -> Bool {
    if case let .groupRcv(currentMember) = current.chatDir {
        if let older = older, case let .groupRcv(olderMember) = older.chatDir {
            return olderMember.memberId != currentMember.memberId
        }
        return true // Show avatar if there is no older item or if older is not a GroupRcv
    }
    return false
}

private func getItemSeparationLargeGap(_ chatItem: ChatItem, at index: Int?) -> Bool {
    let im = ItemsModel.shared

    if let index = index, index > 0, index < im.reversedChatItems.count {
        let nextItem = im.reversedChatItems[index - 1]
        let sameMemberAndDirection = nextItem.chatDir.sameDirection(chatItem.chatDir)
        
        // Return true if they are not the same direction or the time interval is more than 60 seconds.
        return !sameMemberAndDirection || nextItem.meta.itemTs.timeIntervalSince(chatItem.meta.itemTs) > 60
    } else {
        // If there is no next item or it's out of bounds, consider it a large gap.
        return true
    }
}
