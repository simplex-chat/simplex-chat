//
//  ChatItemsMerger.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 02.12.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MergedItems: Hashable, Equatable {
    let items: [MergedItem]
    let splits: [SplitRange]
    // chat item id, index in list
    let indexInParentItems: Dictionary<Int64, Int>

    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.hashValue == rhs.hashValue
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine("\(items.hashValue)")
    }

    static func create(_ items: [ChatItem], _ revealedItems: Set<Int64>, _ chatState: ActiveChatState) -> MergedItems {
        if items.isEmpty {
            return MergedItems(items: [], splits: [], indexInParentItems: [:])
        }

        let unreadCount = chatState.unreadTotal

        let unreadAfterItemId = chatState.unreadAfterItemId
        let itemSplits = chatState.splits
        var mergedItems: [MergedItem] = []
        // Indexes of splits here will be related to reversedChatItems, not chatModel.chatItems
        var splitRanges: [SplitRange] = []
        var indexInParentItems = Dictionary<Int64, Int>()
        var index = 0
        var unclosedSplitIndex: Int? = nil
        var unclosedSplitIndexInParent: Int? = nil
        var visibleItemIndexInParent = -1
        var unreadBefore = unreadCount - chatState.unreadAfterNewestLoaded
        var lastRevealedIdsInMergedItems: BoxedValue<[Int64]>? = nil
        var lastRangeInReversedForMergedItems: BoxedValue<ClosedRange<Int>>? = nil
        var recent: MergedItem? = nil
        while index < items.count {
            let item = items[index]
            let prev = index >= 1 ? items[index - 1] : nil
            let next = index + 1 < items.count ? items[index + 1] : nil
            let category = item.mergeCategory
            let itemIsSplit = itemSplits.contains(item.id)

            if item.id == unreadAfterItemId {
                unreadBefore = unreadCount - chatState.unreadAfter
            }
            if item.isRcvNew {
                unreadBefore -= 1
            }

            let revealed = item.mergeCategory == nil || revealedItems.contains(item.id)
            if recent != nil, case let .grouped(items, _, _, _, mergeCategory, unreadIds, _, _) = recent, mergeCategory == category, let first = items.boxedValue.first, !revealedItems.contains(first.item.id) && !itemIsSplit {
                let listItem = ListItem(item: item, prevItem: prev, nextItem: next, unreadBefore: unreadBefore)
                items.boxedValue.append(listItem)

                if item.isRcvNew {
                    unreadIds.boxedValue.insert(item.id)
                }
                if let lastRevealedIdsInMergedItems, let lastRangeInReversedForMergedItems {
                    if revealed {
                        lastRevealedIdsInMergedItems.boxedValue.append(item.id)
                    }
                    lastRangeInReversedForMergedItems.boxedValue = lastRangeInReversedForMergedItems.boxedValue.lowerBound ... index
                }
            } else {
                visibleItemIndexInParent += 1
                let listItem = ListItem(item: item, prevItem: prev, nextItem: next, unreadBefore: unreadBefore)
                if item.mergeCategory != nil {
                    if item.mergeCategory != prev?.mergeCategory || lastRevealedIdsInMergedItems == nil {
                        lastRevealedIdsInMergedItems = BoxedValue(revealedItems.contains(item.id) ? [item.id] : [])
                    } else if revealed, let lastRevealedIdsInMergedItems {
                        lastRevealedIdsInMergedItems.boxedValue.append(item.id)
                    }
                    lastRangeInReversedForMergedItems = BoxedValue(index ... index)
                    recent = MergedItem.grouped(
                        items: BoxedValue([listItem]),
                        revealed: revealed,
                        revealedIdsWithinGroup: lastRevealedIdsInMergedItems!,
                        rangeInReversed: lastRangeInReversedForMergedItems!,
                        mergeCategory: item.mergeCategory,
                        unreadIds: BoxedValue(item.isRcvNew ? Set(arrayLiteral: item.id) : Set()),
                        startIndexInReversedItems: index,
                        hash: listItem.genHash(revealedItems.contains(prev?.id ?? -1), revealedItems.contains(next?.id ?? -1))
                    )
                } else {
                    lastRangeInReversedForMergedItems = nil
                    recent = MergedItem.single(
                        item: listItem,
                        startIndexInReversedItems: index,
                        hash: listItem.genHash(revealedItems.contains(prev?.id ?? -1), revealedItems.contains(next?.id ?? -1))
                    )
                }
                mergedItems.append(recent!)
            }
            if itemIsSplit {
                // found item that is considered as a split
                if let unclosedSplitIndex, let unclosedSplitIndexInParent {
                    // it was at least second split in the list
                    splitRanges.append(SplitRange(itemId: items[unclosedSplitIndex].id, indexRangeInReversed: unclosedSplitIndex ... index - 1, indexRangeInParentItems: unclosedSplitIndexInParent ... visibleItemIndexInParent - 1))
                }
                unclosedSplitIndex = index
                unclosedSplitIndexInParent = visibleItemIndexInParent
            } else if index + 1 == items.count, let unclosedSplitIndex, let unclosedSplitIndexInParent {
                // just one split for the whole list, there will be no more, it's the end
                splitRanges.append(SplitRange(itemId: items[unclosedSplitIndex].id, indexRangeInReversed: unclosedSplitIndex ... index, indexRangeInParentItems: unclosedSplitIndexInParent ... visibleItemIndexInParent))
            }
            indexInParentItems[item.id] = visibleItemIndexInParent
            index += 1
        }
        return MergedItems(
            items: mergedItems,
            splits: splitRanges,
            indexInParentItems: indexInParentItems
        )
    }

    // Use this check to ensure that mergedItems state based on currently actual state of global
    // splits and reversedChatItems
    func isActualState() -> Bool {
        let im = ItemsModel.shared
        // do not load anything if global splits state is different than in merged items because it
        // will produce undefined results in terms of loading and placement of items. 
        // Same applies to reversedChatItems
        return indexInParentItems.count == im.reversedChatItems.count &&
        splits.count == im.chatState.splits.count &&
        // that's just an optimization because most of the time only 1 split exists
        ((splits.count == 1 && splits[0].itemId == im.chatState.splits[0]) || splits.map({ split in split.itemId }).sorted() == im.chatState.splits.sorted())
    }
}


enum MergedItem: Identifiable, Hashable, Equatable {
    // equatable and hashable implementations allows to see the difference and correctly scroll to items we want
    static func == (lhs: Self, rhs: Self) -> Bool {
        lhs.hash == rhs.hash
    }

    var id: Int64 { newest().item.id }

    func hash(into hasher: inout Hasher) {
        hasher.combine(hash)
    }

    var hash: String {
        switch self {
        case .single(_, _, let hash): hash + " 1"
        case .grouped(let items, _, _, _, _, _, _, let hash): hash + " \(items.boxedValue.count)"
        }
    }

    // the item that is always single, cannot be grouped and always revealed
    case single(
        item: ListItem,
        startIndexInReversedItems: Int,
        hash: String
    )

    /** The item that can contain multiple items or just one depending on revealed state. When the whole group of merged items is revealed,
     * there will be multiple [Grouped] items with revealed flag set to true. When the whole group is collapsed, it will be just one instance
     *  of [Grouped] item with all grouped items inside [items]. In other words, number of [MergedItem] will always be equal to number of
     *  visible items in ChatView's EndlessScrollView  */
    case grouped (
        items: BoxedValue<[ListItem]>,
        revealed: Bool,
        // it stores ids for all consecutive revealed items from the same group in order to hide them all on user's action
        // it's the same list instance for all Grouped items within revealed group
        /** @see reveal */
        revealedIdsWithinGroup: BoxedValue<[Int64]>,
        rangeInReversed: BoxedValue<ClosedRange<Int>>,
        mergeCategory: CIMergeCategory?,
        unreadIds: BoxedValue<Set<Int64>>,
        startIndexInReversedItems: Int,
        hash: String
    )

    func revealItems(_ reveal: Bool, _ revealedItems: Binding<Set<Int64>>) {
        if case .grouped(let items, _, let revealedIdsWithinGroup, _, _, _, _, _) = self {
            var newRevealed = revealedItems.wrappedValue
            var i = 0
            if reveal {
                while i < items.boxedValue.count {
                    newRevealed.insert(items.boxedValue[i].item.id)
                    i += 1
                }
            } else {
                while i < revealedIdsWithinGroup.boxedValue.count {
                    newRevealed.remove(revealedIdsWithinGroup.boxedValue[i])
                    i += 1
                }
                revealedIdsWithinGroup.boxedValue.removeAll()
            }
            revealedItems.wrappedValue = newRevealed
        }
    }

    var startIndexInReversedItems: Int {
        get {
            switch self {
            case let .single(_, startIndexInReversedItems, _): startIndexInReversedItems
            case let .grouped(_, _, _, _, _, _, startIndexInReversedItems, _): startIndexInReversedItems
            }
        }
    }

    func hasUnread() -> Bool {
        switch self {
        case let .single(item, _, _): item.item.isRcvNew
        case let .grouped(_, _, _, _, _, unreadIds, _, _): !unreadIds.boxedValue.isEmpty
        }
    }

    func newest() -> ListItem {
        switch self {
        case let .single(item, _, _): item
        case let .grouped(items, _, _, _, _, _, _, _): items.boxedValue[0]
        }
    }

    func oldest() -> ListItem {
        switch self {
        case let .single(item, _, _): item
        case let .grouped(items, _, _, _, _, _, _, _): items.boxedValue[items.boxedValue.count - 1]
        }
    }

    func lastIndexInReversed() -> Int {
        switch self {
        case .single: startIndexInReversedItems
        case let .grouped(items, _, _, _, _, _, _, _): startIndexInReversedItems + items.boxedValue.count - 1
        }
    }
}

struct SplitRange {
    let itemId: Int64
    /** range of indexes inside reversedChatItems where the first element is the split (it's index is [indexRangeInReversed.first])
     * so [0, 1, 2, -100-, 101] if the 3 is a split, SplitRange(indexRange = 3 .. 4) will be this SplitRange instance
     * (3, 4 indexes of the splitRange with the split itself at index 3)
     * */
    let indexRangeInReversed: ClosedRange<Int>
    /** range of indexes inside LazyColumn where the first element is the split (it's index is [indexRangeInParentItems.first]) */
    let indexRangeInParentItems: ClosedRange<Int>
}

struct ListItem: Hashable {
    let item: ChatItem
    let prevItem: ChatItem?
    let nextItem: ChatItem?
    // how many unread items before (older than) this one (excluding this one)
    let unreadBefore: Int

    private func chatDirHash(_ chatDir: CIDirection?) -> Int {
        guard let chatDir else { return 0 }
        return switch chatDir {
        case .directSnd: 0
        case .directRcv: 1
        case .groupSnd: 2
        case let .groupRcv(mem): "\(mem.groupMemberId) \(mem.displayName) \(mem.memberStatus.rawValue) \(mem.memberRole.rawValue) \(mem.image?.hash ?? 0)".hash
        case .localSnd: 4
        case .localRcv: 5
        }
    }

    // using meta.hashValue instead of parts takes much more time so better to use partial meta here
    func genHash(_ prevRevealed: Bool, _ nextRevealed: Bool) -> String {
        "\(item.meta.itemId) \(item.meta.updatedAt.hashValue) \(item.meta.itemEdited) \(item.meta.itemDeleted?.hashValue ?? 0) \(item.meta.itemTimed?.hashValue ?? 0) \(item.meta.itemStatus.hashValue) \(item.meta.sentViaProxy ?? false) \(item.mergeCategory?.hashValue ?? 0) \(chatDirHash(item.chatDir)) \(item.reactions.hashValue) \(item.meta.isRcvNew) \(item.text.hash) \(item.file?.hashValue ?? 0) \(item.quotedItem?.itemId ?? 0) \(unreadBefore) \(prevItem?.id ?? 0) \(chatDirHash(prevItem?.chatDir)) \(prevItem?.mergeCategory?.hashValue ?? 0) \(prevRevealed) \(nextItem?.id ?? 0) \(chatDirHash(nextItem?.chatDir)) \(nextItem?.mergeCategory?.hashValue ?? 0) \(nextRevealed)"
    }
}

class ActiveChatState {
    var splits: [Int64] = []
    var unreadAfterItemId: Int64 = -1
    // total items after unread after item (exclusive)
    var totalAfter: Int = 0
    var unreadTotal: Int = 0
    // exclusive
    var unreadAfter: Int = 0
    // exclusive
    var unreadAfterNewestLoaded: Int = 0

    func moveUnreadAfterItem(_ toItemId: Int64?, _ nonReversedItems: [ChatItem]) {
        guard let toItemId else { return }
        let currentIndex = nonReversedItems.firstIndex(where: { $0.id == unreadAfterItemId })
        let newIndex = nonReversedItems.firstIndex(where: { $0.id == toItemId })
        guard let currentIndex, let newIndex else {
            return
        }
        unreadAfterItemId = toItemId
        let unreadDiff = newIndex > currentIndex
        ? -nonReversedItems[currentIndex + 1..<newIndex + 1].filter { $0.isRcvNew }.count
        : nonReversedItems[newIndex + 1..<currentIndex + 1].filter { $0.isRcvNew }.count
        unreadAfter += unreadDiff
    }

    func moveUnreadAfterItem(_ fromIndex: Int, _ toIndex: Int, _ nonReversedItems: [ChatItem]) {
        if fromIndex == -1 || toIndex == -1 {
            return
        }
        unreadAfterItemId = nonReversedItems[toIndex].id
        let unreadDiff = toIndex > fromIndex
        ? -nonReversedItems[fromIndex + 1..<toIndex + 1].filter { $0.isRcvNew }.count
        : nonReversedItems[toIndex + 1..<fromIndex + 1].filter { $0.isRcvNew }.count
        unreadAfter += unreadDiff
    }

    func clear() {
        splits = []
        unreadAfterItemId = -1
        totalAfter = 0
        unreadTotal = 0
        unreadAfter = 0
        unreadAfterNewestLoaded = 0
    }

    func itemsRead(_ itemIds: Set<Int64>?, _ newItems: [ChatItem]) {
        guard let itemIds else {
            // special case when the whole chat became read
            unreadTotal = 0
            unreadAfter = 0
            return
        }
        var unreadAfterItemIndex: Int = -1
        // since it's more often that the newest items become read, it's logical to loop from the end of the list to finish it faster
        var i = newItems.count - 1
        var ids = itemIds
        // intermediate variables to prevent re-setting state value a lot of times without reason
        var newUnreadTotal = unreadTotal
        var newUnreadAfter = unreadAfter
        while i >= 0 {
            let item = newItems[i]
            if item.id == unreadAfterItemId {
                unreadAfterItemIndex = i
            }
            if ids.contains(item.id) {
                // was unread, now this item is read
                if (unreadAfterItemIndex == -1) {
                    newUnreadAfter -= 1
                }
                newUnreadTotal -= 1
                ids.remove(item.id)
                if ids.isEmpty {
                    break
                }
            }
            i -= 1
        }
        unreadTotal = newUnreadTotal
        unreadAfter = newUnreadAfter
    }

    func itemAdded(_ item: (Int64, Bool), _ index: Int) {
        if item.1 {
            unreadAfter += 1
            unreadTotal += 1
        }
    }

    func itemsRemoved(_ itemIds: [(Int64, Int, Bool)], _ newItems: [ChatItem]) {
        var newSplits: [Int64] = []
        for split in splits {
            let index = itemIds.firstIndex(where: { (delId, _, _) in delId == split })
            // deleted the item that was right before the split between items, find newer item so it will act like the split
            if let index {
                let idx = itemIds[index].1 - itemIds.filter { (_, delIndex, _) in delIndex <= index }.count
                let newSplit = newItems.count > idx && idx >= 0 ? newItems[idx].id : nil
                // it the  whole section is gone and splits overlap, don't add it at all
                if let newSplit, !newSplits.contains(newSplit) {
                    newSplits.append(newSplit)
                }
            } else {
                newSplits.append(split)
            }
        }
        splits = newSplits

        let index = itemIds.firstIndex(where: { (delId, _, _) in delId == unreadAfterItemId })
        // unread after item was removed
        if let index {
            let idx = itemIds[index].1 - itemIds.filter { (_, delIndex, _) in delIndex <= index }.count
            var newUnreadAfterItemId = newItems.count > idx && idx >= 0 ? newItems[idx].id : nil
            let newUnreadAfterItemWasNull = newUnreadAfterItemId == nil
            if newUnreadAfterItemId == nil {
                // everything on top (including unread after item) were deleted, take top item as unread after id
                newUnreadAfterItemId = newItems.first?.id
            }
            if let newUnreadAfterItemId {
                unreadAfterItemId = newUnreadAfterItemId
                totalAfter -= itemIds.filter { (_, delIndex, _) in delIndex > index }.count
                unreadTotal -= itemIds.filter { (_, delIndex, isRcvNew) in delIndex <= index && isRcvNew }.count
                unreadAfter -= itemIds.filter { (_, delIndex, isRcvNew) in delIndex > index && isRcvNew }.count
                if newUnreadAfterItemWasNull {
                    // since the unread after item was moved one item after initial position, adjust counters accordingly
                    if newItems.first?.isRcvNew == true {
                        unreadTotal += 1
                        unreadAfter -= 1
                    }
                }
            } else {
                // all items were deleted, 0 items in chatItems
                unreadAfterItemId = -1
                totalAfter = 0
                unreadTotal = 0
                unreadAfter = 0
            }
        } else {
            totalAfter -= itemIds.count
        }
    }
}

class BoxedValue<T: Hashable>: Equatable, Hashable {
    static func == (lhs: BoxedValue<T>, rhs: BoxedValue<T>) -> Bool {
        lhs.boxedValue == rhs.boxedValue
    }

    func hash(into hasher: inout Hasher) {
        hasher.combine("\(self)")
    }

    var boxedValue : T
    init(_ value: T) {
        self.boxedValue = value
    }
}

@MainActor
func visibleItemIndexesNonReversed(_ listState: EndlessScrollView<MergedItem>.ListState, _ mergedItems: MergedItems) -> ClosedRange<Int> {
    let zero = 0 ... 0
    let items = mergedItems.items
    if items.isEmpty {
        return zero
    }
    let newest = items.count > listState.firstVisibleItemIndex ? items[listState.firstVisibleItemIndex].startIndexInReversedItems : nil
    let oldest = items.count > listState.lastVisibleItemIndex ? items[listState.lastVisibleItemIndex].lastIndexInReversed() : nil
    guard let newest, let oldest else {
        return zero
    }
    let size = ItemsModel.shared.reversedChatItems.count
    let range = size - oldest ... size - newest
    if range.lowerBound < 0 || range.upperBound < 0 {
        return zero
    }

    // visible items mapped to their underlying data structure which is ItemsModel.shared.reversedChatItems.reversed()
    return range
}
