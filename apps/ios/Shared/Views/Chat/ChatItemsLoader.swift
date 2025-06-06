//
//  ChatItemsLoader.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 17.12.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SimpleXChat
import SwiftUI

let TRIM_KEEP_COUNT = 200

func apiLoadMessages(
    _ chatId: ChatId,
    _ pagination: ChatPagination,
    _ chatState: ActiveChatState,
    _ search: String = "",
    _ openAroundItemId: ChatItem.ID? = nil,
    _ visibleItemIndexesNonReversed: @MainActor () -> ClosedRange<Int> = { 0 ... 0 }
) async {
    let chat: Chat
    let navInfo: NavigationInfo
    do {
        (chat, navInfo) = try await apiGetChat(chatId: chatId, pagination: pagination, search: search)
    } catch let error {
        logger.error("apiLoadMessages error: \(responseError(error))")
        return
    }

    let chatModel = ChatModel.shared

    // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
    let paginationIsInitial = switch pagination { case .initial: true; default: false }
    let paginationIsLast = switch pagination { case .last: true; default: false }
    // When openAroundItemId is provided, chatId can be different too
    if ((chatModel.chatId != chat.id || chat.chatItems.isEmpty) && !paginationIsInitial && !paginationIsLast && openAroundItemId == nil) || Task.isCancelled {
        return
    }

    let unreadAfterItemId = chatState.unreadAfterItemId

    let oldItems = Array(ItemsModel.shared.reversedChatItems.reversed())
    var newItems: [ChatItem] = []
    switch pagination {
    case .initial:
        let newSplits: [Int64] = if !chat.chatItems.isEmpty && navInfo.afterTotal > 0 { [chat.chatItems.last!.id] } else { [] }
        if chatModel.getChat(chat.id) == nil {
            chatModel.addChat(chat)
        }
        await MainActor.run {
            chatModel.chatItemStatuses.removeAll()
            ItemsModel.shared.reversedChatItems = chat.chatItems.reversed()
            chatModel.updateChatInfo(chat.chatInfo)
            chatState.splits = newSplits
            if !chat.chatItems.isEmpty {
                chatState.unreadAfterItemId = chat.chatItems.last!.id
            }
            chatState.totalAfter = navInfo.afterTotal
            chatState.unreadTotal = chat.chatStats.unreadCount
            chatState.unreadAfter = navInfo.afterUnread
            chatState.unreadAfterNewestLoaded = navInfo.afterUnread

            PreloadState.shared.clear()
        }
    case let .before(paginationChatItemId, _):
        newItems.append(contentsOf: oldItems)
        let indexInCurrentItems = oldItems.firstIndex(where: { $0.id == paginationChatItemId })
        guard let indexInCurrentItems else { return }
        let (newIds, _) = mapItemsToIds(chat.chatItems)
        let wasSize = newItems.count
        let visibleItemIndexes = await MainActor.run { visibleItemIndexesNonReversed() }
        let modifiedSplits = removeDuplicatesAndModifySplitsOnBeforePagination(
            unreadAfterItemId, &newItems, newIds, chatState.splits, visibleItemIndexes
        )
        let insertAt = max((indexInCurrentItems - (wasSize - newItems.count) + modifiedSplits.trimmedIds.count), 0)
        newItems.insert(contentsOf: chat.chatItems, at: insertAt)
        let newReversed: [ChatItem] = newItems.reversed()
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = newReversed
            chatState.splits = modifiedSplits.newSplits
            chatState.moveUnreadAfterItem(modifiedSplits.oldUnreadSplitIndex, modifiedSplits.newUnreadSplitIndex, oldItems)
        }
    case let .after(paginationChatItemId, _):
        newItems.append(contentsOf: oldItems)
        let indexInCurrentItems = oldItems.firstIndex(where: { $0.id == paginationChatItemId })
        guard let indexInCurrentItems else { return }

        let mappedItems = mapItemsToIds(chat.chatItems)
        let newIds = mappedItems.0
        let (newSplits, unreadInLoaded) = removeDuplicatesAndModifySplitsOnAfterPagination(
            mappedItems.1, paginationChatItemId, &newItems, newIds, chat, chatState.splits
        )
        let indexToAdd = min(indexInCurrentItems + 1, newItems.count)
        let indexToAddIsLast = indexToAdd == newItems.count
        newItems.insert(contentsOf: chat.chatItems, at: indexToAdd)
        let new: [ChatItem] = newItems
        let newReversed: [ChatItem] = newItems.reversed()
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = newReversed
            chatState.splits = newSplits
            chatState.moveUnreadAfterItem(chatState.splits.first ?? new.last!.id, new)
            // loading clear bottom area, updating number of unread items after the newest loaded item
            if indexToAddIsLast {
                chatState.unreadAfterNewestLoaded -= unreadInLoaded
            }
        }
    case .around:
        var newSplits: [Int64]
        if openAroundItemId == nil {
            newItems.append(contentsOf: oldItems)
            newSplits = await removeDuplicatesAndUpperSplits(&newItems, chat, chatState.splits, visibleItemIndexesNonReversed)
        } else {
            newSplits = []
        }
        let (itemIndex, splitIndex) = indexToInsertAround(chat.chatInfo.chatType, chat.chatItems.last, to: newItems, Set(newSplits))
        //indexToInsertAroundTest()
        newItems.insert(contentsOf: chat.chatItems, at: itemIndex)
        newSplits.insert(chat.chatItems.last!.id, at: splitIndex)
        let newReversed: [ChatItem] = newItems.reversed()
        let orderedSplits = newSplits
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = newReversed
            chatState.splits = orderedSplits
            chatState.unreadAfterItemId = chat.chatItems.last!.id
            chatState.totalAfter = navInfo.afterTotal
            chatState.unreadTotal = chat.chatStats.unreadCount
            chatState.unreadAfter = navInfo.afterUnread

            if let openAroundItemId {
                chatState.unreadAfterNewestLoaded = navInfo.afterUnread
                ChatModel.shared.openAroundItemId = openAroundItemId
                ChatModel.shared.chatId = chatId
            } else {
                // no need to set it, count will be wrong
                // chatState.unreadAfterNewestLoaded = navInfo.afterUnread
            }
            PreloadState.shared.clear()
        }
    case .last:
        newItems.append(contentsOf: oldItems)
        let newSplits = await removeDuplicatesAndUnusedSplits(&newItems, chat, chatState.splits)
        newItems.append(contentsOf: chat.chatItems)
        let items = newItems
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = items.reversed()
            chatState.splits = newSplits
            chatModel.updateChatInfo(chat.chatInfo)
            chatState.unreadAfterNewestLoaded = 0
        }
    }
}


private class ModifiedSplits {
    let oldUnreadSplitIndex: Int
    let newUnreadSplitIndex: Int
    let trimmedIds: Set<Int64>
    let newSplits: [Int64]

    init(oldUnreadSplitIndex: Int, newUnreadSplitIndex: Int, trimmedIds: Set<Int64>, newSplits: [Int64]) {
        self.oldUnreadSplitIndex = oldUnreadSplitIndex
        self.newUnreadSplitIndex = newUnreadSplitIndex
        self.trimmedIds = trimmedIds
        self.newSplits = newSplits
    }
}

private func removeDuplicatesAndModifySplitsOnBeforePagination(
    _ unreadAfterItemId: Int64,
    _ newItems: inout [ChatItem],
    _ newIds: Set<Int64>,
    _ splits: [Int64],
    _ visibleItemIndexes: ClosedRange<Int>
) -> ModifiedSplits {
    var oldUnreadSplitIndex: Int = -1
    var newUnreadSplitIndex: Int = -1
    var lastSplitIndexTrimmed: Int? = nil
    var allowedTrimming = true
    var index = 0
    /** keep the newest [TRIM_KEEP_COUNT] items (bottom area) and oldest [TRIM_KEEP_COUNT] items, trim others */
    let trimLowerBound = visibleItemIndexes.upperBound + TRIM_KEEP_COUNT
    let trimUpperBound = newItems.count - TRIM_KEEP_COUNT
    let trimRange = trimUpperBound >= trimLowerBound ? trimLowerBound ... trimUpperBound : -1 ... -1
    var trimmedIds = Set<Int64>()
    let prevTrimLowerBound = visibleItemIndexes.upperBound + TRIM_KEEP_COUNT + 1
    let prevTrimUpperBound = newItems.count - TRIM_KEEP_COUNT
    let prevItemTrimRange = prevTrimUpperBound >= prevTrimLowerBound ? prevTrimLowerBound ... prevTrimUpperBound : -1 ... -1
    var newSplits = splits

    newItems.removeAll(where: {
        let invisibleItemToTrim = trimRange.contains(index) && allowedTrimming
        let prevItemWasTrimmed = prevItemTrimRange.contains(index) && allowedTrimming
        // may disable it after clearing the whole split range
        if !splits.isEmpty && $0.id == splits.first {
            // trim only in one split range
            allowedTrimming = false
        }
        let indexInSplits = splits.firstIndex(of: $0.id)
        if let indexInSplits {
            lastSplitIndexTrimmed = indexInSplits
        }
        if invisibleItemToTrim {
            if prevItemWasTrimmed {
                trimmedIds.insert($0.id)
            } else {
                newUnreadSplitIndex = index
                // prev item is not supposed to be trimmed, so exclude current one from trimming and set a split here instead.
                // this allows to define splitRange of the oldest items and to start loading trimmed items when user scrolls in the opposite direction
                if let lastSplitIndexTrimmed {
                    var new = newSplits
                    new[lastSplitIndexTrimmed] = $0.id
                    newSplits = new
                } else {
                    newSplits = [$0.id] + newSplits
                }
            }
        }
        if unreadAfterItemId == $0.id {
            oldUnreadSplitIndex = index
        }
        index += 1
        return (invisibleItemToTrim && prevItemWasTrimmed) || newIds.contains($0.id)
    })
    // will remove any splits that now becomes obsolete because items were merged
    newSplits = newSplits.filter { split in !newIds.contains(split) && !trimmedIds.contains(split) }
    return ModifiedSplits(oldUnreadSplitIndex: oldUnreadSplitIndex, newUnreadSplitIndex:  newUnreadSplitIndex, trimmedIds: trimmedIds, newSplits: newSplits)
}

private func removeDuplicatesAndModifySplitsOnAfterPagination(
    _ unreadInLoaded: Int,
    _ paginationChatItemId: Int64,
    _ newItems: inout [ChatItem],
    _ newIds: Set<Int64>,
    _ chat: Chat,
    _ splits: [Int64]
) -> ([Int64], Int) {
    var unreadInLoaded = unreadInLoaded
    var firstItemIdBelowAllSplits: Int64? = nil
    var splitsToRemove: Set<Int64> = []
    let indexInSplitRanges = splits.firstIndex(of: paginationChatItemId)
    // Currently, it should always load from split range
    let loadingFromSplitRange = indexInSplitRanges != nil
    let topSplits: [Int64]
    var splitsToMerge: [Int64]
    if let indexInSplitRanges, loadingFromSplitRange && indexInSplitRanges + 1 <= splits.count {
        splitsToMerge = Array(splits[indexInSplitRanges + 1 ..< splits.count])
        topSplits = Array(splits[0 ..< indexInSplitRanges + 1])
    } else {
        splitsToMerge = []
        topSplits = []
    }
    newItems.removeAll(where: { new in
        let duplicate = newIds.contains(new.id)
        if loadingFromSplitRange && duplicate {
            if splitsToMerge.contains(new.id) {
                splitsToMerge.removeAll(where: { $0 == new.id })
                splitsToRemove.insert(new.id)
            } else if firstItemIdBelowAllSplits == nil && splitsToMerge.isEmpty {
                // we passed all splits and found duplicated item below all of them, which means no splits anymore below the loaded items
                firstItemIdBelowAllSplits = new.id
            }
        }
        if duplicate && new.isRcvNew {
            unreadInLoaded -= 1
        }
        return duplicate
    })
    var newSplits: [Int64] = []
    if firstItemIdBelowAllSplits != nil {
        // no splits below anymore, all were merged with bottom items
        newSplits = topSplits
    } else {
        if !splitsToRemove.isEmpty {
            var new = splits
            new.removeAll(where: { splitsToRemove.contains($0) })
            newSplits = new
        }
        let enlargedSplit = splits.firstIndex(of: paginationChatItemId)
        if let enlargedSplit {
            // move the split to the end of loaded items
            var new = splits
            new[enlargedSplit] = chat.chatItems.last!.id
            newSplits = new
        }
    }
    return (newSplits, unreadInLoaded)
}

private func removeDuplicatesAndUpperSplits(
    _ newItems: inout [ChatItem],
    _ chat: Chat,
    _ splits: [Int64],
    _ visibleItemIndexesNonReversed: @MainActor () -> ClosedRange<Int>
) async -> [Int64] {
    if splits.isEmpty {
        removeDuplicates(&newItems, chat)
        return splits
    }

    var newSplits = splits
    let visibleItemIndexes = await MainActor.run { visibleItemIndexesNonReversed() }
    let (newIds, _) = mapItemsToIds(chat.chatItems)
    var idsToTrim: [BoxedValue<Set<Int64>>] = []
    idsToTrim.append(BoxedValue(Set()))
    var index = 0
    newItems.removeAll(where: {
        let duplicate = newIds.contains($0.id)
        if (!duplicate && visibleItemIndexes.lowerBound > index) {
            idsToTrim.last?.boxedValue.insert($0.id)
        }
        if visibleItemIndexes.lowerBound > index, let firstIndex = newSplits.firstIndex(of: $0.id) {
            newSplits.remove(at: firstIndex)
            // closing previous range. All items in idsToTrim that ends with empty set should be deleted.
            // Otherwise, the last set should be excluded from trimming because it is in currently visible split range
            idsToTrim.append(BoxedValue(Set()))
        }

        index += 1
        return duplicate
    })
    if !idsToTrim.last!.boxedValue.isEmpty {
        // it has some elements to trim from currently visible range which means the items shouldn't be trimmed
        // Otherwise, the last set would be empty
        idsToTrim.removeLast()
    }
    let allItemsToDelete = idsToTrim.compactMap { set in set.boxedValue }.joined()
    if !allItemsToDelete.isEmpty {
        newItems.removeAll(where: { allItemsToDelete.contains($0.id) })
    }
    return newSplits
}

private func removeDuplicatesAndUnusedSplits(
    _ newItems: inout [ChatItem],
    _ chat: Chat,
    _ splits: [Int64]
) async -> [Int64] {
    if splits.isEmpty {
        removeDuplicates(&newItems, chat)
        return splits
    }

    var newSplits = splits
    let (newIds, _) = mapItemsToIds(chat.chatItems)
    newItems.removeAll(where: {
        let duplicate = newIds.contains($0.id)
        if duplicate, let firstIndex = newSplits.firstIndex(of: $0.id) {
            newSplits.remove(at: firstIndex)
        }
        return duplicate
    })
    return newSplits
}

// ids, number of unread items
private func mapItemsToIds(_ items: [ChatItem]) -> (Set<Int64>, Int) {
    var unreadInLoaded = 0
    var ids: Set<Int64> = Set()
    var i = 0
    while i < items.count {
        let item = items[i]
        ids.insert(item.id)
        if item.isRcvNew {
            unreadInLoaded += 1
        }
        i += 1
    }
    return (ids, unreadInLoaded)
}

private func removeDuplicates(_ newItems: inout [ChatItem], _ chat: Chat) {
    let (newIds, _) = mapItemsToIds(chat.chatItems)
    newItems.removeAll { newIds.contains($0.id) }
}

private typealias SameTimeItem = (index: Int, item: ChatItem)

// return (item index, split index)
private func indexToInsertAround(_ chatType: ChatType, _ lastNew: ChatItem?, to: [ChatItem], _ splits: Set<Int64>) -> (Int, Int) {
    guard to.count > 0, let lastNew = lastNew else { return (0, 0) }
    // group sorting: item_ts, item_id
    // everything else: created_at, item_id
    let compareByTimeTs = chatType == .group
    // in case several items have the same time as another item in the `to` array
    var sameTime: [SameTimeItem] = []

    // trying to find new split index for item looks difficult but allows to not use one more loop.
    // The idea is to memorize how many splits were till any index (map number of splits until index)
    // and use resulting itemIndex to decide new split index position.
    // Because of the possibility to have many items with the same timestamp, it's possible to see `itemIndex < || == || > i`.
    var splitsTillIndex: [Int] = []
    var splitsPerPrevIndex = 0

    for i in 0 ..< to.count {
        let item = to[i]

        splitsPerPrevIndex = splits.contains(item.id) ? splitsPerPrevIndex + 1 : splitsPerPrevIndex
        splitsTillIndex.append(splitsPerPrevIndex)

        let itemIsNewer = (compareByTimeTs ? item.meta.itemTs > lastNew.meta.itemTs : item.meta.createdAt > lastNew.meta.createdAt)
        if itemIsNewer || i + 1 == to.count {
            if (compareByTimeTs ? lastNew.meta.itemTs == item.meta.itemTs : lastNew.meta.createdAt == item.meta.createdAt) {
                sameTime.append((i, item))
            }
            // time to stop the loop. Item is newer or it's the last item in `to` array, taking previous items and checking position inside them
            let itemIndex: Int
            if sameTime.count > 1, let first = sameTime.sorted(by: { prev, next in prev.item.meta.itemId < next.item.id }).first(where: { same in same.item.id > lastNew.id }) {
                itemIndex = first.index
            } else if sameTime.count == 1 {
                itemIndex = sameTime[0].item.id > lastNew.id ? sameTime[0].index : sameTime[0].index + 1
            } else {
                itemIndex = itemIsNewer ? i : i + 1
            }
            let splitIndex = splitsTillIndex[min(itemIndex, splitsTillIndex.count - 1)]
            let prevItemSplitIndex = itemIndex == 0 ? 0 : splitsTillIndex[min(itemIndex - 1, splitsTillIndex.count - 1)]
            return (itemIndex, splitIndex == prevItemSplitIndex ? splitIndex : prevItemSplitIndex)
        }

        if (compareByTimeTs ? lastNew.meta.itemTs == item.meta.itemTs : lastNew.meta.createdAt == item.meta.createdAt) {
            sameTime.append(SameTimeItem(index: i, item: item))
        } else {
            sameTime = []
        }
    }
    // shouldn't be here
    return (to.count, splits.count)
}

private func indexToInsertAroundTest() {
    func assert(_ one: (Int, Int), _ two: (Int, Int)) {
        if one != two {
            logger.debug("\(String(describing: one)) != \(String(describing: two))")
            fatalError()
        }
    }

    let itemsToInsert = [ChatItem.getSample(3, .groupSnd, Date.init(timeIntervalSince1970: 3), "")]
    let items1 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 1), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 2), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items1, Set([1])), (3, 1))

    let items2 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 1), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items2, Set([2])), (3, 1))

    let items3 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items3, Set([1])), (3, 1))

    let items4 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items4, Set([4])), (1, 0))

    let items5 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items5, Set([2])), (2, 1))

    let items6 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items6, Set([5])), (0, 0))

    let items7 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, nil, to: items7, Set([6])), (0, 0))

    let items8 = [
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items8, Set([2])), (0, 0))

    let items9 = [
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items9, Set([5])), (1, 0))

    let items10 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items10, Set([4])), (0, 0))

    let items11: [ChatItem] = []
    assert(indexToInsertAround(.group, itemsToInsert.last, to: items11, Set([])), (0, 0))
}
