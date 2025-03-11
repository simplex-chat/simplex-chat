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
        let newSplits: [Int64]
        if openAroundItemId == nil {
            newItems.append(contentsOf: oldItems)
            newSplits = await removeDuplicatesAndUpperSplits(&newItems, chat, chatState.splits, visibleItemIndexesNonReversed)
        } else {
            newSplits = []
        }
        let index = indexToInsertAround(chat.chatInfo.chatType, chat.chatItems, to: newItems)
        //indexToInsertAroundTest()
        newItems.insert(contentsOf: chat.chatItems, at: index)
        let newReversed: [ChatItem] = newItems.reversed()
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = newReversed
            chatState.splits = [chat.chatItems.last!.id] + newSplits
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
        }
    case .last:
        newItems.append(contentsOf: oldItems)
        removeDuplicates(&newItems, chat)
        newItems.append(contentsOf: chat.chatItems)
        let items = newItems
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = items.reversed()
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
    var splitsToMerge: [Int64] = if let indexInSplitRanges, loadingFromSplitRange && indexInSplitRanges + 1 <= splits.count {
        Array(splits[indexInSplitRanges + 1 ..< splits.count])
    } else {
        []
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
        // no splits anymore, all were merged with bottom items
        newSplits = []
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

private func indexToInsertAround(_ chatType: ChatType, _ itemsToInsert: [ChatItem], to: [ChatItem]) -> Int {
    guard to.count > 0, let lastNew = itemsToInsert.last else { return 0 }
    // group sorting: item_ts, item_id
    // everything else: created_at, item_id
    let compareByTimeTs = chatType == .group
    // in case several items have the same time as another item in the `to` array
    // [(index in array, item)]
    var sameTime: [(Int, ChatItem)] = []

    for i in 0 ..< to.count {
        let item = to[i]

        if (compareByTimeTs ? item.meta.itemTs > lastNew.meta.itemTs : item.meta.createdAt > lastNew.meta.createdAt) || i + 1 == to.count {
            if (compareByTimeTs ? lastNew.meta.itemTs == item.meta.itemTs : lastNew.meta.createdAt == item.meta.createdAt) {
                sameTime.append((i, item))
            }
            // time to stop the loop. Item is newer or the last in `to`, taking previous items and checking position inside them
            if sameTime.count > 1, let first = sameTime.sorted(by: { prev, next in prev.1.meta.itemId < next.1.id }).first(where: { same in same.1.id > lastNew.id }) {
                return first.0
            } else if sameTime.count == 1 {
                return sameTime[0].1.id > lastNew.id ? sameTime[0].0 : sameTime[0].0 + 1
            } else {
                return item.id > lastNew.id ? i : i + 1
            }
        }

        if (compareByTimeTs ? lastNew.meta.itemTs == item.meta.itemTs : lastNew.meta.createdAt == item.meta.createdAt) {
            sameTime.append((i, item))
        } else {
            sameTime = []
        }
    }
    // shouldn't be here
    return to.count
}

func indexToInsertAroundTest() {
    func assert(_ one: Int, _ two: Int) {
        if one != two {
            logger.debug("\(one) != \(two)")
            fatalError()
        }
    }

    let itemsToInsert = [ChatItem.getSample(3, .groupSnd, Date.init(timeIntervalSince1970: 3), "")]
    let items1 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 1), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 2), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items1), 3)

    let items2 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 1), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items2), 3)

    let items3 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(1, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items3), 3)

    let items4 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items4), 1)

    let items5 = [
        ChatItem.getSample(0, .groupSnd, Date.init(timeIntervalSince1970: 0), ""),
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items5), 2)

    let items6 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items6), 0)

    let items7 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, [], to: items7), 0)

    let items8 = [
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 4), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items8), 1)

    let items9 = [
        ChatItem.getSample(2, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items9), 1)

    let items10 = [
        ChatItem.getSample(4, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(5, .groupSnd, Date.init(timeIntervalSince1970: 3), ""),
        ChatItem.getSample(6, .groupSnd, Date.init(timeIntervalSince1970: 4), "")
    ]
    assert(indexToInsertAround(.group, itemsToInsert, to: items10), 0)

    let items11: [ChatItem] = []
    assert(indexToInsertAround(.group, itemsToInsert, to: items11), 0)
}
