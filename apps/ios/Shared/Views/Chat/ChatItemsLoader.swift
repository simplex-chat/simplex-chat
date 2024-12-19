//
//  ChatItemsLoader.swift
//  SimpleX (iOS)
//
//  Created by me on 17.12.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SimpleXChat
import SwiftUI

let TRIM_KEEP_COUNT = 200

func apiLoadMessages(
    _ chatType: ChatType,
    _ apiId: Int64,
    _ pagination: ChatPagination,
    _ chatState: ActiveChatState,
    _ search: String = "",
    _ visibleItemIndexesNonReversed: () -> ClosedRange<Int> = { 0 ... 0 }
) async {
    let chat: Chat
    let navInfo: NavigationInfo
    do {
        (chat, navInfo) = try await apiGetChat(type: chatType, id: apiId, pagination: pagination, search: search)
    } catch let error {
        logger.error("apiLoadMessages error: \(responseError(error))")
        return
    }

    let chatModel = ChatModel.shared

    // For .initial allow the chatItems to be empty as well as chatModel.chatId to not match this chat because these values become set after .initial finishes
    let paginationIsInitial = switch pagination { case .initial: true; default: false }
    let paginationIsLast = switch pagination { case .last: true; default: false }
    if ((chatModel.chatId != chat.id || chat.chatItems.isEmpty) && !paginationIsInitial && !paginationIsLast) {
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
            chatModel.chatId = chat.chatInfo.id
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
        let modifiedSplits = removeDuplicatesAndModifySplitsOnBeforePagination(
            unreadAfterItemId, &newItems, newIds, chatState.splits, visibleItemIndexesNonReversed
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
        newItems.append(contentsOf: oldItems)
        let newSplits = removeDuplicatesAndUpperSplits(&newItems, chat, chatState.splits, visibleItemIndexesNonReversed)
        // currently, items will always be added on top, which is index 0
        newItems.insert(contentsOf: chat.chatItems, at: 0)
        let newReversed: [ChatItem] = newItems.reversed()
        await MainActor.run {
            ItemsModel.shared.reversedChatItems = newReversed
            chatState.splits = [chat.chatItems.last!.id] + newSplits
            chatState.unreadAfterItemId = chat.chatItems.last!.id
            chatState.totalAfter = navInfo.afterTotal
            chatState.unreadTotal = chat.chatStats.unreadCount
            chatState.unreadAfter = navInfo.afterUnread
            // no need to set it, count will be wrong
            // unreadAfterNewestLoaded.value = navInfo.afterUnread
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
    _ visibleItemIndexesNonReversed: () -> ClosedRange<Int>
) -> ModifiedSplits {
    var oldUnreadSplitIndex: Int = -1
    var newUnreadSplitIndex: Int = -1
    let visibleItemIndexes = visibleItemIndexesNonReversed()
    var lastSplitIndexTrimmed: Int? = nil
    var allowedTrimming = true
    var index = 0
    /** keep the newest [TRIM_KEEP_COUNT] items (bottom area) and oldest [TRIM_KEEP_COUNT] items, trim others */
    let trimRange = visibleItemIndexes.upperBound + TRIM_KEEP_COUNT ... newItems.count - TRIM_KEEP_COUNT
    var trimmedIds = Set<Int64>()
    let prevItemTrimRange = visibleItemIndexes.upperBound + TRIM_KEEP_COUNT + 1 ... newItems.count - TRIM_KEEP_COUNT
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
    var splitsToRemove: [Int64] = []
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
                splitsToRemove.append(new.id)
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
            // LALAL TODO make it set
            new.removeAll(where: { splitsToRemove.contains($0) })
            newSplits = new
        }
        let enlargedSplit = splits.firstIndex(of: paginationChatItemId)
        if let enlargedSplit {
            // move the split to the end of loaded items
            var new = splits
            new[enlargedSplit] = chat.chatItems.last!.id
            newSplits = new
            // Log.d(TAG, "Enlarged split range $newSplits")
        }
    }
    return (newSplits, unreadInLoaded)
}

private func removeDuplicatesAndUpperSplits(
    _ newItems: inout [ChatItem],
    _ chat: Chat,
    _ splits: [Int64],
    _ visibleItemIndexesNonReversed: () -> ClosedRange<Int>
) -> [Int64] {
    if splits.isEmpty {
        removeDuplicates(&newItems, chat)
        return splits
    }

    var newSplits = splits
    let visibleItemIndexes = visibleItemIndexesNonReversed()
    let (newIds, _) = mapItemsToIds(chat.chatItems)
    var idsToTrim: [BoxedValue<Set<Int64>>] = []
    idsToTrim.append(BoxedValue(Set()))
    var index = 0
    newItems.removeAll(where: {
        let duplicate = newIds.contains($0.id)
        if (!duplicate && visibleItemIndexes.lowerBound > index) {
            idsToTrim.last?.boxedValue.insert($0.id)
        }
        let firstIndex = splits.firstIndex(of: $0.id)
        if visibleItemIndexes.lowerBound > index, let firstIndex {
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
