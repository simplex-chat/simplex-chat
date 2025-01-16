//
//  ChatScrollHelpers.swift
//  SimpleX (iOS)
//
//  Created by me on 20.12.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func loadLastItems(_ loadingMoreItems: Binding<Bool>, _ chatInfo: ChatInfo) {
    //return ()
    if ItemsModel.shared.chatState.totalAfter == 0 {
        return
    }
    loadingMoreItems.wrappedValue = true
    Task {
        try? await Task.sleep(nanoseconds: 1500_000000)
        if ChatModel.shared.chatId != chatInfo.id {
            await MainActor.run {
                loadingMoreItems.wrappedValue = false
            }
            return
        }
        await apiLoadMessages(chatInfo.chatType, chatInfo.apiId, ChatPagination.last(count: 50), ItemsModel.shared.chatState)
        await MainActor.run {
            loadingMoreItems.wrappedValue = false
        }
    }
}

func preloadItems(_ mergedItems: MergedItems, _ allowLoadMoreItems: Bool, _ listState: ListState, _ ignoreLoadingRequests: Binding<[Int64]>, _ loadItems: @escaping (ChatPagination) async -> Bool) async {
    let allowLoad = allowLoadMoreItems || mergedItems.items.count == listState.lastVisibleItemIndex + 1
    let remaining = ChatPagination.UNTIL_PRELOAD_COUNT
    let firstVisibleIndex = listState.firstVisibleItemIndex

    await preloadItemsBefore()
    await preloadItemsAfter()

    func preloadItemsBefore() async {
        let splits = mergedItems.splits
        let lastVisibleIndex = listState.lastVisibleItemIndex
        var lastIndexToLoadFrom: Int? = findLastIndexToLoadFromInSplits(firstVisibleIndex, lastVisibleIndex, remaining, splits)
        logger.debug("LALAL LASTINDEX TO LOAD FROM \(lastIndexToLoadFrom ?? -1)")
        let items: [ChatItem] = ItemsModel.shared.reversedChatItems.reversed()
        if splits.isEmpty && !items.isEmpty && lastVisibleIndex > mergedItems.items.count - remaining && items.count >= ChatPagination.INITIAL_COUNT {
            lastIndexToLoadFrom = items.count - 1
        }
        logger.debug("LALAL LASTINDEX TO LOAD FROM after \(lastIndexToLoadFrom ?? -1), count \(items.count)")
        let loadFromItemId: Int64?
        if allowLoad, let lastIndexToLoadFrom {
            let index = items.count - 1 - lastIndexToLoadFrom
            loadFromItemId = index >= 0 ? items[index].id : nil
            logger.debug("LALAL LASTINDEX TO LOAD FROM inside \(loadFromItemId ?? -1)")
        } else {
            logger.debug("LALAL LOADFROMNIL")
            loadFromItemId = nil
        }
        guard let loadFromItemId, !ignoreLoadingRequests.wrappedValue.contains(loadFromItemId) else {
            return
        }
        let sizeWas = items.count
        let firstItemIdWas = items.first?.id
        let triedToLoad = await loadItems(ChatPagination.before(chatItemId: loadFromItemId, count: ChatPagination.PRELOAD_COUNT))
        logger.debug("LALAL PRELOAD BEFORE \(String(describing: splits)) \(firstVisibleIndex) \(sizeWas) \(triedToLoad) \(lastIndexToLoadFrom ?? -1)")
        if triedToLoad && sizeWas == ItemsModel.shared.reversedChatItems.count && firstItemIdWas == ItemsModel.shared.reversedChatItems.last?.id {
            ignoreLoadingRequests.wrappedValue.append(loadFromItemId)
        }
    }

    func preloadItemsAfter() async {
        let items: [ChatItem] = ItemsModel.shared.reversedChatItems.reversed()
        let splits = mergedItems.splits
        let split = splits.last(where: { $0.indexRangeInParentItems.contains(firstVisibleIndex) })
        // we're inside a splitRange (top --- [end of the splitRange --- we're here --- start of the splitRange] --- bottom)
        logger.debug("LALAL PRELOAD AFTER \(String(describing: splits)) \(firstVisibleIndex) \((split?.indexRangeInParentItems.lowerBound ?? 0) + remaining) \(items.count - 1 - (split?.indexRangeInParentItems.lowerBound ?? -1))")
        if let split, split.indexRangeInParentItems.lowerBound + remaining > firstVisibleIndex {
            let index = items.count - 1 - split.indexRangeInReversed.lowerBound
            if index >= 0 {
                let loadFromItemId = items[index].id
                _ = await loadItems(ChatPagination.after(chatItemId: loadFromItemId, count: ChatPagination.PRELOAD_COUNT))
            }
        }
    }
}

func oldestPartiallyVisibleListItemInListStateOrNull(_ items: [MergedItem], _ listState: ListState) -> ListItem? {
    if listState.lastVisibleItemIndex < items.count {
        return items[listState.lastVisibleItemIndex].oldest()
    } else {
        return items.last?.oldest()
    }
}

private func lastFullyVisibleIemInListState(_ mergedItems: MergedItems, _ listState: ListState) -> ChatItem? {
    if listState.lastVisibleItemIndex < mergedItems.items.count {
        return mergedItems.items[listState.lastVisibleItemIndex].newest().item
    } else {
        return mergedItems.items.last?.newest().item
    }
}

private func findLastIndexToLoadFromInSplits(_ firstVisibleIndex: Int, _ lastVisibleIndex: Int, _ remaining: Int, _ splits: [SplitRange]) -> Int? {
    for split in splits {
        // before any split
        if split.indexRangeInParentItems.lowerBound > firstVisibleIndex {
            if lastVisibleIndex > (split.indexRangeInParentItems.lowerBound - remaining) {
                return split.indexRangeInReversed.lowerBound - 1
            }
            break
        }
        let containsInRange = split.indexRangeInParentItems.contains(firstVisibleIndex)
        if containsInRange {
            if lastVisibleIndex > (split.indexRangeInParentItems.upperBound - remaining) {
                return split.indexRangeInReversed.upperBound
            }
            break
        }
    }
    return nil
}

func tryBlockAndSetLoadingMore(_ loadingMoreItems: Binding<Bool>, _ block: @escaping () async throws -> Void) async {
    do {
        await MainActor.run {
            loadingMoreItems.wrappedValue = true
        }
        try await block()
    } catch {
        logger.error("Error loading more items: \(error)")
    }
    await MainActor.run {
        loadingMoreItems.wrappedValue = false
    }
}
