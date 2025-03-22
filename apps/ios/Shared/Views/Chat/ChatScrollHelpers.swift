//
//  ChatScrollHelpers.swift
//  SimpleX (iOS)
//
//  Created by Stanislav Dmitrenko on 20.12.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

func loadLastItems(_ loadingMoreItems: Binding<Bool>, loadingBottomItems: Binding<Bool>, _ chat: Chat) async {
    await MainActor.run {
        loadingMoreItems.wrappedValue = true
        loadingBottomItems.wrappedValue = true
    }
    try? await Task.sleep(nanoseconds: 500_000000)
    if ChatModel.shared.chatId != chat.chatInfo.id {
        await MainActor.run {
            loadingMoreItems.wrappedValue = false
            loadingBottomItems.wrappedValue = false
        }
        return
    }
    await apiLoadMessages(chat.chatInfo.id, ChatPagination.last(count: 50), ItemsModel.shared.chatState)
    await MainActor.run {
        loadingMoreItems.wrappedValue = false
        loadingBottomItems.wrappedValue = false
    }
}

class PreloadState {
    static let shared = PreloadState()
    var prevFirstVisible: Int64 = Int64.min
    var prevItemsCount: Int = 0
    var preloading: Bool = false

    func clear() {
        prevFirstVisible = Int64.min
        prevItemsCount = 0
        preloading = false
    }
}

func preloadIfNeeded(
    _ allowLoadMoreItems: Binding<Bool>,
    _ ignoreLoadingRequests: Binding<Int64?>,
    _ listState: EndlessScrollView<MergedItem>.ListState,
    _ mergedItems: BoxedValue<MergedItems>,
    loadItems: @escaping (Bool, ChatPagination) async -> Bool,
    loadLastItems: @escaping () async -> Void
) {
    let state = PreloadState.shared
    guard !listState.isScrolling && !listState.isAnimatedScrolling,
          !state.preloading,
          listState.totalItemsCount > 0
    else {
        return
    }
    if state.prevFirstVisible != listState.firstVisibleItemId as! Int64 || state.prevItemsCount != mergedItems.boxedValue.indexInParentItems.count {
        state.preloading = true
        let allowLoadMore = allowLoadMoreItems.wrappedValue
        Task {
            defer { state.preloading = false }
            var triedToLoad = true
            await preloadItems(mergedItems.boxedValue, allowLoadMore, listState, ignoreLoadingRequests) { pagination in
                triedToLoad = await loadItems(false, pagination)
                return triedToLoad
            }
            if triedToLoad {
                state.prevFirstVisible = listState.firstVisibleItemId as! Int64
                state.prevItemsCount = mergedItems.boxedValue.indexInParentItems.count
            }
            // it's important to ask last items when the view is fully covered with items. Otherwise, visible items from one
            // split will be merged with last items and position of scroll will change unexpectedly.
            if listState.itemsCanCoverScreen && !ItemsModel.shared.lastItemsLoaded {
                await loadLastItems()
            }
        }
    } else if listState.itemsCanCoverScreen && !ItemsModel.shared.lastItemsLoaded {
        state.preloading = true
        Task {
            defer { state.preloading = false }
            await loadLastItems()
        }
    }
}

func preloadItems(
    _ mergedItems: MergedItems,
    _ allowLoadMoreItems: Bool,
    _ listState: EndlessScrollView<MergedItem>.ListState,
    _ ignoreLoadingRequests: Binding<Int64?>,
    _ loadItems: @escaping (ChatPagination) async -> Bool) 
async {
    let allowLoad = allowLoadMoreItems || mergedItems.items.count == listState.lastVisibleItemIndex + 1
    let remaining = ChatPagination.UNTIL_PRELOAD_COUNT
    let firstVisibleIndex = listState.firstVisibleItemIndex

    if !(await preloadItemsBefore()) {
        await preloadItemsAfter()
    }

    func preloadItemsBefore() async -> Bool {
        let splits = mergedItems.splits
        let lastVisibleIndex = listState.lastVisibleItemIndex
        var lastIndexToLoadFrom: Int? = findLastIndexToLoadFromInSplits(firstVisibleIndex, lastVisibleIndex, remaining, splits)
        let items: [ChatItem] = ItemsModel.shared.reversedChatItems.reversed()
        if splits.isEmpty && !items.isEmpty && lastVisibleIndex > mergedItems.items.count - remaining {
            lastIndexToLoadFrom = items.count - 1
        }
        let loadFromItemId: Int64?
        if allowLoad, let lastIndexToLoadFrom {
            let index = items.count - 1 - lastIndexToLoadFrom
            loadFromItemId = index >= 0 ? items[index].id : nil
        } else {
            loadFromItemId = nil
        }
        guard let loadFromItemId, ignoreLoadingRequests.wrappedValue != loadFromItemId else {
            return false
        }
        let sizeWas = items.count
        let firstItemIdWas = items.first?.id
        let triedToLoad = await loadItems(ChatPagination.before(chatItemId: loadFromItemId, count: ChatPagination.PRELOAD_COUNT))
        if triedToLoad && sizeWas == ItemsModel.shared.reversedChatItems.count && firstItemIdWas == ItemsModel.shared.reversedChatItems.last?.id {
            ignoreLoadingRequests.wrappedValue = loadFromItemId
            return false
        }
        return triedToLoad
    }

    func preloadItemsAfter() async {
        let splits = mergedItems.splits
        let split = splits.last(where: { $0.indexRangeInParentItems.contains(firstVisibleIndex) })
        // we're inside a splitRange (top --- [end of the splitRange --- we're here --- start of the splitRange] --- bottom)
        let reversedItems: [ChatItem] = ItemsModel.shared.reversedChatItems
        if let split, split.indexRangeInParentItems.lowerBound + remaining > firstVisibleIndex {
            let index = split.indexRangeInReversed.lowerBound
            if index >= 0 {
                let loadFromItemId = reversedItems[index].id
                _ = await loadItems(ChatPagination.after(chatItemId: loadFromItemId, count: ChatPagination.PRELOAD_COUNT))
            }
        }
    }
}

func oldestPartiallyVisibleListItemInListStateOrNull(_ listState: EndlessScrollView<MergedItem>.ListState) -> ListItem? {
    if listState.lastVisibleItemIndex < listState.items.count {
        return listState.items[listState.lastVisibleItemIndex].oldest()
    } else {
        return listState.items.last?.oldest()
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

/// Disable animation on iOS 15
func withConditionalAnimation<Result>(
    _ animation: Animation? = .default,
    _ body: () throws -> Result
) rethrows -> Result {
    if #available(iOS 16.0, *) {
        try withAnimation(animation, body)
    } else {
        try body()
    }
}
