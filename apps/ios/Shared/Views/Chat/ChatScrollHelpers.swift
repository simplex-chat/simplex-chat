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

func preloadItems(_ mergedItems: MergedItems, _ state: ListState, _ ignoreLoadingRequests: Binding<[Int64]>, _ loadItems: @escaping (ChatPagination) -> Void) {
    let remaining = ChatPagination.UNTIL_PRELOAD_COUNT
    let firstVisibleIndex = state.firstVisibleItemIndex

    preloadItemsAfter()

    func preloadItemsAfter() {
        let items: [ChatItem] = ItemsModel.shared.reversedChatItems.reversed()
        let splits = mergedItems.splits
        let split = splits.last(where: { $0.indexRangeInParentItems.contains(firstVisibleIndex) })
        // we're inside a splitRange (top --- [end of the splitRange --- we're here --- start of the splitRange] --- bottom)
        logger.debug("LALAL PRELOAD AFTER \(String(describing: split)) \(firstVisibleIndex) \(items.count - 1 - (split?.indexRangeInReversed.lowerBound ?? -1))")
        if let split, split.indexRangeInParentItems.lowerBound + remaining > firstVisibleIndex {
            let index = items.count - 1 - split.indexRangeInReversed.lowerBound
            if index >= 0 {
                let loadFromItemId = items[index].id
                loadItems(.after(chatItemId: loadFromItemId, count: ChatPagination.PRELOAD_COUNT))
            }

        }
    }

}
