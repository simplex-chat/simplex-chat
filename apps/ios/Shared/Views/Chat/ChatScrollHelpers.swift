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
    Task {
        try? await Task.sleep(nanoseconds: 2500_000000)
        if ItemsModel.shared.chatState.totalAfter == 0 || ChatModel.shared.chatId != chatInfo.id {
            return
        }
        await MainActor.run {
            loadingMoreItems.wrappedValue = true
        }
        await apiLoadMessages(chatInfo.chatType, chatInfo.apiId, ChatPagination.last(count: 50), ItemsModel.shared.chatState)
        await MainActor.run {
            loadingMoreItems.wrappedValue = false
        }
    }
}
