//
//  SecondaryChatView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.04.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SecondaryChatView: View {
    @EnvironmentObject var chatModel: ChatModel
    var chat: Chat
    var im: ItemsModel

    var body: some View {
        ChatView(
            chat: chat,
            im: im,
            mergedItems: BoxedValue(MergedItems.create(im, [])),
            floatingButtonModel: FloatingButtonModel(im: im)
        )
        .onDisappear {
            chatModel.secondaryIM = nil
        }
    }
}

#Preview {
    SecondaryChatView(
        chat: Chat(
            chatInfo: .group(groupInfo: GroupInfo.sampleData, groupChatScope: .memberSupport(groupMember_: GroupMember.sampleData)),
            chatItems: [],
            chatStats: ChatStats()
        ),
        im: ItemsModel.shared
    )
}
