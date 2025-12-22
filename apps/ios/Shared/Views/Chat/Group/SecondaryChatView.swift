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
    @Environment(\.dismiss) var dismiss
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var chat: Chat
    @Binding var scrollToItemId: ChatItem.ID?

    var body: some View {
        if let im = chatModel.secondaryIM {
            ChatView(
                chat: chat,
                im: im,
                mergedItems: BoxedValue(MergedItems.create(im, [])),
                floatingButtonModel: FloatingButtonModel(im: im),
                scrollToItemId: $scrollToItemId
            )
            .modifier(BackButton(disabled: Binding.constant(false)) {
                chatModel.secondaryIM = nil
                dismiss()
            })
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
        scrollToItemId: Binding.constant(nil)
    )
}
