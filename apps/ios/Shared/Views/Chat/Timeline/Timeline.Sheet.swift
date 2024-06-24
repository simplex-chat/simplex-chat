//
//  Timeline.Model.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 18/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

extension Timeline {
    enum Sheet: Identifiable {
        case itemInfo(ChatItem, ChatItemInfo)
        case memberInfo(GMember)
        case memberAdd(GroupInfo)
        case forward(ChatItem)

        var id: String {
            switch self {
            case .itemInfo: "itemInfo"
            case .memberInfo: "memberInfo"
            case .memberAdd: "memberAdd"
            case .forward: "forward"
            }
        }
    }

    struct SheetView: View {
        let sheet: Sheet
        let chat: Chat
        @Binding var composeState: ComposeState

        var body: some View {
            switch sheet {
            case let .itemInfo(chatItem, chatItemInfo):
                ChatItemInfoView(
                    ci: chatItem,
                    chatItemInfo: .constant(chatItemInfo)
                )
            case let .memberInfo(groupMember):
                if let groupInfo = chat.chatInfo.groupInfo {
                    GroupMemberInfoView(
                        groupInfo: groupInfo,
                        groupMember: groupMember
                    )
                }
            case let .memberAdd(groupInfo):
                AddGroupMembersView(
                    chat: chat,
                    groupInfo: groupInfo
                )
            case let .forward(chatItem):
                ChatItemForwardingView(
                    ci: chatItem,
                    fromChatInfo: chat.chatInfo,
                    composeState: $composeState
                )
            }
        }
    }
}
