//
//  ChatItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 30/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChatItemView: View {
    var chatInfo: ChatInfo
    var chatItem: ChatItem
    var showMember = false
    var maxWidth: CGFloat = .infinity

    var body: some View {
        switch chatItem.content {
        case .sndMsgContent: contentItemView()
        case .rcvMsgContent: contentItemView()
        case .sndDeleted: deletedItemView()
        case .rcvDeleted: deletedItemView()
        case let .sndCall(status, duration): callItemView(status, duration)
        case let .rcvCall(status, duration): callItemView(status, duration)
        case .rcvIntegrityError: IntegrityErrorItemView(chatItem: chatItem, showMember: showMember)
        case let .rcvGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case let .sndGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case .rcvGroupEvent: groupEventItemView()
        case .sndGroupEvent: groupEventItemView()
        }
    }

    @ViewBuilder private func contentItemView() -> some View {
        if (chatItem.quotedItem == nil && chatItem.file == nil && isShortEmoji(chatItem.content.text)) {
            EmojiItemView(chatItem: chatItem)
        } else {
            FramedItemView(chatItem: chatItem, showMember: showMember, maxWidth: maxWidth)
        }
    }

    private func deletedItemView() -> some View {
        DeletedItemView(chatItem: chatItem, showMember: showMember)
    }

    private func callItemView(_ status: CICallStatus, _ duration: Int) -> some View {
        CICallItemView(chatInfo: chatInfo, chatItem: chatItem, status: status, duration: duration)
    }

    private func groupInvitationItemView(_ groupInvitation: CIGroupInvitation, _ memberRole: GroupMemberRole) -> some View {
        CIGroupInvitationView(chatItem: chatItem, groupInvitation: groupInvitation, memberRole: memberRole, chatIncognito: chatInfo.incognito)
    }

    private func groupEventItemView() -> some View {
        CIGroupEventView(chatItem: chatItem)
    }
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getDeletedContentSample())
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
