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
    @State var scrollProxy: ScrollViewProxy? = nil
    @Binding var revealed: Bool

    var body: some View {
        let ci = chatItem
        if ci.quotedItem == nil && !ci.meta.itemDeleted {
            if let mc = ci.content.msgContent, mc.isText && isShortEmoji(ci.content.text) {
                EmojiItemView(chatItem: ci)
            } else if ci.content.text.isEmpty, case let .voice(_, duration) = ci.content.msgContent {
                CIVoiceView(chatItem: ci, recordingFile: ci.file, duration: duration)
            } else if ci.content.msgContent == nil {
                ChatItemContentView(chatInfo: chatInfo, chatItem: chatItem, showMember: showMember, msgContentView: { Text(ci.text) }) // msgContent is unreachable branch in this case
            } else {
                framedItemView()
            }
        } else {
            framedItemView()
        }
    }

    private func framedItemView() -> some View {
        FramedItemView(chatInfo: chatInfo, chatItem: chatItem, showMember: showMember, maxWidth: maxWidth, scrollProxy: scrollProxy, revealed: $revealed)
    }
}

struct ChatItemContentView<Content: View>: View {
    var chatInfo: ChatInfo
    var chatItem: ChatItem
    var showMember: Bool
    var msgContentView: () -> Content

    var body: some View {
        switch chatItem.content {
        case .sndMsgContent: msgContentView()
        case .rcvMsgContent: msgContentView()
        case .sndDeleted: deletedItemView()
        case .rcvDeleted: deletedItemView()
        case let .sndCall(status, duration): callItemView(status, duration)
        case let .rcvCall(status, duration): callItemView(status, duration)
        case .rcvIntegrityError: IntegrityErrorItemView(chatItem: chatItem, showMember: showMember)
        case let .rcvGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case let .sndGroupInvitation(groupInvitation, memberRole): groupInvitationItemView(groupInvitation, memberRole)
        case .rcvGroupEvent: eventItemView()
        case .sndGroupEvent: eventItemView()
        case .rcvConnEvent: eventItemView()
        case .sndConnEvent: eventItemView()
        case let .rcvChatFeature(feature, enabled): chatFeatureView(feature, enabled.iconColor)
        case let .sndChatFeature(feature, enabled): chatFeatureView(feature, enabled.iconColor)
        case let .rcvGroupFeature(feature, preference): chatFeatureView(feature, preference.enable.iconColor)
        case let .sndGroupFeature(feature, preference): chatFeatureView(feature, preference.enable.iconColor)
        case let .rcvChatFeatureRejected(feature): chatFeatureView(feature, .red)
        case let .rcvGroupFeatureRejected(feature): chatFeatureView(feature, .red)
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

    private func eventItemView() -> some View {
        CIEventView(chatItem: chatItem)
    }

    private func chatFeatureView(_ feature: Feature, _ iconColor: Color) -> some View {
        CIChatFeatureView(chatItem: chatItem, feature: feature, iconColor: iconColor)
    }
}

struct ChatItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(1, .directSnd, .now, "🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getSample(2, .directRcv, .now, "🙂🙂🙂🙂🙂🙂"), revealed: Binding.constant(false))
            ChatItemView(chatInfo: ChatInfo.sampleData.direct, chatItem: ChatItem.getDeletedContentSample(), revealed: Binding.constant(false))
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
