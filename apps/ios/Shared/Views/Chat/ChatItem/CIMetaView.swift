//
//  CIMetaView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 11/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIMetaView: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    @Environment(\.showTimestamp) var showTimestamp: Bool
    var chatItem: ChatItem
    var metaColor: Color
    var paleMetaColor = Color(UIColor.tertiaryLabel)
    var showStatus = true
    var showEdited = true

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    var body: some View {
        if chatItem.isDeletedContent {
            chatItem.timestampText.font(.caption).foregroundColor(metaColor)
        } else {
            ciMetaText(
                chatItem.meta,
                chatTTL: chat.chatInfo.timedMessagesTTL,
                encrypted: chatItem.encryptedFile,
                color: chatItem.meta.itemStatus.sndProgress == .partial
                    ? paleMetaColor
                    : metaColor,
                showStatus: showStatus,
                showEdited: showEdited,
                showViaProxy: showSentViaProxy,
                showTimesamp: showTimestamp
            )
        }
    }
}

func ciMetaText(
    _ meta: CIMeta,
    chatTTL: Int?,
    encrypted: Bool?,
    color: Color = .clear,
    primaryColor: Color = .accentColor,
    transparent: Bool = false,
    showStatus: Bool = true,
    showEdited: Bool = true,
    showViaProxy: Bool,
    showTimesamp: Bool
) -> Text {
    var r = Text("")
    if showEdited, meta.itemEdited {
        r = r + statusIconText("pencil", color)
    }
    if meta.disappearing {
        r = r + statusIconText("timer", color).font(.caption2)
        let ttl = meta.itemTimed?.ttl
        if ttl != chatTTL {
            r = r + Text(shortTimeText(ttl)).foregroundColor(color)
        }
        r = r + Text(" ")
    }
    if showViaProxy, meta.sentViaProxy == true {
        r = r + statusIconText("arrow.forward", color.opacity(0.67)).font(.caption2)
    }
    if showStatus {
        if let (image, statusColor) = meta.itemStatus.statusIcon(color, primaryColor) {
            r = r + Text(image).foregroundColor(transparent ? .clear : statusColor) + Text(" ")
        } else if !meta.disappearing {
            r = r + statusIconText("circlebadge.fill", .clear) + Text(" ")
        }
    }
    if let enc = encrypted {
        r = r + statusIconText(enc ? "lock" : "lock.open", color) + Text(" ")
    }
    if showTimesamp {
        r = r + meta.timestampText.foregroundColor(color)
    }
    return r.font(.caption)
}

private func statusIconText(_ icon: String, _ color: Color) -> Text {
    Text(Image(systemName: icon)).foregroundColor(color)
}

struct CIMetaView_Previews: PreviewProvider {
    static let metaColor = Color.secondary
    static var previews: some View {
        Group {
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .partial)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .ok, sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .ok, sndProgress: .partial)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .badMsgHash, sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .complete), itemEdited: true), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getDeletedContentSample(), metaColor: metaColor)
        }
        .previewLayout(.fixed(width: 360, height: 100))
    }
}
