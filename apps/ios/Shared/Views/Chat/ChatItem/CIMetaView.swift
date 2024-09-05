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
    var invertedMaterial = false

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    var body: some View {
        if chatItem.isDeletedContent {
            chatItem.timestampText.font(.caption).foregroundColor(metaColor)
        } else {
            ZStack {
                ciMetaText(
                    chatItem.meta,
                    chatTTL: chat.chatInfo.timedMessagesTTL,
                    encrypted: chatItem.encryptedFile,
                    color: chatItem.meta.itemStatus.sndProgress == .partial
                        ? paleMetaColor
                        : metaColor,
                    colorMode: invertedMaterial
                        ? .invertedMaterial
                        : .normal,
                    showStatus: showStatus,
                    showEdited: showEdited,
                    showViaProxy: showSentViaProxy,
                    showTimesamp: showTimestamp
                ).invertedForegroundStyle(enabled: invertedMaterial)
                if invertedMaterial {
                    ciMetaText(
                        chatItem.meta,
                        chatTTL: chat.chatInfo.timedMessagesTTL,
                        encrypted: chatItem.encryptedFile,
                        colorMode: .colorOverride,
                        showStatus: showStatus,
                        showEdited: showEdited,
                        showViaProxy: showSentViaProxy,
                        showTimesamp: showTimestamp
                    )
                }
            }
        }
    }
}

enum MetaColorMode {
    // Renders provided colours
    case normal
    // Fully transparent meta - used for reserving space
    case transparent
    // Renders white on dark backgrounds and black on light ones
    case invertedMaterial
    // Only renders colors that differ from base
    case colorOverride

    func resolve(_ c: Color?, isOverride: Bool = false) -> Color? {
        switch self {
        case .normal: c
        case .transparent: .clear
        case .invertedMaterial: nil
        case .colorOverride: isOverride ? c : .clear
        }
    }

    var statusSpacer: Text {
        switch self {
        case .normal, .transparent: Text(Image(systemName: "circlebadge.fill"))
        case .invertedMaterial, .colorOverride: Text(" ").kerning(13)
        }
    }
}

func ciMetaText(
    _ meta: CIMeta,
    chatTTL: Int?,
    encrypted: Bool?,
    color: Color = .clear,
    primaryColor: Color = .accentColor,
    colorMode: MetaColorMode = .normal,
    showStatus: Bool = true,
    showEdited: Bool = true,
    showViaProxy: Bool,
    showTimesamp: Bool
) -> Text {
    var r = Text("")
    let resolved = colorMode.resolve(color)
    if showEdited, meta.itemEdited {
        r = r + statusIconText("pencil", resolved)
    }
    if meta.disappearing {
        r = r + statusIconText("timer", resolved).font(.caption2)
        let ttl = meta.itemTimed?.ttl
        if ttl != chatTTL {
            r = r + Text(shortTimeText(ttl)).foreground(resolved)
        }
        r = r + Text(" ")
    }
    if showViaProxy, meta.sentViaProxy == true {
        r = r + statusIconText("arrow.forward", resolved?.opacity(0.67)).font(.caption2)
    }
    if showStatus {
        if let (image, statusColor) = meta.itemStatus.statusIcon(color, primaryColor) {
            let metaColor = colorMode.resolve(statusColor, isOverride: statusColor != color)
            r = r + Text(image).foreground(metaColor) + Text(" ")
        } else if !meta.disappearing {
            r = r + colorMode.statusSpacer.foreground(.clear) + Text(" ")
        }
    }
    if let enc = encrypted {
        r = r + statusIconText(enc ? "lock" : "lock.open", resolved) + Text(" ")
    }
    if showTimesamp {
        r = r + meta.timestampText.foreground(resolved)
    }
    return r.font(.caption)
}

private func statusIconText(_ icon: String, _ color: Color?) -> Text {
    Text(Image(systemName: icon)).foreground(color)
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
