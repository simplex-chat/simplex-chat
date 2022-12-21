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
    @EnvironmentObject var chat: Chat
    var chatItem: ChatItem
    var metaColor = Color.secondary

    var body: some View {
        if chatItem.isDeletedContent {
            chatItem.timestampText.font(.caption).foregroundColor(metaColor)
        } else {
            ciMetaText(chatItem.meta, chatTTL: chat.chatInfo.timedMessagesTTL, color: metaColor)
        }
    }

    private func statusImage(_ icon: String, _ color: Color, _ maxHeight: CGFloat = 8) -> some View {
        Image(systemName: icon)
            .resizable()
            .aspectRatio(contentMode: .fit)
            .foregroundColor(color)
            .frame(maxHeight: maxHeight)
    }
}

func ciMetaText(_ meta: CIMeta, chatTTL: Int?, color: Color = .clear) -> Text {
    var r = Text("")
    if meta.itemEdited {
        r = r + statusIconText("pencil", color)
    }
    if meta.disappearing {
        r = r + statusIconText("timer", color).font(.caption2)
        let ttl = meta.itemTimed?.ttl
        if ttl != chatTTL {
            r = r + Text(TimedMessagesPreference.shortTtlText(ttl)).foregroundColor(color)
        }
        r = r + Text(" ")
    }
    if let (icon, color) = meta.statusIcon(color) {
        r = r + statusIconText(icon, color) + Text(" ")
    } else if !meta.disappearing {
        r = r + statusIconText("circlebadge.fill", .clear) + Text(" ")
    }
    return (r + meta.timestampText.foregroundColor(color)).font(.caption)
}

private func statusIconText(_ icon: String, _ color: Color) -> Text {
    Text(Image(systemName: icon)).foregroundColor(color)
}

struct CIMetaView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            CIMetaView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent))
            CIMetaView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent, false, true))
            CIMetaView(chatItem: ChatItem.getDeletedContentSample())
        }
        .previewLayout(.fixed(width: 360, height: 100))
        .environmentObject(Chat.sampleData)
    }
}
