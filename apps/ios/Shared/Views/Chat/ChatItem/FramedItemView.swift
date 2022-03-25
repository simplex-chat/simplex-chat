//
//  FramedItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let sentColorLight = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.12)
private let sentColorDark = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.17)
private let sentQuoteColorLight = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.11)
private let sentQuoteColorDark = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.09)

struct FramedItemView: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    @State var msgWidth: CGFloat = 0

    var body: some View {
        let v = ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading, spacing: 0) {
                if let qi = chatItem.quotedItem {
                    MsgContentView(
                        content: qi,
                        sender: qi.sender
                    )
                    .lineLimit(3)
                    .font(.subheadline)
                    .padding(.vertical, 6)
                    .padding(.horizontal, 12)
                    .frame(minWidth: msgWidth, alignment: .leading)
                    .background(
                        chatItem.chatDir.sent
                        ? (colorScheme == .light ? sentQuoteColorLight : sentQuoteColorDark)
                        : Color(uiColor: .quaternarySystemFill)
                    )
                    .overlay(DetermineWidth())
                }

                if chatItem.formattedText == nil && isShortEmoji(chatItem.content.text) {
                    VStack {
                        emojiText(chatItem.content.text)
                        Text("")
                    }
                    .padding(.vertical, 6)
                    .padding(.horizontal, 12)
                    .overlay(DetermineWidth())
                    .frame(minWidth: msgWidth, alignment: .center)
                    .padding(.bottom, 2)
                } else {
                    MsgContentView(
                        content: chatItem.content,
                        formattedText: chatItem.formattedText,
                        sender: chatItem.memberDisplayName,
                        metaText: chatItem.timestampText,
                        edited: chatItem.meta.itemEdited
                    )
                    .padding(.vertical, 6)
                    .padding(.horizontal, 12)
                    .overlay(DetermineWidth())
                    .frame(minWidth: 0, alignment: .leading)
                    .textSelection(.enabled)
                }
            }
            
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
                .padding(.bottom, 6)
                .overlay(DetermineWidth())
        }
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .onPreferenceChange(DetermineWidth.Key.self) { msgWidth = $0 }

        switch chatItem.meta.itemStatus {
        case .sndErrorAuth:
            v.onTapGesture { msgDeliveryError("Most likely this contact has deleted the connection with you.") }
        case let .sndError(agentError):
            v.onTapGesture { msgDeliveryError("Unexpected error: \(String(describing: agentError))") }
        default: v
        }
    }

    private func msgDeliveryError(_ err: String) {
        AlertManager.shared.showAlertMsg(
            title: "Message delivery error",
            message: err
        )
    }
}

func chatItemFrameColor(_ ci: ChatItem, _ colorScheme: ColorScheme) -> Color {
    ci.chatDir.sent
    ? (colorScheme == .light ? sentColorLight : sentColorDark)
    : Color(uiColor: .tertiarySystemGroupedBackground)
}

struct FramedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            FramedItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"))
            FramedItemView(chatItem: ChatItem.getSample(1, .groupRcv(groupMember: GroupMember.sampleData), .now, "hello", quotedItem: CIQuote.getSample(1, .now, "hi", chatDir: .directSnd)))
            FramedItemView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent, quotedItem: CIQuote.getSample(1, .now, "hi", chatDir: .directRcv)))
            FramedItemView(chatItem: ChatItem.getSample(2, .directSnd, .now, "👍", .sndSent, quotedItem: CIQuote.getSample(1, .now, "Hello too", chatDir: .directRcv)))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this covers -"))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this text has the time on the same line "))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "https://simplex.chat"))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "chaT@simplex.chat"))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}

struct FramedItemViewEdited_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            FramedItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello", .sndSent, false, true))
            FramedItemView(chatItem: ChatItem.getSample(1, .groupRcv(groupMember: GroupMember.sampleData), .now, "hello", quotedItem: CIQuote.getSample(1, .now, "hi", chatDir: .directSnd), false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent, quotedItem: CIQuote.getSample(1, .now, "hi", chatDir: .directRcv), false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directSnd, .now, "👍", .sndSent, quotedItem: CIQuote.getSample(1, .now, "Hello too", chatDir: .directRcv), false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this covers -", .rcvRead, false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this text has the time on the same line ", .rcvRead, false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "https://simplex.chat", .rcvRead, false, true))
            FramedItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "chaT@simplex.chat", .rcvRead, false, true))
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
