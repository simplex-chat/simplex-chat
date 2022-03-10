//
//  TextItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let sentColorLight = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.12)
private let sentColorDark = Color(.sRGB, red: 0.27, green: 0.72, blue: 1, opacity: 0.17)
private let uiLinkColor = UIColor(red: 0, green: 0.533, blue: 1, alpha: 1)
private let linkColor = Color(uiColor: uiLinkColor)

struct TextItemView: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    var width: CGFloat
    private let codeFont = Font.custom("Courier", size: UIFont.preferredFont(forTextStyle: .body).pointSize)

    var body: some View {
        let sent = chatItem.chatDir.sent
        let maxWidth = width * 0.78

        return ZStack(alignment: .bottomTrailing) {
            (messageText(chatItem) + reserveSpaceForMeta(chatItem.timestampText))
                .padding(.vertical, 6)
                .padding(.horizontal, 12)
                .frame(minWidth: 0, alignment: .leading)
                .textSelection(.enabled)

            CIMetaView(chatItem: chatItem)
                .padding(.trailing, 12)
                .padding(.bottom, 6)
        }
        .background(
            sent
            ? (colorScheme == .light ? sentColorLight : sentColorDark)
            : Color(uiColor: .tertiarySystemGroupedBackground)
        )
        .cornerRadius(18)
        .padding(.horizontal)
        .frame(
            maxWidth: maxWidth,
            maxHeight: .infinity,
            alignment: sent ? .trailing : .leading
        )
        .onTapGesture {
            switch chatItem.meta.itemStatus {
            case .sndErrorAuth: msgDeliveryError("Most likely this contact has deleted the connection with you.")
            case let .sndError(agentError): msgDeliveryError("Unexpected error: \(String(describing: agentError))")
            default: return
            }
        }
    }

    private func reserveSpaceForMeta(_ meta: Text) -> Text {
       (Text("      ") + meta)
           .font(.caption)
           .foregroundColor(.clear)
    }

    private func msgDeliveryError(_ err: String) {
        AlertManager.shared.showAlertMsg(
            title: "Message delivery error",
            message: err
        )
    }
}

func messageText(_ chatItem: ChatItem, preview: Bool = false) -> Text {
    let s = chatItem.content.text
    var res: Text
    if let ft = chatItem.formattedText, ft.count > 0 {
        res = formattedText(ft[0], preview)
        var i = 1
        while i < ft.count {
            res = res + formattedText(ft[i], preview)
            i = i + 1
        }
    } else {
        res = Text(s)
    }

    if case let .groupRcv(groupMember) = chatItem.chatDir {
        let m = Text(groupMember.memberProfile.displayName)
        return (preview ? m : m.font(.headline)) + Text(": ") + res
    } else {
        return res
    }
}

private func formattedText(_ ft: FormattedText, _ preview: Bool) -> Text {
    let t = ft.text
    if let f = ft.format {
        switch (f) {
        case .bold: return Text(t).bold()
        case .italic: return Text(t).italic()
        case .strikeThrough: return Text(t).strikethrough()
        case .snippet: return Text(t).font(.body.monospaced())
        case .secret: return Text(t).foregroundColor(.clear).underline(color: .primary)
        case let .colored(color): return Text(t).foregroundColor(color.uiColor)
        case .uri: return linkText(t, t, preview, prefix: "")
        case .email: return linkText(t, t, preview, prefix: "mailto:")
        case .phone: return linkText(t, t.replacingOccurrences(of: " ", with: ""), preview, prefix: "tel:")
        }
    } else {
        return Text(t)
    }
}

private func linkText(_ s: String, _ link: String,
                      _ preview: Bool, prefix: String) -> Text {
    preview
    ? Text(s).foregroundColor(linkColor).underline(color: linkColor)
    : Text(AttributedString(s, attributes: AttributeContainer([
        .link: NSURL(string: prefix + link) as Any,
        .foregroundColor: uiLinkColor as Any
    ]))).underline()
}

struct TextItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            TextItemView(chatItem: ChatItem.getSample(1, .directSnd, .now, "hello"), width: 360)
            TextItemView(chatItem: ChatItem.getSample(1, .groupRcv(groupMember: GroupMember.sampleData), .now, "hello"), width: 360)
            TextItemView(chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent), width: 360)
            TextItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this covers -"), width: 360)
            TextItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "hello there too!!! this text has the time on the same line "), width: 360)
            TextItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "https://simplex.chat"), width: 360)
            TextItemView(chatItem: ChatItem.getSample(2, .directRcv, .now, "chaT@simplex.chat"), width: 360)
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
