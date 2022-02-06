//
//  TextItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct TextItemView: View {
    var chatItem: ChatItem
    var width: CGFloat
    private let codeFont = Font.custom("Courier", size: UIFont.preferredFont(forTextStyle: .body).pointSize)

    var body: some View {
        let sent = chatItem.chatDir.sent
        let minWidth = min(200, width)
        let maxWidth = min(300, width * 0.78)
        return VStack {
            messageText(chatItem.content.text, sent: sent)
                .padding(.top, 8)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .leading)
                .foregroundColor(sent ? .white : .primary)
                .textSelection(.enabled)
            Text(getDateFormatter().string(from: chatItem.meta.itemTs))
                .font(.caption)
                .foregroundColor(sent ? .white : .secondary)
                .padding(.bottom, 8)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .trailing)
        }
        .background(sent ? .blue : Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(10)
        .padding(.horizontal)
        .frame(
            minWidth: 200,
            maxWidth: .infinity,
            minHeight: 0,
            maxHeight: .infinity,
            alignment: sent ? .trailing : .leading
        )
    }

    private func messageText(_ s: String, sent: Bool = false) -> Text {
        if s == "" { return Text("") }
        let parts = s.split(separator: " ")
        print(parts)
        var res = wordToText(parts[0], sent)
        var i = 1
        while i < parts.count {
            res = res + Text(" ") + wordToText(parts[i], sent)
            i = i + 1
        }
        return res
    }

    private func wordToText(_ s: String.SubSequence, _ sent: Bool) -> Text {
        switch true {
        case s.starts(with: "http://") || s.starts(with: "https://"):
            let str = String(s)
            return Text(AttributedString(str, attributes: AttributeContainer([
                .link: NSURL(string: str) as Any,
                .foregroundColor: (sent ? UIColor.white : nil) as Any
            ]))).underline()
        default:
            if (s.count > 1) {
                switch true {
                case s.first == "*" && s.last == "*": return mdText(s).bold()
                case s.first == "_" && s.last == "_": return mdText(s).italic()
                case s.first == "+" && s.last == "+": return mdText(s).underline()
                case s.first == "~" && s.last == "~": return mdText(s).strikethrough()
                default: return Text(s)
                }
            } else {
                return Text(s)
            }
        }
    }

    private func mdText(_ s: String.SubSequence) -> Text {
        Text(s[s.index(s.startIndex, offsetBy: 1)..<s.index(s.endIndex, offsetBy: -1)])
    }
}

struct TextItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group{
            TextItemView(chatItem: chatItemSample(1, .directSnd, .now, "hello"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directSnd, .now, "https://simplex.chat"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "hello there too"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "https://simplex.chat"), width: 360)
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
