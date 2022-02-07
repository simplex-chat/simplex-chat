//
//  TextItemView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

private let emailRegex = try! NSRegularExpression(pattern: "^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)*$", options: .caseInsensitive)

private let phoneRegex = try! NSRegularExpression(pattern: "^\\+?[0-9\\.\\(\\)\\-]{7,20}$")

struct TextItemView: View {
    var chatItem: ChatItem
    var width: CGFloat
    private let codeFont = Font.custom("Courier", size: UIFont.preferredFont(forTextStyle: .body).pointSize)

    var body: some View {
        let sent = chatItem.chatDir.sent
        let minWidth = min(200, width)
        let maxWidth = min(300, width * 0.78)
        let meta = getDateFormatter().string(from: chatItem.meta.itemTs)

        return ZStack(alignment: .bottomTrailing) {
            (messageText(chatItem.content.text, sent: sent) + reserveSpaceForMeta(meta))
                .padding(.top, 6)
                .padding(.bottom, 7)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .leading)
                .foregroundColor(sent ? .white : .primary)
                .textSelection(.enabled)
            Text(meta)
                .font(.caption)
                .foregroundColor(sent ? Color(uiColor: .secondarySystemBackground) : .secondary)
                .padding(.bottom, 4)
                .padding(.horizontal, 12)
                .frame(minWidth: minWidth, maxWidth: maxWidth, alignment: .bottomTrailing)
        }
        .background(sent ? .blue : Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .padding(.horizontal)
        .frame(
            minWidth: 200,
            maxWidth: .infinity,
            minHeight: 0,
            maxHeight: .infinity,
            alignment: sent ? .trailing : .leading
        )
    }

    private func messageText(_ s: String, sent: Bool) -> Text {
        if s == "" { return Text("") }
        let parts = s.split(separator: " ")
        var res = wordToText(parts[0], sent)
        var i = 1
        while i < parts.count {
            res = res + Text(" ") + wordToText(parts[i], sent)
            i = i + 1
        }
        return res
    }

    private func reserveSpaceForMeta(_ meta: String) -> Text {
        Text(AttributedString("   \(meta)", attributes: AttributeContainer([
            .font: UIFont.preferredFont(forTextStyle: .caption1) as Any,
            .foregroundColor: UIColor.clear as Any,
        ])))
    }

    private func wordToText(_ s: String.SubSequence, _ sent: Bool) -> Text {
        let str = String(s)
        switch true {
        case s.starts(with: "http://") || s.starts(with: "https://"):
            return linkText(str, prefix: "", sent: sent)
        case match(str, emailRegex):
            return linkText(str, prefix: "mailto:", sent: sent)
        case match(str, phoneRegex):
            return linkText(str, prefix: "tel:", sent: sent)
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

    private func match(_ s: String, _ regex: NSRegularExpression) -> Bool {
        regex.firstMatch(in: s, options: [], range: NSRange(location: 0, length: s.count)) != nil
    }

    private func linkText(_ s: String, prefix: String, sent: Bool) -> Text {
        Text(AttributedString(s, attributes: AttributeContainer([
            .link: NSURL(string: prefix + s) as Any,
            .foregroundColor: (sent ? UIColor.white : nil) as Any
        ]))).underline()
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
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "hello there too!!! this covers -"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "hello there too!!! this text has the time on the same line "), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "https://simplex.chat"), width: 360)
            TextItemView(chatItem: chatItemSample(2, .directRcv, .now, "chaT@simplex.chat"), width: 360)
        }
        .previewLayout(.fixed(width: 360, height: 70))
    }
}
