//
//  MsgContentView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let uiLinkColor = UIColor(red: 0, green: 0.533, blue: 1, alpha: 1)

struct MsgContentView: View {
    var text: String
    var formattedText: [FormattedText]? = nil
    var sender: String? = nil
    var metaText: Text? = nil
    var edited = false
    var rightToLeft = false

    var body: some View {
        let v = messageText(text, formattedText, sender)
        if let mt = metaText {
            return v + reserveSpaceForMeta(mt, edited)
        } else {
            return v
        }
    }
    
    private func reserveSpaceForMeta(_ meta: Text, _ edited: Bool) -> Text {
        let reserve = rightToLeft ? "\n" : edited ? "          " : "      "
        return (Text(reserve) + meta)
            .font(.caption)
            .foregroundColor(.clear)
    }
}

func messageText(_ text: String, _ formattedText: [FormattedText]?, _ sender: String?, preview: Bool = false) -> Text {
    let s = text
    var res: Text
    if let ft = formattedText, ft.count > 0 {
        res = formatText(ft[0], preview)
        var i = 1
        while i < ft.count {
            res = res + formatText(ft[i], preview)
            i = i + 1
        }
    } else {
        res = Text(s)
    }

    if let s = sender {
        let t = Text(s)
        return (preview ? t : t.fontWeight(.medium)) + Text(": ") + res
    } else {
        return res
    }
}

private func formatText(_ ft: FormattedText, _ preview: Bool) -> Text {
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
        case let .simplexLink(linkType, simplexUri, trustedUri, smpHosts):
            switch privacySimplexLinkModeDefault.get() {
            case .description: return linkText(simplexLinkText(linkType, smpHosts), simplexUri, preview, prefix: "")
            case .full: return linkText(t, simplexUri, preview, prefix: "")
            case .browser: return trustedUri
                                    ? linkText(t, t, preview, prefix: "")
                                    : linkText(t, t, preview, prefix: "", color: .red, uiColor: .red)
            }
        case .email: return linkText(t, t, preview, prefix: "mailto:")
        case .phone: return linkText(t, t.replacingOccurrences(of: " ", with: ""), preview, prefix: "tel:")
        }
    } else {
        return Text(t)
    }
}

private func linkText(_ s: String, _ link: String, _ preview: Bool, prefix: String, color: Color = Color(uiColor: uiLinkColor), uiColor: UIColor = uiLinkColor) -> Text {
    preview
    ? Text(s).foregroundColor(color).underline(color: color)
    : Text(AttributedString(s, attributes: AttributeContainer([
        .link: NSURL(string: prefix + link) as Any,
        .foregroundColor: uiColor as Any
    ]))).underline()
}

private func simplexLinkText(_ linkType: SimplexLinkType, _ smpHosts: [String]) -> String {
    linkType.description + " " + "(via \(smpHosts.first ?? "?"))"
}

struct MsgContentView_Previews: PreviewProvider {
    static var previews: some View {
        let chatItem = ChatItem.getSample(1, .directSnd, .now, "hello")
        return MsgContentView(
            text: chatItem.text,
            formattedText: chatItem.formattedText,
            sender: chatItem.memberDisplayName,
            metaText: chatItem.timestampText
        )
    }
}
