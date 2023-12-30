//
//  MsgContentView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let uiLinkColor = UIColor(red: 0, green: 0.533, blue: 1, alpha: 1)

private let noTyping = Text("   ")

private let typingIndicators: [Text] = [
    (typing(.black) + typing() + typing()),
    (typing(.bold) + typing(.black) + typing()),
    (typing() + typing(.bold) + typing(.black)),
    (typing() + typing() + typing(.bold))
]

private func typing(_ w: Font.Weight = .light) -> Text {
    Text(".").fontWeight(w)
}

struct MsgContentView: View {
    @ObservedObject var chat: Chat
    var text: String
    var formattedText: [FormattedText]? = nil
    var sender: String? = nil
    var meta: CIMeta? = nil
    var rightToLeft = false
    var showSecrets: Bool
    @State private var typingIdx = 0
    @State private var timer: Timer?

    var body: some View {
        if meta?.isLive == true {
            msgContentView()
            .onAppear { switchTyping() }
            .onDisappear(perform: stopTyping)
            .onChange(of: meta?.isLive, perform: switchTyping)
            .onChange(of: meta?.recent, perform: switchTyping)
        } else {
            msgContentView()
        }
    }

    private func switchTyping(_: Bool? = nil) {
        if let meta = meta, meta.isLive && meta.recent {
            timer = timer ?? Timer.scheduledTimer(withTimeInterval: 0.25, repeats: true) { _ in
                typingIdx = (typingIdx + 1) % typingIndicators.count
            }
        } else {
            stopTyping()
        }
    }

    private func stopTyping() {
        timer?.invalidate()
        timer = nil
    }

    private func msgContentView() -> Text {
        var v = messageText(text, formattedText, sender, showSecrets: showSecrets)
        if let mt = meta {
            if mt.isLive {
                v = v + typingIndicator(mt.recent)
            }
            v = v + reserveSpaceForMeta(mt)
        }
        return v
    }

    private func typingIndicator(_ recent: Bool) -> Text {
        return (recent ? typingIndicators[typingIdx] : noTyping)
            .font(.body.monospaced())
            .kerning(-2)
            .foregroundColor(.secondary)
    }

    private func reserveSpaceForMeta(_ mt: CIMeta) -> Text {
        (rightToLeft ? Text("\n") : Text("   ")) + ciMetaText(mt, chatTTL: chat.chatInfo.timedMessagesTTL, encrypted: nil, transparent: true)
    }
}

func messageText(_ text: String, _ formattedText: [FormattedText]?, _ sender: String?, icon: String? = nil, preview: Bool = false, showSecrets: Bool) -> Text {
    let s = text
    var res: Text
    if let ft = formattedText, ft.count > 0 && ft.count <= 200 {
        res = formatText(ft[0], preview, showSecret: showSecrets)
        var i = 1
        while i < ft.count {
            res = res + formatText(ft[i], preview, showSecret: showSecrets)
            i = i + 1
        }
    } else {
        res = Text(s)
    }

    if let i = icon {
        res = Text(Image(systemName: i)).foregroundColor(Color(uiColor: .tertiaryLabel)) + Text(" ") + res
    }

    if let s = sender {
        let t = Text(s)
        return (preview ? t : t.fontWeight(.medium)) + Text(": ") + res
    } else {
        return res
    }
}

private func formatText(_ ft: FormattedText, _ preview: Bool, showSecret: Bool) -> Text {
    let t = ft.text
    if let f = ft.format {
        switch (f) {
        case .bold: return Text(t).bold()
        case .italic: return Text(t).italic()
        case .strikeThrough: return Text(t).strikethrough()
        case .snippet: return Text(t).font(.body.monospaced())
        case .secret: return
            showSecret
            ? Text(t)
            : Text(AttributedString(t, attributes: AttributeContainer([
                .foregroundColor: UIColor.clear as Any,
                .backgroundColor: UIColor.secondarySystemFill as Any
            ])))
        case let .colored(color): return Text(t).foregroundColor(color.uiColor)
        case .uri: return linkText(t, t, preview, prefix: "")
        case let .simplexLink(linkType, simplexUri, smpHosts):
            switch privacySimplexLinkModeDefault.get() {
            case .description: return linkText(simplexLinkText(linkType, smpHosts), simplexUri, preview, prefix: "")
            case .full: return linkText(t, simplexUri, preview, prefix: "")
            case .browser: return linkText(t, simplexUri, preview, prefix: "")
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

func simplexLinkText(_ linkType: SimplexLinkType, _ smpHosts: [String]) -> String {
    linkType.description + " " + "(via \(smpHosts.first ?? "?"))"
}

struct MsgContentView_Previews: PreviewProvider {
    static var previews: some View {
        let chatItem = ChatItem.getSample(1, .directSnd, .now, "hello")
        return MsgContentView(
            chat: Chat.sampleData,
            text: chatItem.text,
            formattedText: chatItem.formattedText,
            sender: chatItem.memberDisplayName,
            meta: chatItem.meta,
            showSecrets: false
        )
        .environmentObject(Chat.sampleData)
    }
}
