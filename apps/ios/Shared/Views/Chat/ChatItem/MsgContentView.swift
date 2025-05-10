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

private func typing(_ theme: AppTheme, _ descr: UIFontDescriptor, _ ws: [UIFont.Weight]) -> NSMutableAttributedString {
    let res = NSMutableAttributedString()
    for w in ws {
        res.append(NSAttributedString(string: ".", attributes: [
            .font: UIFont.monospacedSystemFont(ofSize: descr.pointSize, weight: w),
            .kern: -2 as NSNumber,
            .foregroundColor: UIColor(theme.colors.secondary)
        ]))
    }
    return res
}

struct MsgContentView: View {
    @ObservedObject var chat: Chat
    @Environment(\.showTimestamp) var showTimestamp: Bool
    @EnvironmentObject var theme: AppTheme
    var text: String
    var formattedText: [FormattedText]? = nil
    var textStyle: UIFont.TextStyle
    var sender: String? = nil
    var meta: CIMeta? = nil
    var mentions: [String: CIMention]? = nil
    var userMemberId: String? = nil
    var rightToLeft = false
    var prefix: NSAttributedString? = nil
    @State private var showSecrets: Set<Int> = []
    @State private var typingIdx = 0
    @State private var timer: Timer?
    @State private var typingIndicators: [NSAttributedString] = []
    @State private var noTyping = NSAttributedString(string: "   ")
    @State private var phase: CGFloat = 0

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    var body: some View {
        let v = msgContentView()
        if meta?.isLive == true {
            v.onAppear {
                let descr = UIFontDescriptor.preferredFontDescriptor(withTextStyle: .body)
                noTyping = NSAttributedString(string: "   ", attributes: [
                    .font: UIFont.monospacedSystemFont(ofSize: descr.pointSize, weight: .regular),
                    .kern: -2 as NSNumber,
                    .foregroundColor: UIColor(theme.colors.secondary)
                ])
                switchTyping()
            }
            .onDisappear(perform: stopTyping)
            .onChange(of: meta?.isLive, perform: switchTyping)
            .onChange(of: meta?.recent, perform: switchTyping)
        } else {
            v
        }
    }

    private func switchTyping(_: Bool? = nil) {
        if let meta = meta, meta.isLive && meta.recent {
            if typingIndicators.isEmpty {
                let descr = UIFontDescriptor.preferredFontDescriptor(withTextStyle: .body)
                typingIndicators = [
                    typing(theme, descr, [.black, .light, .light]),
                    typing(theme, descr, [.bold, .black, .light]),
                    typing(theme, descr, [.light, .bold, .black]),
                    typing(theme, descr, [.light, .light, .bold])
                ]
            }
            timer = timer ?? Timer.scheduledTimer(withTimeInterval: 0.25, repeats: true) { _ in
                typingIdx = typingIdx + 1
            }
        } else {
            stopTyping()
        }
    }

    private func stopTyping() {
        timer?.invalidate()
        timer = nil
        typingIdx = 0
    }

    @inline(__always)
    private func msgContentView() -> some View {
        let s = messageText(text, formattedText, textStyle: textStyle, sender: sender, mentions: mentions, userMemberId: userMemberId, showSecrets: showSecrets, secondaryColor: theme.colors.secondary, prefix: prefix)
        let t: Text
        if let mt = meta {
            if mt.isLive {
                s.append(typingIndicator(mt.recent))
            }
            t = Text(AttributedString(s)) + reserveSpaceForMeta(mt)
        } else {
            t = Text(AttributedString(s))
        }
        return t.overlay(handleTextLinks(s, showSecrets: $showSecrets))
    }

    @inline(__always)
    private func typingIndicator(_ recent: Bool) -> NSAttributedString {
        recent && !typingIndicators.isEmpty
        ? typingIndicators[typingIdx % 4]
        : noTyping
    }

    @inline(__always)
    private func reserveSpaceForMeta(_ mt: CIMeta) -> Text {
        (rightToLeft ? textNewLine : Text(verbatim: "   ")) + ciMetaText(mt, chatTTL: chat.chatInfo.timedMessagesTTL, encrypted: nil, colorMode: .transparent, showViaProxy: showSentViaProxy, showTimesamp: showTimestamp)
    }
}

func handleTextLinks(_ s: NSAttributedString, showSecrets: Binding<Set<Int>>? = nil) -> some View {
    return GeometryReader { g in
        Rectangle()
            .fill(Color.clear)
            .contentShape(Rectangle())
            .simultaneousGesture(DragGesture(minimumDistance: 0).onEnded { event in
                let t = event.translation
                if t.width * t.width + t.height * t.height > 100 { return }
                let framesetter = CTFramesetterCreateWithAttributedString(s as CFAttributedString)
                let path = CGPath(rect: CGRect(origin: .zero, size: g.size), transform: nil)
                let frame = CTFramesetterCreateFrame(framesetter, CFRangeMake(0, s.length), path, nil)
                let point = CGPoint(x: event.location.x, y: g.size.height - event.location.y) // Flip y for UIKit
                var index: CFIndex?
                if let lines = CTFrameGetLines(frame) as? [CTLine] {
                    var origins = [CGPoint](repeating: .zero, count: lines.count)
                    CTFrameGetLineOrigins(frame, CFRangeMake(0, 0), &origins)
                    for i in 0 ..< lines.count {
                        let bounds = CTLineGetBoundsWithOptions(lines[i], .useOpticalBounds)
                        if bounds.offsetBy(dx: origins[i].x, dy: origins[i].y).contains(point) {
                            index = CTLineGetStringIndexForPosition(lines[i], point)
                            break
                        }
                    }
                }
                if let index, let (url, browser) = attributedStringLink(s, for: index) {
                    if browser {
                        openBrowserAlert(uri: url)
                    } else {
                        UIApplication.shared.open(url)
                    }
                }
            })
    }

    func attributedStringLink(_ s: NSAttributedString, for index: CFIndex) -> (URL, Bool)? {
        var linkURL: URL?
        var browser: Bool = false
        s.enumerateAttributes(in: NSRange(location: 0, length: s.length)) { attrs, range, stop in
            if index >= range.location && index < range.location + range.length {
                if let url = attrs[linkAttrKey] as? NSURL {
                    linkURL = url.absoluteURL
                    browser = attrs[webLinkAttrKey] != nil
                } else if let showSecrets, let i = attrs[secretAttrKey] as? Int {
                    if showSecrets.wrappedValue.contains(i) {
                        showSecrets.wrappedValue.remove(i)
                    } else {
                        showSecrets.wrappedValue.insert(i)
                    }
                }
                stop.pointee = true
            }
        }
        return if let linkURL { (linkURL, browser) } else { nil }
    }
}

private let linkAttrKey = NSAttributedString.Key("chat.simplex.app.link")

private let webLinkAttrKey = NSAttributedString.Key("chat.simplex.app.webLink")

private let secretAttrKey = NSAttributedString.Key("chat.simplex.app.secret")

func messageText(_ text: String, _ formattedText: [FormattedText]?, textStyle: UIFont.TextStyle = .body, sender: String?, preview: Bool = false, mentions: [String: CIMention]?, userMemberId: String?, showSecrets: Set<Int>?, secondaryColor: Color, prefix: NSAttributedString? = nil) -> NSMutableAttributedString {
    let res = NSMutableAttributedString()
    let descr = UIFontDescriptor.preferredFontDescriptor(withTextStyle: textStyle)
    let font = UIFont.preferredFont(forTextStyle: textStyle)
    let plain: [NSAttributedString.Key: Any] = [
        .font: font,
        .foregroundColor: UIColor.label
    ]
    var link: [NSAttributedString.Key: Any]?

    if let sender {
        if preview {
            res.append(NSAttributedString(string: sender + ": ", attributes: plain))
        } else {
            var attrs = plain
            attrs[.font] = UIFont(descriptor: descr.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.medium]]), size: descr.pointSize)
            res.append(NSAttributedString(string: sender, attributes: attrs))
            res.append(NSAttributedString(string: ": ", attributes: plain))
        }
    }

    if let prefix {
        res.append(prefix)
    }

    if let fts = formattedText, fts.count > 0 {
        var bold: UIFont?
        var italic: UIFont?
        var snippet: UIFont?
        var mention: UIFont?
        var secretIdx: Int = 0
        for ft in fts {
            var t = ft.text
            var attrs = plain
            switch (ft.format) {
            case .bold:
                bold = bold ?? UIFont(descriptor: descr.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.bold]]), size: descr.pointSize)
                attrs[.font] = bold
            case .italic:
                italic = italic ?? UIFont(descriptor: descr.withSymbolicTraits(.traitItalic) ?? descr, size: descr.pointSize)
                attrs[.font] = italic
            case .strikeThrough:
                attrs[.strikethroughStyle] = NSUnderlineStyle.single.rawValue
            case .snippet:
                snippet = snippet ?? UIFont.monospacedSystemFont(ofSize: descr.pointSize, weight: .regular)
                attrs[.font] = snippet
            case .secret:
                if let showSecrets {
                    if !showSecrets.contains(secretIdx) {
                        attrs[.foregroundColor] = UIColor.clear
                        attrs[.backgroundColor] = UIColor.secondarySystemFill // secretColor
                    }
                    attrs[secretAttrKey] = secretIdx
                    secretIdx += 1
                } else {
                    attrs[.foregroundColor] = UIColor.clear
                    attrs[.backgroundColor] = UIColor.secondarySystemFill
                }
            case let .colored(color):
                if let c = color.uiColor {
                    attrs[.foregroundColor] = UIColor(c)
                }
            case .uri:
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = NSURL(string: ft.text)
                    attrs[webLinkAttrKey] = true
                }
            case let .simplexLink(linkType, simplexUri, smpHosts):
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = NSURL(string: simplexUri)
                }
                if case .description = privacySimplexLinkModeDefault.get() {
                    t = simplexLinkText(linkType, smpHosts)
                }
            case let .mention(memberName):
                if let m = mentions?[memberName] {
                    mention = mention ?? UIFont(descriptor: descr.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.semibold]]), size: descr.pointSize)
                    attrs[.font] = mention
                    if let ref = m.memberRef {
                        let name: String = if let alias = ref.localAlias, alias != "" {
                            "\(alias) (\(ref.displayName))"
                        } else {
                            ref.displayName
                        }
                        if m.memberId == userMemberId {
                            attrs[.foregroundColor] = UIColor.tintColor
                        }
                        t = "@'\(name)'"
                    } else {
                        t = "@'\(memberName)'"
                    }
                }
            case .email:
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = NSURL(string: "mailto:" + ft.text)
                }
            case .phone:
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = NSURL(string: "tel:" + t.replacingOccurrences(of: " ", with: ""))
                }
            case .none: ()
            }
            res.append(NSAttributedString(string: t, attributes: attrs))
        }
    } else {
        res.append(NSMutableAttributedString(string: text, attributes: plain))
    }
    
    return res

    func linkAttrs() -> [NSAttributedString.Key: Any] {
        link = link ?? [
            .font: font,
            .foregroundColor: uiLinkColor,
            .underlineStyle: NSUnderlineStyle.single.rawValue
        ]
        return link!
    }
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
            textStyle: .body,
            sender: chatItem.memberDisplayName,
            meta: chatItem.meta
        )
        .environmentObject(Chat.sampleData)
    }
}
