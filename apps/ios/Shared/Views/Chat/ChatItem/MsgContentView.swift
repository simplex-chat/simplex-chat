//
//  MsgContentView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//
// Spec: spec/client/chat-view.md

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

// Spec: spec/client/chat-view.md#MsgContentView
struct MsgContentView: View {
    @ObservedObject var chat: Chat
    @Environment(\.showTimestamp) var showTimestamp: Bool
    @Environment(\.containerBackground) var containerBackground: UIColor
    @EnvironmentObject var theme: AppTheme
    var text: String
    var formattedText: [FormattedText]? = nil
    var markdownBlocks: [MarkdownBlock]? = nil
    var textStyle: UIFont.TextStyle
    var sender: String? = nil
    var meta: CIMeta? = nil
    var mentions: [String: CIMention]? = nil
    var userMemberId: String? = nil
    var rightToLeft = false
    var prefix: NSAttributedString? = nil
    var stripLink: String? = nil
    var blockLayout = false
    var cardColor: Color? = nil
    @State private var showSecrets: Set<Int> = []
    @State private var typingIdx = 0
    @State private var timer: Timer?
    @State private var typingIndicators: [NSAttributedString] = []
    @State private var noTyping = NSAttributedString(string: "   ")
    @State private var phase: CGFloat = 0

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false
    @AppStorage(DEFAULT_PRIVACY_SHOW_SIGNATURE) private var showSignature = true

    var body: some View {
        let v = contentView()
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

    @ViewBuilder
    private func contentView() -> some View {
        let fts = if let stripLink { stripFormattedTextLink(formattedText, stripLink) } else { formattedText }
        if blockLayout, let fts, !fts.isEmpty, let blocks = markdownBlocks {
            MarkdownBlocksView(
                formattedText: fts,
                blocks: blocks,
                textStyle: textStyle,
                cardColor: cardColor ?? theme.appColors.receivedQuote,
                blockText: { index, range, font, secretStart, last in blockTextView(fts, blocks, index, range, font, secretStart, last) },
                metaLine: { metaLineView() }
            )
        } else {
            msgContentView()
        }
    }

    private func blockTextView(_ fts: [FormattedText], _ blocks: [MarkdownBlock], _ index: Int, _ range: Range<Int>, _ font: UIFont, _ secretStart: Int, _ last: Bool) -> some View {
        let r = messageText(
            text,
            fts,
            markdownBlocks: blocks,
            textStyle: textStyle,
            sender: index == 0 ? sender : nil,
            mentions: mentions,
            userMemberId: userMemberId,
            showSecrets: showSecrets,
            commands: chat.chatInfo.useCommands && chat.chatInfo.sndReady,
            backgroundColor: containerBackground,
            prefix: index == 0 ? prefix : nil,
            range: range,
            baseFont: font,
            secretStart: secretStart
        )
        let s = r.string
        let t: Text
        if last, let mt = meta {
            if mt.isLive {
                s.append(typingIndicator(mt.recent))
            }
            t = Text(AttributedString(s)) + reserveSpaceForMeta(mt)
        } else {
            t = Text(AttributedString(s))
        }
        return msgTextResultView(r, t, showSecrets: $showSecrets, sendCommand: { cmd in sendCommandMsg(chat, cmd) })
    }

    @ViewBuilder
    private func metaLineView() -> some View {
        if let mt = meta {
            (mt.isLive ? Text(AttributedString(typingIndicator(mt.recent))) : Text(verbatim: "")) + reserveSpaceForMeta(mt)
        }
    }

    @inline(__always)
    private func msgContentView() -> some View {
        let r = messageText(
            text,
            formattedText,
            markdownBlocks: markdownBlocks,
            textStyle: textStyle,
            sender: sender,
            mentions: mentions,
            userMemberId: userMemberId,
            showSecrets: showSecrets,
            commands: chat.chatInfo.useCommands && chat.chatInfo.sndReady,
            backgroundColor: containerBackground,
            prefix: prefix,
            stripLink: stripLink
        )
        let s = r.string
        let t: Text
        if let mt = meta {
            if mt.isLive {
                s.append(typingIndicator(mt.recent))
            }
            t = Text(AttributedString(s)) + reserveSpaceForMeta(mt)
        } else {
            t = Text(AttributedString(s))
        }
        return msgTextResultView(r, t, showSecrets: $showSecrets, sendCommand: { cmd in sendCommandMsg(chat, cmd) })
    }

    @inline(__always)
    private func typingIndicator(_ recent: Bool) -> NSAttributedString {
        recent && !typingIndicators.isEmpty
        ? typingIndicators[typingIdx % 4]
        : noTyping
    }

    @inline(__always)
    private func reserveSpaceForMeta(_ mt: CIMeta) -> Text {
        (rightToLeft ? textNewLine : Text(verbatim: "   ")) + ciMetaText(mt, chatTTL: chat.chatInfo.timedMessagesTTL, encrypted: nil, colorMode: .transparent, showViaProxy: showSentViaProxy, showTimesamp: showTimestamp, showSignature: showSignature)
    }
}

func msgTextResultView(
    _ r: MsgTextResult,
    _ t: Text,
    showSecrets: Binding<Set<Int>>? = nil,
    sendCommand: ((String) -> Void)? = nil,
    openModal: ((Format) -> Void)? = nil,
    centered: Bool = false,
    smallFont: Bool = false
) -> some View {
    t.if(r.hasSecrets, transform: hiddenSecretsView)
        .if(r.handleTaps) { $0.overlay(handleTextTaps(r.string, showSecrets: showSecrets, sendCommand: sendCommand, openModal: openModal, centered: centered, smallFont: smallFont)) }
}

// smallFont parameter is used to pad height, otherwise CTFrameGetLines fails to see them as lines - it's needed if font is not .body
@inline(__always)
private func handleTextTaps(
    _ s: NSAttributedString,
    showSecrets: Binding<Set<Int>>? = nil,
    sendCommand: ((String) -> Void)? = nil,
    openModal: ((Format) -> Void)? = nil,
    centered: Bool,
    smallFont: Bool
) -> some View {
    return GeometryReader { g in
        Rectangle()
            .fill(Color.clear)
            .contentShape(Rectangle())
            .simultaneousGesture(DragGesture(minimumDistance: 0).onEnded { event in
                let t = event.translation
                if t.width * t.width + t.height * t.height > 100 { return }
                let framesetter = CTFramesetterCreateWithAttributedString(s as CFAttributedString)
                let paddedSize = smallFont ? CGSize(width: g.size.width, height: g.size.height + 1.0) : g.size
                let path = CGPath(rect: CGRect(origin: .zero, size: paddedSize), transform: nil)
                let frame = CTFramesetterCreateFrame(framesetter, CFRangeMake(0, s.length), path, nil)
                let point = CGPoint(x: event.location.x, y: g.size.height - event.location.y) // Flip y for UIKit
                var index: CFIndex?
                if let lines = CTFrameGetLines(frame) as? [CTLine] {
                    var origins = [CGPoint](repeating: .zero, count: lines.count)
                    CTFrameGetLineOrigins(frame, CFRangeMake(0, 0), &origins)
                    var maxWidth: CGFloat = 0
                    if centered {
                        for line in lines {
                            let bounds = CTLineGetBoundsWithOptions(line, .useOpticalBounds)
                            if bounds.width > maxWidth {
                                maxWidth = bounds.width
                            }
                        }
                    }
                    for i in 0 ..< lines.count {
                        let bounds = CTLineGetBoundsWithOptions(lines[i], .useOpticalBounds)
                        let offsetX = centered ? (maxWidth - bounds.width) / 2 : 0
                        if bounds.offsetBy(dx: origins[i].x + offsetX, dy: origins[i].y).contains(point) {
                            let relativePoint = centered ? CGPoint(x: point.x - origins[i].x - offsetX, y: point.y - origins[i].y) : point
                            index = CTLineGetStringIndexForPosition(lines[i], relativePoint)
                            break
                        }
                    }
                }
                if let index, let (uri, browser, simplex) = attributedStringLink(s, for: index) {
                    if browser {
                        openBrowserAlert(uri: uri)
                    } else if simplex, let url = URL(string: uri) {
                        // SimpleX links target this same app (simplex: scheme / simplex.chat universal link),
                        // so UIApplication.shared.open is dropped by iOS while the app is in the foreground.
                        // Route to the in-app connect flow instead (same sink onOpenURL feeds).
                        ChatModel.shared.appOpenUrl = url
                    } else if let url = URL(string: uri) {
                        UIApplication.shared.open(url)
                    } else {
                        showInvalidLinkAlert(uri)
                    }
                }
            })
    }

    func attributedStringLink(_ s: NSAttributedString, for index: CFIndex) -> (String, Bool, Bool)? {
        var linkURL: String?
        var browser: Bool = false
        var simplex: Bool = false
        s.enumerateAttributes(in: NSRange(location: 0, length: s.length)) { attrs, range, stop in
            if index >= range.location && index < range.location + range.length {
                if attrs[nameAttrKey] is SimplexNameInfo {
                    planAndConnect(s.attributedSubstring(from: range).string, theme: AppTheme.shared, dismiss: false)
                } else if let url = attrs[linkAttrKey] as? String {
                    linkURL = url
                    browser = attrs[webLinkAttrKey] != nil
                    simplex = attrs[simplexLinkAttrKey] != nil
                } else if let showSecrets, let i = attrs[secretAttrKey] as? Int {
                    if showSecrets.wrappedValue.contains(i) {
                        showSecrets.wrappedValue.remove(i)
                    } else {
                        showSecrets.wrappedValue.insert(i)
                    }
                } else if let sendCommand, let cmd = attrs[commandAttrKey] as? String {
                    sendCommand(cmd)
                } else if let openModal, let fmt = attrs[modalAttrKey] as? Format {
                    openModal(fmt)
                }
                stop.pointee = true
            }
        }
        return if let linkURL { (linkURL, browser, simplex) } else { nil }
    }
}

func hiddenSecretsView<V: View>(_ v: V) -> some View {
    v.overlay(
        GeometryReader { g in
            let size = (g.size.width + g.size.height) / 1.4142
            Image("vertical_logo")
                .resizable(resizingMode: .tile)
                .frame(width: size, height: size)
                .rotationEffect(.degrees(45), anchor: .center)
                .position(x: g.size.width / 2, y: g.size.height / 2)
                .clipped()
                .saturation(0.65)
                .opacity(0.35)
        }
        .mask(v)
    )
}

private let linkAttrKey = NSAttributedString.Key("chat.simplex.app.link")

private let webLinkAttrKey = NSAttributedString.Key("chat.simplex.app.webLink")

private let simplexLinkAttrKey = NSAttributedString.Key("chat.simplex.app.simplexLink")

private let secretAttrKey = NSAttributedString.Key("chat.simplex.app.secret")

private let commandAttrKey = NSAttributedString.Key("chat.simplex.app.command")
private let nameAttrKey = NSAttributedString.Key("chat.simplex.app.name")
private let modalAttrKey = NSAttributedString.Key("chat.simplex.app.modal")

typealias MsgTextResult = (string: NSMutableAttributedString, hasSecrets: Bool, handleTaps: Bool)

// Reusable profile bio/description header: renders the teaser and opens the full
// description in a sheet when the "Read more" (Format.modal) link is tapped.
struct ProfileDescriptionView: View {
    @EnvironmentObject var theme: AppTheme
    let shortDescr: String?
    let description: String?
    @State private var showSecrets: Set<Int> = []
    @State private var modal: ModalText? = nil

    var body: some View {
        if let r = markdownProfileDescription(shortDescr: shortDescr, description: description, showSecrets: showSecrets, backgroundColor: theme.colors.background) {
            msgTextResultView(r, Text(AttributedString(r.string)), showSecrets: $showSecrets, openModal: openModal, centered: true, smallFont: true)
                .multilineTextAlignment(.center)
                .lineLimit(4)
                .fixedSize(horizontal: false, vertical: true)
                .appSheet(item: $modal) { m in
                    FullProfileDescriptionView(description: m.text).environmentObject(theme)
                }
        }
    }

    private func openModal(_ format: Format) {
        if case let .modal(_, text) = format { modal = ModalText(text: text) }
    }
}

private struct ModalText: Identifiable {
    let id = UUID()
    let text: String
}

private struct FullProfileDescriptionView: View {
    @EnvironmentObject var theme: AppTheme
    let description: String
    @State private var showSecrets: Set<Int> = []

    var body: some View {
        List {
            Text("Description")
                .font(.title)
                .bold()
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .listRowBackground(Color.clear)

            Section {
                let parsed = parseSimpleXMarkdownText(description)
                MarkdownView(
                    text: description, formattedText: parsed?.formattedText, markdownBlocks: parsed?.markdownBlocks,
                    backgroundColor: theme.colors.background, cardColor: theme.appColors.receivedQuote, showSecrets: $showSecrets
                )
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }
}

@inline(__always)
func markdownText(
    _ s: String,
    textStyle: UIFont.TextStyle = .body,
    sender: String? = nil,
    preview: Bool = false,
    mentions: [String: CIMention]? = nil,
    userMemberId: String? = nil,
    showSecrets: Set<Int>? = nil,
    backgroundColor: Color
) -> MsgTextResult {
    let parsed = parseSimpleXMarkdownText(s)
    return messageText(
        s,
        parsed?.formattedText,
        markdownBlocks: parsed?.markdownBlocks,
        textStyle: textStyle,
        sender: sender,
        preview: preview,
        mentions: mentions,
        userMemberId: userMemberId,
        showSecrets: showSecrets,
        commands: false,
        backgroundColor: UIColor(backgroundColor)
    )
}

// Renders a profile bio/description: the bio, a short single-line description, or a
// truncated teaser followed by a "Read more" link that opens the full description (Format.modal).
func markdownProfileDescription(
    shortDescr: String?,
    description: String?,
    showSecrets: Set<Int>? = nil,
    backgroundColor: Color
) -> MsgTextResult? {
    func trimmed(_ s: String?) -> String? {
        guard let t = s?.trimmingCharacters(in: .whitespacesAndNewlines), !t.isEmpty else { return nil }
        return t
    }
    let short = trimmed(shortDescr)
    let descr = trimmed(description)
    guard let descr else {
        return short.map { markdownText($0, textStyle: .subheadline, showSecrets: showSecrets, backgroundColor: backgroundColor) }
    }
    let firstLine = String(descr.prefix(while: { $0 != "\n" }))
    let truncated = firstLine.count > 100
    let multiline = descr.count > firstLine.count
    if short == nil && !truncated && !multiline {
        return markdownText(descr, textStyle: .subheadline, showSecrets: showSecrets, backgroundColor: backgroundColor)
    }
    let teaser = short ?? (truncated ? String(firstLine.prefix(100)).trimmingCharacters(in: .whitespaces) + "…" : firstLine + "…")
    let readMore = NSLocalizedString("Read more", comment: "profile description teaser")
    var formatted = parseSimpleXMarkdown(teaser) ?? [FormattedText(text: teaser)]
    formatted.append(FormattedText(text: " "))
    formatted.append(FormattedText(text: readMore, format: .modal(modalName: Format.modalDescription, text: descr)))
    return messageText(
        "\(teaser) \(readMore)",
        formatted,
        textStyle: .subheadline,
        sender: nil,
        mentions: nil,
        userMemberId: nil,
        showSecrets: showSecrets,
        backgroundColor: UIColor(backgroundColor)
    )
}


func messageText(
    _ text: String,
    _ formattedText: [FormattedText]?,
    markdownBlocks: [MarkdownBlock]? = nil,
    textStyle: UIFont.TextStyle = .body,
    sender: String?,
    preview: Bool = false,
    mentions: [String: CIMention]?,
    userMemberId: String?,
    showSecrets: Set<Int>?,
    commands: Bool = false,
    backgroundColor: UIColor,
    prefix: NSAttributedString? = nil,
    stripLink: String? = nil,
    range: Range<Int>? = nil,
    baseFont: UIFont? = nil,
    secretStart: Int = 0
) -> MsgTextResult {
    let text = if let stripLink { stripTextLink(text, stripLink) } else { text }
    let formattedText = if let stripLink { stripFormattedTextLink(formattedText, stripLink) } else { formattedText }
    let res = NSMutableAttributedString()
    let descr = baseFont?.fontDescriptor ?? UIFontDescriptor.preferredFontDescriptor(withTextStyle: textStyle)
    let font = baseFont ?? UIFont.preferredFont(forTextStyle: textStyle)
    let plain: [NSAttributedString.Key: Any] = [
        .font: font,
        .foregroundColor: UIColor.label
    ]
    let secretColor = backgroundColor.withAlphaComponent(1)
    var hasSecrets = false
    var handleTaps = false
    var secretIdx: Int = secretStart

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
        if let range {
            let start = min(max(range.lowerBound, 0), fts.count)
            appendSegments(fts[start..<max(start, min(range.upperBound, fts.count))], false)
        } else if let markdownBlocks {
            let headings = headingSegments(fts.count, markdownBlocks)
            var i = 0
            while i < fts.count {
                var j = i + 1
                while j < fts.count && headings[j] == headings[i] { j += 1 }
                appendSegments(fts[i..<j], headings[i])
                i = j
            }
        } else {
            appendSegments(fts[...], false)
        }
    } else {
        res.append(NSMutableAttributedString(string: text, attributes: plain))
    }

    return (string: res, hasSecrets: hasSecrets, handleTaps: handleTaps)

    func segmentFonts(_ heading: Bool) -> (descr: UIFontDescriptor, font: UIFont) {
        if !heading { return (descr, font) }
        let d = descr.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.bold]])
        return (d, UIFont(descriptor: d, size: d.pointSize))
    }

    func appendSegments(_ fts: ArraySlice<FormattedText>, _ heading: Bool) {
        let (descr, font) = segmentFonts(heading)
        let plain: [NSAttributedString.Key: Any] = [
            .font: font,
            .foregroundColor: UIColor.label
        ]
        var link: [NSAttributedString.Key: Any]?
        var bold: UIFont?
        var italic: UIFont?
        var snippet: UIFont?
        var small: UIFont?
        var mention: UIFont?
        for ft in fts {
            var t = ft.text
            var attrs = plain
            switch (ft.format) {
            case let .bold(singleStar):
                if singleStar && markdownBlocks != nil {
                    italic = italic ?? UIFont(descriptor: descr.withSymbolicTraits(.traitItalic) ?? descr, size: descr.pointSize)
                    attrs[.font] = italic
                } else {
                    bold = bold ?? UIFont(descriptor: descr.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.bold]]), size: descr.pointSize)
                    attrs[.font] = bold
                }
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
                        attrs[.backgroundColor] = secretColor
                    }
                    attrs[secretAttrKey] = secretIdx
                    secretIdx += 1
                    handleTaps = true
                } else {
                    attrs[.foregroundColor] = UIColor.clear
                    attrs[.backgroundColor] = secretColor
                }
                hasSecrets = true
            case .small:
                small = small ?? UIFont.preferredFont(forTextStyle: .footnote)
                attrs[.font] = small
                attrs[.foregroundColor] = UIColor.secondaryLabel
            case let .colored(color):
                if let c = color.uiColor {
                    attrs[.foregroundColor] = UIColor(c)
                }
            case .uri:
                attrs = linkAttrs()
                if !preview {
                    let link = t.hasPrefix("http://") || t.hasPrefix("https://")
                                ? t
                                : "https://" + t
                    attrs[linkAttrKey] = link
                    attrs[webLinkAttrKey] = true
                    handleTaps = true
                }
                t = displayedUri(t)
            case let .hyperLink(text, uri):
                attrs = linkAttrs()
                if let text { t = text }
                if !preview {
                    attrs[linkAttrKey] = uri
                    if !ft.isEmailLink { attrs[webLinkAttrKey] = true }
                    handleTaps = true
                }
            case let .simplexLink(text, linkType, simplexUri, smpHosts):
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = simplexUri
                    attrs[simplexLinkAttrKey] = true
                    handleTaps = true
                }
                if let s = text ?? (privacySimplexLinkModeDefault.get() == .description ? linkType.description : nil) {
                    res.append(NSAttributedString(string: s + " ", attributes: attrs))
                    italic = italic ?? UIFont(descriptor: descr.withSymbolicTraits(.traitItalic) ?? descr, size: descr.pointSize)
                    attrs[.font] = italic
                    t = viaHost(smpHosts)
                }
            case let .command(cmdStr):
                snippet = snippet ?? UIFont.monospacedSystemFont(ofSize: descr.pointSize, weight: .regular)
                attrs[.font] = snippet
                t = "/" + cmdStr
                if !preview && commands {
                    attrs[.foregroundColor] = uiLinkColor
                    attrs[commandAttrKey] = t
                    handleTaps = true
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
                        t = mentionText(name)
                    } else {
                        t = mentionText(memberName)
                    }
                }
            case let .simplexName(nameInfo):
                attrs = linkAttrs()
                if !preview {
                    attrs[nameAttrKey] = nameInfo
                    handleTaps = true
                }
            case .email:
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = "mailto:" + ft.text
                    handleTaps = true
                }
            case .phone:
                attrs = linkAttrs()
                if !preview {
                    attrs[linkAttrKey] = "tel:" + t.replacingOccurrences(of: " ", with: "")
                    handleTaps = true
                }
            case let .modal(modalName, text):
                attrs = linkAttrs()
                if !preview {
                    attrs[modalAttrKey] = Format.modal(modalName: modalName, text: text)
                    handleTaps = true
                }
            case .escape: t = ""
            case let .markup(token):
                if range != nil { t = token.drawnText } else if markdownBlocks != nil { t = token.previewText }
            case .unknown: ()
            case .none: ()
            }
            res.append(NSAttributedString(string: t, attributes: attrs))
        }

        func linkAttrs() -> [NSAttributedString.Key: Any] {
            link = link ?? [
                .font: font,
                .foregroundColor: uiLinkColor,
                .underlineStyle: NSUnderlineStyle.single.rawValue
            ]
            return link!
        }
    }
}

let swipeExclusionSpace = "swipeExclusion"

struct SwipeExclusionFrames: PreferenceKey {
    static var defaultValue: [CGRect] = []

    static func reduce(value: inout [CGRect], nextValue: () -> [CGRect]) {
        value += nextValue()
    }
}

struct MarkdownView: View {
    var text: String
    var formattedText: [FormattedText]?
    var markdownBlocks: [MarkdownBlock]?
    var sender: String? = nil
    var mentions: [String: CIMention]? = nil
    var userMemberId: String? = nil
    var backgroundColor: Color
    var cardColor: Color
    @Binding var showSecrets: Set<Int>

    var body: some View {
        if let fts = formattedText, !fts.isEmpty, let blocks = markdownBlocks {
            MarkdownBlocksView(
                formattedText: fts,
                blocks: blocks,
                textStyle: .body,
                cardColor: cardColor,
                blockText: { index, range, font, secretStart, _ in
                    let r = messageText(
                        text, fts, markdownBlocks: blocks, sender: index == 0 ? sender : nil, mentions: mentions, userMemberId: userMemberId,
                        showSecrets: showSecrets, backgroundColor: UIColor(backgroundColor), range: range, baseFont: font, secretStart: secretStart
                    )
                    msgTextResultView(r, Text(AttributedString(r.string)), showSecrets: $showSecrets)
                },
                metaLine: { EmptyView() }
            )
        } else {
            let r = messageText(text, formattedText, sender: sender, mentions: mentions, userMemberId: userMemberId, showSecrets: showSecrets, backgroundColor: UIColor(backgroundColor))
            msgTextResultView(r, Text(AttributedString(r.string)), showSecrets: $showSecrets)
        }
    }
}

private struct BlocksLayout {
    let font: UIFont
    let starts: [Int]
    let secrets: [Int]
    let lastIsText: Bool

    init(_ fts: [FormattedText], _ blocks: [MarkdownBlock], _ textStyle: UIFont.TextStyle) {
        font = UIFont.preferredFont(forTextStyle: textStyle)
        starts = blocks.reduce(into: [0]) { $0.append($0[$0.count - 1] + $1.textRanges.count) }
        secrets = fts.reduce(into: [0]) { $0.append($0[$0.count - 1] + ($1.isSecret ? 1 : 0)) }
        lastIsText = switch blocks.last {
        case .paragraph, .heading, .quote, .list: true
        default: false
        }
    }
}

struct MarkdownBlocksView<BlockText: View, MetaLine: View>: View {
    @AppStorage(DEFAULT_CHAT_ITEM_ROUNDNESS) private var roundness = defaultChatItemRoundness
    var formattedText: [FormattedText]
    var blocks: [MarkdownBlock]
    var textStyle: UIFont.TextStyle
    var cardColor: Color
    @ViewBuilder var blockText: (_ index: Int, _ range: Range<Int>, _ font: UIFont, _ secretStart: Int, _ last: Bool) -> BlockText
    @ViewBuilder var metaLine: () -> MetaLine

    var body: some View {
        let l = BlocksLayout(formattedText, blocks, textStyle)
        VStack(alignment: .leading, spacing: 0) {
            ForEach(Array(blocks.enumerated()), id: \.offset) { k, block in
                if k > 0 && paragraphBreak(blocks[k - 1].endIndex, block.startIndex) {
                    Color.clear.frame(height: l.font.lineHeight / 2)
                }
                blockView(block, l.starts[k], l)
            }
            if !l.lastIsText {
                metaLine()
            }
        }
    }

    private func text(_ index: Int, _ r: SegmentRange, _ font: UIFont, _ l: BlocksLayout) -> BlockText {
        blockText(index, r.from..<max(r.from, r.to), font, l.secrets[min(max(r.from, 0), formattedText.count)], l.lastIsText && index == l.starts[l.starts.count - 1] - 1)
    }

    @ViewBuilder private func blockView(_ block: MarkdownBlock, _ start: Int, _ l: BlocksLayout) -> some View {
        switch block {
        case let .paragraph(content):
            text(start, content, l.font, l)
        case let .heading(level, content):
            text(start, content, headingFont(l.font, level), l)
        case let .quote(content):
            HStack(alignment: .top, spacing: 10) {
                RoundedRectangle(cornerRadius: 2).fill(Color(uiColor: .label).opacity(0.16)).frame(width: 4)
                text(start, content, l.font, l).opacity(0.72)
            }
            .fixedSize(horizontal: false, vertical: true)
        case let .list(items):
            ForEach(Array(listEntries(items, 0).enumerated()), id: \.offset) { j, e in
                HStack(alignment: .firstTextBaseline, spacing: 6) {
                    Text(e.entry.number ?? "•").font(.system(size: l.font.pointSize))
                    text(start + j, e.entry.content, l.font, l)
                }
                .padding(.leading, 20 * CGFloat(e.level))
            }
        case let .codeBlock(_, content):
            CodeBlockCard(color: cardColor, radius: 12 * roundness, copyText: codeText(content)) {
                text(start, content, monospacedFont(l.font), l).fixedSize(horizontal: true, vertical: false)
            }
        case let .table(columns, header, rows):
            tableView(columns, [header] + rows, start, l)
        case .rule:
            Rectangle().fill(Color(uiColor: .secondaryLabel).opacity(0.35)).frame(height: 1).padding(.vertical, 6)
        }
    }

    @ViewBuilder private func tableView(_ columns: [TableAlign], _ tableRows: [[SegmentRange]], _ start: Int, _ l: BlocksLayout) -> some View {
        let rowStarts = tableRows.reduce(into: [start]) { $0.append($0[$0.count - 1] + $1.count) }
        let columnCount = max(columns.count, tableRows.map(\.count).max() ?? 0)
        let lineColor = Color(uiColor: .secondaryLabel).opacity(0.35)
        let radius = 12 * roundness
        if #available(iOS 16, *) {
            HScroll {
                Grid(alignment: .leading, horizontalSpacing: 0, verticalSpacing: 0) {
                    ForEach(Array(tableRows.enumerated()), id: \.offset) { r, row in
                        GridRow {
                            ForEach(0..<columnCount, id: \.self) { c in
                                Group {
                                    if c < row.count {
                                        text(rowStarts[r] + c, row[c], r == 0 ? boldFont(l.font) : l.font, l).fixedSize()
                                    } else {
                                        Text(verbatim: "")
                                    }
                                }
                                .padding(.horizontal, 12)
                                .padding(.vertical, 4)
                                .frame(maxWidth: .infinity, alignment: cellAlignment(c < columns.count ? columns[c] : .none))
                                .background(r == 0 ? cardColor : r % 2 == 0 ? Color(uiColor: .secondaryLabel).opacity(0.1) : Color.clear)
                                .overlay(Rectangle().stroke(lineColor, lineWidth: 0.5))
                            }
                        }
                    }
                }
                .fixedSize()
                .clipShape(RoundedRectangle(cornerRadius: radius))
                .overlay(RoundedRectangle(cornerRadius: radius).stroke(lineColor, lineWidth: 1))
            }
        } else {
            let source = tableSource(tableRows)
            CodeBlockCard(color: cardColor, radius: radius, copyText: source) {
                Text(verbatim: source).font(.system(size: l.font.pointSize * 0.875, design: .monospaced)).fixedSize(horizontal: true, vertical: false)
            }
        }
    }

    private func paragraphBreak(_ from: Int, _ to: Int) -> Bool {
        let start = min(max(from, 0), formattedText.count)
        return formattedText[start..<max(start, min(to, formattedText.count))].contains {
            if case .markup(.paragraphBreak) = $0.format { true } else { false }
        }
    }

    private func codeText(_ r: SegmentRange) -> String {
        let start = min(max(r.from, 0), formattedText.count)
        return formattedText[start..<max(start, min(r.to, formattedText.count))].map {
            switch $0.format {
            case let .markup(token): token.drawnText
            case .escape: ""
            default: $0.text
            }
        }.joined()
    }

    private func tableSource(_ tableRows: [[SegmentRange]]) -> String {
        let start = min(max((tableRows.first?.first?.from ?? 1) - 1, 0), formattedText.count)
        let end = min((tableRows.last?.last?.to ?? 0) + 1, formattedText.count)
        return formattedText[start..<max(start, end)].map(\.text).joined()
    }
}

private extension MarkdownBlock {
    var startIndex: Int {
        if case let .rule(mark) = self { mark.from } else { textRanges.first?.range.from ?? 0 }
    }

    var endIndex: Int {
        if case let .rule(mark) = self { mark.to } else { textRanges.last?.range.to ?? 0 }
    }
}

private func headingFont(_ font: UIFont, _ level: Int) -> UIFont {
    let scale: CGFloat = switch level {
    case 1: 1.5
    case 2: 1.3
    case 3: 1.15
    default: 1
    }
    return UIFont(descriptor: boldFont(font).fontDescriptor, size: font.pointSize * scale)
}

private func boldFont(_ font: UIFont) -> UIFont {
    UIFont(descriptor: font.fontDescriptor.addingAttributes([.traits: [UIFontDescriptor.TraitKey.weight: UIFont.Weight.bold]]), size: font.pointSize)
}

private func monospacedFont(_ font: UIFont) -> UIFont {
    UIFont.monospacedSystemFont(ofSize: font.pointSize * 0.875, weight: .regular)
}

private func cellAlignment(_ align: TableAlign) -> Alignment {
    switch align {
    case .right: .trailing
    case .center: .center
    default: .leading
    }
}

private struct CodeBlockCard<Content: View>: View {
    @EnvironmentObject var theme: AppTheme
    var color: Color
    var radius: CGFloat
    var copyText: String
    @ViewBuilder var content: () -> Content
    @State private var showCopy = false

    var body: some View {
        ZStack(alignment: .topTrailing) {
            HScroll {
                content()
                    .padding(.horizontal, 12)
                    .padding(.vertical, 8)
            }
            if showCopy {
                Button {
                    UIPasteboard.general.string = copyText
                    showCopy = false
                } label: {
                    Image(systemName: "doc.on.doc")
                        .foregroundColor(theme.colors.primary)
                        .frame(width: 28, height: 28)
                        .background(color)
                        .clipShape(RoundedRectangle(cornerRadius: radius * 2 / 3))
                }
                .padding(4)
            }
        }
        .background(color)
        .clipShape(RoundedRectangle(cornerRadius: radius))
        .onTapGesture { showCopy.toggle() }
    }
}

private struct HScroll<Content: View>: View {
    @ViewBuilder var content: () -> Content

    var body: some View {
        if #available(iOS 16, *) {
            ViewThatFits(in: .horizontal) {
                content()
                scrollView()
            }
        } else {
            scrollView()
        }
    }

    private func scrollView() -> some View {
        ScrollView(.horizontal, showsIndicators: false) {
            content()
        }
        .background {
            GeometryReader { g in
                Color.clear.preference(key: SwipeExclusionFrames.self, value: [g.frame(in: .named(swipeExclusionSpace))])
            }
        }
    }
}

@inline(__always)
private func mentionText(_ name: String) -> String {
    name.contains(" @") ? "@'\(name)'" : "@\(name)"
}

func simplexLinkText(_ linkType: SimplexLinkType, _ smpHosts: [String]) -> String {
    linkType.description + " " + viaHost(smpHosts)
}

func viaHost(_ smpHosts: [String]) -> String {
    "(via \(smpHosts.first ?? "?"))"
}

func stripTextLink(_ text: String, _ link: String) -> String {
    text == link
        ? ""
        : text.hasSuffix("\n" + link)
        ? String(text.dropLast(link.count + 1))
        : text
}

func stripFormattedTextLink(_ ft: [FormattedText]?, _ link: String) -> [FormattedText]? {
    guard var ft, ft.last?.text == link else { return ft }
    ft.removeLast()
    if let i = ft.indices.last, case .markup = ft[i].format {
        ft.removeLast()
    } else if let i = ft.indices.last, ft[i].format == nil, ft[i].text.hasSuffix("\n") {
        ft[i].text = String(ft[i].text.dropLast())
        if ft[i].text.isEmpty { ft.removeLast() }
    }
    return ft.isEmpty ? nil : ft
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
