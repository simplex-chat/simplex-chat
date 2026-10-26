import SwiftUI
import SimpleXChat

extension EnvironmentValues {
    struct TerminalLayout: EnvironmentKey {
        static let defaultValue: Bool = false
    }

    var terminalLayout: Bool {
        get { self[TerminalLayout.self] }
        set { self[TerminalLayout.self] = newValue }
    }
}

let chatItemTextPadding: CGFloat = 12
var terminalBlockTopGap: CGFloat { terminalGap(5.75) }
var terminalBlockBottomGap: CGFloat { terminalGap(5) }
var terminalLevelStep: CGFloat { terminalGap(4.75) }
var terminalHeadingBottomGap: CGFloat { terminalLevelStep / 2 }
var terminalSenderTopGap: CGFloat { terminalGap(6.5) }
private var terminalSenderBottomGap: CGFloat { terminalGap(5.5) }
var terminalHeaderTopGap: CGFloat { terminalGap(2.5) }
var terminalHeaderBottomGap: CGFloat { terminalGap(1.5) }
var terminalEventTopGap: CGFloat { terminalGap(8.25) }
var terminalEventBottomGap: CGFloat { terminalGap(1.5) }
var terminalFileTopGap: CGFloat { terminalGap(2) }
var terminalEmojiTopGap: CGFloat { terminalGap(4.25) }
var terminalEmojiBottomGap: CGFloat { terminalGap(1.5) }
var terminalMetaGap: CGFloat { terminalGap(6) }
var terminalNameSize: CGFloat { bodyScaled(13.5 * 17 / 16) }
let terminalHeadingRatio: CGFloat = 1.3125
private let terminalLineHeightRatio: CGFloat = 22 / 16
private let terminalLineHeightContrastSlope: CGFloat = 0.1515
private let terminalGapBodySize: CGFloat = 16

func terminalGap(_ size: CGFloat) -> CGFloat {
    size * UIFont.preferredFont(forTextStyle: .body).pointSize / terminalGapBodySize
}

func bodyScaled(_ points: CGFloat) -> CGFloat {
    UIFontMetrics(forTextStyle: .body).scaledValue(for: points)
}

func messageTextColor(_ theme: AppTheme) -> Color {
    theme.colors.onSurface.opacity(theme.colors.isLight ? 1 : 0.87)
}

func terminalBackgroundColor(_ theme: AppTheme) -> Color {
    theme.wallpaper.background ?? theme.wallpaper.type.defaultBackgroundColor(theme.base, theme.colors.background)
}

struct TerminalLineBox {
    var spacing: CGFloat
    var top: CGFloat
    var bottom: CGFloat
}

func terminalLineBox(_ font: UIFont, _ color: Color?, _ theme: AppTheme, capsOf bodyFont: UIFont? = nil) -> TerminalLineBox {
    if let bodyFont {
        let bodyLineHeight = bodyFont.pointSize * terminalLineHeightRatio
        let body = proportionalExtra(bodyFont, bodyLineHeight)
        return TerminalLineBox(
            spacing: max(0, bodyLineHeight * font.pointSize / bodyFont.pointSize - font.lineHeight),
            top: body.top + bodyFont.ascender - bodyFont.capHeight - (font.ascender - font.capHeight),
            bottom: body.bottom - bodyFont.descender + font.descender
        )
    }
    let lineHeight = terminalLineHeight(font, color, theme)
    let extra = proportionalExtra(font, lineHeight)
    return TerminalLineBox(spacing: max(0, lineHeight - font.lineHeight), top: extra.top, bottom: extra.bottom)
}

func terminalLineHeight(_ font: UIFont, _ color: Color?, _ theme: AppTheme) -> CGFloat {
    font.pointSize * terminalLineHeightRatio * contrastLineHeightFactor(color, theme)
}

private func proportionalExtra(_ font: UIFont, _ lineHeight: CGFloat) -> (top: CGFloat, bottom: CGFloat) {
    let extra = max(0, lineHeight - font.lineHeight)
    let ascent = font.ascender
    let descent = -font.descender
    return (extra * ascent / (ascent + descent), extra * descent / (ascent + descent))
}

private func contrastLineHeightFactor(_ color: Color?, _ theme: AppTheme) -> CGFloat {
    guard let color else { return 1 }
    let background = UIColor(theme.colors.background)
    let textContrast = lightnessContrast(UIColor(messageTextColor(theme)), background)
    let weight = textContrast > 0 ? min(max(lightnessContrast(UIColor(color), background) / textContrast, 0), 1) : 1
    return 1 - terminalLineHeightContrastSlope * (1 - weight)
}

private func lightnessContrast(_ color: UIColor, _ background: UIColor) -> CGFloat {
    abs(lightness(composited(color, over: background)) - lightness(background))
}

private func lightness(_ color: UIColor) -> CGFloat {
    let y = luminance(color)
    return y > 216 / 24389 ? 116 * pow(y, 1 / 3) - 16 : y * 24389 / 27
}

private func luminance(_ color: UIColor) -> CGFloat {
    var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
    color.getRed(&r, green: &g, blue: &b, alpha: &a)
    func linear(_ v: CGFloat) -> CGFloat {
        v <= 0.04045 ? v / 12.92 : pow((v + 0.055) / 1.055, 2.4)
    }
    return 0.2126 * linear(r) + 0.7152 * linear(g) + 0.0722 * linear(b)
}

private func composited(_ color: UIColor, over background: UIColor) -> UIColor {
    var r: CGFloat = 0, g: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
    var br: CGFloat = 0, bg: CGFloat = 0, bb: CGFloat = 0, ba: CGFloat = 0
    color.getRed(&r, green: &g, blue: &b, alpha: &a)
    background.getRed(&br, green: &bg, blue: &bb, alpha: &ba)
    return UIColor(red: r * a + br * (1 - a), green: g * a + bg * (1 - a), blue: b * a + bb * (1 - a), alpha: 1)
}

extension View {
    @ViewBuilder func lineBox(_ box: TerminalLineBox?) -> some View {
        if let box {
            lineSpacing(box.spacing)
                .padding(.top, box.top)
                .padding(.bottom, box.bottom)
        } else {
            self
        }
    }
}

func terminalItemShowsSender(_ item: ChatItem, _ older: ChatItem?) -> Bool {
    guard item.content.hasTerminalSender else { return false }
    guard let older else { return true }
    return !older.content.hasTerminalSender || !sameSender(item, older)
}

private func sameSender(_ item: ChatItem, _ older: ChatItem) -> Bool {
    item.channelIsSender || older.channelIsSender
    ? item.channelIsSender && older.channelIsSender
    : item.chatDir.sameDirection(older.chatDir)
}

func terminalRowGap(_ item: ChatItem, _ older: ChatItem?, separated: Bool, largeGap: Bool, hasSenderRow: Bool) -> CGFloat {
    guard let older, !separated else { return 0 }
    return largeGap || hasSenderRow || !item.content.hasTerminalSender || !older.content.hasTerminalSender || (item.textAtItemTop && item.startsWithHeading)
    ? terminalLevelStep
    : 0
}

func terminalHeadingGap(_ item: ChatItem) -> CGFloat {
    !item.textAtItemTop && item.startsWithHeading ? terminalLevelStep : 0
}

extension ChatItem {
    var channelIsSender: Bool {
        switch chatDir {
        case .channelRcv: true
        case .groupSnd: meta.showGroupAsSender
        default: false
        }
    }

    fileprivate var textAtItemTop: Bool {
        let textContent = switch content.msgContent {
        case .text: true
        case .unknown: file == nil
        default: false
        }
        return textContent && quotedItem == nil && meta.itemForwarded == nil && meta.itemDeleted == nil && !meta.isLive && !isReport
    }

    fileprivate var startsWithHeading: Bool {
        if case .heading = markdownBlocks?.first { true } else { false }
    }
}

private extension CIContent {
    var hasTerminalSender: Bool {
        switch self {
        case .sndMsgContent, .sndDeleted, .sndCall, .sndGroupInvitation, .sndModerated: true
        default: showMemberName
        }
    }
}

private enum MessageLastLineRight: AlignmentID {
    static func defaultValue(in d: ViewDimensions) -> CGFloat { d[.trailing] }
}

private enum MessageLastBlockBottom: AlignmentID {
    static func defaultValue(in d: ViewDimensions) -> CGFloat { d[.bottom] }
}

private extension HorizontalAlignment {
    static let messageLastLineRight = HorizontalAlignment(MessageLastLineRight.self)
}

private extension VerticalAlignment {
    static let messageLastBlockBottom = VerticalAlignment(MessageLastBlockBottom.self)
}

extension View {
    func messageLastLine() -> some View {
        alignmentGuide(.messageLastLineRight) { $0[.trailing] }
    }

    func messageLastBlock() -> some View {
        messageLastLine()
            .alignmentGuide(.messageLastBlockBottom) { $0[.bottom] }
    }
}

func metaCopyJoiner(_ rightToLeft: Bool) -> Text {
    Text(verbatim: rightToLeft ? "\u{2060}\u{200F}" : "\u{2060}\u{200E}")
}

struct ItemWithMeta<Meta: View, Content: View>: View {
    @EnvironmentObject private var theme: AppTheme
    @Environment(\.terminalLayout) private var terminal
    var showMeta = true
    var metaReserved = false
    var overlayMeta: AnyView? = nil
    var endButton: AnyView? = nil
    @ViewBuilder var meta: () -> Meta
    @ViewBuilder var content: (_ inlineMeta: Meta?) -> Content

    var body: some View {
        if terminal && showMeta {
            terminalBody
        } else {
            content(showMeta && !terminal ? meta() : nil)
        }
    }

    @ViewBuilder private var terminalBody: some View {
        if #available(iOS 16, *) {
            TerminalMetaLayout(metaReserved: metaReserved, hasOverlay: overlayMeta != nil, hasEndButton: endButton != nil) {
                ZStack(alignment: .topLeading) {
                    content(nil)
                }
                metaBox(meta(), theme.colors.secondary)
                if let overlayMeta {
                    metaBox(overlayMeta, nil)
                }
                if let endButton {
                    endButton.padding(.trailing, chatItemTextPadding)
                }
            }
        } else {
            HStack(alignment: .lastTextBaseline, spacing: 0) {
                content(nil).frame(maxWidth: .infinity, alignment: .leading)
                if let endButton {
                    endButton
                }
                metaBox(meta(), theme.colors.secondary)
            }
        }
    }

    @ViewBuilder private func metaBox(_ meta: some View, _ lineHeightColor: Color?) -> some View {
        let v = meta
            .lineBox(terminalLineBox(UIFont.preferredFont(forTextStyle: .caption1), lineHeightColor, theme))
            .padding(.trailing, chatItemTextPadding)
        if overlayMeta != nil {
            v.frame(maxWidth: .infinity, maxHeight: .infinity).clipped()
        } else {
            v
        }
    }
}

@available(iOS 16, *)
private struct TerminalMetaLayout: Layout {
    var metaReserved: Bool
    var hasOverlay: Bool
    var hasEndButton: Bool

    private struct Placement {
        var size: CGSize
        var contentProposal: ProposedViewSize
        var meta: CGRect?
        var overlay: CGRect?
        var endButton: CGRect?
    }

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        placement(proposal.width, subviews).size
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        let p = placement(bounds.width, subviews)
        subviews[0].place(at: bounds.origin, proposal: p.contentProposal)
        place(subviews[1], p.meta, bounds)
        if hasOverlay {
            place(subviews[2], p.overlay, bounds)
        }
        if hasEndButton {
            place(subviews[hasOverlay ? 3 : 2], p.endButton, bounds)
        }
    }

    private func place(_ subview: LayoutSubview, _ frame: CGRect?, _ bounds: CGRect) {
        if let frame {
            subview.place(at: CGPoint(x: bounds.minX + frame.minX, y: bounds.minY + frame.minY), proposal: ProposedViewSize(frame.size))
        } else {
            subview.place(at: bounds.origin, proposal: .zero)
        }
    }

    private func placement(_ proposedWidth: CGFloat?, _ subviews: Subviews) -> Placement {
        let width = proposedWidth.flatMap { $0.isFinite ? $0 : nil }
        let meta = subviews[1].sizeThatFits(.unspecified)
        let overlay = hasOverlay ? subviews[2].sizeThatFits(.unspecified) : nil
        let endButton = hasEndButton ? subviews[hasOverlay ? 3 : 2].sizeThatFits(.unspecified) : nil
        let reservedWidth = max(!metaReserved && overlay == nil ? meta.width : 0, endButton?.width ?? 0)
        let contentProposal = ProposedViewSize(width: width.map { max(0, $0 - reservedWidth) }, height: nil)
        let content = subviews[0].dimensions(in: contentProposal)
        let rowWidth = width ?? max(content.width + reservedWidth, meta.width)
        let gap = terminalMetaGap
        let metaBelow = content[explicit: .messageLastLineRight].map { $0 + meta.width > rowWidth } ?? false
        let metaBaseline = subviews[1].dimensions(in: .unspecified)[explicit: .firstTextBaseline]
            ?? (meta.height + UIFont.preferredFont(forTextStyle: .caption1).capHeight) / 2
        let textBaseline = overlay == nil ? content[explicit: .lastTextBaseline] : nil
        let metaTop = max(0, metaBelow
            ? content.height + gap
            : textBaseline.map { $0 - metaBaseline } ?? content.height - gap - meta.height
        )
        let blockGap = content[explicit: .messageLastBlockBottom].map { abs($0 - content.height) < 0.5 } == true ? terminalBlockBottomGap : 0
        let overlayFrame: CGRect? = if let overlay, content.width + meta.width > rowWidth {
            CGRect(x: content.width - overlay.width, y: max(0, content.height - gap - overlay.height), width: overlay.width, height: overlay.height)
        } else {
            nil
        }
        let endButtonFrame: CGRect? = endButton.map {
            CGRect(x: rowWidth - $0.width, y: max(0, ((overlayFrame == nil ? metaTop : content.height) - $0.height) / 2), width: $0.width, height: $0.height)
        }
        return Placement(
            size: CGSize(
                width: rowWidth,
                height: max(content.height + blockGap, metaTop + meta.height, overlayFrame?.height ?? 0, endButtonFrame?.maxY ?? 0)
            ),
            contentProposal: contentProposal,
            meta: overlayFrame == nil ? CGRect(x: rowWidth - meta.width, y: metaTop, width: meta.width, height: meta.height) : nil,
            overlay: overlayFrame,
            endButton: endButtonFrame
        )
    }
}

struct ChatEventWithMeta<Content: View>: View {
    @EnvironmentObject private var theme: AppTheme
    @Environment(\.layoutDirection) private var layoutDirection
    var chatItem: ChatItem
    @ViewBuilder var content: (_ ts: Text) -> Content

    var body: some View {
        ItemWithMeta(metaReserved: true) {
            eventMetaText(color: theme.colors.secondary)
        } content: { inlineMeta in
            content(
                inlineMeta == nil
                ? (metaCopyJoiner(layoutDirection == .rightToLeft) + Text(verbatim: "\u{00A0}\u{00A0}") + eventMetaText(colorMode: .transparent, unbreakable: true)).fontWeight(.regular)
                : chatItem.timestampText
            )
        }
    }

    private func eventMetaText(color: Color = .clear, colorMode: MetaColorMode = .normal, unbreakable: Bool = false) -> Text {
        ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, color: color, colorMode: colorMode, showStatus: false, showEdited: false, showViaProxy: false, showTimesamp: true, unbreakable: unbreakable)
    }
}

var terminalEventPadding: EdgeInsets {
    EdgeInsets(top: terminalEventTopGap, leading: chatItemTextPadding, bottom: terminalEventBottomGap, trailing: chatItemTextPadding)
}

struct ChatItemSender {
    var name: Text
    var badge: LocalBadge?
    var role: String?
    var own: Bool
    var image: AnyView?
}

struct TerminalChatItemRow<Content: View>: View {
    @EnvironmentObject private var theme: AppTheme
    var sender: ChatItemSender?
    var gutter: CGFloat
    var topGap: CGFloat
    var senderTopGap: CGFloat = terminalSenderTopGap
    @ViewBuilder var content: () -> Content

    var body: some View {
        HStack(alignment: .top, spacing: 0) {
            if gutter > 0 {
                ZStack(alignment: .topLeading) {
                    Color.clear.frame(width: gutter, height: 0)
                    sender?.image
                }
            }
            VStack(alignment: .leading, spacing: 0) {
                if let sender {
                    senderNameAndRole(sender)
                        .padding(.leading, chatItemTextPadding)
                        .padding(.bottom, terminalSenderBottomGap)
                }
                content()
            }
        }
        .padding(.top, topGap + (sender == nil ? 0 : senderTopGap))
        .padding(.leading, 8)
        .padding(.trailing, 12)
    }

    private func senderNameAndRole(_ sender: ChatItemSender) -> some View {
        let size = terminalNameSize
        return HStack(alignment: .firstTextBaseline, spacing: 0) {
            NameWithBadge(sender.name.font(.system(size: size)).foregroundColor(sender.own ? theme.colors.primary : theme.colors.secondary), sender.badge, .footnote)
                .lineLimit(1)
            if let role = sender.role {
                Text(role)
                    .font(.system(size: size, weight: .medium))
                    .foregroundColor(theme.colors.secondary)
                    .lineLimit(1)
                    .padding(.leading, 15)
                    .layoutPriority(1)
            }
        }
        .lineBox(terminalLineBox(UIFont.systemFont(ofSize: size), nil, theme))
    }
}
