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
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    @Environment(\.showTimestamp) var showTimestamp: Bool
    var chatItem: ChatItem
    var metaColor: Color
    var paleMetaColor = Color(uiColor: .tertiaryLabel)
    var showStatus = true
    var showEdited = true
    var invertedMaterial = false

    @AppStorage(DEFAULT_SHOW_SENT_VIA_RPOXY) private var showSentViaProxy = false

    var body: some View {
        if chatItem.isDeletedContent {
            chatItem.timestampText.font(.caption).foregroundColor(metaColor)
        } else {
            ZStack {
                ciMetaText(
                    chatItem.meta,
                    chatTTL: chat.chatInfo.timedMessagesTTL,
                    encrypted: chatItem.encryptedFile,
                    color: metaColor,
                    paleColor: paleMetaColor,
                    colorMode: invertedMaterial
                        ? .invertedMaterial
                        : .normal,
                    showStatus: showStatus,
                    showEdited: showEdited,
                    showViaProxy: showSentViaProxy,
                    showTimesamp: showTimestamp
                ).invertedForegroundStyle(enabled: invertedMaterial)
                if invertedMaterial {
                    ciMetaText(
                        chatItem.meta,
                        chatTTL: chat.chatInfo.timedMessagesTTL,
                        encrypted: chatItem.encryptedFile,
                        colorMode: .normal,
                        onlyOverrides: true,
                        showStatus: showStatus,
                        showEdited: showEdited,
                        showViaProxy: showSentViaProxy,
                        showTimesamp: showTimestamp
                    )
                }
            }
        }
    }
}

enum MetaColorMode {
    // Renders provided colours
    case normal
    // Fully transparent meta - used for reserving space
    case transparent
    // Renders white on dark backgrounds and black on light ones
    case invertedMaterial

    func resolve(_ c: Color?) -> Color? {
        switch self {
        case .normal: c
        case .transparent: .clear
        case .invertedMaterial: nil
        }
    }

    func resolveAttr(_ c: UIColor?) -> UIColor? {
        switch self {
        case .normal: c
        case .transparent: .clear
        case .invertedMaterial: nil
        }
    }

    func statusSpacer(_ sent: Bool) -> Text {
        switch self {
        case .normal, .transparent:
            Text(
                sent
                ? Image("checkmark.wide")
                : Image(systemName: "circlebadge.fill")
            ).foregroundColor(.clear)
        case .invertedMaterial: textSpace.kerning(13)
        }
    }

    func statusSpacerAttr(_ sent: Bool, _ font: UIFont) -> NSAttributedString {
        switch self {
        case .normal, .transparent:
            if let image = sent ? UIImage(named: "checkmark.wide") : UIImage(systemName: "circlebadge.fill") {
                attachedImage(image, font, .clear)
            } else {
                NSAttributedString("")
            }
        case .invertedMaterial:
            NSAttributedString(string: " ", attributes: [
                .font: font,
                .kern: 13
            ])
        }
    }
}

func ciMetaTextAttributed(
    _ meta: CIMeta,
    chatTTL: Int?,
    encrypted: Bool?,
    color: UIColor = .clear, // we use this function to reserve space without rendering meta
    paleColor: UIColor? = nil,
    primaryColor: UIColor = .tintColor,
    colorMode: MetaColorMode = .normal,
    onlyOverrides: Bool = false, // only render colors that differ from base
    showStatus: Bool = true,
    showEdited: Bool = true,
    showViaProxy: Bool,
    showTimesamp: Bool
) -> NSMutableAttributedString {
    let r = NSMutableAttributedString()
    var space: NSAttributedString? = nil
    let font = UIFont.preferredFont(forTextStyle: .caption1)
    let attrSpace = NSAttributedString(string: " ", attributes: [.font: font])
    let appendSpace = {
        if let sp = space {
            r.append(sp)
            space = nil
        }
    }
    let resolved = colorMode.resolveAttr(color)
    if showEdited, meta.itemEdited {
        r.append(attachedSystemImage("pencil", font, resolved))
    }
    if meta.disappearing {
        r.append(attachedSystemImage("timer", UIFont.preferredFont(forTextStyle: .caption2), resolved))
        let ttl = meta.itemTimed?.ttl
        if ttl != chatTTL {
            r.append(coloredAttributed(shortTimeText(ttl), font, resolved))
        }
        space = attrSpace
    }
    if showViaProxy, meta.sentViaProxy == true {
        appendSpace()
        r.append(attachedSystemImage("arrow.forward", UIFont.preferredFont(forTextStyle: .caption2), resolved?.withAlphaComponent(0.67)))
    }
    if showStatus {
        appendSpace()
        if let (image, statusColor) = meta.itemStatus.statusIconAttr(color, paleColor ?? color, primaryColor) {
            let metaColor = if onlyOverrides && statusColor == color {
                UIColor.clear
            } else {
                colorMode.resolveAttr(statusColor)
            }
            r.append(attachedImage(image, font, metaColor))
        } else if !meta.disappearing {
            r.append(colorMode.statusSpacerAttr(meta.itemStatus.sent, font))
        }
        space = attrSpace
    }
    if let enc = encrypted {
        appendSpace()
        r.append(attachedSystemImage(enc ? "lock" : "lock.open", font, resolved))
        space = attrSpace
    }
    if showTimesamp {
        appendSpace()
        r.append(coloredAttributed(formatTimestampMeta(meta.itemTs), font, resolved))
    }
    return r
}

func ciMetaText(
    _ meta: CIMeta,
    chatTTL: Int?,
    encrypted: Bool?,
    color: Color = .clear, // we use this function to reserve space without rendering meta
    paleColor: Color? = nil,
    primaryColor: Color = .accentColor,
    colorMode: MetaColorMode = .normal,
    onlyOverrides: Bool = false, // only render colors that differ from base
    showStatus: Bool = true,
    showEdited: Bool = true,
    showViaProxy: Bool,
    showTimesamp: Bool
) -> Text {
    var r = Text("")
    var space: Text? = nil
    let appendSpace = {
        if let sp = space {
            r = r + sp
            space = nil
        }
    }
    let resolved = colorMode.resolve(color)
    if showEdited, meta.itemEdited {
        r = r + statusIconText("pencil", resolved)
    }
    if meta.disappearing {
        r = r + statusIconText("timer", resolved).font(.caption2)
        let ttl = meta.itemTimed?.ttl
        if ttl != chatTTL {
            r = r + colored(Text(shortTimeText(ttl)), resolved)
        }
        space = textSpace
    }
    if showViaProxy, meta.sentViaProxy == true {
        appendSpace()
        r = r + statusIconText("arrow.forward", resolved?.opacity(0.67)).font(.caption2)
    }
    if showStatus {
        appendSpace()
        if let (image, statusColor) = meta.itemStatus.statusIcon(color, paleColor ?? color, primaryColor) {
            let metaColor = if onlyOverrides && statusColor == color {
                Color.clear
            } else {
                colorMode.resolve(statusColor)
            }
            r = r + colored(Text(image), metaColor)
        } else if !meta.disappearing {
            r = r + colorMode.statusSpacer(meta.itemStatus.sent)
        }
        space = textSpace
    }
    if let enc = encrypted {
        appendSpace()
        r = r + statusIconText(enc ? "lock" : "lock.open", resolved)
        space = textSpace
    }
    if showTimesamp {
        appendSpace()
        r = r + colored(meta.timestampText, resolved)
    }
    return r.font(.caption)
}

private func statusIconText(_ icon: String, _ color: Color?) -> Text {
    colored(Text(Image(systemName: icon)), color)
}

//private func statusIconTextAttributed(_ icon: String, _ color: UIColor) -> NSMutableAttributedString {
//    colored(Text(Image(systemName: icon)), color)
//}

func attachedSystemImage(_ systemName: String, _ font: UIFont, _ color: UIColor?) -> NSAttributedString {
    if let image = UIImage(systemName: systemName) {
        print("attachedSystemImage \(systemName)")
        return attachedImage(image, font, color)
    } else {
        return NSAttributedString("")
    }
}

func attachedImage(_ image: UIImage, _ font: UIFont, _ color: UIColor?) -> NSAttributedString {
    let attachment = NSTextAttachment()
    attachment.image = image.withTintColor(UIColor.label, renderingMode: .automatic) // coloredImage(image, color)
//    let scale = font.capHeight / max(image.size.height, 1)
//    attachment.bounds = CGRect(x: 0, y: (font.capHeight - image.size.height * scale) / 2, width: image.size.width * scale, height: image.size.height * scale)
//    attachment.bounds = CGRect(x: 0, y: -font.descender, width: image.size.width * scale, height: image.size.height * scale)
//    let result = NSMutableAttributedString(attachment: attachment)
//    result.addAttributes([.font: font, .baselineOffset: 0], range: NSRange(location: 0, length: result.length))
    let res = NSMutableAttributedString(attachment: attachment)
    res.addAttributes([.font: font, .foregroundColor: UIColor.label], range: NSRange(location: 0, length: res.length))
    return res
}

// Applying `foregroundColor(nil)` breaks `.invertedForegroundStyle` modifier
private func colored(_ t: Text, _ color: Color?) -> Text {
    if let color {
        t.foregroundColor(color)
    } else {
        t
    }
}

@inline(__always)
private func coloredImage(_ img: UIImage, _ color: UIColor?) -> UIImage {
    if let color {
        img.withTintColor(color, renderingMode: .alwaysOriginal)
    } else {
        img
    }
}

private func coloredAttributed(_ s: String, _ font: UIFont, _ color: UIColor?) -> NSAttributedString {
    var attrs: [NSAttributedString.Key: Any] = [.font: font]
    if let color {
        attrs[.foregroundColor] = color
    }
    return NSAttributedString(string: s, attributes: attrs)
}

struct CIMetaView_Previews: PreviewProvider {
    static let metaColor = Color.secondary
    static var previews: some View {
        Group {
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .partial)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .ok, sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .ok, sndProgress: .partial)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndRcvd(msgRcptStatus: .badMsgHash, sndProgress: .complete)), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getSample(2, .directSnd, .now, "https://simplex.chat", .sndSent(sndProgress: .complete), itemEdited: true), metaColor: metaColor)
            CIMetaView(chat: Chat.sampleData, chatItem: ChatItem.getDeletedContentSample(), metaColor: metaColor)
        }
        .previewLayout(.fixed(width: 360, height: 100))
    }
}
