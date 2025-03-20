//
//  ChatItemClipShape.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 04/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

/// Modifier, which provides clipping mask for ``ChatItemWithMenu`` view 
/// and it's previews: (drag interaction, context menu, etc.)
/// Supports [Dynamic Type](https://developer.apple.com/documentation/uikit/uifont/scaling_fonts_automatically)
/// by retaining pill shape, even when ``ChatItem``'s height is less that twice its corner radius
struct ChatItemClipped: ViewModifier {
    @AppStorage(DEFAULT_CHAT_ITEM_ROUNDNESS) private var roundness = defaultChatItemRoundness
    @AppStorage(DEFAULT_CHAT_ITEM_TAIL) private var tailEnabled = true
    private let chatItem: (content: CIContent, chatDir: CIDirection)?
    private let tailVisible: Bool

    init() {
        self.chatItem = nil
        self.tailVisible = false
    }

    init(_ ci: ChatItem, tailVisible: Bool) {
        self.chatItem = (ci.content, ci.chatDir)
        self.tailVisible = tailVisible
    }
    
    private func shapeStyle() -> ChatItemShape.Style {
        if let ci = chatItem {
            switch ci.content {
            case
                    .sndMsgContent,
                    .rcvMsgContent,
                    .rcvDecryptionError,
                    .rcvIntegrityError,
                    .invalidJSON:
                let tail = if let mc = ci.content.msgContent, mc.isImageOrVideo && mc.text.isEmpty {
                    false
                } else {
                    tailVisible
                }
                return tailEnabled
                ? .bubble(
                    padding: ci.chatDir.sent ? .trailing : .leading,
                    tailVisible: tail
                )
                : .roundRect(radius: msgRectMaxRadius)
            case .rcvGroupInvitation, .sndGroupInvitation:
                return .roundRect(radius: msgRectMaxRadius)
            default: return .roundRect(radius: 8)
            }
        } else {
            return .roundRect(radius: msgRectMaxRadius)
        }
    }

    func body(content: Content) -> some View {
        let clipShape = ChatItemShape(
            roundness: roundness,
            style: shapeStyle()
        )
        content
            .contentShape(.dragPreview, clipShape)
            .contentShape(.contextMenuPreview, clipShape)
            .clipShape(clipShape)
    }
}

struct ChatTailPadding: ViewModifier {
    func body(content: Content) -> some View {
        content.padding(.horizontal, -msgTailWidth)
    }
}

private let msgRectMaxRadius: Double = 18
private let msgBubbleMaxRadius: Double = msgRectMaxRadius * 1.2
private let msgTailWidth: Double = 9
private let msgTailMinHeight: Double = msgTailWidth * 1.254 // ~56deg
private let msgTailMaxHeight: Double = msgTailWidth * 1.732 // 60deg

struct ChatItemShape: Shape {
    fileprivate enum Style {
        case bubble(padding: HorizontalEdge, tailVisible: Bool)
        case roundRect(radius: Double)
    }

    fileprivate let roundness: Double
    fileprivate let style: Style

    func path(in rect: CGRect) -> Path {
        switch style {
        case let .bubble(padding, tailVisible):
            let w = rect.width
            let h = rect.height
            let rxMax = min(msgBubbleMaxRadius, w / 2)
            let ryMax = min(msgBubbleMaxRadius, h / 2)
            let rx = roundness * rxMax
            let ry = roundness * ryMax
            let tailHeight = min(msgTailMinHeight + roundness * (msgTailMaxHeight - msgTailMinHeight), h / 2)
            var path = Path()
            // top side
            path.move(to: CGPoint(x: rx, y: 0))
            path.addLine(to: CGPoint(x: w - rx, y: 0))
            if roundness > 0 {
                // top-right corner
                path.addQuadCurve(to: CGPoint(x: w, y: ry), control: CGPoint(x: w, y: 0))
            }
            if rect.height > 2 * ry {
                // right side
                path.addLine(to: CGPoint(x: w, y: h - ry))
            }
            if roundness > 0 {
                // bottom-right corner
                path.addQuadCurve(to: CGPoint(x: w - rx, y: h), control: CGPoint(x: w, y: h))
            }
            // bottom side
            if tailVisible {
                path.addLine(to: CGPoint(x: -msgTailWidth, y: h))
                if roundness > 0 {
                    // bottom-left tail
                    // distance of control point from touch point, calculated via ratios
                    let d = tailHeight - msgTailWidth * msgTailWidth / tailHeight
                    // tail control point
                    let tc = CGPoint(x: 0, y: h - tailHeight + d * sqrt(roundness))
                    // bottom-left tail curve
                    path.addQuadCurve(to: CGPoint(x: 0, y: h - tailHeight), control: tc)
                } else {
                    path.addLine(to: CGPoint(x: 0, y: h - tailHeight))
                }
                if rect.height > ry + tailHeight {
                    // left side
                    path.addLine(to: CGPoint(x: 0, y: ry))
                }
            } else {
                path.addLine(to: CGPoint(x: rx, y: h))
                path.addQuadCurve(to: CGPoint(x: 0, y: h - ry), control: CGPoint(x: 0 , y: h))
                if rect.height > 2 * ry {
                    // left side
                    path.addLine(to: CGPoint(x: 0, y: ry))
                }
            }
            if roundness > 0 {
                // top-left corner
                path.addQuadCurve(to: CGPoint(x: rx, y: 0), control: CGPoint(x: 0, y: 0))
            }
            path.closeSubpath()
            return switch padding {
            case .leading: path
            case .trailing: path
                    .scale(x: -1, y: 1, anchor: .center)
                    .path(in: rect)
            }
        case let .roundRect(radius):
            return Path(roundedRect: rect, cornerRadius: radius * roundness)
        }
    }

    var offset: Double? {
        switch style {
        case let .bubble(padding, isTailVisible):
            if isTailVisible {
                switch padding {
                case .leading: -msgTailWidth
                case .trailing: msgTailWidth
                }
            } else { 0 }
        case .roundRect: 0
        }
    }

}
