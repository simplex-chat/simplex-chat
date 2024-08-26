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
    @AppStorage(DEFAULT_CHAT_ITEM_TAIL) private var isTailEnabled = true
    private let shapeStyle: (Double, Bool) -> ChatItemShape.Style

    init() {
        shapeStyle = { roundness, isTailEnabled in
            .roundRect(maxRadius: ChatItemShape.maxRadius)
        }
    }

    init(_ ci: ChatItem, isTailVisible: Bool) {
        shapeStyle = { roundness, isTailEnabled in
            switch ci.content {
            case
                .sndMsgContent,
                .rcvMsgContent,
                .rcvDecryptionError,
                .rcvGroupInvitation,
                .sndGroupInvitation,
                .sndDeleted,
                .rcvDeleted,
                .rcvIntegrityError,
                .sndModerated,
                .rcvModerated,
                .rcvBlocked,
                .invalidJSON: isTailEnabled
                ? .bubble(
                    padding: ci.chatDir.sent ? .trailing : .leading,
                    isTailVisible: {
                        if let mc = ci.content.msgContent, mc.isImageOrVideo, mc.text.isEmpty {
                            false
                        } else {
                            isTailVisible
                        }
                    }()
                )
                : .roundRect(maxRadius: ChatItemShape.maxRadius)
            default: .roundRect(maxRadius: 8)
            }
        }
    }

    func body(content: Content) -> some View {
        let clipShape = ChatItemShape(
            roundness: roundness,
            style: shapeStyle(roundness, isTailEnabled)
        )
        content
            .contentShape(.dragPreview, clipShape)
            .contentShape(.contextMenuPreview, clipShape)
            .clipShape(clipShape)
    }
}

struct ChatTailPadding: ViewModifier {
    func body(content: Content) -> some View {
        content.padding(.horizontal, -ChatItemShape.tailSize)
    }
}

struct ChatItemShape: Shape {

    fileprivate enum Style {
        case bubble(padding: HorizontalEdge, isTailVisible: Bool)
        case roundRect(maxRadius: Double)
    }

    static let tailSize: Double = 8
    static let maxRadius: Double = 18
    fileprivate let roundness: Double
    fileprivate let style: Style

    func path(in rect: CGRect) -> Path {
        switch style {
        case .bubble(let padding, let isTailVisible):
            let rMax = min(Self.maxRadius, min(rect.width, rect.height) / 2)
            let r = roundness * rMax
            let tailHeight = rect.height - (Self.tailSize + (rMax - Self.tailSize) * roundness)
            var path = Path()
            path.addArc(
                center: CGPoint(x: r, y: r),
                radius: r,
                startAngle: .degrees(270),
                endAngle: .degrees(180),
                clockwise: true
            )
            if isTailVisible {
                path.addLine(
                    to: CGPoint(x: 0, y: tailHeight)
                )
                path.addQuadCurve(
                    to: CGPoint(x: -Self.tailSize, y: rect.height),
                    control: CGPoint(x: 0, y: tailHeight + r * 0.64)
                )
            } else {
                path.addArc(
                    center: CGPoint(x: r, y: rect.height - r),
                    radius: r,
                    startAngle: .degrees(180),
                    endAngle: .degrees(90),
                    clockwise: true
                )
            }
            path.addArc(
                center: CGPoint(x: rect.width - r, y: rect.height - r),
                radius: r,
                startAngle: .degrees(90),
                endAngle: .degrees(0),
                clockwise: true
            )
            path.addArc(
                center: CGPoint(x: rect.width - r, y: r),
                radius: r,
                startAngle: .degrees(0),
                endAngle: .degrees(270),
                clockwise: true
            )
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
                case .leading: -Self.tailSize
                case .trailing: Self.tailSize
                }
            } else { 0 }
        case .roundRect: 0
        }
    }

}
