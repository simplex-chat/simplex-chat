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

    enum TailEdge {
        case leading
        case trailing
    }

    struct ClipShape: Shape {
        let maxCornerRadius: Double
        var tailEdge: TailEdge?

        func path(in rect: CGRect) -> Path {
            .roundedRectangle(
                in: rect,
                radius: maxCornerRadius,
                bottomLeading: tailEdge == .leading ? 0 : nil,
                bottomTrailing: tailEdge == .trailing ? 0 : nil
            )
        }
    }
    
    init() {
        clipShape = ClipShape(
            maxCornerRadius: 18
        )
    }
    
    init(_ chatItem: ChatItem, showsTail: Bool) {
        clipShape = ClipShape(
            maxCornerRadius: {
                switch chatItem.content {
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
                    .invalidJSON: 18
                default: 8
                }
            }(),
            tailEdge: showsTail
            ? chatItem.chatDir.sent ? .trailing : .leading
            : nil
        )
    }
    
    private let clipShape: ClipShape
    
    func body(content: Content) -> some View {
        content
            .contentShape(.dragPreview, clipShape)
            .contentShape(.contextMenuPreview, clipShape)
            .clipShape(clipShape)
    }
}

extension Path {
    static func roundedRectangle(
        in rect: CGRect,
        radius: CGFloat,
        topLeading: CGFloat? = nil,
        bottomLeading: CGFloat? = nil,
        bottomTrailing: CGFloat? = nil,
        topTrailing: CGFloat? = nil
    ) -> Path {
        let maxRadius = min(rect.width, rect.height) / 2
        let tl = min(maxRadius, topLeading ?? radius)
        let bl = min(maxRadius, bottomLeading ?? radius)
        let bt = min(maxRadius, bottomTrailing ?? radius)
        let tt = min(maxRadius, topLeading ?? radius)
        var path = Path()
        path.addArc(
            center: CGPoint(x: tl, y: tl),
            radius: tl,
            startAngle: .degrees(270),
            endAngle: .degrees(180),
            clockwise: true
        )
        path.addArc(
            center: CGPoint(x: bl, y: rect.height - bl),
            radius: bl,
            startAngle: .degrees(180),
            endAngle: .degrees(90),
            clockwise: true
        )
        path.addArc(
            center: CGPoint(x: rect.width - bt, y: rect.height - bt),
            radius: bt,
            startAngle: .degrees(90),
            endAngle: .degrees(0),
            clockwise: true
        )
        path.addArc(
            center: CGPoint(x: rect.width - tt, y: tt),
            radius: tt,
            startAngle: .degrees(0),
            endAngle: .degrees(270),
            clockwise: true
        )
        return path
    }
}
