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
    struct ClipShape: Shape {
        let maxCornerRadius: Double
        
        func path(in rect: CGRect) -> Path {
            Path(
                roundedRect: rect,
                cornerRadius: min((rect.height / 2), maxCornerRadius), 
                style: .circular
            )
        }
    }
    
    init() {
        clipShape = ClipShape(
            maxCornerRadius: 18
        )
    }
    
    init(_ chatItem: ChatItem) {
        clipShape = ClipShape(
            maxCornerRadius: {
                switch chatItem.content {
                case 
                    .sndMsgContent, 
                    .rcvMsgContent, 
                    .sndDeleted, 
                    .rcvDeleted, 
                    .sndModerated, 
                    .rcvModerated, 
                    .rcvBlocked: 18
                default: 8
                }
            }()
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


