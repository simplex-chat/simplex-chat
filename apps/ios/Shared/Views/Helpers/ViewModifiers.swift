//
//  ViewModifiers.swift
//  SimpleX (iOS)
//
//  Created by Avently on 12.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension View {
    @ViewBuilder func `if`<Content: View>(_ condition: @autoclosure () -> Bool, transform: (Self) -> Content) -> some View {
        if condition() {
            transform(self)
        } else {
            self
        }
    }
}

extension Notification.Name {
    static let chatViewWillBeginScrolling = Notification.Name("chatWillBeginScrolling")
}

struct PrivacyBlur: ViewModifier {
    var enabled: Bool
    @Binding var blurred: Bool
    @AppStorage(DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS) private var blurRadius: Int = 0

    func body(content: Content) -> some View {
        if blurRadius > 0 {
            content
                .blur(radius: CGFloat(blurred && enabled ? blurRadius : 0))
                .overlay {
                    if blurred && enabled {
                        Color.clear.contentShape(Rectangle())
                            .onTapGesture {
                                blurred = false
                            }
                    }
                }
                .onReceive(NotificationCenter.default.publisher(for: .chatViewWillBeginScrolling)) { _ in
                    if !blurred {
                        blurred = true
                    }
                }
        } else {
            content
        }
    }
}
