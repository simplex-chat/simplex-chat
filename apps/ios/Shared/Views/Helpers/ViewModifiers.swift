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
    @Binding var blurred: Bool
    @AppStorage(DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS) private var blurRadius: Int = 0

    func body(content: Content) -> some View {
        if blurRadius > 0 {
            content
                .if(blurred) { view in
                    view
                        .blur(radius: CGFloat(blurRadius) * 0.5)
                        .overlay {
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
