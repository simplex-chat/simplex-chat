//
//  ViewModifiers.swift
//  SimpleX (iOS)
//
//  Created by Avently on 12.06.2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension Notification.Name {
    static let chatViewWillBeginScrolling = Notification.Name("chatWillBeginScrolling")
}

struct PrivacyBlur: ViewModifier {
    var enabled: Bool = true
    @Binding var blurred: Bool
    @AppStorage(DEFAULT_PRIVACY_MEDIA_BLUR_RADIUS) private var blurRadius: Int = 0

    func body(content: Content) -> some View {
        if blurRadius > 0 {
            // parallel ifs are necessary here because otherwise some views flicker,
            // e.g. when playing video
            content
                .blur(radius: blurred && enabled ? CGFloat(blurRadius) * 0.5 : 0)
                .overlay {
                    if (blurred && enabled) {
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
