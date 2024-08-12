//
//  TimedProgressView.swift
//  SimpleX (iOS)
//
//  Created by User on 12/08/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

/// View modifier that hides it's content until the time has passed
struct AppearAfter: ViewModifier {
    @State private var isVisible: Bool = false
    let seconds: TimeInterval

    func body(content: Content) -> some View {
        content
            .opacity(isVisible ? 1 : 0)
            .task {
                try? await Task.sleep(nanoseconds: UInt64(max(seconds, 0) * 1_000_000_000))
                isVisible = true
            }
    }
}
