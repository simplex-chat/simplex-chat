//
//  Timeline.ItemView.AlignmentView.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 15/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

extension Timeline {
    /// Aligns ``Timeline.ItemView`` without requiring a `GeometryReader`
    struct AlignmentContainer<Content: View>: View {
        /// For small screens `minInset` defines minimum inset,
        /// which indicates if message is sent or received
        let minInset: Double = 32

        /// For larger screens `maxWidth` defines maximum message width,
        /// that still preserves readable line length
        let maxWidth: Double = 320
        
        /// Leading or Trailing
        let alignment: Alignment
        @ViewBuilder let content: () -> Content

        var body: some View {
            HStack(spacing: minInset) {
                if alignment == .trailing { Spacer() }
                content().frame(
                    maxWidth: maxWidth,
                    alignment: alignment
                )
                if alignment == .leading { Spacer() }
            }
        }
    }
}
