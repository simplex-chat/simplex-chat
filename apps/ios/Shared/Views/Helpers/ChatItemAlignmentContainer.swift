//
//  ChatItemAlignmentContainer.swift
//  SimpleX (iOS)
//
//  Created by Levitating Pineapple on 27/06/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatItemAlignmentContainer<Content: View>: View {
    /// Sent items are aligned to trailing edge, while received items to leading
    let isSent: Bool

    /// For small screens minimum inset determines if item is sent or received
    let minInset: Double

    /// For large screens max width limits item's maximum width
    let maxWidth: Double

    let content: () -> Content

    var body: some View {
        content()
            .frame(
                maxWidth: maxWidth,
                alignment: isSent ? .trailing : .leading
            )
            .padding(
                isSent ? .leading : .trailing,
                minInset
            )
            .frame(
                maxWidth: .infinity,
                alignment: isSent ? .trailing : .leading
            )
    }
}
