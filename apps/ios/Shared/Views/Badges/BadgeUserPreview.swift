//
//  BadgeUserPreview.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 30.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgeUserPreview: View {
    @EnvironmentObject var chatModel: ChatModel
    let level: BadgeLevel

    var body: some View {
        let user = chatModel.currentUser
        let displayName = user?.displayName ?? NSLocalizedString("My nickname", comment: "badges preview placeholder")
        let previewBadge = LocalBadge(
            // fabricated for the preview: the status is given here, and NameBadge renders from it alone
            badge: BadgeInfo(badgeType: level.badgeType, badgeExpiry: .distantFuture),
            status: .active
        )
        return VStack(spacing: 12) {
            ProfileImage(imageStr: user?.image, size: 128)
            NameWithBadge(Text(displayName).font(.largeTitle), previewBadge, .largeTitle)
                .lineLimit(1)
                .minimumScaleFactor(0.75)
        }
    }
}
