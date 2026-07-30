//
//  BadgeUserPreview.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 30.07.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// Shows the user's avatar and display name with a synthesized preview badge at the selected
// level, using the same NameWithBadge helper that renders badges everywhere else in the app —
// so the preview matches what will actually appear on the user's name. Any picker affordance
// (e.g. a chevron) is supplied by the caller via `trailing`, and is laid out next to the
// name row without leaking into this view.
struct BadgeUserPreview<Trailing: View>: View {
    @EnvironmentObject var chatModel: ChatModel
    let level: BadgeLevel
    let trailing: () -> Trailing

    init(level: BadgeLevel, @ViewBuilder trailing: @escaping () -> Trailing = { EmptyView() }) {
        self.level = level
        self.trailing = trailing
    }

    var body: some View {
        let user = chatModel.currentUser
        let displayName = user?.displayName ?? NSLocalizedString("My nickname", comment: "badges preview placeholder")
        let previewBadge = LocalBadge(
            badge: BadgeInfo(badgeType: level.badgeType),
            status: .active
        )
        return VStack(spacing: 12) {
            ProfileImage(imageStr: user?.image, size: 128)
            HStack(alignment: .center, spacing: 6) {
                NameWithBadge(Text(displayName).font(.largeTitle), previewBadge, .largeTitle)
                    .lineLimit(1)
                    .minimumScaleFactor(0.75)
                trailing()
            }
        }
    }
}
