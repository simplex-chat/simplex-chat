//
//  BadgesView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject private var badgeModel = BadgeModel.shared
    var showsAsSheet: Bool = false

    private var shownBadge: BadgeState? {
        guard badgeModel.userId == chatModel.currentUser?.userId else { return nil }
        if let badgeState = badgeModel.badgeState, badgeState.shown { return badgeState }
        return nil
    }

    var body: some View {
        Group {
            if let badgeState = shownBadge {
                BadgesYourBadgeView(badgeState: badgeState, showsAsSheet: showsAsSheet)
                    .transition(.opacity)
            } else {
                BadgesSupportSimplexView(showsAsSheet: showsAsSheet)
                    .transition(.opacity)
            }
        }
        .animation(.default, value: shownBadge != nil)
    }
}

var supportSimpleXAlertAction: UIAlertAction {
    UIAlertAction(title: NSLocalizedString("Support SimpleX", comment: "alert button"), style: .default) { _ in
        openBadgesView()
    }
}

func openBadgesView() {
    showAppSheet {
        NavigationView {
            BadgesView(showsAsSheet: true)
                .modifier(ThemedBackground())
        }
    }
}

// false until the badge state is loaded for the current profile, so a supporter is never pitched to
func noShownBadge() -> Bool {
    BadgeModel.shared.badgeState?.shown != true && BadgeModel.shared.userId == ChatModel.shared.currentUser?.userId
}
