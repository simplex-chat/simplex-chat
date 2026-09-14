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
    @State private var redeemCodeActive = false

    private var shownBadge: BadgeState? {
        guard badgeModel.userId == chatModel.currentUser?.userId else { return nil }
        if let badgeState = badgeModel.badgeState, badgeState.shown { return badgeState }
        return nil
    }

    var body: some View {
        ZStack {
            Group {
                if let badgeState = shownBadge {
                    BadgesYourBadgeView(badgeState: badgeState, showsAsSheet: showsAsSheet)
                        .transition(.opacity)
                } else {
                    BadgesSupportSimplexView(showsAsSheet: showsAsSheet, redeemCodeActive: $redeemCodeActive)
                        .transition(.opacity)
                }
            }
            .animation(.default, value: shownBadge != nil)

            // here and not in the Support screen: the redeem view is pushed before the badge is shown and
            // popped after, and a link inside the Support screen is gone by then - on some iOS versions
            // that leaves the pushed view with nothing to pop through
            NavigationLink(isActive: $redeemCodeActive) {
                BadgesRedeemCodeView(active: $redeemCodeActive)
                    .modifier(ThemedBackground())
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
    }
}
