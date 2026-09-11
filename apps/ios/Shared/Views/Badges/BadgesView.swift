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

    private var barCarriesTitle: Bool { shownBadge != nil && !showsAsSheet }

    private var navTitle: LocalizedStringKey { barCarriesTitle ? "Your badge" : "" }

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
        .navigationTitle(navTitle)
        .navigationBarTitleDisplayMode(barCarriesTitle ? .large : .inline)
    }
}
