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
    @ObservedObject private var store = BadgeStore.shared
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
            } else if let purchaseState = store.purchaseState(chatModel.currentUser?.userId) {
                // holds the purchase screens' slot, so a consumable cannot be bought twice
                BadgesPurchaseStateView(title: purchaseState.title, message: purchaseState.message, failure: store.creditError(chatModel.currentUser?.userId), showsAsSheet: showsAsSheet)
                    .transition(.opacity)
                    .onReceive(store.refusals) { refusal in
                        // only the issuing screen belongs to the active profile's held purchase, so a refusal shown there reads as its own
                        if purchaseState == .issuing {
                            showAlert(NSLocalizedString("Purchase error", comment: "alert title"), message: redeemErrorText(refusal, purchase: true))
                        }
                    }
            } else if store.checkingPurchases {
                BadgesPurchaseStateView(title: "Checking your purchases", showsAsSheet: showsAsSheet)
                    .transition(.opacity)
            } else {
                BadgesSupportSimplexView(showsAsSheet: showsAsSheet)
                    .transition(.opacity)
            }
        }
        .animation(.default, value: shownBadge != nil)
        .animation(.default, value: store.purchaseState(chatModel.currentUser?.userId))
        .animation(.default, value: store.checkingPurchases)
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
