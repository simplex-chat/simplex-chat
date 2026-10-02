//
//  BadgesPurchaseStateView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct BadgesPurchaseStateView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) private var dismiss
    let purchaseState: BadgePurchaseState
    var showsAsSheet: Bool = false

    var body: some View {
        VStack(alignment: .center, spacing: 16) {
            Text(title)
                .font(.largeTitle)
                .bold()
                .foregroundColor(theme.colors.primary)
                .multilineTextAlignment(.center)
                .fixedSize(horizontal: false, vertical: true)

            Text(message)
                .font(.body)
                .multilineTextAlignment(.center)
                .fixedSize(horizontal: false, vertical: true)

            Spacer()

            ProgressView().scaleEffect(2)

            Spacer()

            VStack(spacing: 10) {
                Button {
                    dismiss()
                } label: {
                    Text("Dismiss")
                        .font(.body)
                        .fontWeight(.medium)
                        .foregroundColor(theme.colors.primary)
                        .padding()
                }
                .padding(.vertical, 10)
                Color.clear
                    .frame(height: 22)
            }
        }
        .padding(.horizontal, 25)
        .padding(.top, showsAsSheet ? 48 : 0)
        .padding(.bottom, 20)
        .frame(maxHeight: .infinity)
        .navigationBarTitleDisplayMode(.inline)
    }

    private var title: LocalizedStringKey {
        switch purchaseState {
        case .issuing: "Issuing your badge"
        case .waitingForApproval: "Waiting for approval"
        case .checking: "Checking your purchase"
        }
    }

    private var message: LocalizedStringKey {
        switch purchaseState {
        case .issuing: "Your payment is complete. The badge will be added to this profile."
        case .waitingForApproval: "Nothing has been charged."
        case .checking: "The store has not confirmed a purchase yet."
        }
    }
}
