//
//  BadgesPurchaseStateView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 29.09.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct BadgesPurchaseStateView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) private var dismiss
    let title: LocalizedStringKey
    var message: LocalizedStringKey? = nil
    var failure: BadgeIssueFailure? = nil
    var showsAsSheet: Bool = false

    var body: some View {
        GeometryReader { g in
            ScrollView {
                content
                    .padding(.horizontal, 25)
                    .padding(.top, showsAsSheet ? 48 : 0)
                    .padding(.bottom, 20)
                    .frame(minHeight: g.size.height)
            }
        }
        .frame(maxHeight: .infinity)
        .navigationBarTitleDisplayMode(.inline)
    }

    private var content: some View {
        VStack(alignment: .center, spacing: 16) {
            Text(title)
                .font(.largeTitle)
                .bold()
                .foregroundColor(theme.colors.primary)
                .multilineTextAlignment(.center)
                .fixedSize(horizontal: false, vertical: true)

            if let message {
                Text(message)
                    .font(.body)
                    .multilineTextAlignment(.center)
                    .fixedSize(horizontal: false, vertical: true)
            }

            if let failure {
                VStack(spacing: 6) {
                    Image(systemName: "exclamationmark.triangle")
                        .foregroundColor(.red)
                    Text(failure.purchaseText)
                        .font(.body)
                        .foregroundColor(theme.colors.secondary)
                        .multilineTextAlignment(.center)
                        .fixedSize(horizontal: false, vertical: true)
                }
            }

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
    }
}

extension BadgePurchaseState {
    var title: LocalizedStringKey {
        switch self {
        case .issuing: "Issuing your badge"
        case .waitingForApproval: "Waiting for approval"
        }
    }

    var message: LocalizedStringKey {
        switch self {
        case .issuing: "Your payment is complete. The badge will be added to this profile."
        case .waitingForApproval: "Nothing has been charged."
        }
    }
}
