//
//  ContextContactRequestActionsView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 02.05.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContextContactRequestActionsView: View {
    @EnvironmentObject var theme: AppTheme
    var contactRequestId: Int64

    var body: some View {
        HStack(spacing: 0) {
            ZStack {
                Text("Reject")
                    .foregroundColor(.red)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showRejectRequestAlert(contactRequestId)
            }

            ZStack {
                Text("Accept")
                    .foregroundColor(theme.colors.primary)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showAcceptRequestAlert(contactRequestId)
            }
        }
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity)
        .background(.thinMaterial)
    }
}

func showRejectRequestAlert(_ contactRequestId: Int64) {
    showAlert(
        title: NSLocalizedString("Reject contact request", comment: "alert title"),
        message: NSLocalizedString("The sender will NOT be notified", comment: "alert message"),
        buttonTitle: "Reject",
        buttonAction: {
            Task {
                await rejectContactRequest(contactRequestId, dismissToChatList: true)
            }
        },
        cancelButton: true
    )
}

func showAcceptRequestAlert(_ contactRequestId: Int64) {
    showAlert(
        NSLocalizedString("Accept contact request", comment: "alert title"),
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Accept", comment: "alert action"),
                style: .default,
                handler: { _ in
                    Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequestId) }
                }
            ),
            // TODO [short links] if !userAddress.shortLinkDataSet; check other places
            // UIAlertAction(
            //     title: NSLocalizedString("Accept incognito", comment: "alert action"),
            //     style: .default,
            //     handler: { _ in
            //         Task { await acceptContactRequest(incognito: true, contactRequest: contactRequest) }
            //    }
            // ),
            UIAlertAction(
                title: NSLocalizedString("Cancel", comment: "alert action"),
                style: .default
            )
        ]}
    )
}

#Preview {
    ContextContactRequestActionsView(
        contactRequestId: 1
    )
}
