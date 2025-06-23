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
    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial

    var body: some View {
        HStack(spacing: 0) {
            Label("Reject", systemImage: "multiply")
            .foregroundColor(.red)
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showRejectRequestAlert(contactRequestId)
            }

            Label("Accept", systemImage: "checkmark").foregroundColor(theme.colors.primary)
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                if ChatModel.shared.addressShortLinkDataSet {
                    Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequestId) }
                } else {
                    showAcceptRequestAlert(contactRequestId)
                }
            }
        }
        .frame(minHeight: 60)
        .frame(maxWidth: .infinity)
        .background(ToolbarMaterial.material(toolbarMaterial))
    }
}

func showRejectRequestAlert(_ contactRequestId: Int64) {
    showAlert(
        NSLocalizedString("Reject contact request", comment: "alert title"),
        message: NSLocalizedString("The sender will NOT be notified", comment: "alert message"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Reject", comment: "alert action"), style: .destructive) { _ in
                Task { await rejectContactRequest(contactRequestId, dismissToChatList: true) }
            },
            cancelAlertAction
        ]}
    )
}

func showAcceptRequestAlert(_ contactRequestId: Int64) {
    showAlert(
        NSLocalizedString("Accept contact request", comment: "alert title"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Accept", comment: "alert action"), style: .default) { _ in
                Task { await acceptContactRequest(incognito: false, contactRequestId: contactRequestId) }
            },
            UIAlertAction(title: NSLocalizedString("Accept incognito", comment: "alert action"), style: .default) { _ in
                Task { await acceptContactRequest(incognito: true, contactRequestId: contactRequestId) }
            },
            cancelAlertAction
        ]}
    )
}

#Preview {
    ContextContactRequestActionsView(
        contactRequestId: 1
    )
}
