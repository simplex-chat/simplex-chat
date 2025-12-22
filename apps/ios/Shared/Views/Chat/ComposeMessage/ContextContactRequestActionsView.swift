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
    @State private var inProgress = false
    @State private var progressByTimeout = false

    var body: some View {
        HStack(spacing: 0) {
            Button(role: .destructive, action: showRejectRequestAlert) {
                Label("Reject", systemImage: "multiply")
            }
            .frame(maxWidth: .infinity, minHeight: 60)
            
            Button {
                if ChatModel.shared.addressShortLinkDataSet {
                    acceptRequest()
                } else {
                    showAcceptRequestAlert()
                }
            } label: {
                Label("Accept", systemImage: "checkmark")
            }
            .frame(maxWidth: .infinity, minHeight: 60)
        }
        .disabled(inProgress)
        .frame(maxWidth: .infinity)
        .background(ToolbarMaterial.material(toolbarMaterial))
        .opacity(progressByTimeout ? 0.4 : 1)
        .overlay {
            if progressByTimeout {
                ProgressView()
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .onChange(of: inProgress) { inPrgrs in
            if inPrgrs {
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    progressByTimeout = inProgress
                }
            } else {
                progressByTimeout = false
            }
        }
    }

    private func showRejectRequestAlert() {
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

    private func showAcceptRequestAlert() {
        showAlert(
            NSLocalizedString("Accept contact request", comment: "alert title"),
            actions: {[
                UIAlertAction(title: NSLocalizedString("Accept", comment: "alert action"), style: .default) { _ in
                    acceptRequest()
                },
                UIAlertAction(title: NSLocalizedString("Accept incognito", comment: "alert action"), style: .default) { _ in
                    acceptRequest(incognito: true)
                },
                cancelAlertAction
            ]}
        )
    }

    private func acceptRequest(incognito: Bool = false) {
        Task {
            await acceptContactRequest(incognito: incognito, contactRequestId: contactRequestId, inProgress: $inProgress)
        }
    }
}

#Preview {
    ContextContactRequestActionsView(
        contactRequestId: 1
    )
}
