//
//  ContextGroupDirectInvitationActionsView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 31.07.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContextGroupDirectInvitationActionsView: View {
    @EnvironmentObject var theme: AppTheme
    var contact: Contact
    var groupDirectInv: GroupDirectInvitation
    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @State private var inProgress = false
    @State private var progressByTimeout = false

    var body: some View {
        VStack {
            if groupDirectInv.memberRemoved {
                Label("Member is deleted - can't accept request", systemImage: "info.circle")
                    .foregroundColor(theme.colors.secondary)
                    .font(.subheadline)
                    .padding(.horizontal)
                    .frame(maxWidth: .infinity, minHeight: 60)
            } else {
                HStack(spacing: 0) {
                    Button(role: .destructive, action: showRejectRequestAlert) {
                        Label("Reject", systemImage: "multiply")
                    }
                    .frame(maxWidth: .infinity, minHeight: 60)

                    Button {
                        acceptRequest()
                    } label: {
                        Label("Accept", systemImage: "checkmark")
                    }
                    .frame(maxWidth: .infinity, minHeight: 60)
                }
            }
        }
        .disabled(inProgress || groupDirectInv.memberRemoved)
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
                    deleteContact()
                },
                cancelAlertAction
            ]}
        )
    }

    func deleteContact() {
        Task {
            do {
                let _ct = try await apiDeleteContact(id: contact.contactId, chatDeleteMode: .full(notify: false))
                await MainActor.run {
                    ChatModel.shared.removeChat(contact.id)
                    ChatModel.shared.chatId = nil
                }
            } catch let error {
                logger.error("apiDeleteContact: \(responseError(error))")
                await MainActor.run {
                    showAlert(
                        NSLocalizedString("Error deleting chat!", comment: "alert title"),
                        message: responseError(error)
                    )
                }
            }
        }
    }

    private func acceptRequest() {
        Task {
            await acceptMemberContact(contactId: contact.contactId, inProgress: $inProgress)
        }
    }
}

#Preview {
    ContextGroupDirectInvitationActionsView(
        contact: Contact.sampleData,
        groupDirectInv: GroupDirectInvitation.sampleData
    )
}
