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
    @ObservedObject var chat: Chat
    var contact: Contact
    var groupDirectInv: GroupDirectInvitation
    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @State private var inProgress = false
    @State private var progressByTimeout = false
    @State private var alert: SomeAlert? = nil
    @State private var actionSheet: SomeActionSheet? = nil
    @State private var sheet: SomeSheet<AnyView>? = nil

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
                    Button(role: .destructive, action: showDeleteContactAlert) {
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
        .alert(item: $alert) { $0.alert }
        .actionSheet(item: $actionSheet) { $0.actionSheet }
        .sheet(item: $sheet) {
            if #available(iOS 16.0, *) {
                $0.content
                    .presentationDetents([.fraction(0.4)])
            } else {
                $0.content
            }
        }
    }

    private func showDeleteContactAlert() {
        deleteContactDialog(
            chat,
            contact,
            dismissToChatList: true,
            showAlert: { alert = $0 },
            showActionSheet: { actionSheet = $0 },
            showSheetContent: { sheet = $0 }
        )
    }

    private func acceptRequest() {
        Task {
            await acceptMemberContact(contactId: contact.contactId, inProgress: $inProgress)
        }
    }
}

#Preview {
    ContextGroupDirectInvitationActionsView(
        chat: Chat.sampleData,
        contact: Contact.sampleData,
        groupDirectInv: GroupDirectInvitation.sampleData
    )
}
