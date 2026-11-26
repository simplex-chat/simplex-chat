//
//  ContextPendingMemberActionsView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 02.05.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContextPendingMemberActionsView: View {
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss
    var groupInfo: GroupInfo
    var member: GroupMember
    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial

    var body: some View {
        HStack(spacing: 0) {
            ZStack {
                Text("Reject")
                    .foregroundColor(.red)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showRejectMemberAlert(groupInfo, member, dismiss: dismiss)
            }

            ZStack {
                Text("Accept")
                    .foregroundColor(theme.colors.primary)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showAcceptMemberAlert(groupInfo, member, dismiss: dismiss)
            }
        }
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity)
        .background(ToolbarMaterial.material(toolbarMaterial))
    }
}

func showRejectMemberAlert(_ groupInfo: GroupInfo, _ member: GroupMember, dismiss: DismissAction? = nil) {
    showAlert(
        title: NSLocalizedString("Reject member?", comment: "alert title"),
        buttonTitle: "Reject",
        buttonAction: { removeMember(groupInfo, member, withMessages: false,  dismiss: dismiss) },
        cancelButton: true
    )
}

func showAcceptMemberAlert(_ groupInfo: GroupInfo, _ member: GroupMember, dismiss: DismissAction? = nil) {
    let blocked = (ChatModel.shared.getGroupMember(member.groupMemberId)?.wrapped ?? member).blockedByAdmin
    let accept: (GroupMemberRole) -> Void = { role in
        if blocked {
            blockMemberForAll(groupInfo, member, false) { acceptMember(groupInfo, member, role, dismiss: dismiss) }
        } else {
            acceptMember(groupInfo, member, role, dismiss: dismiss)
        }
    }
    showAlert(
        NSLocalizedString("Accept member", comment: "alert title"),
        message: blocked
            ? NSLocalizedString("Member will be unblocked for all and will join the group, accept member?", comment: "alert message")
            : NSLocalizedString("Member will join the group, accept member?", comment: "alert message"),
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Accept as member", comment: "alert action"),
                style: .default,
                handler: { _ in
                    accept(.member)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Accept as observer", comment: "alert action"),
                style: .default,
                handler: { _ in
                    accept(.observer)
                }
            ),
            cancelAlertAction
        ]}
    )
}

func acceptMember(_ groupInfo: GroupInfo, _ member: GroupMember, _ role: GroupMemberRole, dismiss: DismissAction? = nil) {
    Task {
        do {
            let (gInfo, acceptedMember) = try await apiAcceptMember(groupInfo.groupId, member.groupMemberId, role)
            await MainActor.run {
                _ = ChatModel.shared.upsertGroupMember(gInfo, acceptedMember)
                ChatModel.shared.updateGroup(gInfo)
                dismiss?()
            }
        } catch let error {
            logger.error("apiAcceptMember error: \(responseError(error))")
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error accepting member", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

struct ContextPendingMemberActionsView_Previews: PreviewProvider {
    static var previews: some View {
        ContextPendingMemberActionsView(
            groupInfo: GroupInfo.sampleData,
            member: GroupMember.sampleData
        )
    }
}
