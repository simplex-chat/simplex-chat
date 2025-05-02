//
//  ContextPendingMemberActionsView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 02.05.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

// TODO [knocking] go back (close secondary ChatView) on actions
struct ContextPendingMemberActionsView: View {
    @EnvironmentObject var theme: AppTheme
    var groupInfo: GroupInfo
    var member: GroupMember

    var body: some View {
        HStack(spacing: 0) {
            ZStack {
                Text("Remove")
                    .foregroundColor(.red)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showRemoveMemberAlert(groupInfo, member)
            }

            ZStack {
                Text("Accept")
                    .foregroundColor(theme.colors.primary)
            }
            .frame(maxWidth: .infinity)
            .contentShape(Rectangle())
            .onTapGesture {
                showAcceptMemberAlert(groupInfo, member)
            }
        }
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity)
        .background(.thinMaterial)
    }
}

func showAcceptMemberAlert(_ groupInfo: GroupInfo, _ member: GroupMember) {
    showAlert(
        NSLocalizedString("Accept member", comment: "alert title"),
        message: NSLocalizedString("Member will join the group, accept member?", comment: "alert message"),
        actions: {[
            UIAlertAction(
                title: NSLocalizedString("Accept as member", comment: "alert action"),
                style: .default,
                handler: { _ in
                    acceptMember(groupInfo, member, .member)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Accept as observer", comment: "alert action"),
                style: .default,
                handler: { _ in
                    acceptMember(groupInfo, member, .observer)
                }
            ),
            UIAlertAction(
                title: NSLocalizedString("Cancel", comment: "alert action"),
                style: .default
            )
        ]}
    )
}

func acceptMember(_ groupInfo: GroupInfo, _ member: GroupMember, _ role: GroupMemberRole) {
    Task {
        do {
            let acceptedMember = try await apiAcceptMember(groupInfo.groupId, member.groupMemberId, role)
            await MainActor.run {
                _ = ChatModel.shared.upsertGroupMember(groupInfo, acceptedMember)
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

#Preview {
    ContextPendingMemberActionsView(
        groupInfo: GroupInfo.sampleData,
        member: GroupMember.sampleData
    )
}
