//
//  GroupInvitationItemView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupInvitationItemView: View {
    var chatItem: ChatItem
    var groupInvitation: CIGroupInvitation
    var memberRole: GroupMemberRole
    
    var body: some View {
        let sent = chatItem.chatDir.sent
        ZStack(alignment: .bottomTrailing) {
            VStack {
                HStack {
                    ProfileImage(
                        iconName: "person.2.circle.fill",
                        color: accent(sent) ? .white : Color(uiColor: .tertiaryLabel)
                    )
                    .frame(width: 44, height: 44)
                    .padding(.trailing, 6)
                    .padding(.top, 6)
                    VStack(alignment: .leading) {
                        Text(groupInvitation.groupProfile.displayName).font(.title2)
                        Text(groupInvitation.groupProfile.fullName)
                    }
                }
                Divider()
                    .frame(width: 300)
                groupInvitationText(sent)
                    .padding(.bottom, 18)
            }
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(accent(sent) ? .accentColor : Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
        .onTapGesture { groupInvitationAction(sent) }
    }

    @ViewBuilder private func groupInvitationText(_ sent: Bool) -> some View {
        switch (sent, groupInvitation.status) {
        case (true, _): Text("You sent group invitation")
        case (false, .pending): Text("Invitation to join group. Tap to accept")
        case (false, .accepted): Text("Accepted group invitation")
        case (false, .rejected): Text("Rejected group invitation")
        case (false, .expired): Text("Expired group invitation")
        }
    }

    private func accent(_ sent: Bool) -> Bool {
        switch (sent, groupInvitation.status) {
        case (false, .pending): return true
        default: return false
        }
    }

    private func groupInvitationAction(_ sent: Bool) {
        logger.debug("GroupInvitationItemView groupInvitationAction")
        switch (sent, groupInvitation.status) {
        case (false, .pending):
            Task {
                logger.debug("GroupInvitationItemView groupInvitationAction - in (sent, .pending), in Task")
                await joinGroup(groupId: groupInvitation.groupId)
            }
        default: break
        }
    }
}

struct GroupInvitationItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            GroupInvitationItemView(chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(), memberRole: .admin)
            GroupInvitationItemView(chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(status: .accepted), memberRole: .admin)
        }
    }
}
