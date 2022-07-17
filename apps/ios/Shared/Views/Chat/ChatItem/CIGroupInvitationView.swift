//
//  CIGroupInvitationView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 15.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIGroupInvitationView: View {
    @Environment(\.colorScheme) var colorScheme
    var chatItem: ChatItem
    var groupInvitation: CIGroupInvitation
    var memberRole: GroupMemberRole
    @State private var frameWidth: CGFloat = 0

    var body: some View {
        let sent = chatItem.chatDir.sent
        let action = !sent && groupInvitation.status == .pending
        let v = ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading) {
                HStack(alignment: .top) {
                    ProfileImage(
                        iconName: "person.2.circle.fill",
                        color: action ? .accentColor : Color(uiColor: .tertiaryLabel)
                    )
                    .frame(width: 44, height: 44)
                    .padding(.trailing, 4)
                    VStack(alignment: .leading) {
                        let p = groupInvitation.groupProfile
                        Text(p.displayName).font(.headline).lineLimit(2)
                        if p.fullName != "" && p.displayName != p.fullName {
                            Text(p.fullName).font(.subheadline).lineLimit(2)
                        }
                    }
                    .frame(minHeight: 44)
                }
                .padding(.top, 6)
                .padding(.bottom, 6)
                .overlay(DetermineWidth())

                Divider().frame(width: frameWidth)

                let t = Text(groupInvitationText(sent))
                    .padding(.trailing, 60)
                    .overlay(DetermineWidth())
                if action {
                    t.foregroundColor(.accentColor)
                } else {
                    t
                }
            }
            CIMetaView(chatItem: chatItem)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .textSelection(.disabled)
        .onPreferenceChange(DetermineWidth.Key.self) { frameWidth = $0 }

        if action {
            v.onTapGesture { acceptInvitation() }
        } else {
            v
        }
    }

    private func groupInvitationText(_ sent: Bool) -> LocalizedStringKey {
        if sent {
            return "You invited to group"
        } else {
            switch groupInvitation.status {
            case .pending: return "Join group"
            case .accepted: return "Joined group"
            case .rejected: return "Group invitation rejected"
            case .expired: return "Group invitation expired"
            }
        }
    }

    private func acceptInvitation() {
        Task {
            logger.debug("acceptInvitation")
            await joinGroup(groupId: groupInvitation.groupId)
        }
    }
}

struct CIGroupInvitationView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            CIGroupInvitationView(chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(groupProfile: GroupProfile(displayName: "team", fullName: "team")), memberRole: .admin)
            CIGroupInvitationView(chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(status: .accepted), memberRole: .admin)
        }
    }
}
