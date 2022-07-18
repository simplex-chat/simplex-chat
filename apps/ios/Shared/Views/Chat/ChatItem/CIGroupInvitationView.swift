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
        let action = !chatItem.chatDir.sent && groupInvitation.status == .pending
        let v = ZStack(alignment: .bottomTrailing) {
            VStack(alignment: .leading) {
                groupInfoView(action)
                .padding(.horizontal, 2)
                .padding(.top, 8)
                .padding(.bottom, 6)
                .overlay(DetermineWidth())

                Divider().frame(width: frameWidth)

                if action {
                    groupInvitationText().overlay(DetermineWidth())
                    Text("Tap to join")
                        .foregroundColor(.accentColor)
                        .font(.callout)
                } else {
                    groupInvitationText()
                        .padding(.trailing, 60)
                        .overlay(DetermineWidth())
                }
            }
            .padding(.bottom, 2)
            chatItem.timestampText
                .font(.caption)
                .foregroundColor(.secondary)
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

    private func groupInfoView(_ action: Bool) -> some View {
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
    }

    private func groupInvitationText() -> some View {
        Text(groupInvitationStr())
            .font(.callout)
    }

    private func groupInvitationStr() -> LocalizedStringKey {
        if chatItem.chatDir.sent {
            return "You sent group invitation"
        } else {
            switch groupInvitation.status {
            case .pending: return "You are invited to group"
            case .accepted: return "You joined this group"
            case .rejected: return "You rejected group invitation"
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
