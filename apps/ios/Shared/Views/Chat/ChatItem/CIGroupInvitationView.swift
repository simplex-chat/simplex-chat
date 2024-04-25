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
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.colorScheme) var colorScheme
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    var groupInvitation: CIGroupInvitation
    var memberRole: GroupMemberRole
    var chatIncognito: Bool = false
    @State private var frameWidth: CGFloat = 0
    @State private var inProgress = false
    @State private var progressByTimeout = false

    var body: some View {
        let action = !chatItem.chatDir.sent && groupInvitation.status == .pending
        let v = ZStack(alignment: .bottomTrailing) {
            ZStack {
                VStack(alignment: .leading) {
                    groupInfoView(action)
                        .padding(.horizontal, 2)
                        .padding(.top, 8)
                        .padding(.bottom, 6)
                        .overlay(DetermineWidth())

                    Divider().frame(width: frameWidth)

                    if action {
                        VStack(alignment: .leading, spacing: 2) {
                            groupInvitationText()
                                .overlay(DetermineWidth())
                            (
                                Text(chatIncognito ? "Tap to join incognito" : "Tap to join")
                                    .foregroundColor(inProgress ? .secondary : chatIncognito ? .indigo : .accentColor)
                                    .font(.callout)
                                + Text("   ")
                                + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, transparent: true, showStatus: false, showEdited: false)
                            )
                            .overlay(DetermineWidth())
                        }
                    } else {
                        (
                            groupInvitationText()
                            + Text("   ")
                            + ciMetaText(chatItem.meta, chatTTL: nil, encrypted: nil, transparent: true, showStatus: false, showEdited: false)
                        )
                        .overlay(DetermineWidth())
                    }
                }
                .padding(.bottom, 2)

                if progressByTimeout {
                    ProgressView().scaleEffect(2)
                }
            }

            CIMetaView(chat: chat, chatItem: chatItem, showStatus: false, showEdited: false)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 6)
        .background(chatItemFrameColor(chatItem, colorScheme))
        .cornerRadius(18)
        .textSelection(.disabled)
        .onPreferenceChange(DetermineWidth.Key.self) { frameWidth = $0 }
        .onChange(of: inProgress) { inProgress in
            if inProgress {
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    progressByTimeout = inProgress
                }
            } else {
                progressByTimeout = false
            }
        }

        if action {
            v.onTapGesture {
                inProgress = true
                joinGroup(groupInvitation.groupId) {
                    await MainActor.run { inProgress = false }
                }
            }
            .disabled(inProgress)
        } else {
            v
        }
    }

    private func groupInfoView(_ action: Bool) -> some View {
        var color: Color
        if action && !inProgress {
            color = chatIncognito ? .indigo : .accentColor
        } else {
            color = Color(uiColor: .tertiaryLabel)
        }
        return HStack(alignment: .top) {
            ProfileImage(
                imageStr: groupInvitation.groupProfile.image,
                iconName: "person.2.circle.fill",
                color: color
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

    private func groupInvitationText() -> Text {
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
}

struct CIGroupInvitationView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            CIGroupInvitationView(chat: Chat.sampleData, chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(groupProfile: GroupProfile(displayName: "team", fullName: "team")), memberRole: .admin)
            CIGroupInvitationView(chat: Chat.sampleData, chatItem: ChatItem.getGroupInvitationSample(), groupInvitation: CIGroupInvitation.getSample(status: .accepted), memberRole: .admin)
        }
    }
}
