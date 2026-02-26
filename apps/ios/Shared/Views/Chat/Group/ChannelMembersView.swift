//
//  ChannelMembersView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 20.02.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChannelMembersView: View {
    @ObservedObject var chat: Chat
    var groupInfo: GroupInfo
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var scrollToItemId: ChatItem.ID? = nil

    var body: some View {
        let allMembers = chatModel.groupMembers
            .filter { m in
                let s = m.wrapped.memberStatus
                return s != .memLeft && s != .memRemoved && m.wrapped.groupMemberId != groupInfo.membership.groupMemberId
            }
        let owners = allMembers.filter { $0.wrapped.memberRole >= .owner }
        // TODO [relays] subscriber/owner counts require backend support for accurate totals
        let subscribers = allMembers.filter { $0.wrapped.memberRole < .owner && $0.wrapped.memberRole != .relay }
        List {
            Section(header: Text("Owners").foregroundColor(theme.colors.secondary)) {
                if groupInfo.membership.memberRole >= .owner {
                    memberRow(GMember(groupInfo.membership), user: true)
                }
                ForEach(owners) { member in
                    memberRow(member, user: false)
                }
            }
            if groupInfo.isOwner {
                Section(header: Text("\(subscribers.count) subscribers").foregroundColor(theme.colors.secondary)) {
                    if subscribers.isEmpty {
                        Text("No subscribers")
                            .foregroundColor(theme.colors.secondary)
                    } else {
                        ForEach(subscribers) { member in
                            memberRow(member, user: false)
                        }
                    }
                }
            }
        }
    }

    @ViewBuilder private func memberRow(_ gMember: GMember, user: Bool) -> some View {
        let member = gMember.wrapped
        let nameText = Text(member.chatViewName)
            .foregroundColor(member.memberIncognito ? .indigo : theme.colors.onBackground)
        let displayName = member.verified
            ? (Text(Image(systemName: "checkmark.shield")) + textSpace)
                .font(.caption).baselineOffset(2).kerning(-2)
                .foregroundColor(theme.colors.secondary) + nameText
            : nameText
        let row = HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            displayName
                .lineLimit(1)
            Spacer()
        }
        if user {
            row
        } else {
            NavigationLink {
                GroupMemberInfoView(
                    groupInfo: groupInfo,
                    chat: chat,
                    groupMember: gMember,
                    scrollToItemId: $scrollToItemId
                )
                .navigationBarHidden(false)
            } label: {
                row
            }
        }
    }
}

#Preview {
    ChannelMembersView(
        chat: Chat.sampleData,
        groupInfo: GroupInfo.sampleData
    )
}
