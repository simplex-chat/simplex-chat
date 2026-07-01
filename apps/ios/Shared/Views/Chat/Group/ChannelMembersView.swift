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

    var body: some View {
        let members = chatModel.groupMembers
            .filter { m in
                let s = m.wrapped.memberStatus
                return s != .memLeft && s != .memRemoved && m.wrapped.memberRole != .relay
            }
            .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }
        let subscriberCount = groupInfo.groupSummary.publicMemberCount ?? Int64(members.count + 1)
        if groupInfo.isOwner {
            List {
                Section(header: Text(subscriberCountStr(subscriberCount)).foregroundColor(theme.colors.secondary)) {
                    memberRow(GMember(groupInfo.membership), user: true, showRole: true)
                    ForEach(members) { member in
                        memberRow(member, user: false, showRole: member.wrapped.memberRole >= .member)
                    }
                }
            }
        } else {
            let contributors = members.filter { $0.wrapped.memberRole >= .member && $0.wrapped.memberStatus != .memUnknown }
            let contributorCount = contributors.count + (groupInfo.membership.memberRole >= .member ? 1 : 0)
            let withContributors = contributors.contains { $0.wrapped.memberRole < .owner }
                || groupInfo.membership.memberRole >= .member
            List {
                Section(header: Text(ownersContributorsCountStr(contributorCount, withContributors: withContributors)).foregroundColor(theme.colors.secondary)) {
                    if groupInfo.membership.memberRole >= .member {
                        memberRow(GMember(groupInfo.membership), user: true, showRole: true)
                    }
                    ForEach(contributors) { member in
                        memberRow(member, user: false, showRole: member.wrapped.memberRole >= .moderator)
                    }
                }
            }
        }
    }

    @ViewBuilder private func memberRow(_ gMember: GMember, user: Bool, showRole: Bool) -> some View {
        let member = gMember.wrapped
        let nameText = Text(member.chatViewName)
            .foregroundColor(member.memberIncognito ? .indigo : theme.colors.onBackground)
        let displayName = member.verified
            ? (Text(Image(systemName: "checkmark.shield")) + textSpace)
                .font(.caption).baselineOffset(2).kerning(-2)
                .foregroundColor(theme.colors.secondary) + nameText
            : nameText
        let shortDescr = member.shortDescr?.trimmingCharacters(in: .whitespacesAndNewlines)
        let row = HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                NameWithBadge(displayName, member.nameBadge)
                    .lineLimit(1)
                if let shortDescr = shortDescr, shortDescr != "" {
                    Text(shortDescr)
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                        .lineLimit(1)
                }
                if user {
                    Text("you")
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                }
            }
            Spacer()
            if showRole {
                Text(member.memberRole.text(isChannel: groupInfo.isChannel))
                    .foregroundColor(theme.colors.secondary)
            }
        }
        if user {
            row
        } else {
            NavigationLink {
                GroupMemberInfoView(
                    groupInfo: groupInfo,
                    chat: chat,
                    groupMember: gMember,
                    scrollToItemId: Binding.constant(nil)
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
