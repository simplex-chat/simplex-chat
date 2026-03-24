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
        if groupInfo.isOwner {
            let subscriberCount = groupInfo.groupSummary.publicMemberCount ?? Int64(members.count + 1)
            List {
                Section(header: Text(subscriberCountStr(subscriberCount)).foregroundColor(theme.colors.secondary)) {
                    memberRow(GMember(groupInfo.membership), user: true, showRole: true)
                    ForEach(members) { member in
                        memberRow(member, user: false, showRole: member.wrapped.memberRole >= .owner)
                    }
                }
            }
        } else {
            let owners = members.filter { $0.wrapped.memberRole >= .owner }
            List {
                Section(header: Text("Owners").foregroundColor(theme.colors.secondary)) {
                    ForEach(owners) { member in
                        memberRow(member, user: false, showRole: false)
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
        let row = HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                displayName
                    .lineLimit(1)
                if user {
                    Text("you")
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                }
            }
            Spacer()
            if showRole {
                Text(member.memberRole.text)
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
