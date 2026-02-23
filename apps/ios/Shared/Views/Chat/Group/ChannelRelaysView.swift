//
//  ChannelRelaysView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 20.02.2026.
//  Copyright © 2026 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ChannelRelaysView: View {
    @ObservedObject var chat: Chat
    var groupInfo: GroupInfo
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var groupRelays: [GroupRelay] = []
    @State private var scrollToItemId: ChatItem.ID? = nil

    var body: some View {
        let isOwner = groupInfo.isOwner
        List {
            if isOwner {
                ownerRelaysList()
            } else {
                memberRelaysList()
            }
        }
        .onAppear {
            Task {
                await chatModel.loadGroupMembers(groupInfo)
                if isOwner {
                    groupRelays = await apiGetGroupRelays(groupInfo.groupId)
                }
            }
        }
    }

    @ViewBuilder private func ownerRelaysList() -> some View {
        let relayMembers = chatModel.groupMembers.filter { $0.wrapped.memberRole == .relay }
        if relayMembers.isEmpty {
            Section {
                Text("No chat relays")
                    .foregroundColor(theme.colors.secondary)
            }
        } else {
            Section {
                ForEach(relayMembers) { member in
                    NavigationLink {
                        GroupMemberInfoView(
                            groupInfo: groupInfo,
                            chat: chat,
                            groupMember: member,
                            scrollToItemId: $scrollToItemId
                        )
                        .navigationBarHidden(false)
                    } label: {
                        relayMemberRow(member.wrapped, relayStatus: relayStatusForMember(member.wrapped))
                    }
                }
            } footer: {
                Text("Chat relays forward messages to channel subscribers.")
            }
        }
    }

    @ViewBuilder private func memberRelaysList() -> some View {
        let relayMembers = chatModel.groupMembers.filter { $0.wrapped.memberRole == .relay }
        if relayMembers.isEmpty {
            Section {
                Text("No chat relays")
                    .foregroundColor(theme.colors.secondary)
            }
        } else {
            Section {
                ForEach(relayMembers) { member in
                    NavigationLink {
                        GroupMemberInfoView(
                            groupInfo: groupInfo,
                            chat: chat,
                            groupMember: member,
                            scrollToItemId: $scrollToItemId
                        )
                        .navigationBarHidden(false)
                    } label: {
                        relayMemberRow(member.wrapped, relayStatus: nil)
                    }
                }
            } footer: {
                Text("Chat relays forward messages to channel subscribers.")
            }
        }
    }

    private func relayStatusForMember(_ member: GroupMember) -> RelayStatus? {
        groupRelays.first(where: { $0.groupMemberId == member.groupMemberId })?.relayStatus
    }

    private func relayMemberRow(_ member: GroupMember, relayStatus: RelayStatus?) -> some View {
        HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                Text(member.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                Text(relayStatus?.text ?? relayConnStatus(member))
                    .lineLimit(1)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Spacer()
        }
    }

    // TODO [relays] review connection status display once relay join statuses are finalized
    private func relayConnStatus(_ member: GroupMember) -> LocalizedStringKey {
        if member.activeConn?.connDisabled ?? false {
            return "disabled"
        } else if member.activeConn?.connInactive ?? false {
            return "inactive"
        } else {
            return member.sndReady ? "connected" : "connecting"
        }
    }

}

func hostFromRelayLink(_ link: String) -> String {
    guard let atIdx = link.firstIndex(of: "@") else { return link }
    let afterAt = link[link.index(after: atIdx)...]
    if let colonIdx = afterAt.firstIndex(of: ":") {
        return String(afterAt[afterAt.startIndex..<colonIdx])
    }
    return String(afterAt)
}

#Preview {
    ChannelRelaysView(chat: Chat.sampleData, groupInfo: GroupInfo.sampleData)
}
