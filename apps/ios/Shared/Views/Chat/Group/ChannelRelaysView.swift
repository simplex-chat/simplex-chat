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
            if isOwner {
                Task { groupRelays = await apiGetGroupRelays(groupInfo.groupId) }
            }
        }
    }

    @ViewBuilder private func ownerRelaysList() -> some View {
        let relayMembers = chatModel.groupMembers.filter { $0.wrapped.memberRole == .relay }
        if groupRelays.isEmpty && relayMembers.isEmpty {
            Section {
                Text("No chat relays")
                    .foregroundColor(theme.colors.secondary)
            }
        } else {
            Section {
                ForEach(groupRelays) { relay in
                    HStack {
                        Text(ownerRelayDisplayName(relay))
                        Spacer()
                        relayStatusIndicator(relay.relayStatus)
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
                        relayMemberRow(member.wrapped)
                    }
                }
            } footer: {
                Text("Chat relays forward messages to channel subscribers.")
            }
        }
    }

    private func ownerRelayDisplayName(_ relay: GroupRelay) -> String {
        if let member = chatModel.groupMembers.first(where: { $0.wrapped.groupMemberId == relay.groupMemberId }) {
            return member.wrapped.chatViewName
        }
        if let link = relay.relayLink {
            return hostFromRelayLink(link)
        }
        return "relay\(relay.groupRelayId)"
    }

    private func relayStatusIndicator(_ status: RelayStatus) -> some View {
        HStack(spacing: 4) {
            Circle()
                .fill(relayStatusColor(status))
                .frame(width: 8, height: 8)
            Text(status.text)
                .font(.caption)
                .foregroundColor(theme.colors.secondary)
        }
    }

    private func relayMemberRow(_ member: GroupMember) -> some View {
        HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                Text(member.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                Text(relayConnStatus(member))
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

    private func relayStatusColor(_ status: RelayStatus) -> Color {
        switch status {
        case .rsActive: .green
        case .rsNew: .red
        case .rsInvited, .rsAccepted: .orange
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
