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
            relaysList(showRelayStatus: isOwner)
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

    @ViewBuilder private func relaysList(showRelayStatus: Bool) -> some View {
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
                        relayMemberRow(member.wrapped, relayStatus: showRelayStatus ? relayStatusForMember(member.wrapped) : nil)
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

struct RelayProgressIndicator: View {
    var active: Int
    var total: Int

    var body: some View {
        if active == 0 {
            ProgressView()
                .frame(width: 20, height: 20)
        } else {
            ZStack {
                Circle()
                    .stroke(Color(uiColor: .tertiaryLabel), style: StrokeStyle(lineWidth: 2.5))
                Circle()
                    .trim(from: 0, to: Double(active) / Double(max(total, 1)))
                    .stroke(Color.accentColor, style: StrokeStyle(lineWidth: 2.5, lineCap: .round))
                    .rotationEffect(.degrees(-90))
            }
            .frame(width: 20, height: 20)
        }
    }
}

func hostFromRelayLink(_ link: String) -> String {
    if let ft = parseSimpleXMarkdown(link) {
        for f in ft {
            if case let .simplexLink(_, _, _, smpHosts) = f.format,
               let host = smpHosts.first {
                return host
            }
        }
    }
    return link
}

#Preview {
    ChannelRelaysView(chat: Chat.sampleData, groupInfo: GroupInfo.sampleData)
}
