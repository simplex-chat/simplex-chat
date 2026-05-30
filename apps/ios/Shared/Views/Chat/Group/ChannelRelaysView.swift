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
    @ObservedObject private var channelRelaysModel = ChannelRelaysModel.shared
    @State private var showAddRelay = false

    private var groupRelays: [GroupRelay] {
        channelRelaysModel.groupId == groupInfo.groupId ? channelRelaysModel.groupRelays : []
    }

    var body: some View {
        List {
            relaysList()
            // TODO [relays] re-enable when relay management ships
            // if groupInfo.isOwner {
            //     Section {
            //         Button {
            //             showAddRelay = true
            //         } label: {
            //             Label("Add relay", systemImage: "plus")
            //         }
            //     }
            // }
        }
        // TODO [relays] re-enable when relay management ships
        // .sheet(isPresented: $showAddRelay) {
        //     // Backend gate (APIAddGroupRelays) rejects any chatRelayId already in group_relays
        //     // regardless of relayStatus, so all current rows must be excluded from the add list.
        //     let existingRelayIds = Set(groupRelays.compactMap { $0.userChatRelay.chatRelayId })
        //     AddGroupRelayView(groupInfo: groupInfo, existingRelayIds: existingRelayIds) {
        //         Task { await chatModel.loadGroupMembers(groupInfo) }
        //     }
        // }
        .onAppear {
            Task {
                await chatModel.loadGroupMembers(groupInfo)
                if groupInfo.isOwner {
                    let relays = await apiGetGroupRelays(groupInfo.groupId)
                    await MainActor.run {
                        ChannelRelaysModel.shared.set(groupId: groupInfo.groupId, groupRelays: relays)
                    }
                }
            }
        }
    }

    @ViewBuilder private func relaysList() -> some View {
        let relayMembers = chatModel.groupMembers.filter { $0.wrapped.memberRole == .relay && $0.wrapped.memberStatus != .memRemoved && $0.wrapped.memberStatus != .memGroupDeleted }
        if relayMembers.isEmpty {
            Section {
                Text("No chat relays")
                    .foregroundColor(theme.colors.secondary)
            }
        } else {
            Section {
                ForEach(relayMembers) { member in
                    let link = NavigationLink {
                        GroupMemberInfoView(
                            groupInfo: groupInfo,
                            chat: chat,
                            groupMember: member,
                            scrollToItemId: Binding.constant(nil),
                            groupRelay: groupRelays.first(where: { $0.groupMemberId == member.wrapped.groupMemberId })
                        )
                        .navigationBarHidden(false)
                    } label: {
                        let statusText = groupInfo.isOwner
                            ? ownerRelayStatusText(member.wrapped)
                            : subscriberRelayStatusText(member.wrapped)
                        relayMemberRow(member.wrapped, statusText: statusText)
                    }
                    // TODO [relays] re-enable when relay management ships
                    // if groupInfo.isOwner && member.wrapped.canBeRemoved(groupInfo: groupInfo) {
                    //     link.swipeActions(edge: .trailing) {
                    //         Button {
                    //             showRemoveMemberAlert(groupInfo, member.wrapped)
                    //         } label: {
                    //             Label("Remove relay", systemImage: "trash")
                    //         }
                    //         .tint(.red)
                    //     }
                    // } else {
                    //     link
                    // }
                    link
                }
            } footer: {
                Text("Chat relays forward messages to channel subscribers.")
            }
        }
    }

    private func subscriberRelayStatusText(_ member: GroupMember) -> LocalizedStringKey {
        if member.activeConn?.connDisabled ?? false {
            "disabled"
        } else if member.activeConn?.connInactive ?? false {
            "inactive"
        } else {
            relayConnStatus(member).text
        }
    }

    private func ownerRelayStatusText(_ member: GroupMember) -> LocalizedStringKey {
        let relayStatus = groupRelays.first(where: { $0.groupMemberId == member.groupMemberId })?.relayStatus
        return if relayStatus == .rejected {
            "rejected"
        } else if [.memLeft, .memRemoved, .memGroupDeleted].contains(member.memberStatus) {
            relayConnStatus(member).text
        } else if case .failed = member.activeConn?.connStatus {
            "failed"
        } else if member.activeConn?.connDisabled ?? false {
            "disabled"
        } else if member.activeConn?.connInactive ?? false {
            "inactive"
        } else {
            relayStatus?.text ?? relayConnStatus(member).text
        }
    }

    private func relayMemberRow(_ member: GroupMember, statusText: LocalizedStringKey) -> some View {
        HStack {
            MemberProfileImage(member, size: 38)
                .padding(.trailing, 2)
            VStack(alignment: .leading) {
                Text(member.chatViewName)
                    .foregroundColor(theme.colors.onBackground)
                    .lineLimit(1)
                Text(statusText)
                    .lineLimit(1)
                    .font(.caption)
                    .foregroundColor(theme.colors.secondary)
            }
            Spacer()
        }
    }
}

func relayConnStatus(_ member: GroupMember) -> (text: LocalizedStringKey, color: Color) {
    switch member.memberStatus {
    case .memLeft: ("removed by operator", .red)
    case .memRemoved, .memGroupDeleted: (member.memberStatus.text, .red)
    default:
        switch member.activeConn?.connStatus {
        case .ready: ("connected", .green)
        case .deleted: ("deleted", .red)
        case .failed: ("failed", .red)
        default: ("connecting", .yellow)
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
