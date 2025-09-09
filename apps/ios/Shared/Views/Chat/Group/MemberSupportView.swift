//
//  MemberSupportView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.04.2025.
//  Copyright © 2025 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MemberSupportView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @State private var searchText: String = ""
    @FocusState private var searchFocussed
    var groupInfo: GroupInfo
    @Binding var scrollToItemId: ChatItem.ID?

    var body: some View {
        viewBody()
            .onAppear {
                Task {
                    await chatModel.loadGroupMembers(groupInfo)
                }
            }
            .toolbar {
                ToolbarItem(placement: .navigationBarTrailing) {
                    Button {
                        Task {
                            await chatModel.loadGroupMembers(groupInfo)
                        }
                    } label: {
                        Image(systemName: "arrow.clockwise")
                    }
                }
            }
    }

    @ViewBuilder private func viewBody() -> some View {
        let membersWithChats = sortedMembersWithChats()
        let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
        let filteredMembersWithChats = s == ""
        ? membersWithChats
        : membersWithChats.filter { $0.wrapped.localAliasAndFullName.localizedLowercase.contains(s) }

        if membersWithChats.isEmpty {
            Text("No chats with members")
                .foregroundColor(.secondary)
        } else {
            List {
                searchFieldView(text: $searchText, focussed: $searchFocussed, theme.colors.onBackground, theme.colors.secondary)
                    .padding(.leading, 8)
                ForEach(filteredMembersWithChats) { memberWithChat in
                    MemberSupportChatNavLink(
                        groupInfo: groupInfo,
                        memberWithChat: memberWithChat,
                        scrollToItemId: $scrollToItemId
                    )
                }
            }
        }
    }

    struct MemberSupportChatNavLink: View {
        @EnvironmentObject var chatModel: ChatModel
        @EnvironmentObject var theme: AppTheme
        @State private var memberSupportChatNavLinkActive = false
        var groupInfo: GroupInfo
        var memberWithChat: GMember
        @Binding var scrollToItemId: ChatItem.ID?

        var body: some View {
            ZStack {
                let scopeInfo: GroupChatScopeInfo = .memberSupport(groupMember_: memberWithChat.wrapped)
                Button {
                    ItemsModel.loadSecondaryChat(groupInfo.id, chatFilter: .groupChatScopeContext(groupScopeInfo: scopeInfo)) {
                        memberSupportChatNavLinkActive = true
                    }
                } label: {
                    SupportChatRowView(groupMember: memberWithChat, groupInfo: groupInfo)
                }

                NavigationLink(isActive: $memberSupportChatNavLinkActive) {
                    SecondaryChatView(
                        chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: scopeInfo), chatItems: [], chatStats: ChatStats()),
                        scrollToItemId: $scrollToItemId
                    )
                } label: {
                    EmptyView()
                }
                .frame(width: 1, height: 1)
                .hidden()
            }
            .if(!memberWithChat.wrapped.memberPending && memberWithChat.wrapped.supportChatNotRead) { v in
                v.swipeActions(edge: .leading, allowsFullSwipe: true) {
                    Button {
                        Task { await markSupportChatRead(groupInfo, memberWithChat.wrapped) }
                    } label: {
                        Label("Read", systemImage: "checkmark")
                    }
                    .tint(theme.colors.primary)
                }
            }
            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
                if memberWithChat.wrapped.memberPending {
                    Button {
                        showAcceptMemberAlert(groupInfo, memberWithChat.wrapped)
                    } label: {
                        Label("Accept", systemImage: "checkmark")
                    }
                    .tint(theme.colors.primary)
                } else {
                    Button {
                        showDeleteMemberSupportChatAlert(groupInfo, memberWithChat.wrapped)
                    } label: {
                        Label("Delete", systemImage: "trash")
                    }
                    .tint(.red)
                }
            }
        }
    }

    func sortedMembersWithChats() -> [GMember] {
        chatModel.groupMembers
            .filter {
                $0.wrapped.supportChat != nil &&
                $0.wrapped.memberStatus != .memLeft &&
                $0.wrapped.memberStatus != .memRemoved
            }
            .sorted { (m0: GMember, m1: GMember) -> Bool in
                if m0.wrapped.memberPending != m1.wrapped.memberPending {
                    return m0.wrapped.memberPending
                }

                let mentions0 = (m0.wrapped.supportChat?.mentions ?? 0) > 0
                let mentions1 = (m1.wrapped.supportChat?.mentions ?? 0) > 0
                if mentions0 != mentions1 {
                    return mentions0
                }

                let attention0 = (m0.wrapped.supportChat?.memberAttention ?? 0) > 0
                let attention1 = (m1.wrapped.supportChat?.memberAttention ?? 0) > 0
                if attention0 != attention1 {
                    return attention0
                }

                let unread0 = (m0.wrapped.supportChat?.unread ?? 0) > 0
                let unread1 = (m1.wrapped.supportChat?.unread ?? 0) > 0
                if unread0 != unread1 {
                    return unread0
                }

                return (m0.wrapped.supportChat?.chatTs ?? .distantPast) > (m1.wrapped.supportChat?.chatTs ?? .distantPast)
            }
    }

    private struct SupportChatRowView: View {
        @EnvironmentObject var chatModel: ChatModel
        @ObservedObject var groupMember: GMember
        @EnvironmentObject var theme: AppTheme
        @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
        var groupInfo: GroupInfo

        var dynamicChatInfoSize: CGFloat { dynamicSize(userFont).chatInfoSize }

        var body: some View {
            let member = groupMember.wrapped
            HStack{
                MemberProfileImage(member, size: 38)
                    .padding(.trailing, 2)
                VStack(alignment: .leading) {
                    let t = Text(member.chatViewName).foregroundColor(theme.colors.onBackground)
                    (member.verified ? memberVerifiedShield + t : t)
                        .lineLimit(1)
                    Text(memberStatus(member))
                        .lineLimit(1)
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                }

                Spacer()

                if member.memberPending {
                    Image(systemName: "flag.fill")
                        .resizable()
                        .scaledToFill()
                        .frame(width: dynamicChatInfoSize * 0.8, height: dynamicChatInfoSize * 0.8)
                        .foregroundColor(theme.colors.primary)
                }
                if let supportChat = member.supportChat {
                    SupportChatUnreadIndicator(supportChat: supportChat)
                }
            }
        }

        private func memberStatus(_ member: GroupMember) -> LocalizedStringKey {
            if member.activeConn?.connDisabled ?? false {
                return "disabled"
            } else if member.activeConn?.connInactive ?? false {
                return "inactive"
            } else if member.memberPending {
                return member.memberStatus.text
            } else {
                return LocalizedStringKey(member.memberRole.text)
            }
        }

        struct SupportChatUnreadIndicator: View {
            @EnvironmentObject var theme: AppTheme
            @Environment(\.dynamicTypeSize) private var userFont: DynamicTypeSize
            var supportChat: GroupSupportChat

            var dynamicChatInfoSize: CGFloat { dynamicSize(userFont).chatInfoSize }

            private var indicatorTint: Color {
                if supportChat.mentions > 0 || supportChat.memberAttention > 0 {
                    return theme.colors.primary
                } else {
                    return theme.colors.secondary
                }
            }

            var body: some View {
                HStack(alignment: .center, spacing: 2) {
                    if supportChat.unread > 0 || supportChat.mentions > 0 || supportChat.memberAttention > 0 {
                        if supportChat.mentions > 0 && supportChat.unread > 1 {
                            Text("\(MENTION_START)")
                                .font(userFont <= .xxxLarge ? .body : .callout)
                                .foregroundColor(indicatorTint)
                                .frame(minWidth: dynamicChatInfoSize, minHeight: dynamicChatInfoSize)
                                .cornerRadius(dynamicSize(userFont).unreadCorner)
                                .padding(.bottom, 1)
                        }
                        let singleUnreadIsMention = supportChat.mentions > 0 && supportChat.unread == 1
                        (singleUnreadIsMention ? Text("\(MENTION_START)") : unreadCountText(supportChat.unread))
                            .font(userFont <= .xxxLarge ? .caption : .caption2)
                            .foregroundColor(.white)
                            .padding(.horizontal, dynamicSize(userFont).unreadPadding)
                            .frame(minWidth: dynamicChatInfoSize, minHeight: dynamicChatInfoSize)
                            .background(indicatorTint)
                            .cornerRadius(dynamicSize(userFont).unreadCorner)
                    }
                }
                .frame(height: dynamicChatInfoSize)
                .frame(minWidth: 22)
            }
        }

        private var memberVerifiedShield: Text {
            (Text(Image(systemName: "checkmark.shield")) + textSpace)
                .font(.caption)
                .baselineOffset(2)
                .kerning(-2)
                .foregroundColor(theme.colors.secondary)
        }
    }
}

func showDeleteMemberSupportChatAlert(_ groupInfo: GroupInfo, _ member: GroupMember) {
    showAlert(
        title: NSLocalizedString("Delete chat with member?", comment: "alert title"),
        buttonTitle: "Delete",
        buttonAction: { deleteMemberSupportChat(groupInfo, member) },
        cancelButton: true
    )
}

func deleteMemberSupportChat(_ groupInfo: GroupInfo, _ member: GroupMember) {
    Task {
        do {
            let (gInfo, updatedMember) = try await apiDeleteMemberSupportChat(groupInfo.groupId, member.groupMemberId)
            await MainActor.run {
                _ = ChatModel.shared.upsertGroupMember(gInfo, updatedMember)
                ChatModel.shared.updateGroup(gInfo)
            }
            // TODO member row doesn't get removed from list (upsertGroupMember correctly sets supportChat to nil) - this repopulates list to fix it
            await ChatModel.shared.loadGroupMembers(gInfo)
        } catch let error {
            logger.error("apiDeleteMemberSupportChat error: \(responseError(error))")
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error deleting chat", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

#Preview {
    MemberSupportView(
        groupInfo: GroupInfo.sampleData,
        scrollToItemId: Binding.constant(nil)
    )
}
