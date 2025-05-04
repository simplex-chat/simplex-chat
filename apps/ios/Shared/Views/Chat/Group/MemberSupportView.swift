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
            Text("No support chats")
                .foregroundColor(.secondary)
        } else {
            List {
                searchFieldView(text: $searchText, focussed: $searchFocussed, theme.colors.onBackground, theme.colors.secondary)
                    .padding(.leading, 8)
                ForEach(filteredMembersWithChats) { memberWithChat in
                    let scopeInfo: GroupChatScopeInfo = .memberSupport(groupMember_: memberWithChat.wrapped)
                    MemberSupportChatNavLink(
                        groupInfo: groupInfo,
                        memberWithChat: memberWithChat,
                        chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: scopeInfo), chatItems: [], chatStats: ChatStats()),
                        im: ItemsModel(secondaryIMFilter: .groupChatScopeContext(groupScopeInfo: scopeInfo))
                    )
                }
            }
        }
    }

    struct MemberSupportChatNavLink: View {
        @EnvironmentObject var chatModel: ChatModel
        @State private var memberSupportChatNavLinkActive = false
        var groupInfo: GroupInfo
        var memberWithChat: GMember
        @ObservedObject var chat: Chat
        var im: ItemsModel

        var body: some View {
            ZStack {
                Button {
                    im.loadOpenChat(chat.id) {
                        memberSupportChatNavLinkActive = true
                    }
                } label: {
                    SupportChatRowView(groupMember: memberWithChat, groupInfo: groupInfo)
                }

                NavigationLink(isActive: $memberSupportChatNavLinkActive) {
                    if let secondaryIM = chatModel.secondaryIM {
                        SecondaryChatView(
                            chat: chat,
                            im: secondaryIM
                        )
                    }
                } label: {
                    EmptyView()
                }
                .frame(width: 1, height: 1)
                .hidden()
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
            // TODO [knocking] swipe actions are broken
//            .swipeActions(edge: .trailing, allowsFullSwipe: true) {
//                if member.memberPending {
//                    Button {
//                        showAcceptMemberAlert(groupInfo, member)
//                    } label: {
//                        Label("Accept", systemImage: "checkmark")
//                    }
//                    .tint(theme.colors.primary)
//                }
//
//                Button {
//                    showRemoveMemberAlert(groupInfo, member)
//                } label: {
//                    Label("Remove", systemImage: "trash")
//                }
//                .tint(.red)
//            }
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

func showRemoveMemberAlert(_ groupInfo: GroupInfo, _ member: GroupMember) {
    showAlert(
        title: NSLocalizedString("Remove member?", comment: "alert title"),
        buttonTitle: "Remove",
        buttonAction: { removeMember(groupInfo, member) },
        cancelButton: true
    )
}

#Preview {
    MemberSupportView(
        groupInfo: GroupInfo.sampleData
    )
}
