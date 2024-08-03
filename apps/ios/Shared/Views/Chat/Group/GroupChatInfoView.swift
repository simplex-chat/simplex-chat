//
//  GroupChatInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 14.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let SMALL_GROUPS_RCPS_MEM_LIMIT: Int = 20

struct GroupChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    @Binding var groupInfo: GroupInfo
    @State private var alert: GroupChatInfoViewAlert? = nil
    @State private var groupLink: String?
    @State private var groupLinkMemberRole: GroupMemberRole = .member
    @State private var showAddMembersSheet: Bool = false
    @State private var connectionStats: ConnectionStats?
    @State private var connectionCode: String?
    @State private var sendReceipts = SendReceipts.userDefault(true)
    @State private var sendReceiptsUserDefault = true
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed

    enum GroupChatInfoViewAlert: Identifiable {
        case deleteGroupAlert
        case clearChatAlert
        case leaveGroupAlert
        case cantInviteIncognitoAlert
        case largeGroupReceiptsDisabled
        case blockMemberAlert(mem: GroupMember)
        case unblockMemberAlert(mem: GroupMember)
        case blockForAllAlert(mem: GroupMember)
        case unblockForAllAlert(mem: GroupMember)
        case removeMemberAlert(mem: GroupMember)
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case .deleteGroupAlert: return "deleteGroupAlert"
            case .clearChatAlert: return "clearChatAlert"
            case .leaveGroupAlert: return "leaveGroupAlert"
            case .cantInviteIncognitoAlert: return "cantInviteIncognitoAlert"
            case .largeGroupReceiptsDisabled: return "largeGroupReceiptsDisabled"
            case let .blockMemberAlert(mem): return "blockMemberAlert \(mem.groupMemberId)"
            case let .unblockMemberAlert(mem): return "unblockMemberAlert \(mem.groupMemberId)"
            case let .blockForAllAlert(mem): return "blockForAllAlert \(mem.groupMemberId)"
            case let .unblockForAllAlert(mem): return "unblockForAllAlert \(mem.groupMemberId)"
            case let .removeMemberAlert(mem): return "removeMemberAlert \(mem.groupMemberId)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            let members = chatModel.groupMembers
                .filter { m in let status = m.wrapped.memberStatus; return status != .memLeft && status != .memRemoved }
                .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }

            List {
                groupInfoHeader()
                    .listRowBackground(Color.clear)

                Section {
                    if groupInfo.canEdit {
                        editGroupButton()
                    }
                    if groupInfo.groupProfile.description != nil || groupInfo.canEdit {
                        addOrEditWelcomeMessage()
                    }
                    groupPreferencesButton($groupInfo)
                    if members.filter({ $0.wrapped.memberCurrent }).count <= SMALL_GROUPS_RCPS_MEM_LIMIT {
                        sendReceiptsOption()
                    } else {
                        sendReceiptsOptionDisabled()
                    }
                    NavigationLink {
                        ChatWallpaperEditorSheet(chat: chat)
                    } label: {
                        Label("Chat theme", systemImage: "photo")
                    }
                } header: {
                    Text("")
                } footer: {
                    Text("Only group owners can change group preferences.")
                        .foregroundColor(theme.colors.secondary)
                }

                Section(header: Text("\(members.count + 1) members").foregroundColor(theme.colors.secondary)) {
                    if groupInfo.canAddMembers {
                        groupLinkButton()
                        if (chat.chatInfo.incognito) {
                            Label("Invite members", systemImage: "plus")
                                .foregroundColor(Color(uiColor: .tertiaryLabel))
                                .onTapGesture { alert = .cantInviteIncognitoAlert }
                        } else {
                            addMembersButton()
                        }
                    }
                    if members.count > 8 {
                        searchFieldView(text: $searchText, focussed: $searchFocussed, theme.colors.onBackground, theme.colors.secondary)
                            .padding(.leading, 8)
                    }
                    let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                    let filteredMembers = s == "" ? members : members.filter { $0.wrapped.chatViewName.localizedLowercase.contains(s) }
                    MemberRowView(groupInfo: groupInfo, groupMember: GMember(groupInfo.membership), user: true, alert: $alert)
                    ForEach(filteredMembers) { member in
                        ZStack {
                            NavigationLink {
                                memberInfoView(member)
                            } label: {
                                EmptyView()
                            }
                            .opacity(0)
                            MemberRowView(groupInfo: groupInfo, groupMember: member, alert: $alert)
                        }
                    }
                }

                Section {
                    clearChatButton()
                    if groupInfo.canDelete {
                        deleteGroupButton()
                    }
                    if groupInfo.membership.memberCurrent {
                        leaveGroupButton()
                    }
                }

                if developerTools {
                    Section(header: Text("For console").foregroundColor(theme.colors.secondary)) {
                        infoRow("Local name", chat.chatInfo.localDisplayName)
                        infoRow("Database ID", "\(chat.chatInfo.apiId)")
                    }
                }
            }
            .modifier(ThemedBackground(grouped: true))
            .navigationBarHidden(true)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .deleteGroupAlert: return deleteGroupAlert()
            case .clearChatAlert: return clearChatAlert()
            case .leaveGroupAlert: return leaveGroupAlert()
            case .cantInviteIncognitoAlert: return cantInviteIncognitoAlert()
            case .largeGroupReceiptsDisabled: return largeGroupReceiptsDisabledAlert()
            case let .blockMemberAlert(mem): return blockMemberAlert(groupInfo, mem)
            case let .unblockMemberAlert(mem): return unblockMemberAlert(groupInfo, mem)
            case let .blockForAllAlert(mem): return blockForAllAlert(groupInfo, mem)
            case let .unblockForAllAlert(mem): return unblockForAllAlert(groupInfo, mem)
            case let .removeMemberAlert(mem): return removeMemberAlert(mem)
            case let .error(title, error): return mkAlert(title: title, message: error)
            }
        }
        .onAppear {
            if let currentUser = chatModel.currentUser {
                sendReceiptsUserDefault = currentUser.sendRcptsSmallGroups
            }
            sendReceipts = SendReceipts.fromBool(groupInfo.chatSettings.sendRcpts, userDefault: sendReceiptsUserDefault)
            do {
                if let link = try apiGetGroupLink(groupInfo.groupId) {
                    (groupLink, groupLinkMemberRole) = link
                }
            } catch let error {
                logger.error("GroupChatInfoView apiGetGroupLink: \(responseError(error))")
            }
        }
        .keyboardPadding()
    }

    private func groupInfoHeader() -> some View {
        VStack {
            let cInfo = chat.chatInfo
            ChatInfoImage(chat: chat, size: 192, color: Color(uiColor: .tertiarySystemFill))
                .padding(.top, 12)
                .padding()
            Text(cInfo.displayName)
                .font(.largeTitle)
                .multilineTextAlignment(.center)
                .lineLimit(4)
                .padding(.bottom, 2)
            if cInfo.fullName != "" && cInfo.fullName != cInfo.displayName {
                Text(cInfo.fullName)
                    .font(.title2)
                    .multilineTextAlignment(.center)
                    .lineLimit(8)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    private func addMembersButton() -> some View {
        NavigationLink {
            AddGroupMembersView(chat: chat, groupInfo: groupInfo)
                .onAppear {
                    searchFocussed = false
                    Task {
                        let groupMembers = await apiListMembers(groupInfo.groupId)
                        await MainActor.run {
                            chatModel.groupMembers = groupMembers.map { GMember.init($0) }
                            chatModel.populateGroupMembersIndexes()
                        }
                    }
                }
        } label: {
            Label("Invite members", systemImage: "plus")
        }
    }

    private struct MemberRowView: View {
        var groupInfo: GroupInfo
        @ObservedObject var groupMember: GMember
        @EnvironmentObject var theme: AppTheme
        var user: Bool = false
        @Binding var alert: GroupChatInfoViewAlert?

        var body: some View {
            let member = groupMember.wrapped
            let v = HStack{
                ProfileImage(imageStr: member.image, size: 38)
                    .padding(.trailing, 2)
                // TODO server connection status
                VStack(alignment: .leading) {
                    let t = Text(member.chatViewName).foregroundColor(member.memberIncognito ? .indigo : theme.colors.onBackground)
                    (member.verified ? memberVerifiedShield + t : t)
                        .lineLimit(1)
                    (user ? Text ("you: ") + Text(member.memberStatus.shortText) : Text(memberConnStatus(member)))
                        .lineLimit(1)
                        .font(.caption)
                        .foregroundColor(theme.colors.secondary)
                }
                Spacer()
                memberInfo(member)
            }
            
            if user {
                v
            } else if groupInfo.membership.memberRole >= .admin {
                // TODO if there are more actions, refactor with lists of swipeActions
                let canBlockForAll = member.canBlockForAll(groupInfo: groupInfo)
                let canRemove = member.canBeRemoved(groupInfo: groupInfo)
                if canBlockForAll && canRemove {
                    removeSwipe(member, blockForAllSwipe(member, v))
                } else if canBlockForAll {
                    blockForAllSwipe(member, v)
                } else if canRemove {
                    removeSwipe(member, v)
                } else {
                    v
                }
            } else {
                if !member.blockedByAdmin {
                    blockSwipe(member, v)
                } else {
                    v
                }
            }
        }

        private func memberConnStatus(_ member: GroupMember) -> LocalizedStringKey {
            if member.activeConn?.connDisabled ?? false {
                return "disabled"
            } else if member.activeConn?.connInactive ?? false {
                return "inactive"
            } else {
                return member.memberStatus.shortText
            }
        }

        @ViewBuilder private func memberInfo(_ member: GroupMember) -> some View {
            if member.blocked {
                Text("blocked")
                    .foregroundColor(theme.colors.secondary)
            } else {
                let role = member.memberRole
                if [.owner, .admin, .observer].contains(role) {
                    Text(member.memberRole.text)
                        .foregroundColor(theme.colors.secondary)
                }
            }
        }

        private func blockSwipe<V: View>(_ member: GroupMember, _ v: V) -> some View {
            v.swipeActions(edge: .leading) {
                if member.memberSettings.showMessages {
                    Button {
                        alert = .blockMemberAlert(mem: member)
                    } label: {
                        Label("Block member", systemImage: "hand.raised").foregroundColor(theme.colors.secondary)
                    }
                } else {
                    Button {
                        alert = .unblockMemberAlert(mem: member)
                    } label: {
                        Label("Unblock member", systemImage: "hand.raised.slash").foregroundColor(theme.colors.primary)
                    }
                }
            }
        }

        private func blockForAllSwipe<V: View>(_ member: GroupMember, _ v: V) -> some View {
            v.swipeActions(edge: .leading) {
                if member.blockedByAdmin {
                    Button {
                        alert = .unblockForAllAlert(mem: member)
                    } label: {
                        Label("Unblock for all", systemImage: "hand.raised.slash").foregroundColor(theme.colors.primary)
                    }
                } else {
                    Button {
                        alert = .blockForAllAlert(mem: member)
                    } label: {
                        Label("Block for all", systemImage: "hand.raised").foregroundColor(theme.colors.secondary)
                    }
                }
            }
        }

        private func removeSwipe<V: View>(_ member: GroupMember, _ v: V) -> some View {
            v.swipeActions(edge: .trailing) {
                Button(role: .destructive) {
                    alert = .removeMemberAlert(mem: member)
                } label: {
                    Label("Remove member", systemImage: "trash")
                        .foregroundColor(Color.red)
                }
            }
        }

        private var memberVerifiedShield: Text {
            (Text(Image(systemName: "checkmark.shield")) + Text(" "))
                .font(.caption)
                .baselineOffset(2)
                .kerning(-2)
                .foregroundColor(theme.colors.secondary)
        }
    }

    private func memberInfoView(_ groupMember: GMember) -> some View {
        GroupMemberInfoView(groupInfo: groupInfo, groupMember: groupMember)
            .navigationBarHidden(false)
    }

    private func groupLinkButton() -> some View {
        NavigationLink {
            GroupLinkView(
                groupId: groupInfo.groupId,
                groupLink: $groupLink,
                groupLinkMemberRole: $groupLinkMemberRole,
                showTitle: false,
                creatingGroup: false
            )
            .navigationBarTitle("Group link")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            if groupLink == nil {
                Label("Create group link", systemImage: "link.badge.plus")
            } else {
                Label("Group link", systemImage: "link")
            }
        }
    }

    private func editGroupButton() -> some View {
        NavigationLink {
            GroupProfileView(
                groupInfo: $groupInfo,
                groupProfile: groupInfo.groupProfile
            )
            .navigationBarTitle("Group profile")
            .modifier(ThemedBackground())
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Edit group profile", systemImage: "pencil")
        }
    }

    private func addOrEditWelcomeMessage() -> some View {
        NavigationLink {
            GroupWelcomeView(
                groupInfo: $groupInfo,
                groupProfile: groupInfo.groupProfile,
                welcomeText: groupInfo.groupProfile.description ?? ""
            )
            .navigationTitle("Welcome message")
            .modifier(ThemedBackground(grouped: true))
            .navigationBarTitleDisplayMode(.large)
        } label: {
            groupInfo.groupProfile.description == nil
                ? Label("Add welcome message", systemImage: "plus.message")
                : Label("Welcome message", systemImage: "message")
        }
    }

    private func deleteGroupButton() -> some View {
        Button(role: .destructive) {
            alert = .deleteGroupAlert
        } label: {
            Label("Delete group", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    private func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }

    private func leaveGroupButton() -> some View {
        Button(role: .destructive) {
            alert = .leaveGroupAlert
        } label: {
            Label("Leave group", systemImage: "rectangle.portrait.and.arrow.right")
                .foregroundColor(Color.red)
        }
    }

    // TODO reuse this and clearChatAlert with ChatInfoView
    private func deleteGroupAlert() -> Alert {
        return Alert(
            title: Text("Delete group?"),
            message: deleteGroupAlertMessage(),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        await MainActor.run {
                            dismiss()
                            chatModel.chatId = nil
                            chatModel.removeChat(chat.chatInfo.id)
                        }
                    } catch let error {
                        logger.error("deleteGroupAlert apiDeleteChat error: \(error.localizedDescription)")
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func deleteGroupAlertMessage() -> Text {
        groupInfo.membership.memberCurrent ? Text("Group will be deleted for all members - this cannot be undone!") : Text("Group will be deleted for you - this cannot be undone!")
    }

    private func clearChatAlert() -> Alert {
        Alert(
            title: Text("Clear conversation?"),
            message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
            primaryButton: .destructive(Text("Clear")) {
                Task {
                    await clearChat(chat)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func leaveGroupAlert() -> Alert {
        Alert(
            title: Text("Leave group?"),
            message: Text("You will stop receiving messages from this group. Chat history will be preserved."),
            primaryButton: .destructive(Text("Leave")) {
                Task {
                    await leaveGroup(chat.chatInfo.apiId)
                    await MainActor.run { dismiss() }
                }
            },
            secondaryButton: .cancel()
        )
    }

    private func sendReceiptsOption() -> some View {
        Picker(selection: $sendReceipts) {
            ForEach([.yes, .no, .userDefault(sendReceiptsUserDefault)]) { (opt: SendReceipts) in
                Text(opt.text)
            }
        } label: {
            Label("Send receipts", systemImage: "checkmark.message")
        }
        .frame(height: 36)
        .onChange(of: sendReceipts) { _ in
            setSendReceipts()
        }
    }

    private func setSendReceipts() {
        var chatSettings = chat.chatInfo.chatSettings ?? ChatSettings.defaults
        chatSettings.sendRcpts = sendReceipts.bool()
        updateChatSettings(chat, chatSettings: chatSettings)
    }

    private func sendReceiptsOptionDisabled() -> some View {
        HStack {
            Label("Send receipts", systemImage: "checkmark.message")
            Spacer()
            Text("disabled")
                .foregroundStyle(.secondary)
        }
        .onTapGesture {
            alert = .largeGroupReceiptsDisabled
        }
    }

    private func removeMemberAlert(_ mem: GroupMember) -> Alert {
        Alert(
            title: Text("Remove member?"),
            message: Text("Member will be removed from group - this cannot be undone!"),
            primaryButton: .destructive(Text("Remove")) {
                Task {
                    do {
                        let updatedMember = try await apiRemoveMember(groupInfo.groupId, mem.groupMemberId)
                        await MainActor.run {
                            _ = chatModel.upsertGroupMember(groupInfo, updatedMember)
                        }
                    } catch let error {
                        logger.error("apiRemoveMember error: \(responseError(error))")
                        let a = getErrorAlert(error, "Error removing member")
                        alert = .error(title: a.title, error: a.message)
                    }
                }
            },
            secondaryButton: .cancel()
        )
    }
}

func groupPreferencesButton(_ groupInfo: Binding<GroupInfo>, _ creatingGroup: Bool = false) -> some View {
    NavigationLink {
        GroupPreferencesView(
            groupInfo: groupInfo,
            preferences: groupInfo.wrappedValue.fullGroupPreferences,
            currentPreferences: groupInfo.wrappedValue.fullGroupPreferences,
            creatingGroup: creatingGroup
        )
        .navigationBarTitle("Group preferences")
        .modifier(ThemedBackground(grouped: true))
        .navigationBarTitleDisplayMode(.large)
    } label: {
        if creatingGroup {
            Text("Set group preferences")
        } else {
            Label("Group preferences", systemImage: "switch.2")
        }
    }
}

func cantInviteIncognitoAlert() -> Alert {
    Alert(
        title: Text("Can't invite contacts!"),
        message: Text("You're using an incognito profile for this group - to prevent sharing your main profile inviting contacts is not allowed")
    )
}

func largeGroupReceiptsDisabledAlert() -> Alert {
    Alert(
        title: Text("Receipts are disabled"),
        message: Text("This group has over \(SMALL_GROUPS_RCPS_MEM_LIMIT) members, delivery receipts are not sent.")
    )
}

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupChatInfoView(
            chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []),
            groupInfo: Binding.constant(GroupInfo.sampleData)
        )
    }
}
