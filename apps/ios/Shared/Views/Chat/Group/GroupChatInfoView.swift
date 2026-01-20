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
    @Binding var scrollToItemId: ChatItem.ID?
    var onSearch: () -> Void
    @State var localAlias: String
    @FocusState private var aliasTextFieldFocused: Bool
    @State private var alert: GroupChatInfoViewAlert? = nil
    @State private var groupLink: GroupLink?
    @State private var groupLinkMemberRole: GroupMemberRole = .member
    @State private var groupLinkNavLinkActive: Bool = false
    @State private var addMembersNavLinkActive: Bool = false
    @State private var settingsNavLinkActive: Bool = false
    @State private var connectionStats: ConnectionStats?
    @State private var connectionCode: String?
    @State private var sendReceipts = SendReceipts.userDefault(true)
    @State private var sendReceiptsUserDefault = true
    @State private var progressIndicator = false
    @State private var searchText: String = ""
    @FocusState private var searchFocussed
    @State private var showSecrets: Set<Int> = []
    @State private var selectedTab: GroupInfoTab = .members
    @State private var selectedImage: ChatItem?
    @State private var showImageFullScreen = false
    
    enum GroupInfoTab: CaseIterable {
        case members
        case images
        case videos
        case files
        case links
        case voices
        
        var imageName: String {
            switch self {
            case .members: return "person.2"
            case .images: return "photo"
            case .videos: return "video"
            case .files: return "doc"
            case .links: return "link"
            case .voices: return "mic"
            }
        }
    }
    
    enum GroupChatInfoViewAlert: Identifiable {
        case cantInviteIncognitoAlert
        case blockMemberAlert(mem: GroupMember)
        case unblockMemberAlert(mem: GroupMember)
        case blockForAllAlert(mem: GroupMember)
        case unblockForAllAlert(mem: GroupMember)
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)
        
        var id: String {
            switch self {
            case .cantInviteIncognitoAlert: return "cantInviteIncognitoAlert"
            case let .blockMemberAlert(mem): return "blockMemberAlert \(mem.groupMemberId)"
            case let .unblockMemberAlert(mem): return "unblockMemberAlert \(mem.groupMemberId)"
            case let .blockForAllAlert(mem): return "blockForAllAlert \(mem.groupMemberId)"
            case let .unblockForAllAlert(mem): return "unblockForAllAlert \(mem.groupMemberId)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }
    
    var body: some View {
        NavigationView {
            let members = chatModel.groupMembers
                .filter { m in let status = m.wrapped.memberStatus; return status != .memLeft && status != .memRemoved }
                .sorted { $0.wrapped.memberRole > $1.wrapped.memberRole }
            
            ZStack {
                List {
                    groupInfoHeader()
                        .listRowBackground(Color.clear)
                    
                    localAliasTextEdit()
                        .listRowBackground(Color.clear)
                        .listRowSeparator(.hidden)
                        .padding(.bottom, 18)
                    
                    infoActionButtons()
                        .padding(.horizontal)
                        .frame(maxWidth: .infinity)
                        .frame(height: infoViewActionButtonHeight)
                        .listRowBackground(Color.clear)
                        .listRowSeparator(.hidden)
                        .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                        .padding(.bottom, 18)
                    
                    tabContentView(members: members)
                }
                .modifier(ThemedBackground(grouped: true))
                .navigationBarHidden(true)
                .disabled(progressIndicator)
                .opacity(progressIndicator ? 0.6 : 1)
                
                if progressIndicator {
                    ProgressView().scaleEffect(2)
                }
            }
            .fullScreenCover(isPresented: $showImageFullScreen) {
                if let item = selectedImage, let image = getLoadedImage(item.file) {
                    FullScreenMediaView(
                        chatItem: item,
                        scrollToItem: nil,
                        image: image,
                        showView: $showImageFullScreen
                    )
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .cantInviteIncognitoAlert: return cantInviteIncognitoAlert()
            case let .blockMemberAlert(mem): return blockMemberAlert(groupInfo, mem)
            case let .unblockMemberAlert(mem): return unblockMemberAlert(groupInfo, mem)
            case let .blockForAllAlert(mem): return blockForAllAlert(groupInfo, mem)
            case let .unblockForAllAlert(mem): return unblockForAllAlert(groupInfo, mem)
            case let .error(title, error): return mkAlert(title: title, message: error)
            }
        }
        .onAppear {
            if let currentUser = chatModel.currentUser {
                sendReceiptsUserDefault = currentUser.sendRcptsSmallGroups
            }
            sendReceipts = SendReceipts.fromBool(groupInfo.chatSettings.sendRcpts, userDefault: sendReceiptsUserDefault)
            do {
                if let gLink = try apiGetGroupLink(groupInfo.groupId) {
                    groupLink = gLink
                    groupLinkMemberRole = gLink.acceptMemberRole
                }
            } catch let error {
                logger.error("GroupChatInfoView apiGetGroupLink: \(responseError(error))")
            }
        }
    }
    
    @ViewBuilder
    private func tabContentView(members: [GMember]) -> some View {
        switch selectedTab {
        case .members:
            membersTabContent(members: members)
        case .images:
            Section {
                GroupImagesTabView(
                    groupInfo: groupInfo,
                    selectedImage: $selectedImage,
                    showImageFullScreen: $showImageFullScreen
                )
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .listRowBackground(Color.clear)
        case .videos:
            Section {
                GroupVideosTabView(groupInfo: groupInfo)
            }
            .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
            .listRowBackground(Color.clear)
        case .files:
            Section {
                GroupFilesTabView(groupInfo: groupInfo)
            }
        case .links:
            Section {
                GroupLinksTabView(groupInfo: groupInfo)
            }
        case .voices:
            Section {
                GroupVoicesTabView(groupInfo: groupInfo)
            }
        }
    }
    
    private func groupInfoHeader() -> some View {
        VStack {
            let cInfo = chat.chatInfo
            // show actual display name, alias can be edited in this view
            let displayName = (cInfo.groupInfo?.groupProfile.displayName ?? cInfo.displayName).trimmingCharacters(in: .whitespacesAndNewlines)
            let fullName = cInfo.fullName.trimmingCharacters(in: .whitespacesAndNewlines)
            ChatInfoImage(chat: chat, size: 192, color: Color(uiColor: .tertiarySystemFill))
                .padding(.top, 12)
                .padding()
            Text(displayName)
                .font(.largeTitle)
                .multilineTextAlignment(.center)
                .lineLimit(4)
                .padding(.bottom, 2)
            if fullName != "" && fullName != displayName && fullName != cInfo.displayName.trimmingCharacters(in: .whitespacesAndNewlines) {
                Text(cInfo.fullName)
                    .font(.title2)
                    .multilineTextAlignment(.center)
                    .lineLimit(3)
                    .padding(.bottom, 2)
            }
            if let descr = cInfo.shortDescr?.trimmingCharacters(in: .whitespacesAndNewlines), descr != "" {
                let r = markdownText(descr, textStyle: .subheadline, showSecrets: showSecrets, backgroundColor: theme.colors.background)
                msgTextResultView(r, Text(AttributedString(r.string)), showSecrets: $showSecrets, centered: true, smallFont: true)
                    .multilineTextAlignment(.center)
                    .lineLimit(4)
                    .fixedSize(horizontal: false, vertical: true)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }
    
    private func localAliasTextEdit() -> some View {
        TextField("Set chat name…", text: $localAlias)
            .disableAutocorrection(true)
            .focused($aliasTextFieldFocused)
            .submitLabel(.done)
            .onChange(of: aliasTextFieldFocused) { focused in
                if !focused {
                    setGroupAlias()
                }
            }
            .onSubmit {
                setGroupAlias()
            }
            .multilineTextAlignment(.center)
            .foregroundColor(theme.colors.secondary)
    }
    
    private func setGroupAlias() {
        Task {
            do {
                if let gInfo = try await apiSetGroupAlias(groupId: chat.chatInfo.apiId, localAlias: localAlias) {
                    await MainActor.run {
                        chatModel.updateGroup(gInfo)
                    }
                }
            } catch {
                logger.error("setGroupAlias error: \(responseError(error))")
            }
        }
    }
    
    func infoActionButtons() -> some View {
        GeometryReader { g in
            let spacing: CGFloat = 8
            let horizontalPadding: CGFloat = 32
            let availableWidth = g.size.width - horizontalPadding
            let totalSpacing: CGFloat = spacing * 3
            let buttonWidth = (availableWidth - totalSpacing) / 4
            
            HStack(alignment: .center, spacing: spacing) {
                searchButton(width: buttonWidth)
                if groupInfo.canAddMembers {
                    addMembersActionButton(width: buttonWidth)
                }
                if let nextNtfMode = chat.chatInfo.nextNtfMode {
                    muteButton(width: buttonWidth, nextNtfMode: nextNtfMode)
                }
                settingsButton(width: buttonWidth)
            }
            .frame(maxWidth: .infinity, alignment: .center)
        }
    }
    
    private func searchButton(width: CGFloat) -> some View {
        InfoViewButton(image: "magnifyingglass", title: "search", width: width) {
            dismiss()
            onSearch()
        }
        .disabled(!groupInfo.ready || chat.chatItems.isEmpty)
    }
    
    private func addMembersActionButton(width: CGFloat) -> some View {
        ZStack {
            if chat.chatInfo.incognito {
                InfoViewButton(image: "link.badge.plus", title: "invite", width: width) {
                    groupLinkNavLinkActive = true
                }
                
                NavigationLink(isActive: $groupLinkNavLinkActive) {
                    groupLinkDestinationView()
                } label: {
                    EmptyView()
                }
                .frame(width: 1, height: 1)
                .hidden()
            } else {
                InfoViewButton(image: "person.fill.badge.plus", title: "invite", width: width) {
                    addMembersNavLinkActive = true
                }
                
                NavigationLink(isActive: $addMembersNavLinkActive) {
                    addMembersDestinationView()
                } label: {
                    EmptyView()
                }
                .frame(width: 1, height: 1)
                .hidden()
            }
        }
        .disabled(!groupInfo.ready)
    }
    
    private func muteButton(width: CGFloat, nextNtfMode: MsgFilter) -> some View {
        return InfoViewButton(
            image: nextNtfMode.iconFilled,
            title: "\(nextNtfMode.text(mentions: true))",
            width: width
        ) {
            toggleNotifications(chat, enableNtfs: nextNtfMode)
        }
        .disabled(!groupInfo.ready)
    }
    
    private func settingsButton(width: CGFloat) -> some View {
        let isOwner = groupInfo.isOwner
        let image = isOwner ? "pencil" : "info.circle"
        let title: LocalizedStringKey = isOwner ? "edit" : "info"
        
        return ZStack {
            InfoViewButton(image: image, title: title, width: width) {
                settingsNavLinkActive = true
            }
            
            NavigationLink(isActive: $settingsNavLinkActive) {
                settingsDestinationView()
            } label: {
                EmptyView()
            }
            .frame(width: 1, height: 1)
            .hidden()
        }
        .disabled(!groupInfo.ready)
    }
    
    private func settingsDestinationView() -> some View {
        GroupSettingsView(
            chat: chat,
            groupInfo: $groupInfo,
            sendReceipts: $sendReceipts,
            sendReceiptsUserDefault: sendReceiptsUserDefault,
            progressIndicator: $progressIndicator,
            dismiss: dismiss
        )
    }
    
    private func membersTabContent(members: [GMember]) -> some View {
        Group {
            Section {
                if groupInfo.canAddMembers && groupInfo.businessChat == nil {
                    groupLinkButton()
                }
                if groupInfo.businessChat == nil && groupInfo.membership.memberRole >= .moderator {
                    memberSupportButton()
                }
                if groupInfo.canModerate {
                    GroupReportsChatNavLink(chat: chat, groupInfo: groupInfo, scrollToItemId: $scrollToItemId)
                }
                if groupInfo.membership.memberActive
                    && (groupInfo.membership.memberRole < .moderator || groupInfo.membership.supportChat != nil) {
                    UserSupportChatNavLink(chat: chat, groupInfo: groupInfo, scrollToItemId: $scrollToItemId)
                }
            }
            
            if !groupInfo.nextConnectPrepared {
                Section(header: Text("\(members.count + 1) members").foregroundColor(theme.colors.secondary)) {
                    if groupInfo.canAddMembers {
                        if (chat.chatInfo.incognito) {
                            Label("Invite members", systemImage: "plus")
                                .foregroundColor(Color(uiColor: .tertiaryLabel))
                                .onTapGesture { alert = .cantInviteIncognitoAlert }
                        } else {
                            addMembersButton()
                        }
                    }
                    searchFieldView(text: $searchText, focussed: $searchFocussed, theme.colors.onBackground, theme.colors.secondary)
                        .padding(.leading, 8)
                    let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                    let filteredMembers = s == ""
                    ? members
                    : members.filter { $0.wrapped.localAliasAndFullName.localizedLowercase.contains(s) }
                    MemberRowView(
                        chat: chat,
                        groupInfo: groupInfo,
                        groupMember: GMember(groupInfo.membership),
                        scrollToItemId: $scrollToItemId,
                        user: true,
                        alert: $alert
                    )
                    ForEach(filteredMembers) { member in
                        MemberRowView(chat: chat, groupInfo: groupInfo, groupMember: member, scrollToItemId: $scrollToItemId, alert: $alert)
                    }
                }
            }
        }
    }
    
    private func noDataAvailableView() -> some View {
        Section {
            HStack {
                Spacer()
                Text("No data available")
                    .foregroundColor(theme.colors.secondary)
                    .padding(.vertical, 40)
                Spacer()
            }
        }
    }
    
    private func addMembersButton() -> some View {
        let label: LocalizedStringKey = switch groupInfo.businessChat?.chatType {
        case .customer: "Add team members"
        case .business: "Add friends"
        case .none: "Invite members"
        }
        return NavigationLink {
            addMembersDestinationView()
        } label: {
            Label(label, systemImage: "plus")
        }
    }
    
    private func addMembersDestinationView() -> some View {
        AddGroupMembersView(chat: chat, groupInfo: groupInfo)
            .onAppear {
                searchFocussed = false
                Task {
                    await chatModel.loadGroupMembers(groupInfo)
                }
            }
    }
    
    private struct MemberRowView: View {
        var chat: Chat
        var groupInfo: GroupInfo
        @ObservedObject var groupMember: GMember
        @Binding var scrollToItemId: ChatItem.ID?
        @EnvironmentObject var theme: AppTheme
        var user: Bool = false
        @Binding var alert: GroupChatInfoViewAlert?
        
        var body: some View {
            let member = groupMember.wrapped
            let v1 = HStack{
                MemberProfileImage(member, size: 38)
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
            
            let v = ZStack {
                if user {
                    v1
                } else {
                    NavigationLink {
                        memberInfoView()
                    } label: {
                        EmptyView()
                    }
                    .opacity(0)
                    v1
                }
            }
            
            if user {
                v
            } else if groupInfo.membership.memberRole >= .moderator {
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
        
        private func memberInfoView() -> some View {
            GroupMemberInfoView(groupInfo: groupInfo, chat: chat, groupMember: groupMember, scrollToItemId: $scrollToItemId)
                .navigationBarHidden(false)
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
                if [.owner, .admin, .moderator, .observer].contains(role) {
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
                    showRemoveMemberAlert(groupInfo, member)
                } label: {
                    Label("Remove member", systemImage: "trash")
                        .foregroundColor(Color.red)
                }
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
    
    private func groupLinkButton() -> some View {
        NavigationLink {
            groupLinkDestinationView()
        } label: {
            if groupLink == nil {
                Label("Create group link", systemImage: "link.badge.plus")
            } else {
                Label("Group link", systemImage: "link")
            }
        }
    }
    
    private func groupLinkDestinationView() -> some View {
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
    }
    
    struct UserSupportChatNavLink: View {
        @ObservedObject var chat: Chat
        @EnvironmentObject var theme: AppTheme
        var groupInfo: GroupInfo
        @EnvironmentObject var chatModel: ChatModel
        @Binding var scrollToItemId: ChatItem.ID?
        @State private var navLinkActive = false
        
        var body: some View {
            let scopeInfo: GroupChatScopeInfo = .memberSupport(groupMember_: nil)
            NavigationLink(isActive: $navLinkActive) {
                SecondaryChatView(
                    chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: scopeInfo), chatItems: [], chatStats: ChatStats()),
                    scrollToItemId: $scrollToItemId
                )
            } label: {
                HStack {
                    Label("Chat with admins", systemImage: chat.supportUnreadCount > 0 ? "flag.fill" : "flag")
                    Spacer()
                    if chat.supportUnreadCount > 0 {
                        UnreadBadge(count: chat.supportUnreadCount, color: theme.colors.primary)
                    }
                }
            }
            .onChange(of: navLinkActive) { active in
                if active {
                    ItemsModel.loadSecondaryChat(groupInfo.id, chatFilter: .groupChatScopeContext(groupScopeInfo: scopeInfo))
                }
            }
        }
    }
    
    private func memberSupportButton() -> some View {
        NavigationLink {
            MemberSupportView(groupInfo: groupInfo, scrollToItemId: $scrollToItemId)
                .navigationBarTitle("Chats with members")
                .modifier(ThemedBackground())
                .navigationBarTitleDisplayMode(.large)
        } label: {
            HStack {
                Label(
                    "Chats with members",
                    systemImage: chat.supportUnreadCount > 0 ? "flag.fill" : "flag"
                )
                Spacer()
                if chat.supportUnreadCount > 0 {
                    UnreadBadge(count: chat.supportUnreadCount, color: theme.colors.primary)
                }
            }
        }
    }
    
    struct GroupReportsChatNavLink: View {
        @ObservedObject var chat: Chat
        @EnvironmentObject var theme: AppTheme
        var groupInfo: GroupInfo
        @EnvironmentObject var chatModel: ChatModel
        @Binding var scrollToItemId: ChatItem.ID?
        @State private var navLinkActive = false
        
        var body: some View {
            NavigationLink(isActive: $navLinkActive) {
                SecondaryChatView(
                    chat: Chat(chatInfo: .group(groupInfo: groupInfo, groupChatScope: .reports), chatItems: [], chatStats: ChatStats()),
                    scrollToItemId: $scrollToItemId
                )
            } label: {
                HStack {
                    Label {
                        Text("Member reports")
                    } icon: {
                        Image(systemName: chat.chatStats.reportsCount > 0 ? "flag.fill" : "flag").foregroundColor(.red)
                    }
                    Spacer()
                    if chat.chatStats.reportsCount > 0 {
                        UnreadBadge(count: chat.chatStats.reportsCount, color: .red)
                    }
                }
            }
            .onChange(of: navLinkActive) { active in
                if active {
                    ItemsModel.loadSecondaryChat(chat.id, chatFilter: .msgContentTagContext(contentTag: .report))
                }
            }
        }
    }
}

func showRemoveMemberAlert(_ groupInfo: GroupInfo, _ mem: GroupMember, dismiss: DismissAction? = nil) {
    showAlert(
        NSLocalizedString("Remove member?", comment: "alert title"),
        message:
            groupInfo.businessChat == nil
            ? NSLocalizedString("Member will be removed from group - this cannot be undone!", comment: "alert message")
            : NSLocalizedString("Member will be removed from chat - this cannot be undone!", comment: "alert message"),
        actions: {[
            UIAlertAction(title: NSLocalizedString("Remove", comment: "alert action"), style: .destructive) { _ in
                removeMember(groupInfo, mem, withMessages: false, dismiss: dismiss)
            },
            UIAlertAction(title: NSLocalizedString("Remove and delete messages", comment: "alert action"), style: .destructive) { _ in
                removeMember(groupInfo, mem, withMessages: true, dismiss: dismiss)
            },
            cancelAlertAction
        ]}
    )
}

func removeMember(_ groupInfo: GroupInfo, _ mem: GroupMember, withMessages: Bool, dismiss: DismissAction?) {
    Task {
        do {
            let (updatedGroupInfo, updatedMembers) = try await apiRemoveMembers(groupInfo.groupId, [mem.groupMemberId], withMessages)
            await MainActor.run {
                ChatModel.shared.updateGroup(updatedGroupInfo)
                updatedMembers.forEach { updatedMember in
                    _ = ChatModel.shared.upsertGroupMember(updatedGroupInfo, updatedMember)
                    if withMessages {
                        ChatModel.shared.removeMemberItems(updatedMember, byMember: groupInfo.membership, groupInfo)
                    }
                }
                dismiss?()
            }
        } catch let error {
            logger.error("apiRemoveMembers error: \(responseError(error))")
            await MainActor.run {
                showAlert(
                    NSLocalizedString("Error removing member", comment: "alert title"),
                    message: responseError(error)
                )
            }
        }
    }
}

func deleteGroupAlertMessage(_ groupInfo: GroupInfo) -> Text {
    groupInfo.businessChat == nil ? (
        groupInfo.membership.memberCurrent ? Text("Group will be deleted for all members - this cannot be undone!") : Text("Group will be deleted for you - this cannot be undone!")
    ) : (
        groupInfo.membership.memberCurrent ? Text("Chat will be deleted for all members - this cannot be undone!") : Text("Chat will be deleted for you - this cannot be undone!")
    )
}

func cantInviteIncognitoAlert() -> Alert {
    Alert(
        title: Text("Can't invite contacts!"),
        message: Text("You're using an incognito profile for this group - to prevent sharing your main profile inviting contacts is not allowed")
    )
}

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupChatInfoView(
            chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []),
            groupInfo: Binding.constant(GroupInfo.sampleData),
            scrollToItemId: Binding.constant(nil),
            onSearch: {},
            localAlias: ""
        )
    }
}
