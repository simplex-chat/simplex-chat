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
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    @State var groupInfo: GroupInfo
    @ObservedObject private var alertManager = AlertManager.shared
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

        var id: GroupChatInfoViewAlert { get { self } }
    }

    var body: some View {
        NavigationView {
            let members = chatModel.groupMembers
                .filter { $0.memberStatus != .memLeft && $0.memberStatus != .memRemoved }
                .sorted { $0.displayName.lowercased() < $1.displayName.lowercased() }

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
                    if members.filter({ $0.memberCurrent }).count <= SMALL_GROUPS_RCPS_MEM_LIMIT {
                        sendReceiptsOption()
                    } else {
                        sendReceiptsOptionDisabled()
                    }
                } header: {
                    Text("")
                } footer: {
                    Text("Only group owners can change group preferences.")
                }

                Section("\(members.count + 1) members") {
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
                        searchFieldView(text: $searchText, focussed: $searchFocussed)
                            .padding(.leading, 8)
                    }
                    let s = searchText.trimmingCharacters(in: .whitespaces).localizedLowercase
                    let filteredMembers = s == "" ? members : members.filter { $0.chatViewName.localizedLowercase.contains(s) }
                    memberView(groupInfo.membership, user: true)
                    ForEach(filteredMembers) { member in
                        ZStack {
                            NavigationLink {
                                memberInfoView(member.groupMemberId)
                            } label: {
                                EmptyView()
                            }
                            .opacity(0)
                            memberView(member)
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
                    Section(header: Text("For console")) {
                        infoRow("Local name", chat.chatInfo.localDisplayName)
                        infoRow("Database ID", "\(chat.chatInfo.apiId)")
                    }
                }
            }
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
            ChatInfoImage(chat: chat, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
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
                            ChatModel.shared.groupMembers = groupMembers
                        }
                    }
                }
        } label: {
            Label("Invite members", systemImage: "plus")
        }
    }

    private func memberView(_ member: GroupMember, user: Bool = false) -> some View {
        HStack{
            ProfileImage(imageStr: member.image)
                .frame(width: 38, height: 38)
                .padding(.trailing, 2)
            // TODO server connection status
            VStack(alignment: .leading) {
                let t = Text(member.chatViewName).foregroundColor(member.memberIncognito ? .indigo : .primary)
                (member.verified ? memberVerifiedShield + t : t)
                    .lineLimit(1)
                let s = Text(member.memberStatus.shortText)
                (user ? Text ("you: ") + s : s)
                    .lineLimit(1)
                    .font(.caption)
                    .foregroundColor(.secondary)
            }
            Spacer()
            let role = member.memberRole
            if role == .owner || role == .admin {
                Text(member.memberRole.text)
                    .foregroundColor(.secondary)
            }
        }
    }

    private var memberVerifiedShield: Text {
        (Text(Image(systemName: "checkmark.shield")) + Text(" "))
            .font(.caption)
            .baselineOffset(2)
            .kerning(-2)
            .foregroundColor(.secondary)
    }

    @ViewBuilder private func memberInfoView(_ groupMemberId: Int64?) -> some View {
        if let mId = groupMemberId, let member = chatModel.groupMembers.first(where: { $0.groupMemberId == mId }) {
            GroupMemberInfoView(groupInfo: groupInfo, member: member)
                .navigationBarHidden(false)
        }
    }

    private func groupLinkButton() -> some View {
        NavigationLink {
            GroupLinkView(groupId: groupInfo.groupId, groupLink: $groupLink, groupLinkMemberRole: $groupLinkMemberRole)
                .navigationBarTitle("Group link")
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
            .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Edit group profile", systemImage: "pencil")
        }
    }

    private func addOrEditWelcomeMessage() -> some View {
        NavigationLink {
            GroupWelcomeView(groupId: groupInfo.groupId, groupInfo: $groupInfo)
            .navigationTitle("Welcome message")
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
        GroupChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []), groupInfo: GroupInfo.sampleData)
    }
}
