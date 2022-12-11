//
//  GroupChatInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 14.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupChatInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @ObservedObject var chat: Chat
    @State var groupInfo: GroupInfo
    @State var selectedMember: Int64? = nil
    @ObservedObject private var alertManager = AlertManager.shared
    @State private var alert: GroupChatInfoViewAlert? = nil
    @State private var groupLink: String?
    @State private var showAddMembersSheet: Bool = false
    @State private var connectionStats: ConnectionStats?
    @State private var connectionCode: String?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum GroupChatInfoViewAlert: Identifiable {
        case deleteGroupAlert
        case clearChatAlert
        case leaveGroupAlert
        case cantInviteIncognitoAlert

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
                    groupPreferencesButton($groupInfo)
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
                    memberView(groupInfo.membership, user: true)
                    ForEach(members) { member in
                        NavLinkPlain(
                            tag: member.groupMemberId,
                            selection: $selectedMember,
                            label: { memberView(member) }
                        )
                    }
                    .background(
                        NavigationLink(
                            destination: memberInfoView(selectedMember),
                            isActive: Binding(
                                get: { selectedMember != nil },
                                set: { _, _ in selectedMember = nil }
                            )
                        ) { EmptyView() }
                        .opacity(0)
                    )
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
            }
        }
        .onAppear {
            do {
                groupLink = try apiGetGroupLink(groupInfo.groupId)
            } catch let error {
                logger.error("GroupChatInfoView apiGetGroupLink: \(responseError(error))")
            }
        }
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
                .lineLimit(1)
                .padding(.bottom, 2)
            if cInfo.fullName != "" && cInfo.fullName != cInfo.displayName {
                Text(cInfo.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    private func addMembersButton() -> some View {
        NavigationLink {
            AddGroupMembersView(chat: chat, groupInfo: groupInfo)
                .onAppear {
                    ChatModel.shared.groupMembers = apiListMembersSync(groupInfo.groupId)
                }
        } label: {
            Label("Invite members", systemImage: "plus")
        }
    }

    private func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
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
            GroupLinkView(groupId: groupInfo.groupId, groupLink: $groupLink)
                .navigationBarTitle("Group link")
                .navigationBarTitleDisplayMode(.large)
        } label: {
            Label("Group link", systemImage: "link")
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
                            chatModel.removeChat(chat.chatInfo.id)
                            chatModel.chatId = nil
                            dismiss()
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

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []), groupInfo: GroupInfo.sampleData)
    }
}
