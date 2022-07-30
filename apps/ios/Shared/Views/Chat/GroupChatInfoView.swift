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
    var groupInfo: GroupInfo
    @ObservedObject private var alertManager = AlertManager.shared
    @State private var members: [GroupMember] = []
    @State private var alert: GroupChatInfoViewAlert? = nil
    @State private var showAddMembersSheet: Bool = false
    @State private var selectedMember: GroupMember? = nil
    @State private var showGroupProfile: Bool = false

    enum GroupChatInfoViewAlert: Identifiable {
        case deleteGroupAlert
        case clearChatAlert
        case leaveGroupAlert

        var id: GroupChatInfoViewAlert { get { self } }
    }

    var body: some View {
        NavigationView {
            List {
                groupInfoHeader()
                    .listRowBackground(Color.clear)

                Section {
                    Button {
                        showGroupProfile = true
                    } label: {
                        Label("Edit group profile", systemImage: "pencil")
                    }
                }
                .sheet(isPresented: $showGroupProfile) {
                    GroupProfileView(groupId: groupInfo.apiId, groupProfile: groupInfo.groupProfile)
                }

                Section("\(members.count + 1) members") {
                    if (groupInfo.canAddMembers) {
                        addMembersButton()
                    }
                    memberView(groupInfo.membership, user: true)
                    ForEach(members) { member in
                        Button { selectedMember = member } label: { memberView(member) }
                    }
                }
                .sheet(isPresented: $showAddMembersSheet) {
                    AddGroupMembersView(chat: chat, groupInfo: groupInfo, membersToAdd: filterMembersToAdd(members))
                }
                .sheet(item: $selectedMember) { member in
                    GroupMemberInfoView(groupInfo: groupInfo, member: member)
                }

                Section {
                    clearChatButton()
                    if groupInfo.canDelete {
                        deleteGroupButton()
                    }
                    if (groupInfo.membership.memberStatus != .memLeft) {
                        leaveGroupButton()
                    }
                }

                Section(header: Text("For console")) {
                    infoRow("Local name", chat.chatInfo.localDisplayName)
                    infoRow("Database ID", "\(chat.chatInfo.apiId)")
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
            }
        }
        .task {
            let ms = await apiListMembers(chat.chatInfo.apiId)
            await MainActor.run {
                members = ms.sorted { $0.displayName.lowercased() < $1.displayName.lowercased() }
            }
        }
    }

    func groupInfoHeader() -> some View {
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
        Button {
            showAddMembersSheet = true
        } label: {
            Label("Invite members", systemImage: "plus")
        }
    }

    func serverImage() -> some View {
        let status = chat.serverInfo.networkStatus
        return Image(systemName: status.imageName)
            .foregroundColor(status == .connected ? .green : .secondary)
    }

    func memberView(_ member: GroupMember, user: Bool = false) -> some View {
        HStack{
            ProfileImage(imageStr: member.image)
                .frame(width: 38, height: 38)
                .padding(.trailing, 2)
            // TODO server connection status
            VStack(alignment: .leading) {
                Text(member.chatViewName)
                    .lineLimit(1)
                    .foregroundColor(.primary)
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

    func deleteGroupButton() -> some View {
        Button(role: .destructive) {
            alert = .deleteGroupAlert
        } label: {
            Label("Delete group", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    func clearChatButton() -> some View {
        Button() {
            alert = .clearChatAlert
        } label: {
            Label("Clear conversation", systemImage: "gobackward")
                .foregroundColor(Color.orange)
        }
    }

    func leaveGroupButton() -> some View {
        Button(role: .destructive) {
            alert = .leaveGroupAlert
        } label: {
            Label("Leave group", systemImage: "rectangle.portrait.and.arrow.right")
                .foregroundColor(Color.red)
        }
    }

    // TODO reuse this and clearChatAlert with ChatInfoView
    private func deleteGroupAlert() -> Alert {
        Alert(
            title: Text("Delete group?"),
            message: Text("Group will be deleted for all members - this cannot be undone!"),
            primaryButton: .destructive(Text("Delete")) {
                Task {
                    do {
                        try await apiDeleteChat(type: chat.chatInfo.chatType, id: chat.chatInfo.apiId)
                        await MainActor.run {
                            chatModel.removeChat(chat.chatInfo.id)
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

struct GroupChatInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupChatInfoView(chat: Chat(chatInfo: ChatInfo.sampleData.group, chatItems: []), groupInfo: GroupInfo.sampleData)
    }
}
