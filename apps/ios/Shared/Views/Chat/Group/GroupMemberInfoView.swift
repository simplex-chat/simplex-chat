//
//  GroupMemberInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 25.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupMemberInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    var groupInfo: GroupInfo
    @Binding var member: GroupMember?
    @Binding var connectionStats: ConnectionStats?
    @State private var newRole: GroupMemberRole = .member
    @State private var alert: GroupMemberInfoViewAlert?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum GroupMemberInfoViewAlert: Identifiable {
        case removeMemberAlert(mem: GroupMember)
        case changeMemberRoleAlert(mem: GroupMember, role: GroupMemberRole)
        case switchAddressAlert
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case .removeMemberAlert: return "removeMemberAlert"
            case let .changeMemberRoleAlert(_, role): return "changeMemberRoleAlert \(role.rawValue)"
            case .switchAddressAlert: return "switchAddressAlert"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            if let member = member {
                List {
                    groupMemberInfoHeader(member)
                        .listRowBackground(Color.clear)

                    if let contactId = member.memberContactId {
                        if let chat = chatModel.getContactChat(contactId),
                           chat.chatInfo.contact?.directContact ?? false {
                            Section {
                                knownDirectChatButton(chat)
                            }
                        } else if groupInfo.fullGroupPreferences.directMessages.on {
                            Section {
                                newDirectChatButton(contactId)
                            }
                        }
                    }

                    Section("Member") {
                        infoRow("Group", groupInfo.displayName)

                        if let roles = member.canChangeRoleTo(groupInfo: groupInfo) {
                            Picker("Change role", selection: $newRole) {
                                ForEach(roles) { role in
                                    Text(role.text)
                                }
                            }
                        } else {
                            infoRow("Role", member.memberRole.text)
                        }

                        // TODO invited by - need to get contact by contact id
                        if let conn = member.activeConn {
                            let connLevelDesc = conn.connLevel == 0 ? NSLocalizedString("direct", comment: "connection level description") : String.localizedStringWithFormat(NSLocalizedString("indirect (%d)", comment: "connection level description"), conn.connLevel)
                            infoRow("Connection", connLevelDesc)
                        }
                    }

                    Section("Servers") {
                        // TODO network connection status
                        Button("Change receiving address") {
                            alert = .switchAddressAlert
                        }
                        if let connStats = connectionStats {
                            smpServers("Receiving via", connStats.rcvServers)
                            smpServers("Sending via", connStats.sndServers)
                        }
                    }

                    if member.canBeRemoved(groupInfo: groupInfo) {
                        Section {
                            removeMemberButton(member)
                        }
                    }

                    if developerTools {
                        Section("For console") {
                            infoRow("Local name", member.localDisplayName)
                            infoRow("Database ID", "\(member.groupMemberId)")
                        }
                    }
                }
                .navigationBarHidden(true)
                .onAppear { newRole = member.memberRole }
                .onChange(of: newRole) { _ in
                    if newRole != member.memberRole {
                        alert = .changeMemberRoleAlert(mem: member, role: newRole)
                    }
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case let .removeMemberAlert(mem): return removeMemberAlert(mem)
            case let .changeMemberRoleAlert(mem, _): return changeMemberRoleAlert(mem)
            case .switchAddressAlert: return switchAddressAlert(switchMemberAddress)
            case let .error(title, error): return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    func knownDirectChatButton(_ chat: Chat) -> some View {
        Button {
            dismissAllSheets(animated: true)
            DispatchQueue.main.async {
                chatModel.chatId = chat.id
            }
        } label: {
            Label("Send direct message", systemImage: "message")
        }
    }

    func newDirectChatButton(_ contactId: Int64) -> some View {
        Button {
            do {
                let chat = try apiGetChat(type: .direct, id: contactId)
                // TODO it's not correct to blindly set network status to connected - we should manage network status in model / backend
                chat.serverInfo = Chat.ServerInfo(networkStatus: .connected)
                chatModel.addChat(chat)
                dismissAllSheets(animated: true)
                DispatchQueue.main.async {
                    chatModel.chatId = chat.id
                }
            } catch let error {
                logger.error("openDirectChatButton apiGetChat error: \(responseError(error))")
            }
        } label: {
            Label("Send direct message", systemImage: "message")
        }
    }

    private func groupMemberInfoHeader(_ mem: GroupMember) -> some View {
        VStack {
            ProfileImage(imageStr: mem.image, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
                .padding(.top, 12)
                .padding()
            Text(mem.displayName)
                .font(.largeTitle)
                .lineLimit(1)
                .padding(.bottom, 2)
            if mem.fullName != "" && mem.fullName != mem.displayName {
                Text(mem.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    func removeMemberButton(_ mem: GroupMember) -> some View {
        Button(role: .destructive) {
            alert = .removeMemberAlert(mem: mem)
        } label: {
            Label("Remove member", systemImage: "trash")
                .foregroundColor(Color.red)
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
                            dismiss()
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

    private func changeMemberRoleAlert(_ mem: GroupMember) -> Alert {
        Alert(
            title: Text("Change member role?"),
            message: mem.memberCurrent ? Text("Member role will be changed to \"\(newRole.text)\". All group members will be notified.") : Text("Member role will be changed to \"\(newRole.text)\". The member will receive a new invitation."),
            primaryButton: .default(Text("Change")) {
                Task {
                    do {
                        let updatedMember = try await apiMemberRole(groupInfo.groupId, mem.groupMemberId, newRole)
                        await MainActor.run {
                            member = updatedMember
                            _ = chatModel.upsertGroupMember(groupInfo, updatedMember)
                        }
                        
                    } catch let error {
                        newRole = mem.memberRole
                        logger.error("apiMemberRole error: \(responseError(error))")
                        let a = getErrorAlert(error, "Error changing role")
                        alert = .error(title: a.title, error: a.message)
                    }
                }
            },
            secondaryButton: .cancel {
                newRole = mem.memberRole
            }
        )
    }

    private func switchMemberAddress() {
        Task {
            do {
                if let member = member {
                    try await apiSwitchGroupMember(groupInfo.apiId, member.groupMemberId)
                }
            } catch let error {
                logger.error("switchMemberAddress apiSwitchGroupMember error: \(responseError(error))")
                let a = getErrorAlert(error, "Error changing address")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupMemberInfoView(
            groupInfo: GroupInfo.sampleData,
            member: Binding.constant(GroupMember.sampleData),
            connectionStats: Binding.constant(nil)
        )
    }
}
