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
    @State var member: GroupMember
    @Binding var connectionStats: ConnectionStats?
    @State private var newRole: GroupMemberRole = .member
    @State private var alert: GroupMemberInfoViewAlert?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false

    enum GroupMemberInfoViewAlert: Identifiable {
        case removeMemberAlert
        case changeMemberRoleAlert(role: GroupMemberRole)
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case .removeMemberAlert: return "removeMemberAlert"
            case let .changeMemberRoleAlert(role): return "changeMemberRoleAlert \(role.rawValue)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        NavigationView {
            List {
                groupMemberInfoHeader()
                    .listRowBackground(Color.clear)

                if let contactId = member.memberContactId {
                    Section {
                        openDirectChatButton(contactId)
                    }
                }

                Section("Member") {
                    infoRow("Group", groupInfo.displayName)

                    HStack {
                        if let roles = member.canChangeRoleTo(groupInfo: groupInfo) {
                            Picker("Change role", selection: $newRole) {
                                ForEach(roles) { role in
                                    Text(role.text)
                                        .foregroundStyle(.secondary)
                                }
                            }
                        } else {
                            Text("Role")
                            Spacer()
                            Text(member.memberRole.text)
                                .foregroundStyle(.secondary)
                        }
                    }
                    .onAppear { newRole = member.memberRole }
                    .onChange(of: newRole) { _ in
                        if newRole != member.memberRole {
                            alert = .changeMemberRoleAlert(role: newRole)
                        }
                    }

                    // TODO invited by - need to get contact by contact id
                    if let conn = member.activeConn {
                        let connLevelDesc = conn.connLevel == 0 ? NSLocalizedString("direct", comment: "connection level description") : String.localizedStringWithFormat(NSLocalizedString("indirect (%d)", comment: "connection level description"), conn.connLevel)
                        infoRow("Connection", connLevelDesc)
                    }
                }

                Section("Servers") {
                    // TODO network connection status
                    Button("Switch receiving address") {
                        
                    }
                    if let connStats = connectionStats {
                        smpServers("Receiving via", connStats.rcvServers)
                        smpServers("Sending via", connStats.sndServers)
                    }
                }

                if member.canBeRemoved(groupInfo: groupInfo) {
                    Section {
                        removeMemberButton()
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
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        .alert(item: $alert) { alertItem in
            switch(alertItem) {
            case .removeMemberAlert: return removeMemberAlert()
            case .changeMemberRoleAlert: return changeMemberRoleAlert()
            case let .error(title, error): return Alert(title: Text(title), message: Text(error))
            }
        }
    }

    func openDirectChatButton(_ contactId: Int64) -> some View {
        Button {
            var chat = chatModel.getContactChat(contactId)
            if chat == nil {
                do {
                    chat = try apiGetChat(type: .direct, id: contactId)
                    if let chat = chat {
                        // TODO it's not correct to blindly set network status to connected - we should manage network status in model / backend
                        chat.serverInfo = Chat.ServerInfo(networkStatus: .connected)
                        chatModel.addChat(chat)
                    }
                } catch let error {
                    logger.error("openDirectChatButton apiGetChat error: \(responseError(error))")
                }
            }
            if let chat = chat {
                dismissAllSheets(animated: true)
                chatModel.chatId = chat.id
            }
        } label: {
            Label("Send direct message", systemImage: "message")
        }
    }

    private func groupMemberInfoHeader() -> some View {
        VStack {
            ProfileImage(imageStr: member.image, color: Color(uiColor: .tertiarySystemFill))
                .frame(width: 192, height: 192)
                .padding(.top, 12)
                .padding()
            Text(member.displayName)
                .font(.largeTitle)
                .lineLimit(1)
                .padding(.bottom, 2)
            if member.fullName != "" && member.fullName != member.displayName {
                Text(member.fullName)
                    .font(.title2)
                    .lineLimit(2)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    func removeMemberButton() -> some View {
        Button(role: .destructive) {
            alert = .removeMemberAlert
        } label: {
            Label("Remove member", systemImage: "trash")
                .foregroundColor(Color.red)
        }
    }

    private func removeMemberAlert() -> Alert {
        Alert(
            title: Text("Remove member?"),
            message: Text("Member will be removed from group - this cannot be undone!"),
            primaryButton: .destructive(Text("Remove")) {
                Task {
                    do {
                        let member = try await apiRemoveMember(groupInfo.groupId, member.groupMemberId)
                        await MainActor.run {
                            _ = chatModel.upsertGroupMember(groupInfo, member)
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

    private func changeMemberRoleAlert() -> Alert {
        Alert(
            title: Text("Change member role?"),
            message: member.memberCurrent ? Text("Member role will be changed to \"\(newRole.text)\". All group members will be notified.") : Text("Member role will be changed to \"\(newRole.text)\". The member will receive a new invitation."),
            primaryButton: .default(Text("Change")) {
                Task {
                    do {
                        let mem = try await apiMemberRole(groupInfo.groupId, member.groupMemberId, newRole)
                        await MainActor.run {
                            member = mem
                            _ = chatModel.upsertGroupMember(groupInfo, mem)
                        }
                    } catch let error {
                        newRole = member.memberRole
                        logger.error("apiMemberRole error: \(responseError(error))")
                        let a = getErrorAlert(error, "Error changing role")
                        alert = .error(title: a.title, error: a.message)
                    }
                }
            },
            secondaryButton: .cancel {
                newRole = member.memberRole
            }
        )
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupMemberInfoView(
            groupInfo: GroupInfo.sampleData,
            member: GroupMember.sampleData,
            connectionStats: Binding.constant(nil)
        )
    }
}
