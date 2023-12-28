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
    @State var groupInfo: GroupInfo
    @ObservedObject var groupMember: GMember
    var navigation: Bool = false
    @State private var connectionStats: ConnectionStats? = nil
    @State private var connectionCode: String? = nil
    @State private var newRole: GroupMemberRole = .member
    @State private var alert: GroupMemberInfoViewAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var justOpened = true
    @State private var progressIndicator = false

    enum GroupMemberInfoViewAlert: Identifiable {
        case blockMemberAlert(mem: GroupMember)
        case unblockMemberAlert(mem: GroupMember)
        case removeMemberAlert(mem: GroupMember)
        case changeMemberRoleAlert(mem: GroupMember, role: GroupMemberRole)
        case switchAddressAlert
        case abortSwitchAddressAlert
        case syncConnectionForceAlert
        case planAndConnectAlert(alert: PlanAndConnectAlert)
        case error(title: LocalizedStringKey, error: LocalizedStringKey)

        var id: String {
            switch self {
            case let .blockMemberAlert(mem): return "blockMemberAlert \(mem.groupMemberId)"
            case let .unblockMemberAlert(mem): return "unblockMemberAlert \(mem.groupMemberId)"
            case let .removeMemberAlert(mem): return "removeMemberAlert \(mem.groupMemberId)"
            case let .changeMemberRoleAlert(mem, role): return "changeMemberRoleAlert \(mem.groupMemberId) \(role.rawValue)"
            case .switchAddressAlert: return "switchAddressAlert"
            case .abortSwitchAddressAlert: return "abortSwitchAddressAlert"
            case .syncConnectionForceAlert: return "syncConnectionForceAlert"
            case let .planAndConnectAlert(alert): return "planAndConnectAlert \(alert.id)"
            case let .error(title, _): return "error \(title)"
            }
        }
    }

    var body: some View {
        if navigation {
            NavigationView { groupMemberInfoView() }
        } else {
            groupMemberInfoView()
        }
    }

    private func knownDirectChat(_ contactId: Int64) -> Chat? {
        if let chat = chatModel.getContactChat(contactId),
           chat.chatInfo.contact?.directOrUsed == true {
            return chat
        } else {
            return nil
        }
    }

    private func groupMemberInfoView() -> some View {
        ZStack {
            VStack {
                let member = groupMember.wrapped
                List {
                    groupMemberInfoHeader(member)
                        .listRowBackground(Color.clear)

                    if member.memberActive {
                        Section {
                            if let contactId = member.memberContactId, let chat = knownDirectChat(contactId) {
                                knownDirectChatButton(chat)
                            } else if groupInfo.fullGroupPreferences.directMessages.on {
                                if let contactId = member.memberContactId {
                                    newDirectChatButton(contactId)
                                } else if member.activeConn?.peerChatVRange.isCompatibleRange(CREATE_MEMBER_CONTACT_VRANGE) ?? false {
                                    createMemberContactButton()
                                }
                            }
                            if let code = connectionCode { verifyCodeButton(code) }
                            if let connStats = connectionStats,
                               connStats.ratchetSyncAllowed {
                                synchronizeConnectionButton()
                            }
                            // } else if developerTools {
                            //     synchronizeConnectionButtonForce()
                            // }
                        }
                    }

                    if let contactLink = member.contactLink {
                        Section {
                            SimpleXLinkQRCode(uri: contactLink)
                            Button {
                                showShareSheet(items: [simplexChatLink(contactLink)])
                            } label: {
                                Label("Share address", systemImage: "square.and.arrow.up")
                            }
                            if let contactId = member.memberContactId {
                                if knownDirectChat(contactId) == nil && !groupInfo.fullGroupPreferences.directMessages.on {
                                    connectViaAddressButton(contactLink)
                                }
                            } else {
                                connectViaAddressButton(contactLink)
                            }
                        } header: {
                            Text("Address")
                        } footer: {
                            Text("You can share this address with your contacts to let them connect with **\(member.displayName)**.")
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
                            .frame(height: 36)
                        } else {
                            infoRow("Role", member.memberRole.text)
                        }

                        // TODO invited by - need to get contact by contact id
                        if let conn = member.activeConn {
                            let connLevelDesc = conn.connLevel == 0 ? NSLocalizedString("direct", comment: "connection level description") : String.localizedStringWithFormat(NSLocalizedString("indirect (%d)", comment: "connection level description"), conn.connLevel)
                            infoRow("Connection", connLevelDesc)
                        }
                    }

                    if let connStats = connectionStats {
                        Section("Servers") {
                            // TODO network connection status
                            Button("Change receiving address") {
                                alert = .switchAddressAlert
                            }
                            .disabled(
                                connStats.rcvQueuesInfo.contains { $0.rcvSwitchStatus != nil }
                                || connStats.ratchetSyncSendProhibited
                            )
                            if connStats.rcvQueuesInfo.contains(where: { $0.rcvSwitchStatus != nil }) {
                                Button("Abort changing address") {
                                    alert = .abortSwitchAddressAlert
                                }
                                .disabled(
                                    connStats.rcvQueuesInfo.contains { $0.rcvSwitchStatus != nil && !$0.canAbortSwitch }
                                    || connStats.ratchetSyncSendProhibited
                                )
                            }
                            smpServers("Receiving via", connStats.rcvQueuesInfo.map { $0.rcvServer })
                            smpServers("Sending via", connStats.sndQueuesInfo.map { $0.sndServer })
                        }
                    }

                    Section {
                        if member.memberSettings.showMessages {
                            blockMemberButton(member)
                        } else {
                            unblockMemberButton(member)
                        }
                        if member.canBeRemoved(groupInfo: groupInfo) {
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
                .onAppear {
                    if #unavailable(iOS 16) {
                        // this condition prevents re-setting picker
                        if !justOpened { return }
                    }
                    justOpened = false
                    DispatchQueue.main.async {
                        newRole = member.memberRole
                        do {
                            let (_, stats) = try apiGroupMemberInfo(groupInfo.apiId, member.groupMemberId)
                            let (mem, code) = member.memberActive ? try apiGetGroupMemberCode(groupInfo.apiId, member.groupMemberId) : (member, nil)
                            _ = chatModel.upsertGroupMember(groupInfo, mem)
                            connectionStats = stats
                            connectionCode = code
                        } catch let error {
                            logger.error("apiGroupMemberInfo or apiGetGroupMemberCode error: \(responseError(error))")
                        }
                    }
                }
                .onChange(of: newRole) { newRole in
                    if newRole != member.memberRole {
                        alert = .changeMemberRoleAlert(mem: member, role: newRole)
                    }
                }
                .onChange(of: member.memberRole) { role in
                    newRole = role
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .alert(item: $alert) { alertItem in
                switch(alertItem) {
                case let .blockMemberAlert(mem): return blockMemberAlert(groupInfo, mem)
                case let .unblockMemberAlert(mem): return unblockMemberAlert(groupInfo, mem)
                case let .removeMemberAlert(mem): return removeMemberAlert(mem)
                case let .changeMemberRoleAlert(mem, _): return changeMemberRoleAlert(mem)
                case .switchAddressAlert: return switchAddressAlert(switchMemberAddress)
                case .abortSwitchAddressAlert: return abortSwitchAddressAlert(abortSwitchMemberAddress)
                case .syncConnectionForceAlert: return syncConnectionForceAlert({ syncMemberConnection(force: true) })
                case let .planAndConnectAlert(alert): return planAndConnectAlert(alert, dismiss: true)
                case let .error(title, error): return Alert(title: Text(title), message: Text(error))
                }
            }
            .actionSheet(item: $sheet) { s in planAndConnectActionSheet(s, dismiss: true) }

            if progressIndicator {
                ProgressView().scaleEffect(2)
            }
        }
    }

    func connectViaAddressButton(_ contactLink: String) -> some View {
        Button {
            planAndConnect(
                contactLink,
                showAlert: { alert = .planAndConnectAlert(alert: $0) },
                showActionSheet: { sheet = $0 },
                dismiss: true,
                incognito: nil
            )
        } label: {
            Label("Connect", systemImage: "link")
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

    func createMemberContactButton() -> some View {
        Button {
            progressIndicator = true
            Task {
                do {
                    let memberContact = try await apiCreateMemberContact(groupInfo.apiId, groupMember.groupMemberId)
                    await MainActor.run {
                        progressIndicator = false
                        chatModel.addChat(Chat(chatInfo: .direct(contact: memberContact)))
                        dismissAllSheets(animated: true)
                        chatModel.chatId = memberContact.id
                        chatModel.setContactNetworkStatus(memberContact, .connected)
                    }
                } catch let error {
                    logger.error("createMemberContactButton apiCreateMemberContact error: \(responseError(error))")
                    let a = getErrorAlert(error, "Error creating member contact")
                    await MainActor.run {
                        progressIndicator = false
                        alert = .error(title: a.title, error: a.message)
                    }
                }
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
            if mem.verified {
                (
                    Text(Image(systemName: "checkmark.shield"))
                        .foregroundColor(.secondary)
                        .font(.title2)
                    + Text(" ")
                    + Text(mem.displayName)
                        .font(.largeTitle)
                )
                .multilineTextAlignment(.center)
                .lineLimit(2)
                .padding(.bottom, 2)
            } else {
                Text(mem.displayName)
                    .font(.largeTitle)
                    .multilineTextAlignment(.center)
                    .lineLimit(2)
                    .padding(.bottom, 2)
            }
            if mem.fullName != "" && mem.fullName != mem.displayName {
                Text(mem.fullName)
                    .font(.title2)
                    .multilineTextAlignment(.center)
                    .lineLimit(4)
            }
        }
        .frame(maxWidth: .infinity, alignment: .center)
    }

    private func verifyCodeButton(_ code: String) -> some View {
        let member = groupMember.wrapped
        return NavigationLink {
            VerifyCodeView(
                displayName: member.displayName,
                connectionCode: code,
                connectionVerified: member.verified,
                verify: { code in
                    var member = groupMember.wrapped
                    if let r = apiVerifyGroupMember(member.groupId, member.groupMemberId, connectionCode: code) {
                        let (verified, existingCode) = r
                        let connCode = verified ? SecurityCode(securityCode: existingCode, verifiedAt: .now) : nil
                        connectionCode = existingCode
                        member.activeConn?.connectionCode = connCode
                        _ = chatModel.upsertGroupMember(groupInfo, member)
                        return r
                    }
                    return nil
                }
            )
            .navigationBarTitleDisplayMode(.inline)
            .navigationTitle("Security code")
        } label: {
            Label(
                member.verified ? "View security code" : "Verify security code",
                systemImage: member.verified ? "checkmark.shield" : "shield"
            )
        }
    }

    private func synchronizeConnectionButton() -> some View {
        Button {
            syncMemberConnection(force: false)
        } label: {
            Label("Fix connection", systemImage: "exclamationmark.arrow.triangle.2.circlepath")
                .foregroundColor(.orange)
        }
    }

    private func synchronizeConnectionButtonForce() -> some View {
        Button {
            alert = .syncConnectionForceAlert
        } label: {
            Label("Renegotiate encryption", systemImage: "exclamationmark.triangle")
                .foregroundColor(.red)
        }
    }

    private func blockMemberButton(_ mem: GroupMember) -> some View {
        Button(role: .destructive) {
            alert = .blockMemberAlert(mem: mem)
        } label: {
            Label("Block member", systemImage: "hand.raised")
                .foregroundColor(.red)
        }
    }

    private func unblockMemberButton(_ mem: GroupMember) -> some View {
        Button {
            alert = .unblockMemberAlert(mem: mem)
        } label: {
            Label("Unblock member", systemImage: "hand.raised.slash")
        }
    }

    private func removeMemberButton(_ mem: GroupMember) -> some View {
        Button(role: .destructive) {
            alert = .removeMemberAlert(mem: mem)
        } label: {
            Label("Remove member", systemImage: "trash")
                .foregroundColor(.red)
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
                let stats = try apiSwitchGroupMember(groupInfo.apiId, groupMember.groupMemberId)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateGroupMemberConnectionStats(groupInfo, groupMember.wrapped, stats)
                    dismiss()
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

    private func abortSwitchMemberAddress() {
        Task {
            do {
                let stats = try apiAbortSwitchGroupMember(groupInfo.apiId, groupMember.groupMemberId)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateGroupMemberConnectionStats(groupInfo, groupMember.wrapped, stats)
                }
            } catch let error {
                logger.error("abortSwitchMemberAddress apiAbortSwitchGroupMember error: \(responseError(error))")
                let a = getErrorAlert(error, "Error aborting address change")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }

    private func syncMemberConnection(force: Bool) {
        Task {
            do {
                let (mem, stats) = try apiSyncGroupMemberRatchet(groupInfo.apiId, groupMember.groupMemberId, force)
                connectionStats = stats
                await MainActor.run {
                    chatModel.updateGroupMemberConnectionStats(groupInfo, mem, stats)
                    dismiss()
                }
            } catch let error {
                logger.error("syncMemberConnection apiSyncGroupMemberRatchet error: \(responseError(error))")
                let a = getErrorAlert(error, "Error synchronizing connection")
                await MainActor.run {
                    alert = .error(title: a.title, error: a.message)
                }
            }
        }
    }
}

func blockMemberAlert(_ gInfo: GroupInfo, _ mem: GroupMember) -> Alert {
    Alert(
        title: Text("Block member?"),
        message: Text("All new messages from \(mem.chatViewName) will be hidden!"),
        primaryButton: .destructive(Text("Block")) {
            toggleShowMemberMessages(gInfo, mem, false)
        },
        secondaryButton: .cancel()
    )
}

func unblockMemberAlert(_ gInfo: GroupInfo, _ mem: GroupMember) -> Alert {
    Alert(
        title: Text("Unblock member?"),
        message: Text("Messages from \(mem.chatViewName) will be shown!"),
        primaryButton: .default(Text("Unblock")) {
            toggleShowMemberMessages(gInfo, mem, true)
        },
        secondaryButton: .cancel()
    )
}

func toggleShowMemberMessages(_ gInfo: GroupInfo, _ member: GroupMember, _ showMessages: Bool) {
    var memberSettings = member.memberSettings
    memberSettings.showMessages = showMessages
    updateMemberSettings(gInfo, member, memberSettings)
}

func updateMemberSettings(_ gInfo: GroupInfo, _ member: GroupMember, _ memberSettings: GroupMemberSettings) {
    Task {
        do {
            try await apiSetMemberSettings(gInfo.groupId, member.groupMemberId, memberSettings)
            await MainActor.run {
                var mem = member
                mem.memberSettings = memberSettings
                _ = ChatModel.shared.upsertGroupMember(gInfo, mem)
            }
        } catch let error {
            logger.error("apiSetMemberSettings error \(responseError(error))")
        }
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupMemberInfoView(
            groupInfo: GroupInfo.sampleData,
            groupMember: GMember.sampleData
        )
    }
}
