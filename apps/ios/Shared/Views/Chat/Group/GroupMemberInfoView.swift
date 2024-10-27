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
    @EnvironmentObject var theme: AppTheme
    @Environment(\.dismiss) var dismiss: DismissAction
    @State var groupInfo: GroupInfo
    @ObservedObject var groupMember: GMember
    var navigation: Bool = false
    @State private var connectionStats: ConnectionStats? = nil
    @State private var connectionCode: String? = nil
    @State private var connectionLoaded: Bool = false
    @State private var newRole: GroupMemberRole = .member
    @State private var alert: GroupMemberInfoViewAlert?
    @State private var sheet: PlanAndConnectActionSheet?
    @AppStorage(DEFAULT_DEVELOPER_TOOLS) private var developerTools = false
    @State private var justOpened = true
    @State private var progressIndicator = false

    enum GroupMemberInfoViewAlert: Identifiable {
        case blockMemberAlert(mem: GroupMember)
        case unblockMemberAlert(mem: GroupMember)
        case blockForAllAlert(mem: GroupMember)
        case unblockForAllAlert(mem: GroupMember)
        case removeMemberAlert(mem: GroupMember)
        case changeMemberRoleAlert(mem: GroupMember, role: GroupMemberRole)
        case switchAddressAlert
        case abortSwitchAddressAlert
        case syncConnectionForceAlert
        case planAndConnectAlert(alert: PlanAndConnectAlert)
        case queueInfo(info: String)
        case someAlert(alert: SomeAlert)
        case error(title: LocalizedStringKey, error: LocalizedStringKey?)

        var id: String {
            switch self {
            case let .blockMemberAlert(mem): return "blockMemberAlert \(mem.groupMemberId)"
            case let .unblockMemberAlert(mem): return "unblockMemberAlert \(mem.groupMemberId)"
            case let .blockForAllAlert(mem): return "blockForAllAlert \(mem.groupMemberId)"
            case let .unblockForAllAlert(mem): return "unblockForAllAlert \(mem.groupMemberId)"
            case let .removeMemberAlert(mem): return "removeMemberAlert \(mem.groupMemberId)"
            case let .changeMemberRoleAlert(mem, role): return "changeMemberRoleAlert \(mem.groupMemberId) \(role.rawValue)"
            case .switchAddressAlert: return "switchAddressAlert"
            case .abortSwitchAddressAlert: return "abortSwitchAddressAlert"
            case .syncConnectionForceAlert: return "syncConnectionForceAlert"
            case let .planAndConnectAlert(alert): return "planAndConnectAlert \(alert.id)"
            case let .queueInfo(info): return "queueInfo \(info)"
            case let .someAlert(alert): return "someAlert \(alert.id)"
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

    private func knownDirectChat(_ contactId: Int64) -> (Chat, Contact)? {
        if let chat = chatModel.getContactChat(contactId),
           let contact = chat.chatInfo.contact,
           contact.directOrUsed == true {
            return (chat, contact)
        } else {
            return nil
        }
    }

    private func groupMemberInfoView() -> some View {
        ZStack {
            let member = groupMember.wrapped
            List {
                groupMemberInfoHeader(member)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .padding(.bottom, 18)

                infoActionButtons(member)
                    .padding(.horizontal)
                    .frame(maxWidth: .infinity)
                    .frame(height: infoViewActionButtonHeight)
                    .listRowBackground(Color.clear)
                    .listRowSeparator(.hidden)
                    .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

                if connectionLoaded {

                    if member.memberActive {
                        Section {
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
                                if knownDirectChat(contactId) == nil && !groupInfo.fullGroupPreferences.directMessages.on(for: groupInfo.membership) {
                                    connectViaAddressButton(contactLink)
                                }
                            } else {
                                connectViaAddressButton(contactLink)
                            }
                        } header: {
                            Text("Address")
                                .foregroundColor(theme.colors.secondary)
                        } footer: {
                            Text("You can share this address with your contacts to let them connect with **\(member.displayName)**.")
                                .foregroundColor(theme.colors.secondary)
                        }
                    }

                    Section(header: Text("Member").foregroundColor(theme.colors.secondary)) {
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
                    }

                    if let connStats = connectionStats {
                        Section(header: Text("Servers").foregroundColor(theme.colors.secondary)) {
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
                            smpServers("Receiving via", connStats.rcvQueuesInfo.map { $0.rcvServer }, theme.colors.secondary)
                            smpServers("Sending via", connStats.sndQueuesInfo.map { $0.sndServer }, theme.colors.secondary)
                        }
                    }

                    if groupInfo.membership.memberRole >= .admin {
                        adminDestructiveSection(member)
                    } else {
                        nonAdminBlockSection(member)
                    }

                    if developerTools {
                        Section(header: Text("For console").foregroundColor(theme.colors.secondary)) {
                            infoRow("Local name", member.localDisplayName)
                            infoRow("Database ID", "\(member.groupMemberId)")
                            if let conn = member.activeConn {
                                let connLevelDesc = conn.connLevel == 0 ? NSLocalizedString("direct", comment: "connection level description") : String.localizedStringWithFormat(NSLocalizedString("indirect (%d)", comment: "connection level description"), conn.connLevel)
                                infoRow("Connection", connLevelDesc)
                            }
                            Button ("Debug delivery") {
                                Task {
                                    do {
                                        let info = queueInfoText(try await apiGroupMemberQueueInfo(groupInfo.apiId, member.groupMemberId))
                                        await MainActor.run { alert = .queueInfo(info: info) }
                                    } catch let e {
                                        logger.error("apiContactQueueInfo error: \(responseError(e))")
                                        let a = getErrorAlert(e, "Error")
                                        await MainActor.run { alert = .error(title: a.title, error: a.message) }
                                    }
                                }
                            }
                        }
                    }

                }
            }
            .navigationBarHidden(true)
            .task {
                if #unavailable(iOS 16) {
                    // this condition prevents re-setting picker
                    if !justOpened { return }
                }
                justOpened = false
                newRole = member.memberRole
                do {
                    let (_, stats) = try await apiGroupMemberInfo(groupInfo.apiId, member.groupMemberId)
                    let (mem, code) = member.memberActive ? try await apiGetGroupMemberCode(groupInfo.apiId, member.groupMemberId) : (member, nil)
                    await MainActor.run {
                        _ = chatModel.upsertGroupMember(groupInfo, mem)
                        connectionStats = stats
                        connectionCode = code
                        connectionLoaded = true
                    }
                } catch let error {
                    await MainActor.run {
                        connectionLoaded = true
                    }
                    logger.error("apiGroupMemberInfo or apiGetGroupMemberCode error: \(responseError(error))")
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
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
            .alert(item: $alert) { alertItem in
                switch(alertItem) {
                case let .blockMemberAlert(mem): return blockMemberAlert(groupInfo, mem)
                case let .unblockMemberAlert(mem): return unblockMemberAlert(groupInfo, mem)
                case let .blockForAllAlert(mem): return blockForAllAlert(groupInfo, mem)
                case let .unblockForAllAlert(mem): return unblockForAllAlert(groupInfo, mem)
                case let .removeMemberAlert(mem): return removeMemberAlert(mem)
                case let .changeMemberRoleAlert(mem, _): return changeMemberRoleAlert(mem)
                case .switchAddressAlert: return switchAddressAlert(switchMemberAddress)
                case .abortSwitchAddressAlert: return abortSwitchAddressAlert(abortSwitchMemberAddress)
                case .syncConnectionForceAlert: return syncConnectionForceAlert({ syncMemberConnection(force: true) })
                case let .planAndConnectAlert(alert): return planAndConnectAlert(alert, dismiss: true)
                case let .queueInfo(info): return queueInfoAlert(info)
                case let .someAlert(a): return a.alert
                case let .error(title, error): return mkAlert(title: title, message: error)
                }
            }
            .actionSheet(item: $sheet) { s in planAndConnectActionSheet(s, dismiss: true) }

            if progressIndicator {
                ProgressView().scaleEffect(2)
            }
        }
        .modifier(ThemedBackground(grouped: true))
    }

    func infoActionButtons(_ member: GroupMember) -> some View {
        GeometryReader { g in
            let buttonWidth = g.size.width / 4
            HStack(alignment: .center, spacing: 8) {
                if let contactId = member.memberContactId, let (chat, contact) = knownDirectChat(contactId) {
                    knownDirectChatButton(chat, width: buttonWidth)
                    AudioCallButton(chat: chat, contact: contact, width: buttonWidth) { alert = .someAlert(alert: $0) }
                    VideoButton(chat: chat, contact: contact, width: buttonWidth) { alert = .someAlert(alert: $0) }
                } else if groupInfo.fullGroupPreferences.directMessages.on(for: groupInfo.membership) {
                    if let contactId = member.memberContactId {
                        newDirectChatButton(contactId, width: buttonWidth)
                    } else if member.activeConn?.peerChatVRange.isCompatibleRange(CREATE_MEMBER_CONTACT_VRANGE) ?? false {
                        createMemberContactButton(width: buttonWidth)
                    }
                    InfoViewButton(image: "phone.fill", title: "call", disabledLook: true, width: buttonWidth) { showSendMessageToEnableCallsAlert()
                    }
                    InfoViewButton(image: "video.fill", title: "video", disabledLook: true, width: buttonWidth) { showSendMessageToEnableCallsAlert()
                    }
                } else { // no known contact chat && directMessages are off
                    InfoViewButton(image: "message.fill", title: "message", disabledLook: true, width: buttonWidth) { showDirectMessagesProhibitedAlert("Can't message member")
                    }
                    InfoViewButton(image: "phone.fill", title: "call", disabledLook: true, width: buttonWidth) { showDirectMessagesProhibitedAlert("Can't call member")
                    }
                    InfoViewButton(image: "video.fill", title: "video", disabledLook: true, width: buttonWidth) { showDirectMessagesProhibitedAlert("Can't call member")
                    }
                }
            }
            .frame(maxWidth: .infinity, alignment: .center)
        }
    }

    func showSendMessageToEnableCallsAlert() {
        alert = .someAlert(alert: SomeAlert(
            alert: mkAlert(
                title: "Can't call member",
                message: "Send message to enable calls."
            ),
            id: "can't call member, send message"
        ))
    }

    func showDirectMessagesProhibitedAlert(_ title: LocalizedStringKey) {
        alert = .someAlert(alert: SomeAlert(
            alert: mkAlert(
                title: title,
                message: "Direct messages between members are prohibited in this group."
            ),
            id: "can't message member, direct messages prohibited"
        ))
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

    func knownDirectChatButton(_ chat: Chat, width: CGFloat) -> some View {
        InfoViewButton(image: "message.fill", title: "message", width: width) {
            ItemsModel.shared.loadOpenChat(chat.id) {
                dismissAllSheets(animated: true)
            }
        }
    }

    func newDirectChatButton(_ contactId: Int64, width: CGFloat) -> some View {
        InfoViewButton(image: "message.fill", title: "message", width: width) {
            Task {
                do {
                    let chat = try await apiGetChat(type: .direct, id: contactId)
                    chatModel.addChat(chat)
                    ItemsModel.shared.loadOpenChat(chat.id) {
                        dismissAllSheets(animated: true)
                    }
                } catch let error {
                    logger.error("openDirectChatButton apiGetChat error: \(responseError(error))")
                }
            }
        }
    }

    func createMemberContactButton(width: CGFloat) -> some View {
        InfoViewButton(image: "message.fill", title: "message", width: width) {
            progressIndicator = true
            Task {
                do {
                    let memberContact = try await apiCreateMemberContact(groupInfo.apiId, groupMember.groupMemberId)
                    await MainActor.run {
                        progressIndicator = false
                        chatModel.addChat(Chat(chatInfo: .direct(contact: memberContact)))
                        ItemsModel.shared.loadOpenChat(memberContact.id) {
                            dismissAllSheets(animated: true)
                        }
                        NetworkModel.shared.setContactNetworkStatus(memberContact, .connected)
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
        }
    }

    private func groupMemberInfoHeader(_ mem: GroupMember) -> some View {
        VStack {
            MemberProfileImage(mem, size: 192, color: Color(uiColor: .tertiarySystemFill))
                .padding(.top, 12)
                .padding()
            if mem.verified {
                (
                    Text(Image(systemName: "checkmark.shield"))
                        .foregroundColor(theme.colors.secondary)
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
            .modifier(ThemedBackground())
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

    @ViewBuilder private func adminDestructiveSection(_ mem: GroupMember) -> some View {
        let canBlockForAll = mem.canBlockForAll(groupInfo: groupInfo)
        let canRemove = mem.canBeRemoved(groupInfo: groupInfo)
        if canBlockForAll || canRemove {
            Section {
                if canBlockForAll {
                    if mem.blockedByAdmin {
                        unblockForAllButton(mem)
                    } else {
                        blockForAllButton(mem)
                    }
                }
                if canRemove {
                    removeMemberButton(mem)
                }
            }
        }
    }

    private func nonAdminBlockSection(_ mem: GroupMember) -> some View {
        Section {
            if mem.blockedByAdmin {
                Label("Blocked by admin", systemImage: "hand.raised")
                    .foregroundColor(theme.colors.secondary)
            } else if mem.memberSettings.showMessages {
                blockMemberButton(mem)
            } else {
                unblockMemberButton(mem)
            }
        }
    }

    private func blockForAllButton(_ mem: GroupMember) -> some View {
        Button(role: .destructive) {
            alert = .blockForAllAlert(mem: mem)
        } label: {
            Label("Block for all", systemImage: "hand.raised")
                .foregroundColor(.red)
        }
    }

    private func unblockForAllButton(_ mem: GroupMember) -> some View {
        Button {
            alert = .unblockForAllAlert(mem: mem)
        } label: {
            Label("Unblock for all", systemImage: "hand.raised.slash")
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

func MemberProfileImage(
    _ mem: GroupMember,
    size: CGFloat,
    color: Color = Color(uiColor: .tertiarySystemGroupedBackground),
    backgroundColor: Color? = nil
) -> some View {
    ProfileImage(
        imageStr: mem.image,
        size: size,
        color: color,
        backgroundColor: backgroundColor,
        blurred: mem.blocked
    )
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

func blockForAllAlert(_ gInfo: GroupInfo, _ mem: GroupMember) -> Alert {
    Alert(
        title: Text("Block member for all?"),
        message: Text("All new messages from \(mem.chatViewName) will be hidden!"),
        primaryButton: .destructive(Text("Block for all")) {
            blockMemberForAll(gInfo, mem, true)
        },
        secondaryButton: .cancel()
    )
}

func unblockForAllAlert(_ gInfo: GroupInfo, _ mem: GroupMember) -> Alert {
    Alert(
        title: Text("Unblock member for all?"),
        message: Text("Messages from \(mem.chatViewName) will be shown!"),
        primaryButton: .default(Text("Unblock for all")) {
            blockMemberForAll(gInfo, mem, false)
        },
        secondaryButton: .cancel()
    )
}

func blockMemberForAll(_ gInfo: GroupInfo, _ member: GroupMember, _ blocked: Bool) {
    Task {
        do {
            let updatedMember = try await apiBlockMemberForAll(gInfo.groupId, member.groupMemberId, blocked)
            await MainActor.run {
                _ = ChatModel.shared.upsertGroupMember(gInfo, updatedMember)
            }
        } catch let error {
            logger.error("apiBlockMemberForAll error: \(responseError(error))")
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
