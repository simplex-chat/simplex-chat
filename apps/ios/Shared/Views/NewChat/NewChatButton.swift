//
//  NewChatButton.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum NewChatAction: Identifiable {
    case createLink(link: String, connection: PendingContactConnection)
    case connectViaLink
    case createGroup

    var id: String {
        switch self {
        case let .createLink(link, _): return "createLink \(link)"
        case .connectViaLink: return "connectViaLink"
        case .createGroup: return "createGroup"
        }
    }
}

struct NewChatButton: View {
    @Binding var showAddChat: Bool
    @State private var actionSheet: NewChatAction?

    var body: some View {
        Button { showAddChat = true } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .confirmationDialog("Start a new chat", isPresented: $showAddChat, titleVisibility: .visible) {
            Button("Share one-time invitation link") { addContactAction() }
            Button("Connect via link / QR code") { actionSheet = .connectViaLink }
            Button("Create secret group") { actionSheet = .createGroup }
        }
        .sheet(item: $actionSheet) { sheet in
            switch sheet {
            case let .createLink(link, pcc):
                CreateLinkView(selection: .oneTime, connReqInvitation: link, contactConnection: pcc)
            case .connectViaLink: ConnectViaLinkView()
            case .createGroup: AddGroupView()
            }
        }
    }

    func addContactAction() {
        Task {
            let (r, _) = await apiAddContact(incognito: incognitoGroupDefault.get())
            if let (connReq, pcc) = r {
                await MainActor.run {
                    ChatModel.shared.updateContactConnection(pcc)
                }
                actionSheet = .createLink(link: connReq, connection: pcc)
            }
        }
    }
}

enum PlanAndConnectAlert: Identifiable {
    case ownInvitationLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case invitationLinkConnecting(connectionLink: String)
    case ownContactAddressConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case contactAddressConnectingConfirmReconnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnectingConfirmReconnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case groupLinkConnecting(connectionLink: String, groupInfo: GroupInfo?)

    var id: String {
        switch self {
        case let .ownInvitationLinkConfirmConnect(connectionLink, _, _): return "ownInvitationLinkConfirmConnect \(connectionLink)"
        case let .invitationLinkConnecting(connectionLink): return "invitationLinkConnecting \(connectionLink)"
        case let .ownContactAddressConfirmConnect(connectionLink, _, _): return "ownContactAddressConfirmConnect \(connectionLink)"
        case let .contactAddressConnectingConfirmReconnect(connectionLink, _, _): return "contactAddressConnectingConfirmReconnect \(connectionLink)"
        case let .groupLinkConfirmConnect(connectionLink, _, _): return "groupLinkConfirmConnect \(connectionLink)"
        case let .groupLinkConnectingConfirmReconnect(connectionLink, _, _): return "groupLinkConnectingConfirmReconnect \(connectionLink)"
        case let .groupLinkConnecting(connectionLink, _): return "groupLinkConnecting \(connectionLink)"
        }
    }
}

func planAndConnectAlert(_ alert: PlanAndConnectAlert, dismiss: Bool, onCancel: (() -> Void)? = nil) -> Alert {
    switch alert {
    case let .ownInvitationLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own one-time link!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel() { onCancel?() }
        )
    case .invitationLinkConnecting:
        return Alert(
            title: Text("Already connecting!"),
            message: Text("You are already connecting via this one-time link!")
        )
    case let .ownContactAddressConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Connect to yourself?"),
            message: Text("This is your own SimpleX address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel() { onCancel?() }
        )
    case let .contactAddressConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat connection request?"),
            message: Text("You have already requested connection via this address!"),
            primaryButton: .destructive(
                Text(incognito ? "Connect incognito" : "Connect"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel() { onCancel?() }
        )
    case let .groupLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Join group?"),
            message: Text("You will connect to all group members."),
            primaryButton: .default(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel() { onCancel?() }
        )
    case let .groupLinkConnectingConfirmReconnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text("Repeat join request?"),
            message: Text("You are already joining the group via this link!"),
            primaryButton: .destructive(
                Text(incognito ? "Join incognito" : "Join"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel() { onCancel?() }
        )
    case let .groupLinkConnecting(_, groupInfo):
        if let groupInfo = groupInfo {
            return Alert(
                title: Text("Group already exists!"),
                message: Text("You are already joining the group \(groupInfo.displayName).")
            )
        } else {
            return Alert(
                title: Text("Already joining the group!"),
                message: Text("You are already joining the group via this link.")
            )
        }
    }
}

enum PlanAndConnectActionSheet: Identifiable {
    case askCurrentOrIncognitoProfile(connectionLink: String, connectionPlan: ConnectionPlan?, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileDestructive(connectionLink: String, connectionPlan: ConnectionPlan, title: LocalizedStringKey)
    case askCurrentOrIncognitoProfileConnectContactViaAddress(contact: Contact)
    case ownGroupLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool?, groupInfo: GroupInfo)

    var id: String {
        switch self {
        case let .askCurrentOrIncognitoProfile(connectionLink, _, _): return "askCurrentOrIncognitoProfile \(connectionLink)"
        case let .askCurrentOrIncognitoProfileDestructive(connectionLink, _, _): return "askCurrentOrIncognitoProfileDestructive \(connectionLink)"
        case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact): return "askCurrentOrIncognitoProfileConnectContactViaAddress \(contact.contactId)"
        case let .ownGroupLinkConfirmConnect(connectionLink, _, _, _): return "ownGroupLinkConfirmConnect \(connectionLink)"
        }
    }
}

func planAndConnectActionSheet(_ sheet: PlanAndConnectActionSheet, dismiss: Bool, onCancel: (() -> Void)? = nil) -> ActionSheet {
    switch sheet {
    case let .askCurrentOrIncognitoProfile(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .default(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false) },
                .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true) },
                .cancel() { onCancel?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileDestructive(connectionLink, connectionPlan, title):
        return ActionSheet(
            title: Text(title),
            buttons: [
                .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false) },
                .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true) },
                .cancel() { onCancel?() }
            ]
        )
    case let .askCurrentOrIncognitoProfileConnectContactViaAddress(contact):
        return ActionSheet(
            title: Text("Connect with \(contact.chatViewName)"),
            buttons: [
                .default(Text("Use current profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: false) },
                .default(Text("Use new incognito profile")) { connectContactViaAddress_(contact, dismiss: dismiss, incognito: true) },
                .cancel() { onCancel?() }
            ]
        )
    case let .ownGroupLinkConfirmConnect(connectionLink, connectionPlan, incognito, groupInfo):
        if let incognito = incognito {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text(incognito ? "Join incognito" : "Join with current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) },
                    .cancel() { onCancel?() }
                ]
            )
        } else {
            return ActionSheet(
                title: Text("Join your group?\nThis is your link for group \(groupInfo.displayName)!"),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss, showAlreadyExistsAlert: nil) },
                    .destructive(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false) },
                    .destructive(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true) },
                    .cancel() { onCancel?() }
                ]
            )
        }
    }
}

func planAndConnect(
    _ connectionLink: String,
    showAlert: @escaping (PlanAndConnectAlert) -> Void,
    showActionSheet: @escaping (PlanAndConnectActionSheet) -> Void,
    dismiss: Bool,
    incognito: Bool?
) {
    Task {
        do {
            let connectionPlan = try await apiConnectPlan(connReq: connectionLink)
            switch connectionPlan {
            case let .invitationLink(ilp):
                switch ilp {
                case .ok:
                    logger.debug("planAndConnect, .invitationLink, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via one-time link"))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .invitationLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownInvitationLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own one-time link!"))
                    }
                case let .connecting(contact_):
                    logger.debug("planAndConnect, .invitationLink, .connecting, incognito=\(incognito?.description ?? "nil")")
                    if let contact = contact_ {
                        openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                    } else {
                        showAlert(.invitationLinkConnecting(connectionLink: connectionLink))
                    }
                case let .known(contact):
                    logger.debug("planAndConnect, .invitationLink, .known, incognito=\(incognito?.description ?? "nil")")
                    openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                }
            case let .contactAddress(cap):
                switch cap {
                case .ok:
                    logger.debug("planAndConnect, .contactAddress, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect via contact address"))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .contactAddress, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownContactAddressConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Connect to yourself?\nThis is your own SimpleX address!"))
                    }
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .contactAddress, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.contactAddressConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You have already requested connection!\nRepeat connection request?"))
                    }
                case let .connectingProhibit(contact):
                    logger.debug("planAndConnect, .contactAddress, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyConnectingAlert(contact)) }
                case let .known(contact):
                    logger.debug("planAndConnect, .contactAddress, .known, incognito=\(incognito?.description ?? "nil")")
                    openKnownContact(contact, dismiss: dismiss) { AlertManager.shared.showAlert(contactAlreadyExistsAlert(contact)) }
                case let .contactViaAddress(contact):
                    logger.debug("planAndConnect, .contactAddress, .contactViaAddress, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectContactViaAddress_(contact, dismiss: dismiss, incognito: incognito)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileConnectContactViaAddress(contact: contact))
                    }
                }
            case let .groupLink(glp):
                switch glp {
                case .ok:
                    if let incognito = incognito {
                        showAlert(.groupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "Join group"))
                    }
                case let .ownLink(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    showActionSheet(.ownGroupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito, groupInfo: groupInfo))
                case .connectingConfirmReconnect:
                    logger.debug("planAndConnect, .groupLink, .connectingConfirmReconnect, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.groupLinkConnectingConfirmReconnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfileDestructive(connectionLink: connectionLink, connectionPlan: connectionPlan, title: "You are already joining the group!\nRepeat join request?"))
                    }
                case let .connectingProhibit(groupInfo_):
                    logger.debug("planAndConnect, .groupLink, .connectingProhibit, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.groupLinkConnecting(connectionLink: connectionLink, groupInfo: groupInfo_))
                case let .known(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .known, incognito=\(incognito?.description ?? "nil")")
                    openKnownGroup(groupInfo, dismiss: dismiss) { AlertManager.shared.showAlert(groupAlreadyExistsAlert(groupInfo)) }
                }
            }
        } catch {
            logger.debug("planAndConnect, plan error")
            if let incognito = incognito {
                connectViaLink(connectionLink, connectionPlan: nil, dismiss: dismiss, incognito: incognito)
            } else {
                showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: nil, title: "Connect via link"))
            }
        }
    }
}

private func connectContactViaAddress_(_ contact: Contact, dismiss: Bool, incognito: Bool) {
    Task {
        if dismiss {
            DispatchQueue.main.async {
                dismissAllSheets(animated: true)
            }
        }
        _ = await connectContactViaAddress(contact.contactId, incognito)
    }
}

private func connectViaLink(_ connectionLink: String, connectionPlan: ConnectionPlan?, dismiss: Bool, incognito: Bool) {
    Task {
        if let (connReqType, pcc) = await apiConnect(incognito: incognito, connReq: connectionLink) {
            await MainActor.run {
                ChatModel.shared.updateContactConnection(pcc)
            }
            let crt: ConnReqType
            if let plan = connectionPlan {
                crt = planToConnReqType(plan)
            } else {
                crt = connReqType
            }
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        AlertManager.shared.showAlert(connReqSentAlert(crt))
                    }
                } else {
                    AlertManager.shared.showAlert(connReqSentAlert(crt))
                }
            }
        } else {
            if dismiss {
                DispatchQueue.main.async {
                    dismissAllSheets(animated: true)
                }
            }
        }
    }
}

func openKnownContact(_ contact: Contact, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    Task {
        let m = ChatModel.shared
        if let c = m.getContactChat(contact.contactId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = c.id
                        showAlreadyExistsAlert?()
                    }
                } else {
                    m.chatId = c.id
                    showAlreadyExistsAlert?()
                }
            }
        }
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: Bool, showAlreadyExistsAlert: (() -> Void)?) {
    Task {
        let m = ChatModel.shared
        if let g = m.getGroupChat(groupInfo.groupId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = g.id
                        showAlreadyExistsAlert?()
                    }
                } else {
                    m.chatId = g.id
                    showAlreadyExistsAlert?()
                }
            }
        }
    }
}

func contactAlreadyConnectingAlert(_ contact: Contact) -> Alert {
    mkAlert(
        title: "Contact already exists",
        message: "You are already connecting to \(contact.displayName)."
    )
}

func groupAlreadyExistsAlert(_ groupInfo: GroupInfo) -> Alert {
    mkAlert(
        title: "Group already exists",
        message: "You are already in group \(groupInfo.displayName)."
    )
}

enum ConnReqType: Equatable {
    case invitation
    case contact
    case groupLink

    var connReqSentText: LocalizedStringKey {
        switch self {
        case .invitation: return "You will be connected when your contact's device is online, please wait or check later!"
        case .contact: return "You will be connected when your connection request is accepted, please wait or check later!"
        case .groupLink: return "You will be connected when group link host's device is online, please wait or check later!"
        }
    }
}

private func planToConnReqType(_ connectionPlan: ConnectionPlan) -> ConnReqType {
    switch connectionPlan {
    case .invitationLink: return .invitation
    case .contactAddress: return .contact
    case .groupLink: return .groupLink
    }
}

func connReqSentAlert(_ type: ConnReqType) -> Alert {
    return mkAlert(
        title: "Connection request sent!",
        message: type.connReqSentText
    )
}

struct NewChatButton_Previews: PreviewProvider {
    static var previews: some View {
        NewChatButton(showAddChat: Binding.constant(false))
    }
}
