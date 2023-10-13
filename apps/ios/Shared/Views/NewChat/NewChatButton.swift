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
            if let (connReq, pcc) = await apiAddContact(incognito: incognitoGroupDefault.get()) {
                actionSheet = .createLink(link: connReq, connection: pcc)
            }
        }
    }
}

enum PlanAndConnectAlert: Identifiable {
    case ownLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case alreadyConnectingContact(contact: Contact?, connectionPlan: ConnectionPlan)
    case alreadyConnectedContact(contact: Contact, connectionPlan: ConnectionPlan)
    case groupLinkConfirmConnect(connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool)
    case alreadyConnectingGroup(groupInfo: GroupInfo?, connectionPlan: ConnectionPlan)
    case alreadyConnectedGroup(groupInfo: GroupInfo, connectionPlan: ConnectionPlan)

    var id: String {
        switch self {
        case let .ownLinkConfirmConnect(connectionLink, _, _): return "ownLinkConfirmConnect \(connectionLink)"
        case .alreadyConnectingContact: return "alreadyConnectingContact"
        case .alreadyConnectedContact: return "alreadyConnectedContact"
        case let .groupLinkConfirmConnect(connectionLink, _, _): return "groupLinkConfirmConnect \(connectionLink)"
        case .alreadyConnectingGroup: return "alreadyConnectingGroup"
        case .alreadyConnectedGroup: return "alreadyConnectedGroup"
        }
    }
}

func planAndConnectAlert(_ alert: PlanAndConnectAlert, dismiss: Bool) -> Alert {
    switch alert {
    case let .ownLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text(connectionPlan.planTitle),
            message: Text("This is your own link. Confirm to connect."),
            primaryButton: .default(
                Text("Connect via own link"),
                action: { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) }
            ),
            secondaryButton: .cancel()
        )
    case let .alreadyConnectingContact(contact, connectionPlan):
        if let contact = contact {
            return Alert(
                title: Text(connectionPlan.planTitle),
                message: Text("You are already connecting to \(contact.displayName)."),
                primaryButton: .default(
                    Text("Open contact"),
                    action: { openKnownContact(contact, dismiss: dismiss) }
                ),
                secondaryButton: .cancel()
            )
        } else {
            return Alert(
                title: Text(connectionPlan.planTitle),
                message: Text("You are already connecting via this link.")
            )
        }
    case let .alreadyConnectedContact(contact, connectionPlan):
        return Alert(
            title: Text(connectionPlan.planTitle),
            message: Text("You are already connected to \(contact.displayName)."),
            primaryButton: .default(
                Text("Open contact"),
                action: { openKnownContact(contact, dismiss: dismiss) }
            ),
            secondaryButton: .cancel()
        )
    case let .groupLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text(connectionPlan.planTitle),
            message: Text("You will join a group this link refers to and connect to its group members."),
            primaryButton: .default(Text(incognito ? "Connect incognito" : "Connect")) {
                connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito)
            },
            secondaryButton: .cancel()
        )
    case let .alreadyConnectingGroup(groupInfo, connectionPlan):
        if let groupInfo = groupInfo {
            return Alert(
                title: Text(connectionPlan.planTitle),
                message: Text("You are already connecting to group \(groupInfo.displayName).")
            )
        } else {
            return Alert(
                title: Text(connectionPlan.planTitle),
                message: Text("You are already connecting to this group.")
            )
        }
    case let .alreadyConnectedGroup(groupInfo, connectionPlan):
        return Alert(
            title: Text(connectionPlan.planTitle),
            message: Text("You are already connected to group \(groupInfo.displayName)."),
            primaryButton: .default(
                Text("Open group"),
                action: { openKnownGroup(groupInfo, dismiss: dismiss) }
            ),
            secondaryButton: .cancel()
        )
    }
}

enum PlanAndConnectActionSheet: Identifiable {
    case askCurrentOrIncognitoProfile(connectionLink: String, connectionPlan: ConnectionPlan?)
    case ownGroupLinkConfirmConnect(groupInfo: GroupInfo, connectionLink: String, connectionPlan: ConnectionPlan, incognito: Bool?)

    var id: String {
        switch self {
        case let .askCurrentOrIncognitoProfile(connectionLink, _): return "askCurrentOrIncognitoProfile \(connectionLink)"
        case let .ownGroupLinkConfirmConnect(_, connectionLink, _, _): return "ownGroupLinkConfirmConnect \(connectionLink)"
        }
    }
}

func planAndConnectActionSheet(_ sheet: PlanAndConnectActionSheet, dismiss: Bool) -> ActionSheet {
    switch sheet {
    case let .askCurrentOrIncognitoProfile(connectionLink, connectionPlan):
        return ActionSheet(
            title: Text(planTitle_(connectionPlan)),
            buttons: [
                .default(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false) },
                .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true) },
                .cancel()
            ]
        )
    case let .ownGroupLinkConfirmConnect(groupInfo, connectionLink, connectionPlan, incognito):
        if let incognito = incognito {
            return ActionSheet(
                title: Text(connectionPlan.planTitle),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss) },
                    .default(Text(incognito ? "Connect incognito" : "Connect")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito) },
                    .cancel()
                ]
            )
        } else {
            return ActionSheet(
                title: Text(connectionPlan.planTitle),
                buttons: [
                    .default(Text("Open group")) { openKnownGroup(groupInfo, dismiss: dismiss) },
                    .default(Text("Use current profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: false) },
                    .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: true) },
                    .cancel()
                ]
            )
        }
    }
}

func planTitle_(_ connectionPlan: ConnectionPlan?) -> String {
    if let connectionPlan = connectionPlan {
        return connectionPlan.planTitle
    } else {
        return NSLocalizedString("Connect via link", comment: "connection plan title")
    }
}

func openKnownContact(_ contact: Contact, dismiss: Bool) {
    Task {
        let m = ChatModel.shared
        if let c = m.getContactChat(contact.contactId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = c.id
                    }
                } else {
                    m.chatId = c.id
                }
            }
        }
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: Bool) {
    Task {
        let m = ChatModel.shared
        if let g = m.getGroupChat(groupInfo.groupId) {
            DispatchQueue.main.async {
                if dismiss {
                    dismissAllSheets(animated: true) {
                        m.chatId = g.id
                    }
                } else {
                    m.chatId = g.id
                }
            }
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
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .invitationLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan))
                    }
                case let .connecting(contact_):
                    logger.debug("planAndConnect, .invitationLink, .connecting, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectingContact(contact: contact_, connectionPlan: connectionPlan))
                case let .known(contact):
                    logger.debug("planAndConnect, .invitationLink, .known, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectedContact(contact: contact, connectionPlan: connectionPlan))
                }
            case let .contactAddress(cap):
                switch cap {
                case .ok:
                    logger.debug("planAndConnect, .contactAddress, .ok, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        connectViaLink(connectionLink, connectionPlan: connectionPlan, dismiss: dismiss, incognito: incognito)
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan))
                    }
                case .ownLink:
                    logger.debug("planAndConnect, .contactAddress, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    if let incognito = incognito {
                        showAlert(.ownLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan))
                    }
                case let .connecting(contact):
                    logger.debug("planAndConnect, .contactAddress, .connecting, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectingContact(contact: contact, connectionPlan: connectionPlan))
                case let .known(contact):
                    logger.debug("planAndConnect, .contactAddress, .known, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectedContact(contact: contact, connectionPlan: connectionPlan))
                }
            case let .groupLink(glp):
                switch glp {
                case .ok:
                    if let incognito = incognito {
                        showAlert(.groupLinkConfirmConnect(connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                    } else {
                        showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: connectionPlan))
                    }
                case let .ownLink(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .ownLink, incognito=\(incognito?.description ?? "nil")")
                    showActionSheet(.ownGroupLinkConfirmConnect(groupInfo: groupInfo, connectionLink: connectionLink, connectionPlan: connectionPlan, incognito: incognito))
                case let .connecting(groupInfo_):
                    logger.debug("planAndConnect, .groupLink, .connecting, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectingGroup(groupInfo: groupInfo_, connectionPlan: connectionPlan))
                case let .known(groupInfo):
                    logger.debug("planAndConnect, .groupLink, .known, incognito=\(incognito?.description ?? "nil")")
                    showAlert(.alreadyConnectedGroup(groupInfo: groupInfo, connectionPlan: connectionPlan))
                }
            }
        } catch {
            logger.debug("planAndConnect, plan error")
            if let incognito = incognito {
                connectViaLink(connectionLink, connectionPlan: nil, dismiss: dismiss, incognito: incognito)
            } else {
                showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: nil))
            }
        }
    }
}

private func connectViaLink(_ connectionLink: String, connectionPlan: ConnectionPlan?, dismiss: Bool, incognito: Bool) {
    Task {
        if let connReqType = await apiConnect(incognito: incognito, connReq: connectionLink) {
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
