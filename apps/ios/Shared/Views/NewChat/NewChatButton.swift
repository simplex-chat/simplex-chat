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

enum ConnReqType: Equatable {
    case contact
    case invitation
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

func planAndConnectAlert(_ alert: PlanAndConnectAlert, dismiss: DismissAction? = nil) -> Alert {
    switch alert {
    case let .ownLinkConfirmConnect(connectionLink, connectionPlan, incognito):
        return Alert(
            title: Text(connectionPlan.planTitle),
            message: Text("This is your own link. Confirm to connect."),
            primaryButton: .default(
                Text("Connect via own link"),
                action: { connectViaLink(connectionLink, dismiss: dismiss, incognito: incognito) }
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
                connectViaLink(connectionLink, incognito: incognito)
            },
            secondaryButton: .cancel()
        )
    case let .alreadyConnectingGroup(groupInfo, connectionPlan):
        if let groupInfo = groupInfo {
            return Alert(
                title: Text(connectionPlan.planTitle),
                message: Text("You are already connecting to \(groupInfo.displayName).")
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
            message: Text("You are already connected to \(groupInfo.displayName)."),
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

func planAndConnectActionSheet(_ sheet: PlanAndConnectActionSheet, dismiss: DismissAction? = nil) -> ActionSheet {
    switch sheet {
    case let .askCurrentOrIncognitoProfile(connectionLink, connectionPlan):
        return ActionSheet(
            title: Text(planTitle_(connectionPlan)),
            buttons: [
                .default(Text("Use current profile")) { connectViaLink(connectionLink, dismiss: dismiss, incognito: false) },
                .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, dismiss: dismiss, incognito: true) },
                .cancel()
            ]
        )
    case let .ownGroupLinkConfirmConnect(groupInfo, connectionLink, connectionPlan, incognito):
        if let incognito = incognito {
            return ActionSheet(
                title: Text(connectionPlan.planTitle),
                buttons: [
                    .default(Text("Open \(groupInfo.displayName)")) { openKnownGroup(groupInfo, dismiss: dismiss) },
                    .default(Text(incognito ? "Connect incognito" : "Connect")) { connectViaLink(connectionLink, dismiss: dismiss, incognito: incognito) },
                    .cancel()
                ]
            )
        } else {
            return ActionSheet(
                title: Text(connectionPlan.planTitle),
                buttons: [
                    .default(Text("Open \(groupInfo.displayName)")) { openKnownGroup(groupInfo, dismiss: dismiss) },
                    .default(Text("Use current profile")) { connectViaLink(connectionLink, dismiss: dismiss, incognito: false) },
                    .default(Text("Use new incognito profile")) { connectViaLink(connectionLink, dismiss: dismiss, incognito: true) },
                    .cancel()
                ]
            )
        }
    }
}

func planTitle_(_ connectionPlan: ConnectionPlan?) -> LocalizedStringKey {
    if let connectionPlan = connectionPlan {
        return connectionPlan.planTitle
    } else {
        return "Connect via link"
    }
}

func openKnownContact(_ contact: Contact, dismiss: DismissAction?) {
    Task {
        let m = ChatModel.shared
        if let c = m.getContactChat(contact.contactId) {
            dismiss?()
            await MainActor.run { m.chatId = c.id }
        }
    }
}

func openKnownGroup(_ groupInfo: GroupInfo, dismiss: DismissAction?) {
    Task {
        let m = ChatModel.shared
        if let g = m.getGroupChat(groupInfo.groupId) {
            dismiss?()
            await MainActor.run { m.chatId = g.id }
        }
    }
}

func planAndConnect(
    _ connectionLink: String,
    showAlert: @escaping (PlanAndConnectAlert) -> Void,
    showActionSheet: @escaping (PlanAndConnectActionSheet) -> Void,
    dismiss: DismissAction? = nil,
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
                        connectViaLink(connectionLink, dismiss: dismiss, incognito: incognito)
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
                        connectViaLink(connectionLink, dismiss: dismiss, incognito: incognito)
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
                connectViaLink(connectionLink, dismiss: dismiss, incognito: incognito)
            } else {
                showActionSheet(.askCurrentOrIncognitoProfile(connectionLink: connectionLink, connectionPlan: nil))
            }
        }
    }
}

// TODO replace connReqType with connection plan type?
func connectViaLink(_ connectionLink: String, dismiss: DismissAction? = nil, incognito: Bool) {
    Task {
        if let connReqType = await apiConnect(incognito: incognito, connReq: connectionLink) {
            DispatchQueue.main.async {
                dismiss?()
                AlertManager.shared.showAlert(connReqSentAlert(connReqType))
            }
        } else {
            DispatchQueue.main.async {
                dismiss?()
            }
        }
    }
}

struct CReqClientData: Decodable {
    var type: String
    var groupLinkId: String?
}

func parseLinkQueryData(_ connectionLink: String) -> CReqClientData? {
    if let hashIndex = connectionLink.firstIndex(of: "#"),
       let urlQuery = URL(string: String(connectionLink[connectionLink.index(after: hashIndex)...])),
       let components = URLComponents(url: urlQuery, resolvingAgainstBaseURL: false),
       let data = components.queryItems?.first(where: { $0.name == "data" })?.value,
       let d = data.data(using: .utf8),
       let crData = try? getJSONDecoder().decode(CReqClientData.self, from: d) {
        return crData
    } else {
        return nil
    }
}

func checkCRDataGroup(_ crData: CReqClientData) -> Bool {
    return crData.type == "group" && crData.groupLinkId != nil
}

func groupLinkAlert(_ connectionLink: String, incognito: Bool) -> Alert {
    return Alert(
        title: Text("Connect via group link?"),
        message: Text("You will join a group this link refers to and connect to its group members."),
        primaryButton: .default(Text(incognito ? "Connect incognito" : "Connect")) {
            connectViaLink(connectionLink, incognito: incognito)
        },
        secondaryButton: .cancel()
    )
}

func connReqSentAlert(_ type: ConnReqType) -> Alert {
    return mkAlert(
        title: "Connection request sent!",
        message: type == .contact
            ? "You will be connected when your connection request is accepted, please wait or check later!"
            : "You will be connected when your contact's device is online, please wait or check later!"
    )
}

struct NewChatButton_Previews: PreviewProvider {
    static var previews: some View {
        NewChatButton(showAddChat: Binding.constant(false))
    }
}
