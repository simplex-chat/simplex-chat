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
