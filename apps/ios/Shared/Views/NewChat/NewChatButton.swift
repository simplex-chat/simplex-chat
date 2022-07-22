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
    case createLink
    case pasteLink
    case scanQRCode
    case createGroup

    var id: NewChatAction { get { self } }
}

struct NewChatButton: View {
    @State private var showAddChat = false
    @State private var connReq: String = ""
    @State private var actionSheet: NewChatAction?

    var body: some View {
        Button { showAddChat = true } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
        .confirmationDialog("Add contact to start a new chat", isPresented: $showAddChat, titleVisibility: .visible) {
            Button("Create link / QR code") { addContactAction() }
            Button("Paste received link") { actionSheet = .pasteLink }
            Button("Scan QR code") { actionSheet = .scanQRCode }
            Button("Create group") { actionSheet = .createGroup }
        }
        .sheet(item: $actionSheet) { sheet in
            switch sheet {
            case .createLink: AddContactView(connReqInvitation: connReq)
            case .pasteLink: PasteToConnectView(openedSheet: $actionSheet)
            case .scanQRCode: ScanToConnectView(openedSheet: $actionSheet)
            case .createGroup: AddGroupView(openedSheet: $actionSheet)
            }
        }
    }

    func addContactAction() {
        do {
            connReq = try apiAddContact()
            actionSheet = .createLink
        } catch {
            DispatchQueue.global().async {
                connectionErrorAlert(error)
            }
            logger.error("NewChatButton.addContactAction apiAddContact error: \(error.localizedDescription)")
        }
    }
}

enum ConnReqType: Equatable {
    case contact
    case invitation
}

func connectViaLink(_ connectionLink: String, _ openedSheet: Binding<NewChatAction?>? = nil) {
    Task {
        do {
            let res = try await apiConnect(connReq: connectionLink)
            DispatchQueue.main.async {
                openedSheet?.wrappedValue = nil
                if let connReqType = res {
                    connectionReqSentAlert(connReqType)
                }
            }
        } catch {
            logger.error("connectViaLink apiConnect error: \(responseError(error))")
            DispatchQueue.main.async {
                openedSheet?.wrappedValue = nil
                connectionErrorAlert(error)
            }
        }
    }
}

func connectionErrorAlert(_ error: Error) {
    AlertManager.shared.showAlertMsg(title: "Connection error", message: "Error: \(responseError(error))")
}

func connectionReqSentAlert(_ type: ConnReqType) {
    AlertManager.shared.showAlertMsg(
        title: "Connection request sent!",
        message: type == .contact
            ? "You will be connected when your connection request is accepted, please wait or check later!"
            : "You will be connected when your contact's device is online, please wait or check later!"
    )
}

struct NewChatButton_Previews: PreviewProvider {
    static var previews: some View {
        NewChatButton()
    }
}
