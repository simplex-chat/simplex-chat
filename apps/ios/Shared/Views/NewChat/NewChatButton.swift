//
//  NewChatButton.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 31/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct NewChatButton: View {
    @State private var showAddChat = false
    @State private var addContact = false
    @State private var connReqInvitation: String = ""
    @State private var scanToConnect = false
    @State private var pasteToConnect = false

    var body: some View {
        Button { showAddChat = true } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
        .confirmationDialog("Start new chat", isPresented: $showAddChat, titleVisibility: .visible) {
            Button("Add contact") { addContactAction() }
            Button("Scan QR code") { scanToConnect = true }
            Button("Paste link to connect") { pasteToConnect = true }
        }
        .sheet(isPresented: $addContact, content: {
            AddContactView(connReqInvitation: connReqInvitation)
        })
        .sheet(isPresented: $scanToConnect, content: {
            scanToConnectSheet()
        })
        .sheet(isPresented: $pasteToConnect, content: { pasteToConnectSheet() })
    }

    func addContactAction() {
        do {
            connReqInvitation = try apiAddContact()
            addContact = true
        } catch {
            DispatchQueue.global().async {
                connectionErrorAlert(error)
            }
            logger.error("NewChatButton.addContactAction apiAddContact error: \(error.localizedDescription)")
        }
    }
    
    func addContactSheet() -> some View {
        AddContactView(connReqInvitation: connReqInvitation)
    }

    private func onCompletedConnectionAttempt(_ result: Result<Bool, Error>) {
        DispatchQueue.global().async {
            switch (result) {
            case let .success(ok):
                if ok { connectionReqSentAlert(.invitation) }
            case let .failure(error):
                connectionErrorAlert(error)
            }
        }
    }

    func scanToConnectSheet() -> some View {
        ScanQRToContactView(
            completed: { r in
                pasteToConnect = false
                onCompletedConnectionAttempt(r)
            }
        )
    }

    func pasteToConnectSheet() -> some View {
        PasteToConnectView(
            completed: { r in
                scanToConnect = false
                onCompletedConnectionAttempt(r)
            }
        )
    }

    func connectionErrorAlert(_ error: Error) {
        AlertManager.shared.showAlertMsg(title: "Connection error", message: "Error: \(error.localizedDescription)")
    }
}

enum ConnReqType: Equatable {
    case contact
    case invitation
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
