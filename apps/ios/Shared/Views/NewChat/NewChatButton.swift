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
        .confirmationDialog("Add contact to start a new chat", isPresented: $showAddChat, titleVisibility: .visible) {
            Button("Create link / QR code") { addContactAction() }
            Button("Paste received link") { pasteToConnect = true }
            Button("Scan QR code") { scanToConnect = true }
        }
        .sheet(isPresented: $addContact, content: {
            AddContactView(connReqInvitation: connReqInvitation)
        })
        .sheet(isPresented: $scanToConnect, content: {
            ScanToConnectView(openedSheet: $scanToConnect)
        })
        .sheet(isPresented: $pasteToConnect, content: {
            PasteToConnectView(openedSheet: $pasteToConnect)
        })
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
}

enum ConnReqType: Equatable {
    case contact
    case invitation
}

func connectViaLink(_ connectionLink: String, _ openedSheet: Binding<Bool>? = nil) {
    Task {
        do {
            let res = try await apiConnect(connReq: connectionLink)
            DispatchQueue.main.async {
                openedSheet?.wrappedValue = false
                if let connReqType = res {
                    connectionReqSentAlert(connReqType)
                }
            }
        } catch {
            logger.error("connectViaLink apiConnect error: \(responseError(error))")
            DispatchQueue.main.async {
                openedSheet?.wrappedValue = false
                connectionErrorAlert(error)
            }
        }
    }
}

func connectionErrorAlert(_ error: Error) {
    AlertManager.shared.showAlertMsg(title: "Connection error", message: "Error: \(error.localizedDescription)")
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
