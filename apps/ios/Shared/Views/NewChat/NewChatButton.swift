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
    @State private var connectContact = false
    @State private var createGroup = false

    var body: some View {
        Button { showAddChat = true } label: {
            Image(systemName: "person.crop.circle.badge.plus")
        }
        .confirmationDialog("Start new chat", isPresented: $showAddChat, titleVisibility: .visible) {
            Button("Add contact") { addContactAction() }
            Button("Scan QR code") { connectContact = true }
            Button("Create group") { createGroup = true }
                .disabled(true)
        }
        .sheet(isPresented: $addContact, content: {
            AddContactView(connReqInvitation: connReqInvitation)
        })
        .sheet(isPresented: $connectContact, content: {
            connectContactSheet()
        })
        .sheet(isPresented: $createGroup, content: { CreateGroupView() })
    }

    func addContactAction() {
        do {
            connReqInvitation = try apiAddContact()
            addContact = true
        } catch {
            connectionErrorAlert(error)
            logger.error("NewChatButton.addContactAction apiAddContact error: \(error.localizedDescription)")
        }
    }

    func connectContactSheet() -> some View {
        ConnectContactView(completed: { err in
            connectContact = false
            if let error = err {
                connectionErrorAlert(error)
            }
        })
    }

    func connectionErrorAlert(_ error: Error) {
        AlertManager.shared.showAlertMsg(title: "Connection error", message: error.localizedDescription)
    }
}

struct NewChatButton_Previews: PreviewProvider {
    static var previews: some View {
        NewChatButton()
    }
}
