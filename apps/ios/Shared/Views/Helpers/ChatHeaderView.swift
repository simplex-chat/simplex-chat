//
//  ChatHeaderView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct ChatHeaderView: View {
    @State private var showAddChat = false
    @State private var addContact = false
    @State private var addContactAlert = false
    @State private var addContactError: Error?
    @State private var connReqInvitation: String = ""
    @State private var connectContact = false
    @State private var connectAlert = false
    @State private var connectError: Error?
    @State private var createGroup = false

    var body: some View {
        HStack {
            Button("Edit", action: {})
            Spacer()
            Text("Your chats")
            Spacer()
            Button { showAddChat = true } label: {
                Image(systemName: "square.and.pencil")
            }
            .confirmationDialog("Start new chat", isPresented: $showAddChat, titleVisibility: .visible) {
                Button("Add contact") { addContactAction() }
                Button("Scan QR code") { connectContact = true }
                Button("Create group") { createGroup = true }
            }
            .sheet(isPresented: $addContact, content: {
                AddContactView(connReqInvitation: connReqInvitation)
            })
            .alert(isPresented: $addContactAlert) {
                connectionError(addContactError)
            }
            .sheet(isPresented: $connectContact, content: {
                connectContactSheet()
            })
            .alert(isPresented: $connectAlert) {
                connectionError(connectError)
            }
            .sheet(isPresented: $createGroup, content: { CreateGroupView() })
        }
        .padding(.horizontal)
        .padding(.top)
    }

    func addContactAction() {
        do {
            connReqInvitation = try apiAddContact()
            addContact = true
        } catch {
            addContactAlert = true
            addContactError = error
            print(error)
        }
    }

    func connectContactSheet() -> some View {
        ConnectContactView(completed: { err in
            connectContact = false
            if err != nil {
                connectAlert = true
                connectError = err
            }
        })
    }

    func connectionError(_ error: Error?) -> Alert {
        Alert(
            title: Text("Connection error"),
            message: Text(error?.localizedDescription ?? "")
        )
    }
}

struct ChatHeaderView_Previews: PreviewProvider {
    static var previews: some View {
        ChatHeaderView()
    }
}
