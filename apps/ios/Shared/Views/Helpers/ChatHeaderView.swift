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
    @State private var inviteContact = false
    @State private var scanQRCode = false
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
                Button("Invite contact") { inviteContact = true }
                Button("Scan QR code") { scanQRCode = true }
                Button("Create group") { createGroup = true }
            }
            .sheet(isPresented: $inviteContact, content: { InviteContactView() })
            .sheet(isPresented: $scanQRCode, content: { ScanQRCodeView() })
            .sheet(isPresented: $createGroup, content: { CreateGroupView() })
        }
        .padding(.horizontal)
        .padding(.top)
    }
}

struct ChatHeaderView_Previews: PreviewProvider {
    static var previews: some View {
        ChatHeaderView()
    }
}
