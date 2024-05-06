//
//  NewChatMenuButton.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

enum NewChatMenuOption: Identifiable {
    case newContact
    case scanPaste
    case newGroup

    var id: Self { self }
}

struct NewChatMenuButton: View {
    @Binding var newChatMenuOption: NewChatMenuOption?

    var body: some View {
        Menu {
            Button {
                newChatMenuOption = .newContact
            } label: {
                Text("Add contact")
            }
            Button {
                newChatMenuOption = .scanPaste
            } label: {
                Text("Scan / Paste link")
            }
            Button {
                newChatMenuOption = .newGroup
            } label: {
                Text("Create group")
            }
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(item: $newChatMenuOption) { opt in
            switch opt {
            case .newContact: NewChatView(selection: .invite)
            case .scanPaste: NewChatView(selection: .connect, showQRCodeScanner: true)
            case .newGroup: AddGroupView()
            }
        }
    }
}

#Preview {
    NewChatMenuButton(
        newChatMenuOption: Binding.constant(nil)
    )
}
