//
//  NewChatButton2.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 28.11.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

enum NewChatSheet: Identifiable {
    case newChat(link: String, connection: PendingContactConnection)

    var id: String {
        switch self {
        case let .newChat(link, _): return "newChat \(link)"
        }
    }
}

struct NewChatButton2: View {
    @State private var actionSheet: NewChatSheet?

    var body: some View {
        Button {
            addContactAction()
        } label: {
            Image(systemName: "square.and.pencil")
                .resizable()
                .scaledToFit()
                .frame(width: 24, height: 24)
        }
        .sheet(item: $actionSheet) { sheet in
            switch sheet {
            case let .newChat(link, pcc):
                NewChatView(selection: .invite, connReqInvitation: link, contactConnection: pcc)
            }
        }
    }

    func addContactAction() {
        Task {
            if let (connReq, pcc) = await apiAddContact(incognito: incognitoGroupDefault.get()) {
                actionSheet = .newChat(link: connReq, connection: pcc)
            }
        }
    }
}

//#Preview {
//    NewChatButton2()
//}
