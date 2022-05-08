//
//  MakeConnection.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct MakeConnection: View {
    @EnvironmentObject var chatModel: ChatModel
    @State private var connReq: String = ""
    @State private var actionSheet: NewChatAction?

    var body: some View {
        VStack(alignment: .leading) {
            Text("Make a private connection")
                .font(.largeTitle)
                .padding(.bottom)

            Text("To make your first connection:")
                .padding(.bottom)

            actionRow(
                icon: "qrcode",
                title: "Create 1-time link / QR code",
                text: "Only one contact can connect via it – it's secure to share via any channel."
            ) { addContactAction() }

            actionRow(
                icon: "link",
                title: "Paste the link you received",
                text: "Or you can open the link in the browser and tap **Open in mobile** button."
            ) { actionSheet = .pasteLink }

            actionRow(
                icon: "qrcode.viewfinder",
                title: "Scan QR code shown to you",
                text: "In person or in the video call – this is the most secure way to connect."
            ) { actionSheet = .scanQRCode }
        }
        .sheet(item: $actionSheet) { sheet in
            switch sheet {
            case .createLink: AddContactView(connReqInvitation: connReq)
            case .pasteLink: PasteToConnectView(openedSheet: $actionSheet)
            case .scanQRCode: ScanToConnectView(openedSheet: $actionSheet)
            }
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
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

    private func actionRow(icon: String, title: LocalizedStringKey, text: LocalizedStringKey, action: @escaping () -> Void) -> some View {
        HStack(alignment: .top) {
            Button(action: action, label: {
                ZStack(alignment: .center) {
                    Circle()
                        .frame(width: 60, height: 60)
                    Image(systemName: icon)
                        .resizable()
                        .scaledToFit()
                        .foregroundColor(.white)
                        .frame(width: 30, height: 30)
                }
                .padding(.trailing, 8)
                .padding(.top, 4)
            })
            VStack(alignment: .leading) {
                Button(title, action: action).font(.headline)
                Text(text).padding(.bottom)
            }
        }
    }
}

struct MakeConnection_Previews: PreviewProvider {
    static var previews: some View {
        MakeConnection()
    }
}
