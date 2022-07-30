//
//  MakeConnection.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 07/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct MakeConnection: View {
    @EnvironmentObject var m: ChatModel
    @State private var connReq: String = ""
    @State private var actionSheet: NewChatAction?

    var body: some View {
        VStack(alignment: .leading) {
            HStack {
                SettingsButton()
                if m.chatRunning == false {
                    Spacer()
                    chatStoppedIcon()
                }
            }
            .padding(.bottom, 1)

            if let user = m.currentUser {
                Text("Welcome \(user.displayName)!")
                    .font(.largeTitle)
                    .multilineTextAlignment(.leading)
                    .padding(.bottom, 8)
            } else {
                Text("Make a private connection")
                    .font(.largeTitle)
                    .padding(.bottom)
            }

            ScrollView {
                VStack(alignment: .leading) {
                    Text("To make your first private connection, choose **one of the following**:")
                        .padding(.bottom)

                    actionRow(
                        icon: "link.badge.plus",
                        title: "Create 1-time link / QR code",
                        text: "It's secure to share - only one contact can use it."
                    ) { addContactAction() }

                    actionRow(
                        icon: "doc.plaintext",
                        title: "Paste the link you received",
                        text: "Or open the link in the browser and tap **Open in mobile**."
                    ) { actionSheet = .pasteLink }

                    actionRow(
                        icon: "qrcode.viewfinder",
                        title: "Scan contact's QR code",
                        text: "In person or via a video call – the most secure way to connect."
                    ) { actionSheet = .scanQRCode }

                    Text("or")
                        .padding(.bottom)
                        .frame(maxWidth: .infinity)

                    actionRow(
                        icon: "number",
                        title: "Connect with the developers",
                        text: "To ask any questions and to receive SimpleX Chat updates."
                    ) {
                        DispatchQueue.main.async {
                            UIApplication.shared.open(simplexTeamURL)
                        }
                    }
                }
                .disabled(m.chatRunning != true)
            }

            Spacer()

            Button {
                withAnimation { m.onboardingStage = .step1_SimpleXInfo }
            } label: {
                HStack {
                    Image(systemName: "lessthan")
                    Text("About SimpleX")
                }
            }
            .padding(.bottom, 8)
            .padding(.bottom)
        }
        .sheet(item: $actionSheet) { sheet in
            switch sheet {
            case .createLink: AddContactView(connReqInvitation: connReq)
            case .pasteLink: PasteToConnectView()
            case .scanQRCode: ScanToConnectView()
            case .createGroup: EmptyView() // TODO refactor / show during onboarding?
            }
        }
        .onChange(of: actionSheet) { _ in checkOnboarding() }
        .onChange(of: m.chats.isEmpty) { _ in checkOnboarding() }
        .onChange(of: m.appOpenUrl) { _ in connectViaUrl() }
        .onAppear() { connectViaUrl() }
        .padding(.horizontal)
        .frame(maxHeight: .infinity, alignment: .top)
    }

    private func checkOnboarding() {
        if actionSheet == nil && !m.chats.isEmpty {
            withAnimation { m.onboardingStage = .onboardingComplete }
        }
    }

    private func addContactAction() {
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
        Button(action: action, label: {
            HStack(alignment: .top, spacing: 20) {
                Image(systemName: icon)
                    .resizable()
                    .scaledToFit()
                    .frame(width: 30, height: 30)
                    .padding(.leading, 4)
                    .padding(.top, 6)
                VStack(alignment: .leading) {
                    Group {
                        Text(title).font(.headline)
                        Text(text).foregroundColor(.primary)
                    }
                    .multilineTextAlignment(.leading)
                }
            }
        })
        .padding(.bottom)
    }
}

struct MakeConnection_Previews: PreviewProvider {
    static var previews: some View {
        let chatModel = ChatModel()
        chatModel.currentUser = User.sampleData
        return MakeConnection()
            .environmentObject(chatModel)
    }
}
