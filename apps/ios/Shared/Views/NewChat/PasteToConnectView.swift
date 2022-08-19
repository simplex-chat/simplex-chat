//
//  PasteToConnectView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 22/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PasteToConnectView: View {
    @EnvironmentObject var chatModel: ChatModel
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var connectionLink: String = ""

    var body: some View {
        VStack(alignment: .leading) {
            Text("Connect via link")
                .font(.title)
                .padding(.vertical)
            Text("Paste the link you received into the box below to connect with your contact.")
                .padding(.bottom)
            if (chatModel.incognito) {
                HStack {
                    Image(systemName: "theatermasks.fill").foregroundColor(.indigo)
                    Spacer().frame(width: 12)
                    Text("Random profile will be sent to the contact that you received this link from")
                }
                .padding(.bottom)
            } else {
                Text("Your profile will be sent to the contact that you received this link from")
                    .padding(.bottom)
            }
            TextEditor(text: $connectionLink)
                .onSubmit(connect)
                .textInputAutocapitalization(.never)
                .disableAutocorrection(true)
                .allowsTightening(false)
                .frame(height: 180)
                .overlay(
                    RoundedRectangle(cornerRadius: 10)
                        .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                )

            HStack(spacing: 20) {
                if connectionLink == "" {
                    Button {
                        connectionLink = UIPasteboard.general.string ?? ""
                    } label: {
                        Label("Paste", systemImage: "doc.plaintext")
                    }
                } else {
                    Button {
                        connectionLink = ""
                    } label: {
                        Label("Clear", systemImage: "multiply")
                    }

                }
                Spacer()
                Button(action: connect, label: {
                    Label("Connect", systemImage: "link")
                })
                .disabled(connectionLink == "" || connectionLink.trimmingCharacters(in: .whitespaces).firstIndex(of: " ") != nil)
            }
            .frame(height: 48)
            .padding(.bottom)

            Text("You can also connect by clicking the link. If it opens in the browser, click **Open in mobile app** button")
        }
        .padding()
        .frame(maxHeight: .infinity, alignment: .top)
    }

    private func connect() {
        connectViaLink(connectionLink.trimmingCharacters(in: .whitespaces), dismiss)
    }
}

struct PasteToConnectView_Previews: PreviewProvider {
    static var previews: some View {
        PasteToConnectView()
    }
}
