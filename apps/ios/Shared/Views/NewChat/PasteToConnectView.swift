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
        ScrollView {
            VStack(alignment: .leading) {
                Text("Connect via link")
                    .font(.largeTitle)
                    .bold()
                    .fixedSize(horizontal: false, vertical: true)
                    .padding(.vertical)
                Text("Paste the link you received into the box below to connect with your contact.")
                    .padding(.bottom, 4)
                if (chatModel.incognito) {
                    HStack {
                        Image(systemName: "theatermasks").foregroundColor(.indigo).font(.footnote)
                        Spacer().frame(width: 8)
                        Text("A random profile will be sent to the contact that you received this link from").font(.footnote)
                    }
                    .padding(.bottom)
                } else {
                    HStack {
                        Image(systemName: "info.circle").foregroundColor(.secondary).font(.footnote)
                        Spacer().frame(width: 8)
                        Text("Your profile will be sent to the contact that you received this link from").font(.footnote)
                    }
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

                Text("You can also connect by clicking the link. If it opens in the browser, click **Open in mobile app** button.")
            }
            .padding()
            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
        }
    }

    private func connect() {
        let link = connectionLink.trimmingCharacters(in: .whitespaces)
        if let crData = parseLinkQueryData(link),
           checkCRDataGroup(crData) {
            dismiss()
            AlertManager.shared.showAlert(groupLinkAlert(link))
        } else {
            connectViaLink(link, dismiss)
        }
    }
}

struct PasteToConnectView_Previews: PreviewProvider {
    static var previews: some View {
        PasteToConnectView()
    }
}
