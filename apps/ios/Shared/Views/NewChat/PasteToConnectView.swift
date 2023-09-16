//
//  PasteToConnectView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 22/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct PasteToConnectView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var connectionLink: String = ""
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false
    @FocusState private var linkEditorFocused: Bool

    var body: some View {
        List {
            Text("Connect via link")
                .font(.largeTitle)
                .bold()
                .fixedSize(horizontal: false, vertical: true)
                .listRowBackground(Color.clear)
                .listRowSeparator(.hidden)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))
                .onTapGesture { linkEditorFocused = false }

            Section {
                linkEditor()

                Button {
                    if connectionLink == "" {
                        connectionLink = UIPasteboard.general.string ?? ""
                    } else {
                        connectionLink = ""
                    }
                } label: {
                    if connectionLink == "" {
                        settingsRow("doc.plaintext") { Text("Paste") }
                    } else {
                        settingsRow("multiply") { Text("Clear") }
                    }
                }
                
                Button {
                    connect()
                } label: {
                    settingsRow("link") { Text("Connect") }
                }
                .disabled(connectionLink == "" || connectionLink.trimmingCharacters(in: .whitespaces).firstIndex(of: " ") != nil)
                
                IncognitoToggle(incognitoEnabled: $incognitoDefault)
            } footer: {
                sharedProfileInfo(incognitoDefault)
                + Text(String("\n\n"))
                + Text("You can also connect by clicking the link. If it opens in the browser, click **Open in mobile app** button.")
            }
        }
    }

    private func linkEditor() -> some View {
        ZStack {
            Group {
                if connectionLink.isEmpty {
                    TextEditor(text: Binding.constant(NSLocalizedString("Paste the link you received to connect with your contact.", comment: "placeholder")))
                        .foregroundColor(.secondary)
                        .disabled(true)
                }
                TextEditor(text: $connectionLink)
                    .onSubmit(connect)
                    .textInputAutocapitalization(.never)
                    .disableAutocorrection(true)
                    .focused($linkEditorFocused)
            }
            .allowsTightening(false)
            .padding(.horizontal, -5)
            .padding(.top, -8)
            .frame(height: 180, alignment: .topLeading)
            .frame(maxWidth: .infinity, alignment: .leading)
        }
    }

    private func connect() {
        let link = connectionLink.trimmingCharacters(in: .whitespaces)
        if let crData = parseLinkQueryData(link),
           checkCRDataGroup(crData) {
            dismiss()
            AlertManager.shared.showAlert(groupLinkAlert(link, incognito: incognitoDefault))
        } else {
            connectViaLink(link, dismiss: dismiss, incognito: incognitoDefault)
        }
    }
}

struct PasteToConnectView_Previews: PreviewProvider {
    static var previews: some View {
        PasteToConnectView()
    }
}
