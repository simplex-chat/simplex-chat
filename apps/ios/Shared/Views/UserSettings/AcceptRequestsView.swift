//
//  AcceptRequestsView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 23/10/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct AcceptRequestsView: View {
    @EnvironmentObject private var m: ChatModel
    @State var contactLink: UserContactLink
    @State private var a = AutoAcceptState()
    @State private var saved = AutoAcceptState()
    @FocusState private var keyboardVisible: Bool

    var body: some View {
        List {
            Section {
                settingsRow("checkmark") {
                    Toggle("Automatically", isOn: $a.enable)
                }
                if a.enable {
                    settingsRow(
                        a.incognito ? "theatermasks.fill" : "theatermasks",
                        color: a.incognito ? .indigo : .secondary
                    ) {
                        Toggle("Incognito", isOn: $a.incognito)
                    }
                }
            } header: {
                Text("Accept requests")
            } footer: {
                saveButtons()
            }
            if a.enable {
                Section {
                    TextEditor(text: $a.welcomeText)
                        .focused($keyboardVisible)
                        .padding(.horizontal, -5)
                        .padding(.top, -8)
                        .frame(height: 90, alignment: .topLeading)
                        .frame(maxWidth: .infinity, alignment: .leading)
                } header: {
                    Text("Welcome message")
                }
            }
        }
        .onAppear {
            a = AutoAcceptState(contactLink: contactLink)
            saved = a
        }
        .onChange(of: a.enable) { _ in
            if !a.enable { a = AutoAcceptState() }
        }
    }

    @ViewBuilder private func saveButtons() -> some View {
        HStack {
            Button {
                a = saved
            } label: {
                Label("Cancel", systemImage: "arrow.counterclockwise")
            }
            Spacer()
            Button {
                Task {
                    do {
                        if let link = try await userAddressAutoAccept(a.autoAccept) {
                            contactLink = link
                            m.userAddress = link
                            saved = a
                        }
                    } catch let error {
                        logger.error("userAddressAutoAccept error: \(responseError(error))")
                    }
                }
            } label: {
                Label("Save", systemImage: "checkmark")
            }
        }
        .font(.body)
        .disabled(a == saved)
    }

    private struct AutoAcceptState: Equatable {
        var enable = false
        var incognito = false
        var welcomeText = ""

        init(enable: Bool = false, incognito: Bool = false, welcomeText: String = "") {
            self.enable = enable
            self.incognito = incognito
            self.welcomeText = welcomeText
        }

        init(contactLink: UserContactLink) {
            if let aa = contactLink.autoAccept {
                enable = true
                incognito = aa.acceptIncognito
                if let msg = aa.autoReply {
                    welcomeText = msg.text
                } else {
                    welcomeText = ""
                }
            } else {
                enable = false
                incognito = false
                welcomeText = ""
            }
        }

        var autoAccept: AutoAccept? {
            if enable {
                var autoReply: MsgContent? = nil
                let s = welcomeText.trimmingCharacters(in: .whitespacesAndNewlines)
                if s != "" { autoReply = .text(s) }
                return AutoAccept(acceptIncognito: incognito, autoReply: autoReply)
            }
            return nil
        }
    }
}

struct AcceptRequestsView_Previews: PreviewProvider {
    static var previews: some View {
        AcceptRequestsView(contactLink: UserContactLink(connReqContact: ""))
    }
}
