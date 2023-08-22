//
//  AddContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CoreImage.CIFilterBuiltins
import SimpleXChat

struct AddContactView: View {
    @EnvironmentObject private var chatModel: ChatModel
    @Binding var contactConnection: PendingContactConnection?
    var connReqInvitation: String
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        VStack {
            List {
                Section {
                    if connReqInvitation != "" {
                        QRCode(uri: connReqInvitation)
                    } else {
                        ProgressView()
                            .progressViewStyle(.circular)
                            .scaleEffect(2)
                            .frame(maxWidth: .infinity)
                            .padding(.vertical)
                    }
                    IncognitoToggle(incognitoEnabled: $incognitoDefault)
                        .disabled(contactConnection == nil)
                    shareLinkButton(connReqInvitation)
                    oneTimeLinkLearnMoreButton()
                } header: {
                    Text("1-time link")
                } footer: {
                    sharedProfileInfo(incognitoDefault)
                }
            }
        }
        .onAppear { chatModel.connReqInv = connReqInvitation }
        .onChange(of: incognitoDefault) { incognito in
            Task {
                do {
                    if let contactConn = contactConnection,
                       let conn = try await apiSetConnectionIncognito(connId: contactConn.pccConnId, incognito: incognito) {
                        await MainActor.run {
                            contactConnection = conn
                            ChatModel.shared.updateContactConnection(conn)
                        }
                    }
                } catch {
                    logger.error("apiSetConnectionIncognito error: \(responseError(error))")
                }
            }
        }
    }
}

struct IncognitoToggle: View {
    @Binding var incognitoEnabled: Bool
    @State private var showIncognitoSheet = false

    var body: some View {
        ZStack(alignment: .leading) {
            Image(systemName: incognitoEnabled ? "theatermasks.fill" : "theatermasks")
                .frame(maxWidth: 24, maxHeight: 24, alignment: .center)
                .foregroundColor(incognitoEnabled ? Color.indigo : .secondary)
                .font(.system(size: 14))
            Toggle(isOn: $incognitoEnabled) {
                HStack(spacing: 6) {
                    Text("Incognito")
                    Image(systemName: "info.circle")
                        .foregroundColor(.accentColor)
                        .font(.system(size: 14))
                }
                .onTapGesture {
                    showIncognitoSheet = true
                }
            }
            .padding(.leading, 36)
        }
        .sheet(isPresented: $showIncognitoSheet) {
            IncognitoHelp()
        }
    }
}

func sharedProfileInfo(_ incognito: Bool) -> Text {
    let name = ChatModel.shared.currentUser?.displayName ?? ""
    return Text(
        incognito
        ? "A new random profile will be shared."
        : "Your profile **\(name)** will be shared."
    )
}

func shareLinkButton(_ connReqInvitation: String) -> some View {
    Button {
        showShareSheet(items: [connReqInvitation])
    } label: {
        settingsRow("square.and.arrow.up") {
            Text("Share 1-time link")
        }
    }
}

func oneTimeLinkLearnMoreButton() -> some View {
    NavigationLink {
        AddContactLearnMore()
            .navigationTitle("One-time invitation link")
            .navigationBarTitleDisplayMode(.large)
    } label: {
        settingsRow("info.circle") {
            Text("Learn more")
        }
    }
}

struct AddContactView_Previews: PreviewProvider {
    static var previews: some View {
        AddContactView(
            contactConnection: Binding.constant(PendingContactConnection.getSampleData()),
            connReqInvitation: "https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FFe5ICmvrm4wkrr6X1LTMii-lhBqLeB76%23MCowBQYDK2VuAyEAdhZZsHpuaAk3Hh1q0uNb_6hGTpuwBIrsp2z9U2T0oC0%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAcz6jJk71InuxA0bOX7OUhddfB8Ov7xwQIlIDeXBRZaOntUU4brU5Y3rBzroZBdQJi0FKdtt_D7I%3D%2CMEIwBQYDK2VvAzkA-hDvk1duBi1hlOr08VWSI-Ou4JNNSQjseY69QyKm7Kgg1zZjbpGfyBqSZ2eqys6xtoV4ZtoQUXQ%3D"
        )
    }
}
