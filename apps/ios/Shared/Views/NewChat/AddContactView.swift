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
    var contactConnection: PendingContactConnection? = nil
    var connReqInvitation: String

    var body: some View {
        List {
            OneTimeLinkProfileText(contactConnection: contactConnection, connReqInvitation: connReqInvitation)
                .listRowBackground(Color.clear)
                .listRowInsets(EdgeInsets(top: 0, leading: 0, bottom: 0, trailing: 0))

            Section("1-time link") {
                oneTimeLinkSection(contactConnection: contactConnection, connReqInvitation: connReqInvitation)
            }
        }
        .onAppear { chatModel.connReqInv = connReqInvitation }
    }
}

@ViewBuilder func oneTimeLinkSection(contactConnection: PendingContactConnection? = nil, connReqInvitation: String) -> some View {
    if connReqInvitation != "" {
        QRCode(uri: connReqInvitation)
    } else {
        ProgressView()
            .progressViewStyle(.circular)
            .scaleEffect(2)
            .frame(maxWidth: .infinity)
            .padding(.vertical)
    }
    shareLinkButton(connReqInvitation)
    oneTimeLinkLearnMoreButton()
}

private func shareLinkButton(_ connReqInvitation: String) -> some View {
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

struct OneTimeLinkProfileText: View {
    @EnvironmentObject private var chatModel: ChatModel
    var contactConnection: PendingContactConnection? = nil
    var connReqInvitation: String

    var body: some View {
        HStack {
            if (contactConnection?.incognito ?? chatModel.incognito) {
                Image(systemName: "theatermasks").foregroundColor(.indigo)
                Text("A random profile will be sent to your contact")
            } else {
                Image(systemName: "info.circle").foregroundColor(.secondary)
                Text("Your chat profile will be sent to your contact")
            }
        }
    }
}

struct AddContactView_Previews: PreviewProvider {
    static var previews: some View {
        AddContactView(connReqInvitation: "https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FFe5ICmvrm4wkrr6X1LTMii-lhBqLeB76%23MCowBQYDK2VuAyEAdhZZsHpuaAk3Hh1q0uNb_6hGTpuwBIrsp2z9U2T0oC0%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAcz6jJk71InuxA0bOX7OUhddfB8Ov7xwQIlIDeXBRZaOntUU4brU5Y3rBzroZBdQJi0FKdtt_D7I%3D%2CMEIwBQYDK2VvAzkA-hDvk1duBi1hlOr08VWSI-Ou4JNNSQjseY69QyKm7Kgg1zZjbpGfyBqSZ2eqys6xtoV4ZtoQUXQ%3D")
    }
}
