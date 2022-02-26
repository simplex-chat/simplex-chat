//
//  AddContactView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import CoreImage.CIFilterBuiltins

struct AddContactView: View {
    var connReqInvitation: String

    var body: some View {
        VStack {
            Text("Add contact")
                .font(.title)
                .padding(.bottom)
            Text("Show QR code to your contact\nto scan from the app")
                .font(.title2)
                .multilineTextAlignment(.center)
            QRCode(uri: connReqInvitation)
                .padding()
            (Text("If you cannot meet in person, you can ") +
             Text("scan QR code in the video call").bold() +
             Text(", or you can share the invitation link via any other channel."))
                .font(.subheadline)
                .multilineTextAlignment(.center)
                .padding(.horizontal)
            Button {
                showShareSheet(items: [connReqInvitation])
            } label: {
                Label("Share invitation link", systemImage: "square.and.arrow.up")
            }
            .padding()
        }
    }
}

struct AddContactView_Previews: PreviewProvider {
    static var previews: some View {
        AddContactView(connReqInvitation: "https://simplex.chat/invitation#/?v=1&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FFe5ICmvrm4wkrr6X1LTMii-lhBqLeB76%23MCowBQYDK2VuAyEAdhZZsHpuaAk3Hh1q0uNb_6hGTpuwBIrsp2z9U2T0oC0%3D&e2e=v%3D1%26x3dh%3DMEIwBQYDK2VvAzkAcz6jJk71InuxA0bOX7OUhddfB8Ov7xwQIlIDeXBRZaOntUU4brU5Y3rBzroZBdQJi0FKdtt_D7I%3D%2CMEIwBQYDK2VvAzkA-hDvk1duBi1hlOr08VWSI-Ou4JNNSQjseY69QyKm7Kgg1zZjbpGfyBqSZ2eqys6xtoV4ZtoQUXQ%3D")
    }
}
