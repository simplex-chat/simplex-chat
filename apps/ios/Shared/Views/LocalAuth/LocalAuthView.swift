//
//  LocalAuthView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct LocalAuthView: View {
    @EnvironmentObject var m: ChatModel
    var authRequest: LocalAuthRequest
    @State private var password = ""

    var body: some View {
        GeometryReader { g in
            VStack {
                Text(authRequest.title).font(.title).bold().padding(.top, 16)
                Text(authRequest.reason)
                DigitalPasswordEntry(width: g.size.width, password: $password)
                    .padding(.bottom, 36)
                HStack(spacing: 48) {
                    Button {
                        m.laRequest = nil
                        authRequest.completed(.failed(authError: NSLocalizedString("Authentication cancelled", comment: "PIN entry")))
                    } label: {
                        Label("Cancel", systemImage: "multiply")
                    }
                    Button {
                        let r: LAResult = password == authRequest.password
                                        ? .success
                                        : .failed(authError: NSLocalizedString("Incorrect password", comment: "PIN entry"))
                        m.laRequest = nil
                        authRequest.completed(r)
                    } label: {
                        Label("Submit", systemImage: "checkmark")
                    }
                    .disabled(password.count < 4)
                }
                .font(.title2)
            }
        }
        .padding()
        .padding(.horizontal, 16)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color(uiColor: .systemBackground))
    }
}

struct LocalAuthView_Previews: PreviewProvider {
    static var previews: some View {
        LocalAuthView(authRequest: LocalAuthRequest.sample)
    }
}
