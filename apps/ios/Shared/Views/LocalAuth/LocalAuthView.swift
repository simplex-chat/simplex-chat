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
                Text("Enter password").font(.title).bold().padding(.top, 24)
                Text(authRequest.reason)
                DigitalPasswordEntry(width: g.size.width, password: $password)
                    .padding(.bottom, 48)
                HStack(spacing: 48) {
                    Button {
                        authRequest.completed(.failed(authError: NSLocalizedString("Authentication cancelled", comment: "PIN entry")))
                    } label: {
                        Label("Cancel", systemImage: "multiply")
                    }
                    Button {
                        let r: LAResult = password == authRequest.password
                                        ? .success
                                        : .failed(authError: NSLocalizedString("Incorrect password", comment: "PIN entry"))
                        if case .success = r {
                            m.laRequest = nil
                        }
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
        .padding(.horizontal)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }
}

struct LocalAuthView_Previews: PreviewProvider {
    static var previews: some View {
        LocalAuthView(authRequest: LocalAuthRequest.sample)
    }
}
