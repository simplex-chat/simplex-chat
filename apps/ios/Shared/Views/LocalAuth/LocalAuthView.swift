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
        PasscodeView(passcode: $password, title: authRequest.title ?? "Enter Passcode", reason: authRequest.reason, submitLabel: "Submit") {
            let r: LAResult = password == authRequest.password
                            ? .success
                            : .failed(authError: NSLocalizedString("Incorrect passcode", comment: "PIN entry"))
            m.laRequest = nil
            authRequest.completed(r)
        } cancel: {
            m.laRequest = nil
            authRequest.completed(.failed(authError: NSLocalizedString("Authentication cancelled", comment: "PIN entry")))
        }
    }
}

struct LocalAuthView_Previews: PreviewProvider {
    static var previews: some View {
        LocalAuthView(authRequest: LocalAuthRequest.sample)
    }
}
