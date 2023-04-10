//
//  LocalAuthView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct LocalAuthView: View {
    var authRequest: LocalAuthRequest

    var body: some View {
        VStack {
            Text("Enter password")
            Text(authRequest.reason)
            DigitalPasswordEntry { pwd in
                let r: LAResult =
                    pwd == authRequest.password
                    ? .success
                    : .failed(authError: NSLocalizedString("Incorrect password", comment: "PIN entry"))
                authRequest.completed(r)
            }
        }
    }
}

struct LocalAuthView_Previews: PreviewProvider {
    static var previews: some View {
        LocalAuthView(authRequest: LocalAuthRequest.sample)
    }
}
