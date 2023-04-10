//
//  SetAppPaswordView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SetAppPaswordView: View {
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var showKeychainError = false

    var body: some View {
        VStack {
            Text("Enter password")
            DigitalPasswordEntry { pwd in
                if kcAppPassword.set(pwd) {
                    dismiss()
                } else {
                    showKeychainError = true
                }
            }
        }
        .alert(isPresented: $showKeychainError) {
            mkAlert(title: "KeyChain error", message: "Error saving password")
        }
    }
}

struct SetAppPaswordView_Previews: PreviewProvider {
    static var previews: some View {
        SetAppPaswordView()
    }
}
