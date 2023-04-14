//
//  SetAppPaswordView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SetAppPasscodeView: View {
    var submit: () -> Void
    var cancel: () -> Void
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var showKeychainError = false
    @State private var passcode = ""
    @State private var enteredPassword = ""
    @State private var confirming = false

    var body: some View {
        ZStack {
            if confirming {
                setPasswordView(
                    title: "Confirm Passcode",
                    submitLabel: "Confirm",
                    submitEnabled: { pwd in pwd == enteredPassword }
                ) {
                    if passcode == enteredPassword {
                        if kcAppPassword.set(passcode) {
                            enteredPassword = ""
                            passcode = ""
                            dismiss()
                            submit()
                        } else {
                            showKeychainError = true
                        }
                    }
                }
            } else {
                setPasswordView(title: "New Passcode", submitLabel: "Save") {
                    enteredPassword = passcode
                    passcode = ""
                    confirming = true
                }
            }
        }
        .alert(isPresented: $showKeychainError) {
            mkAlert(title: "KeyChain error", message: "Error saving passcode")
        }
    }

    private func setPasswordView(title: LocalizedStringKey, submitLabel: LocalizedStringKey, submitEnabled: (((String) -> Bool))? = nil, submit: @escaping () -> Void) -> some View {
        PasscodeView(passcode: $passcode, title: title, submitLabel: submitLabel, submitEnabled: submitEnabled, submit: submit) {
            dismiss()
            cancel()
        }
    }
}

struct SetAppPasscodeView_Previews: PreviewProvider {
    static var previews: some View {
        SetAppPasscodeView(submit: {}, cancel: {})
    }
}
