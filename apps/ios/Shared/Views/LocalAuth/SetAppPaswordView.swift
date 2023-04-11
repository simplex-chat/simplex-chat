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
    var submit: () -> Void
    var cancel: () -> Void
    @Environment(\.dismiss) var dismiss: DismissAction
    @State private var showKeychainError = false
    @State private var password = ""
    @State private var enteredPassword = ""
    @State private var confirming = false

    var body: some View {
        ZStack {
            if confirming {
                setPasswordView(title: "Confirm password", label: "Confirm") {
                    if password == enteredPassword {
                        if kcAppPassword.set(password) {
                            enteredPassword = ""
                            password = ""
                            dismiss()
                            submit()
                        } else {
                            showKeychainError = true
                        }
                    }
                }
            } else {
                setPasswordView(title: "Set password", label: "Set password") {
                    enteredPassword = password
                    password = ""
                    confirming = true
                }
            }
        }
        .alert(isPresented: $showKeychainError) {
            mkAlert(title: "KeyChain error", message: "Error saving password")
        }
        .padding()
        .padding(.horizontal, 16)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func setPasswordView(title: LocalizedStringKey, label: LocalizedStringKey, submit: @escaping () -> Void) -> some View {
        GeometryReader { g in
            VStack {
                Text(title).font(.title).bold().padding(.top, 16)
                DigitalPasswordEntry(width: g.size.width, password: $password)
                    .padding(.bottom, 36)
                HStack(spacing: 48) {
                    Button {
                        dismiss()
                        cancel()
                    } label: {
                        Label("Cancel", systemImage: "multiply")
                    }
                    Button(action: submit) {
                        Label(label, systemImage: "checkmark")
                    }
                    .disabled(password.count < 4 || !(enteredPassword == "" || password == enteredPassword))
                }
                .font(.title2)
            }
        }
    }
}

struct SetAppPaswordView_Previews: PreviewProvider {
    static var previews: some View {
        SetAppPaswordView(submit: {}, cancel: {})
    }
}
