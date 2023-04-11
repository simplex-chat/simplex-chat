//
//  PasscodeView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 11/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PasscodeView: View {
    @Binding var passcode: String
    var title: LocalizedStringKey
    var reason: String? = nil
    var submitLabel: LocalizedStringKey
    var submitEnabled: ((String) -> Bool)?
    var submit: () -> Void
    var cancel: () -> Void

    var body: some View {
        GeometryReader { g in
            VStack(spacing: 8) {
                Text(title)
                    .font(.title)
                    .bold()
                    .padding(.top, 8)
                if let reason = reason {
                    Text(reason).padding(.top, 4)
                }
                PasscodeEntry(width: g.size.width, password: $passcode)
                    .padding(.top, 8)
                    .padding(.bottom, 27)
                HStack(spacing: 48) {
                    Button(action: cancel) {
                        Label("Cancel", systemImage: "multiply")
                    }
                    Button(action: submit) {
                        Label(submitLabel, systemImage: "checkmark")
                    }
                    .disabled(submitEnabled?(passcode) == false || passcode.count < 4)
                }
            }
        }
        .padding()
        .padding(.horizontal, 24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color(uiColor: .systemBackground))
    }
}

struct PasscodeViewView_Previews: PreviewProvider {
    static var previews: some View {
        PasscodeView(
            passcode: Binding.constant(""),
            title: "Enter Passcode",
            reason: "Unlock app",
            submitLabel: "Submit",
            submit: {},
            cancel: {}
        )
    }
}
