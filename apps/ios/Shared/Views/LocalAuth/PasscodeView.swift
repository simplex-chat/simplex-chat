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
    @Binding var buttonsEnabled: Bool

    var submit: () -> Void
    var cancel: () -> Void

    var body: some View {
        GeometryReader { g in
            if g.size.width < g.size.height * 2 / 3 {
                verticalPasscodeView(g)
            } else {
                horizontalPasscodeView(g)
            }
        }
        .padding(.horizontal, 40)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(AppTheme.shared.colors.background)
    }

    private func verticalPasscodeView(_ g: GeometryProxy) -> some View {
        VStack(spacing: 8) {
            passcodeEntry(g)
            Spacer()
            HStack(spacing: 48) {
                buttonsView()
            }
        }
        .padding(.vertical, 32)
    }

    private func horizontalPasscodeView(_ g: GeometryProxy) -> some View {
        HStack(alignment: .bottom, spacing: 48) {
            VStack(spacing: 8) {
                passcodeEntry(g)
            }
            VStack(spacing: 48) {
                buttonsView()
            }
            .frame(maxHeight: g.size.height / 5 * 3 * 0.97)
        }
        .frame(maxWidth: .infinity)
        .padding(.vertical)
    }

    @ViewBuilder private func passcodeEntry(_ g: GeometryProxy) -> some View {
        Text(title)
            .font(.title)
            .bold()
            .padding(.top, 8)
        if let reason = reason {
            Text(reason).padding(.top, 4)
        }
        Spacer()
        PasscodeEntry(width: g.size.width, height: g.size.height, password: $passcode)
    }

    @ViewBuilder private func buttonsView() -> some View {
        Button(action: cancel) {
            Label("Cancel", systemImage: "multiply")
        }.disabled(!buttonsEnabled)
        Button(action: submit) {
            Label(submitLabel, systemImage: "checkmark")
        }
        .disabled(submitEnabled?(passcode) == false || passcode.count < 4 || !buttonsEnabled)
    }
}

struct PasscodeViewView_Previews: PreviewProvider {
    static var previews: some View {
        PasscodeView(
            passcode: Binding.constant(""),
            title: "Enter Passcode",
            reason: "Unlock app",
            submitLabel: "Submit",
            buttonsEnabled: Binding.constant(true),
            submit: {},
            cancel: {}
        )
    }
}
