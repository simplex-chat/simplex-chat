//
//  PasscodeEntry.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PasscodeEntry: View {
    @EnvironmentObject var m: ChatModel
    var width: CGFloat
    @Binding var password: String
    @State private var showPassword = false

    var body: some View {
        VStack {
            passwordView()
                .padding(.bottom, 4)
            passwordGrid(width)
                .frame(minHeight: 0)
        }
    }

    @ViewBuilder private func passwordView() -> some View {
        Text(
            password == ""
            ? " "
            : splitPassword()
        )
        .font(showPassword ? .title2.monospacedDigit() : .body)
        .onTapGesture {
            showPassword = !showPassword
        }
        .frame(height: 30)
    }

    private func splitPassword() -> String {
        let n = password.count < 8 ? 8 : 4
        return password.enumerated().reduce("") { acc, c in
            acc
            + (showPassword ? String(c.element) : "●")
            + ((c.offset + 1) % n == 0 ? " " : "")
        }
    }

    private func passwordGrid(_ width: CGFloat) -> some View {
        let s = width / 3
        return VStack(spacing: 0) {
            digitsRow(s, 1, 2, 3)
            Divider()
            digitsRow(s, 4, 5, 6)
            Divider()
            digitsRow(s, 7, 8, 9)
            Divider()
            HStack(spacing: 0) {
                passwordEdit(s, image: "multiply") {
                    password = ""
                }
                Divider()
                passwordDigit(s, 0)
                Divider()
                passwordEdit(s, image: "delete.backward") {
                    if password != "" { password.removeLast() }
                }
            }
            .frame(height: s)
        }
        .frame(width: width, height: s * 4)
    }

    private func digitsRow(_ size: CGFloat, _ d1: Int, _ d2: Int, _ d3: Int) -> some View {
        HStack(spacing: 0) {
            passwordDigit(size, d1)
            Divider()
            passwordDigit(size, d2)
            Divider()
            passwordDigit(size, d3)
        }
        .frame(height: size * 0.97)
    }


    private func passwordDigit(_ size: CGFloat, _ d: Int) -> some View {
        let s = String(describing: d)
        return passwordButton(size) {
            if password.count < 16 {
                password = password + s
            }
        } label: {
            Text(s).font(.title)
        }
        .disabled(password.count >= 16)
    }

    private func passwordEdit(_ size: CGFloat, image: String, action: @escaping () -> Void) -> some View {
        passwordButton(size, action: action) {
            Image(systemName: image)
        }
    }

    private func passwordButton<V: View>(_ size: CGFloat, action: @escaping () -> Void, label: () -> V) -> some View {
        let h = size * 0.97
        return Button(action: action) {
            ZStack {
                Circle()
                    .frame(width: h, height: h)
                    .foregroundColor(Color(uiColor: .systemBackground))
                label()
            }
        }
        .foregroundColor(.secondary)
        .frame(width: size, height: h)
    }
}

struct PasscodeEntry_Previews: PreviewProvider {
    static var previews: some View {
        PasscodeEntry(width: 360, password: Binding.constant(""))
    }
}
