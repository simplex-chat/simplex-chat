//
//  DigitalPasswordEntry.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 10/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct DigitalPasswordEntry: View {
    @EnvironmentObject var m: ChatModel
    var submit: (String) -> Void
    @State private var password = ""
    @State private var showPassword = false

    var body: some View {
        VStack {
            passwordView()
            GeometryReader { g in
                passwordGrid(size: g.size.width)
            }
        }
        .padding()
    }

    @ViewBuilder private func passwordView() -> some View {
        Text(
            password == ""
            ? " "
            : showPassword
            ? password
            : String(repeating: "●", count: password.count)
        )
        .font(showPassword ? .title2 : .body)
        .onTapGesture {
            showPassword = !showPassword
        }
        .frame(height: 30)
    }

    private func passwordGrid(size: CGFloat) -> some View {
        VStack(spacing: 0) {
            let s = size / 3
            digitsRow(s, 1, 2, 3)
            Divider()
            digitsRow(s, 4, 5, 6)
            Divider()
            digitsRow(s, 7, 8, 9)
            Divider()
            HStack(spacing: 0) {
                Button("Clear") { password = "" }
                .frame(width: s)
                Divider()
                passwordDigit(s, 0)
                Divider()
                Button("Submit") { submit(password) }
                .frame(width: s)
            }
            .frame(height: s)
        }
        .frame(width: size, height: size * 4 / 3)
    }

    private func digitsRow(_ size: CGFloat, _ d1: Int, _ d2: Int, _ d3: Int) -> some View {
        HStack(spacing: 0) {
            passwordDigit(size, d1)
            Divider()
            passwordDigit(size, d2)
            Divider()
            passwordDigit(size, d3)
        }
        .frame(height: size)
    }

    private func passwordDigit(_ size: CGFloat, _ d: Int) -> some View {
        let s = String(describing: d)
        return Button {
            password = password + s
        } label: {
            ZStack {
                Circle()
                    .frame(width: size, height: size)
                    .foregroundColor(Color(uiColor: .systemBackground))
                Text(s)
                    .font(.title)
                    .bold()
            }
        }
        .foregroundColor(.secondary)
        .frame(width: size, height: size)
    }
}

struct DigitalPasswordEntry_Previews: PreviewProvider {
    static var previews: some View {
        DigitalPasswordEntry(submit: { _ in })
    }
}
