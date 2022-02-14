//
//  SendMessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SendMessageView: View {
    var sendMessage: (String) -> Void
    var inProgress: Bool = false
    @State private var message: String = "" //Lorem ipsum dolor sit amet, consectetur" // adipiscing elit, sed do eiusmod tempor incididunt ut labor7 et dolore magna aliqua. Ut enim ad minim veniam, quis"// nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."// Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    @Namespace var namespace
    @FocusState.Binding var keyboardVisible: Bool
    @State private var teHeight: CGFloat = 42
    @State private var teFont: Font = .body
    var maxHeight: CGFloat = 360
    var minHeight: CGFloat = 37

    var body: some View {
        ZStack {
            HStack(alignment: .bottom) {
                ZStack(alignment: .leading) {
                    Text(message)
                        .font(teFont)
                        .foregroundColor(.clear)
                        .padding(.horizontal, 10)
                        .padding(.vertical, 8)
                        .matchedGeometryEffect(id: "te", in: namespace)
                        .background(GeometryReader(content: updateHeight))
                    TextEditor(text: $message)
                        .onSubmit(submit)
                        .focused($keyboardVisible)
                        .font(teFont)
                        .textInputAutocapitalization(.sentences)
                        .padding(.horizontal, 5)
                        .allowsTightening(false)
                        .frame(height: teHeight)
                }

                if (inProgress) {
                    ProgressView()
                        .scaleEffect(1.4)
                        .frame(width: 31, height: 31, alignment: .center)
                        .padding([.bottom, .trailing], 3)
                } else {
                    Button(action: submit) {
                        Image(systemName: "arrow.up.circle.fill")
                            .resizable()
                            .foregroundColor(.accentColor)
                    }
                    .disabled(message.isEmpty)
                    .frame(width: 29, height: 29)
                    .padding([.bottom, .trailing], 4)
                }
            }

            RoundedRectangle(cornerSize: CGSize(width: 20, height: 20))
                .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                .frame(height: teHeight)
        }
        .padding(.horizontal, 12)
        .padding(.vertical, 8)
    }

    func submit() {
        sendMessage(message)
        message = ""
    }

    func updateHeight(_ g: GeometryProxy) -> Color {
        DispatchQueue.main.async {
            teHeight = min(max(g.frame(in: .local).size.height, minHeight), maxHeight)
            teFont = isShortEmoji(message)
                ? message.count < 4
                    ? largeEmojiFont
                    : mediumEmojiFont
                : .body
        }
        return Color.clear
    }
}

struct SendMessageView_Previews: PreviewProvider {
    static var previews: some View {
        @FocusState var keyboardVisible: Bool

        return VStack {
            Text("")
            Spacer(minLength: 0)
            SendMessageView(
                sendMessage: { print ($0) },
                keyboardVisible: $keyboardVisible
            )
        }
    }
}
