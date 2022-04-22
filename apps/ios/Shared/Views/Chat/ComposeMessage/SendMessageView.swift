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
    @Binding var message: String
    @Namespace var namespace
    @FocusState.Binding var keyboardVisible: Bool
    @Binding var editing: Bool
    @Binding var sendEnabled: Bool
    @State private var teHeight: CGFloat = 42
    @State private var teFont: Font = .body
    var maxHeight: CGFloat = 360
    var minHeight: CGFloat = 37

    var body: some View {
        ZStack {
            HStack(alignment: .bottom) {
                ZStack(alignment: .leading) {
                    Text(message)
                        .lineLimit(10)
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
                        Image(systemName: editing ? "checkmark.circle.fill" : "arrow.up.circle.fill")
                            .resizable()
                            .foregroundColor(.accentColor)
                    }
                    .disabled(!sendEnabled)
                    .frame(width: 29, height: 29)
                    .padding([.bottom, .trailing], 4)
                }
            }

            RoundedRectangle(cornerSize: CGSize(width: 20, height: 20))
                .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                .frame(height: teHeight)
        }
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
        @State var message: String = ""
        @FocusState var keyboardVisible: Bool
        @State var editingOff: Bool = false
        @State var editingOn: Bool = true
        @State var sendEnabled: Bool = true
        @State var item: ChatItem? = ChatItem.getSample(1, .directSnd, .now, "hello")

        return Group {
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    sendMessage: { print ($0) },
                    message: $message,
                    keyboardVisible: $keyboardVisible,
                    editing: $editingOff,
                    sendEnabled: $sendEnabled
                )
            }
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    sendMessage: { print ($0) },
                    message: $message,
                    keyboardVisible: $keyboardVisible,
                    editing: $editingOn,
                    sendEnabled: $sendEnabled
                )
            }
        }
    }
}
