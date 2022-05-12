//
//  SendMessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct SendMessageView: View {
    @Binding var composeState: ComposeState
    var sendMessage: () -> Void
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
                    Text(composeState.message)
                        .lineLimit(10)
                        .font(teFont)
                        .foregroundColor(.clear)
                        .padding(.horizontal, 10)
                        .padding(.vertical, 8)
                        .matchedGeometryEffect(id: "te", in: namespace)
                        .background(GeometryReader(content: updateHeight))
                    TextEditor(text: $composeState.message)
                        .focused($keyboardVisible)
                        .font(teFont)
                        .textInputAutocapitalization(.sentences)
                        .padding(.horizontal, 5)
                        .allowsTightening(false)
                        .frame(height: teHeight)
                }

                if (composeState.inProgress) {
                    ProgressView()
                        .scaleEffect(1.4)
                        .frame(width: 31, height: 31, alignment: .center)
                        .padding([.bottom, .trailing], 3)
                } else {
                    Button(action: { sendMessage() }) {
                        Image(systemName: composeState.editing() ? "checkmark.circle.fill" : "arrow.up.circle.fill")
                            .resizable()
                            .foregroundColor(.accentColor)
                    }
                    .disabled(!composeState.sendEnabled())
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

    func updateHeight(_ g: GeometryProxy) -> Color {
        DispatchQueue.main.async {
            teHeight = min(max(g.frame(in: .local).size.height, minHeight), maxHeight)
            teFont = isShortEmoji(composeState.message)
            ? composeState.message.count < 4
                    ? largeEmojiFont
                    : mediumEmojiFont
                : .body
        }
        return Color.clear
    }
}

struct SendMessageView_Previews: PreviewProvider {
    static var previews: some View {
        @State var composeStateNew = ComposeState()
        let ci = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var composeStateEditing = ComposeState(editingItem: ci)
        @FocusState var keyboardVisible: Bool
        @State var sendEnabled: Bool = true

        return Group {
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateNew,
                    sendMessage: {},
                    keyboardVisible: $keyboardVisible
                )
            }
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateEditing,
                    sendMessage: {},
                    keyboardVisible: $keyboardVisible
                )
            }
        }
    }
}
