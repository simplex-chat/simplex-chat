//
//  SendMessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct SendMessageView: View {
    @Binding var composeState: ComposeState
    var sendMessage: () -> Void
    @Binding var voiceMessageAllowed: Bool
    var startVoiceMessageRecording: (() -> Void)? = nil
    var finishVoiceMessageRecording: (() -> Void)? = nil
    @State private var longPressingVMR = false
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
                    if case .voicePreview = composeState.preview {
                        Text("Voice message…")
                            .font(teFont.italic())
                            .multilineTextAlignment(.leading)
                            .foregroundColor(.secondary)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 8)
                            .frame(maxWidth: .infinity)
                    } else {
                        let alignment: TextAlignment = isRightToLeft(composeState.message) ? .trailing : .leading
                        Text(composeState.message)
                            .lineLimit(10)
                            .font(teFont)
                            .multilineTextAlignment(alignment)
                            .foregroundColor(.clear)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 8)
                            .matchedGeometryEffect(id: "te", in: namespace)
                            .background(GeometryReader(content: updateHeight))
                        TextEditor(text: $composeState.message)
                            .focused($keyboardVisible)
                            .font(teFont)
                            .textInputAutocapitalization(.sentences)
                            .multilineTextAlignment(alignment)
                            .padding(.horizontal, 5)
                            .allowsTightening(false)
                            .frame(height: teHeight)
                    }
                }

                if (composeState.inProgress) {
                    ProgressView()
                        .scaleEffect(1.4)
                        .frame(width: 31, height: 31, alignment: .center)
                        .padding([.bottom, .trailing], 3)
                } else {
                    let vmrs = composeState.voiceMessageRecordingState
                    if voiceMessageAllowed,
                       composeState.message.isEmpty,
                       !composeState.editing,
                       (composeState.noPreview && vmrs == .noRecording)
                        || (vmrs == .recording && longPressingVMR) {
                        recordVoiceMessageButton()
                    } else if vmrs == .recording && !longPressingVMR {
                        finishVoiceMessageRecordingButton()
                    } else {
                        sendMessageButton()
                    }
                }
            }

            RoundedRectangle(cornerSize: CGSize(width: 20, height: 20))
                .strokeBorder(.secondary, lineWidth: 0.3, antialiased: true)
                .frame(height: teHeight)
        }
        .padding(.vertical, 8)
    }

    private func sendMessageButton() -> some View {
        Button(action: { sendMessage() }) {
            Image(systemName: composeState.editing ? "checkmark.circle.fill" : "arrow.up.circle.fill")
                .resizable()
                .foregroundColor(.accentColor)
        }
        .disabled(!composeState.sendEnabled || composeState.disabled)
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private func recordVoiceMessageButton() -> some View {
        Button(action: {
            if !longPressingVMR {
                startVoiceMessageRecording?()
            } else {
                finishVoiceMessageRecording?()
            }
            longPressingVMR = false
        }) {
            Image(systemName: "mic")
                .foregroundColor(.secondary)
        }
        .simultaneousGesture(
            LongPressGesture()
                .onEnded { _ in
                    longPressingVMR = true
                    startVoiceMessageRecording?()
                }
        )
        .disabled(composeState.disabled)
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)

    }

    private func finishVoiceMessageRecordingButton() -> some View {
        Button(action: { finishVoiceMessageRecording?() }) {
            Image(systemName: "stop.fill")
                .foregroundColor(.accentColor)
        }
        .disabled(composeState.disabled)
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private func updateHeight(_ g: GeometryProxy) -> Color {
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
                    voiceMessageAllowed: Binding.constant(true),
                    keyboardVisible: $keyboardVisible
                )
            }
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateEditing,
                    sendMessage: {},
                    voiceMessageAllowed: Binding.constant(true),
                    keyboardVisible: $keyboardVisible
                )
            }
        }
    }
}
