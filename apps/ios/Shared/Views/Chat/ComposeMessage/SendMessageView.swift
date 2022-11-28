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
    var showVoiceMessageButton: Bool = true
    var voiceMessageAllowed: Bool = true
    var showEnableVoiceMessagesAlert: ChatInfo.ShowEnableVoiceMessagesAlert = .other
    var startVoiceMessageRecording: (() -> Void)? = nil
    var finishVoiceMessageRecording: (() -> Void)? = nil
    var allowVoiceMessagesToContact: (() -> Void)? = nil
    @State private var holdingVMR = false
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
                    if showVoiceMessageButton,
                       composeState.message.isEmpty,
                       !composeState.editing,
                       (composeState.noPreview && vmrs == .noRecording)
                        || (vmrs == .recording && holdingVMR) {
                        if voiceMessageAllowed {
                            RecordVoiceMessageButton(
                                startVoiceMessageRecording: startVoiceMessageRecording,
                                finishVoiceMessageRecording: finishVoiceMessageRecording,
                                holdingVMR: $holdingVMR,
                                disabled: composeState.disabled
                            )
                        } else {
                            voiceMessageNotAllowedButton()
                        }
                    } else if vmrs == .recording && !holdingVMR {
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
        .disabled(
            !composeState.sendEnabled ||
            composeState.disabled ||
            (!voiceMessageAllowed && composeState.voicePreview)
        )
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private struct RecordVoiceMessageButton: View {
        var startVoiceMessageRecording: (() -> Void)?
        var finishVoiceMessageRecording: (() -> Void)?
        @Binding var holdingVMR: Bool
        var disabled: Bool
        @State private var pressed: TimeInterval? = nil

        var body: some View {
            Button(action: {}) {
                Image(systemName: "mic.fill")
                    .foregroundColor(.accentColor)
            }
            .disabled(disabled)
            .frame(width: 29, height: 29)
            .padding([.bottom, .trailing], 4)
            ._onButtonGesture { down in
                if down {
                    holdingVMR = true
                    pressed = ProcessInfo.processInfo.systemUptime
                    startVoiceMessageRecording?()
                } else {
                    let now = ProcessInfo.processInfo.systemUptime
                    if let pressed = pressed,
                       now - pressed >= 1 {
                        finishVoiceMessageRecording?()
                    }
                    holdingVMR = false
                    pressed = nil
                }
            } perform: {}
        }
    }

    private func voiceMessageNotAllowedButton() -> some View {
        Button(action: {
            switch showEnableVoiceMessagesAlert {
            case .userEnable:
                AlertManager.shared.showAlert(Alert(
                    title: Text("Allow voice messages?"),
                    message: Text("You need to allow your contact to send voice messages to be able to send them."),
                    primaryButton: .default(Text("Allow")) {
                        allowVoiceMessagesToContact?()
                    },
                    secondaryButton: .cancel()
                ))
            case .askContact:
                AlertManager.shared.showAlertMsg(
                    title: "Voice messages prohibited!",
                    message: "Please ask your contact to enable sending voice messages."
                )
            case .groupOwnerCan:
                AlertManager.shared.showAlertMsg(
                    title: "Voice messages prohibited!",
                    message: "Only group owners can enable voice messages."
                )
            case .other:
                AlertManager.shared.showAlertMsg(
                    title: "Voice messages prohibited!",
                    message: "Please check yours and your contact preferences."
                )
            }
        }) {
            Image(systemName: "mic")
                .foregroundColor(.secondary)
        }
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
