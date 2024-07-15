//
//  SendMessageView.swift
//  SimpleX
//
//  Created by Evgeny Poberezkin on 29/01/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

private let liveMsgInterval: UInt64 = 3000_000000

struct SendMessageView: View {
    @Binding var composeState: ComposeState
    @EnvironmentObject var theme: AppTheme
    var sendMessage: (Int?) -> Void
    var sendLiveMessage: (() async -> Void)? = nil
    var updateLiveMessage: (() async -> Void)? = nil
    var cancelLiveMessage: (() -> Void)? = nil
    var nextSendGrpInv: Bool = false
    var showVoiceMessageButton: Bool = true
    var voiceMessageAllowed: Bool = true
    var disableSendButton = false
    var showEnableVoiceMessagesAlert: ChatInfo.ShowEnableVoiceMessagesAlert = .other
    var startVoiceMessageRecording: (() -> Void)? = nil
    var finishVoiceMessageRecording: (() -> Void)? = nil
    var allowVoiceMessagesToContact: (() -> Void)? = nil
    var timedMessageAllowed: Bool = false
    var onMediaAdded: ([UploadContent]) -> Void
    @State private var holdingVMR = false
    @Namespace var namespace
    @Binding var keyboardVisible: Bool
    var sendButtonColor = Color.accentColor
    @State private var teHeight: CGFloat = 42
    @State private var teFont: Font = .body
    @State private var sendButtonSize: CGFloat = 29
    @State private var sendButtonOpacity: CGFloat = 1
    @State private var showCustomDisappearingMessageDialogue = false
    @State private var showCustomTimePicker = false
    @State private var selectedDisappearingMessageTime: Int? = customDisappearingMessageTimeDefault.get()
    @State private var progressByTimeout = false
    @AppStorage(DEFAULT_LIVE_MESSAGE_ALERT_SHOWN) private var liveMessageAlertShown = false

    var body: some View {
        ZStack {
            HStack(alignment: .bottom) {
                ZStack(alignment: .leading) {
                    if case .voicePreview = composeState.preview {
                        Text("Voice message…")
                            .font(teFont.italic())
                            .multilineTextAlignment(.leading)
                            .foregroundColor(theme.colors.secondary)
                            .padding(.horizontal, 10)
                            .padding(.vertical, 8)
                            .frame(maxWidth: .infinity)
                    } else {
                        NativeTextEditor(
                            text: $composeState.message,
                            disableEditing: $composeState.inProgress,
                            height: $teHeight,
                            focused: $keyboardVisible,
                            onImagesAdded: onMediaAdded
                        )
                        .allowsTightening(false)
                        .fixedSize(horizontal: false, vertical: true)
                    }
                }

                if progressByTimeout {
                    ProgressView()
                        .scaleEffect(1.4)
                        .frame(width: 31, height: 31, alignment: .center)
                        .padding([.bottom, .trailing], 3)
                } else {
                    VStack(alignment: .trailing) {
                        if teHeight > 100 && !composeState.inProgress {
                            deleteTextButton()
                            Spacer()
                        }
                        composeActionButtons()
                    }
                    .frame(height: teHeight, alignment: .bottom)
                }
            }
            .padding(.vertical, 1)
            .overlay(
            RoundedRectangle(cornerRadius: 20)
                .strokeBorder(.tertiary, lineWidth: 1).opacity(0.5)
            )
        }
        .onChange(of: composeState.message, perform: { text in updateFont(text) })
        .onChange(of: composeState.inProgress) { inProgress in
            if inProgress {
                DispatchQueue.main.asyncAfter(deadline: .now() + 3) {
                    progressByTimeout = composeState.inProgress
                }
            } else {
                progressByTimeout = false
            }
        }
        .padding(.vertical, 8)
    }

    @ViewBuilder private func composeActionButtons() -> some View {
        let vmrs = composeState.voiceMessageRecordingState
        if nextSendGrpInv {
            inviteMemberContactButton()
        } else if showVoiceMessageButton
            && composeState.message.isEmpty
            && !composeState.editing
            && !composeState.forwarding
            && composeState.liveMessage == nil
            && ((composeState.noPreview && vmrs == .noRecording)
                || (vmrs == .recording && holdingVMR)) {
            HStack {
                if voiceMessageAllowed {
                    RecordVoiceMessageButton(
                        startVoiceMessageRecording: startVoiceMessageRecording,
                        finishVoiceMessageRecording: finishVoiceMessageRecording,
                        holdingVMR: $holdingVMR,
                        disabled: composeState.inProgress
                    )
                } else {
                    voiceMessageNotAllowedButton()
                }
                if let send = sendLiveMessage,
                   let update = updateLiveMessage,
                   case .noContextItem = composeState.contextItem {
                    startLiveMessageButton(send: send, update: update)
                }
            }
        } else if vmrs == .recording && !holdingVMR {
            finishVoiceMessageRecordingButton()
        } else if composeState.liveMessage != nil && composeState.liveMessage?.sentMsg == nil && composeState.message.isEmpty {
            cancelLiveMessageButton {
                cancelLiveMessage?()
            }
        } else {
            sendMessageButton()
        }
    }

    private func deleteTextButton() -> some View {
        Button {
            composeState.message = ""
        } label: {
            Image(systemName: "multiply.circle.fill")
        }
        .foregroundColor(Color(uiColor: .tertiaryLabel))
        .padding([.top, .trailing], 4)
    }

    private func inviteMemberContactButton() -> some View {
        Button {
            sendMessage(nil)
        } label: {
            Image(systemName: "arrow.up.circle.fill")
                .resizable()
                .foregroundColor(sendButtonColor)
                .frame(width: sendButtonSize, height: sendButtonSize)
                .opacity(sendButtonOpacity)
        }
        .disabled(
            !composeState.sendEnabled ||
            composeState.inProgress
        )
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private func sendMessageButton() -> some View {
        Button {
            sendMessage(nil)
        } label: {
            Image(systemName: composeState.editing || composeState.liveMessage != nil
                  ? "checkmark.circle.fill"
                  : "arrow.up.circle.fill")
            .resizable()
            .foregroundColor(sendButtonColor)
            .frame(width: sendButtonSize, height: sendButtonSize)
            .opacity(sendButtonOpacity)
        }
        .disabled(
            !composeState.sendEnabled ||
            composeState.inProgress ||
            (!voiceMessageAllowed && composeState.voicePreview) ||
            composeState.endLiveDisabled ||
            disableSendButton
        )
        .frame(width: 29, height: 29)
        .contextMenu{
            sendButtonContextMenuItems()
        }
        .padding([.bottom, .trailing], 4)
        .confirmationDialog("Send disappearing message", isPresented: $showCustomDisappearingMessageDialogue, titleVisibility: .visible) {
            Button("30 seconds") { sendMessage(30) }
            Button("1 minute") { sendMessage(60) }
            Button("5 minutes") { sendMessage(300) }
            Button("Custom time") { showCustomTimePicker = true }
        }
        .sheet(isPresented: $showCustomTimePicker, onDismiss: { selectedDisappearingMessageTime = customDisappearingMessageTimeDefault.get() }) {
            if #available(iOS 16.0, *) {
                disappearingMessageCustomTimePicker()
                    .presentationDetents([.medium])
            } else {
                disappearingMessageCustomTimePicker()
            }
        }
    }

    private func disappearingMessageCustomTimePicker() -> some View {
        CustomTimePickerView(
            selection: $selectedDisappearingMessageTime,
            confirmButtonText: "Send",
            confirmButtonAction: {
                if let time = selectedDisappearingMessageTime {
                    sendMessage(time)
                    customDisappearingMessageTimeDefault.set(time)
                }
            },
            description: "Delete after"
        )
    }

    @ViewBuilder private func sendButtonContextMenuItems() -> some View {
        if composeState.liveMessage == nil,
           !composeState.editing {
            if case .noContextItem = composeState.contextItem,
               !composeState.voicePreview,
               let send = sendLiveMessage,
               let update = updateLiveMessage {
                Button {
                    startLiveMessage(send: send, update: update)
                } label: {
                    Label("Send live message", systemImage: "bolt.fill")
                }
            }
            if timedMessageAllowed {
                Button {
                    hideKeyboard()
                    showCustomDisappearingMessageDialogue = true
                } label: {
                    Label("Disappearing message", systemImage: "stopwatch")
                }
            }
        }
    }

    private struct RecordVoiceMessageButton: View {
        @EnvironmentObject var theme: AppTheme
        var startVoiceMessageRecording: (() -> Void)?
        var finishVoiceMessageRecording: (() -> Void)?
        @Binding var holdingVMR: Bool
        var disabled: Bool
        @State private var pressed: TimeInterval? = nil

        var body: some View {
            Button(action: {}) {
                Image(systemName: "mic.fill")
                    .foregroundColor(theme.colors.primary)
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
        Button {
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
        } label: {
            Image(systemName: "mic")
                .foregroundColor(theme.colors.secondary)
        }
        .disabled(composeState.inProgress)
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private func cancelLiveMessageButton(cancel: @escaping () -> Void) -> some View {
        return Button {
            cancel()
        } label: {
            Image(systemName: "multiply")
                .resizable()
                .scaledToFit()
                .foregroundColor(theme.colors.primary)
                .frame(width: 15, height: 15)
        }
        .frame(width: 29, height: 29)
        .padding([.bottom, .horizontal], 4)
    }

    private func startLiveMessageButton(send:  @escaping () async -> Void, update: @escaping () async -> Void) -> some View {
        return Button {
            switch composeState.preview {
            case .noPreview: startLiveMessage(send: send, update: update)
            default: ()
            }
        } label: {
            Image(systemName: "bolt.fill")
                .resizable()
                .scaledToFit()
                .foregroundColor(theme.colors.primary)
                .frame(width: 20, height: 20)
        }
        .frame(width: 29, height: 29)
        .padding([.bottom, .horizontal], 4)
    }

    private func startLiveMessage(send:  @escaping () async -> Void, update: @escaping () async -> Void) {
        if liveMessageAlertShown {
            start()
        } else {
            AlertManager.shared.showAlert(Alert(
                title: Text("Live message!"),
                message: Text("Send a live message - it will update for the recipient(s) as you type it"),
                primaryButton: .default(Text("Send")) {
                    liveMessageAlertShown = true
                    start()
                },
                secondaryButton: .cancel()
            ))
        }

        func start() {
            Task {
                await send()
                await MainActor.run { run() }
            }
        }

        @Sendable func run() {
            Timer.scheduledTimer(withTimeInterval: 0.75, repeats: true) { t in
                withAnimation(.easeInOut(duration: 0.7)) {
                    sendButtonSize = sendButtonSize == 29 ? 26 : 29
                    sendButtonOpacity = sendButtonOpacity == 1 ? 0.75 : 1
                }
                if composeState.liveMessage == nil {
                    t.invalidate()
                    sendButtonSize = 29
                    sendButtonOpacity = 1
                }
            }
            Task {
                _ = try? await Task.sleep(nanoseconds: liveMsgInterval)
                while await composeState.liveMessage != nil {
                    await update()
                    _ = try? await Task.sleep(nanoseconds: liveMsgInterval)
                }
            }
        }
    }

    private func finishVoiceMessageRecordingButton() -> some View {
        Button(action: { finishVoiceMessageRecording?() }) {
            Image(systemName: "stop.fill")
                .foregroundColor(theme.colors.primary)
        }
        .disabled(composeState.inProgress)
        .frame(width: 29, height: 29)
        .padding([.bottom, .trailing], 4)
    }

    private func updateFont(_ text: String) {
        DispatchQueue.main.async {
            teFont = isShortEmoji(text)
            ? (text.count < 4 ? largeEmojiFont : mediumEmojiFont)
            : .body
        }
    }
}

struct SendMessageView_Previews: PreviewProvider {
    static var previews: some View {
        @State var composeStateNew = ComposeState()
        let ci = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var composeStateEditing = ComposeState(editingItem: ci)
        @State var sendEnabled: Bool = true

        return Group {
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateNew,
                    sendMessage: { _ in },
                    onMediaAdded: { _ in },
                    keyboardVisible: Binding.constant(true)
                )
            }
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateEditing,
                    sendMessage: { _ in },
                    onMediaAdded: { _ in },
                    keyboardVisible: Binding.constant(true)
                )
            }
        }
    }
}
