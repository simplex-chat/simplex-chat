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
    var placeholder: String?
    @Binding var composeState: ComposeState
    @Binding var selectedRange: NSRange
    @EnvironmentObject var theme: AppTheme
    @Environment(\.isEnabled) var isEnabled
    var sendMessage: (Int?) -> Void
    var sendLiveMessage: (() async -> Void)? = nil
    var updateLiveMessage: (() async -> Void)? = nil
    var cancelLiveMessage: (() -> Void)? = nil
    var sendToConnect: (() -> Void)? = nil
    var hideSendButton: Bool = false
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
    @Binding var keyboardHiddenDate: Date
    var sendButtonColor = Color.accentColor
    @State private var teHeight: CGFloat = NativeTextEditor.minHeight
    @State private var teFont: Font = .body
    @State private var sendButtonSize: CGFloat = 29
    @State private var sendButtonOpacity: CGFloat = 1
    @State private var showCustomDisappearingMessageDialogue = false
    @State private var showCustomTimePicker = false
    @State private var selectedDisappearingMessageTime: Int? = customDisappearingMessageTimeDefault.get()
    @UserDefault(DEFAULT_LIVE_MESSAGE_ALERT_SHOWN) private var liveMessageAlertShown = false

    var body: some View {
        let composeShape = RoundedRectangle(cornerSize: CGSize(width: 20, height: 20))
        ZStack(alignment: .leading) {
            if case .voicePreview = composeState.preview {
                Text("Voice message…")
                    .font(teFont.italic())
                    .multilineTextAlignment(.leading)
                    .foregroundColor(theme.colors.secondary)
                    .padding(.horizontal, 10)
                    .padding(.vertical, 8)
                    .padding(.trailing, 32)
                    .frame(maxWidth: .infinity)
            } else {
                NativeTextEditor(
                    text: $composeState.message,
                    disableEditing: $composeState.inProgress,
                    height: $teHeight,
                    focused: $keyboardVisible,
                    lastUnfocusedDate: $keyboardHiddenDate,
                    placeholder: Binding(get: { placeholder ?? composeState.placeholder }, set: { _ in }),
                    selectedRange: $selectedRange,
                    onImagesAdded: onMediaAdded
                )
                .padding(.trailing, 32)
                .allowsTightening(false)
                .fixedSize(horizontal: false, vertical: true)
            }
        }
        .overlay(alignment: .topTrailing, content: {
            if !composeState.progressByTimeout && teHeight > 100 && !composeState.inProgress {
                deleteTextButton()
            }
        })
        .overlay(alignment: .bottomTrailing) {
            if composeState.progressByTimeout {
                ProgressView()
                    .scaleEffect(1.4)
                    .frame(width: 31, height: 31, alignment: .center)
                    .padding([.bottom, .trailing], 4)
            } else {
                composeActionButtons()
                // required for intercepting clicks
                    .background(.white.opacity(0.000001))
            }
        }
        .padding(.vertical, 1)
        .background(theme.colors.background)
        .clipShape(composeShape)
        .overlay(composeShape.strokeBorder(.secondary, lineWidth: 0.5).opacity(0.7))
        .onChange(of: composeState.message, perform: { text in updateFont(text) })
        .padding(.vertical, 8)
    }

    @ViewBuilder private func composeActionButtons() -> some View {
        let vmrs = composeState.voiceMessageRecordingState
        if hideSendButton {
            EmptyView()
        } else if let connect = sendToConnect {
            sendToConnectButton(connect)
        } else if case .reportedItem = composeState.contextItem {
            sendMessageButton()
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

    private func sendToConnectButton(_ connect: @escaping () -> Void) -> some View {
        let disabled = !composeState.sendEnabled || composeState.inProgress || disableSendButton
        return Button(action: connect) {
            Image(systemName: "arrow.up.circle.fill")
                .resizable()
                .foregroundColor(disabled ? theme.colors.secondary.opacity(0.67) : sendButtonColor)
                .frame(width: sendButtonSize, height: sendButtonSize)
                .opacity(sendButtonOpacity)
        }
        .disabled(disabled)
        .frame(width: 31, height: 31)
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
        .frame(width: 31, height: 31)
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
               !composeState.manyMediaPreviews,
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
        @Environment(\.isEnabled) var isEnabled
        @EnvironmentObject var theme: AppTheme
        var startVoiceMessageRecording: (() -> Void)?
        var finishVoiceMessageRecording: (() -> Void)?
        @Binding var holdingVMR: Bool
        var disabled: Bool
        @State private var pressed: TimeInterval? = nil

        var body: some View {
            Image(systemName: isEnabled ? "mic.fill" : "mic")
            .resizable()
            .scaledToFit()
            .frame(width: 20, height: 20)
            .foregroundColor(isEnabled ? theme.colors.primary : theme.colors.secondary)
            .opacity(holdingVMR ? 0.7 : 1)
            .disabled(disabled)
            .frame(width: 31, height: 31)
            .padding([.bottom, .trailing], 4)
            ._onButtonGesture { down in
                if down {
                    holdingVMR = true
                    pressed = ProcessInfo.processInfo.systemUptime
                    startVoiceMessageRecording?()
                } else {
                    if let pressed, ProcessInfo.processInfo.systemUptime - pressed >= 1 {
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
                .resizable()
                .scaledToFit()
                .frame(width: 20, height: 20)
                .foregroundColor(theme.colors.secondary)
        }
        .disabled(composeState.inProgress)
        .frame(width: 31, height: 31)
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
                .foregroundColor(isEnabled ? theme.colors.primary : theme.colors.secondary)
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
        .frame(width: 31, height: 31)
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
        @State var selectedRange = NSRange()
        let ci = ChatItem.getSample(1, .directSnd, .now, "hello")
        @State var composeStateEditing = ComposeState(editingItem: ci)
        @State var selectedRangeEditing = NSRange()
        @State var sendEnabled: Bool = true

        return Group {
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateNew,
                    selectedRange: $selectedRange,
                    sendMessage: { _ in },
                    onMediaAdded: { _ in },
                    keyboardVisible: Binding.constant(true),
                    keyboardHiddenDate: Binding.constant(Date.now)
                )
            }
            VStack {
                Text("")
                Spacer(minLength: 0)
                SendMessageView(
                    composeState: $composeStateEditing,
                    selectedRange: $selectedRangeEditing,
                    sendMessage: { _ in },
                    onMediaAdded: { _ in },
                    keyboardVisible: Binding.constant(true),
                    keyboardHiddenDate: Binding.constant(Date.now)
                )
            }
        }
    }
}
