//
//  ComposeView.swift
//  SimpleX
//
//  Created by Evgeny on 13/03/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import SwiftyGif
import PhotosUI

enum ComposePreview {
    case noPreview
    case linkPreview(linkPreview: LinkPreview?)
    case imagePreviews(imagePreviews: [(String, UploadContent?)])
    case voicePreview(recordingFileName: String, duration: Int)
    case filePreview(fileName: String, file: URL)
}

enum ComposeContextItem {
    case noContextItem
    case quotedItem(chatItem: ChatItem)
    case editingItem(chatItem: ChatItem)
}

enum VoiceMessageRecordingState {
    case noRecording
    case recording
    case finished
}

struct LiveMessage {
    var chatItem: ChatItem
    var typedMsg: String
    var sentMsg: String?
}

struct ComposeState {
    var message: String
    var liveMessage: LiveMessage? = nil
    var preview: ComposePreview
    var contextItem: ComposeContextItem
    var voiceMessageRecordingState: VoiceMessageRecordingState
    var inProgress = false
    var disabled = false
    var useLinkPreviews: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_LINK_PREVIEWS)

    init(
        message: String = "",
        liveMessage: LiveMessage? = nil,
        preview: ComposePreview = .noPreview,
        contextItem: ComposeContextItem = .noContextItem,
        voiceMessageRecordingState: VoiceMessageRecordingState = .noRecording
    ) {
        self.message = message
        self.liveMessage = liveMessage
        self.preview = preview
        self.contextItem = contextItem
        self.voiceMessageRecordingState = voiceMessageRecordingState
    }

    init(editingItem: ChatItem) {
        self.message = editingItem.content.text
        self.preview = chatItemPreview(chatItem: editingItem)
        self.contextItem = .editingItem(chatItem: editingItem)
        if let emc = editingItem.content.msgContent,
           case .voice = emc {
            self.voiceMessageRecordingState = .finished
        } else {
            self.voiceMessageRecordingState = .noRecording
        }
    }

    func copy(
        message: String? = nil,
        liveMessage: LiveMessage? = nil,
        preview: ComposePreview? = nil,
        contextItem: ComposeContextItem? = nil,
        voiceMessageRecordingState: VoiceMessageRecordingState? = nil
    ) -> ComposeState {
        ComposeState(
            message: message ?? self.message,
            liveMessage: liveMessage ?? self.liveMessage,
            preview: preview ?? self.preview,
            contextItem: contextItem ?? self.contextItem,
            voiceMessageRecordingState: voiceMessageRecordingState ?? self.voiceMessageRecordingState
        )
    }

    var editing: Bool {
        switch contextItem {
        case .editingItem: return true
        default: return false
        }
    }

    var quoting: Bool {
        switch contextItem {
        case .quotedItem: return true
        default: return false
        }
    }

    var sendEnabled: Bool {
        switch preview {
        case .imagePreviews: return true
        case .voicePreview: return voiceMessageRecordingState == .finished
        case .filePreview: return true
        default: return !message.isEmpty || liveMessage != nil
        }
    }

    var endLiveDisabled: Bool {
        liveMessage != nil && message.isEmpty && noPreview && !quoting
    }

    var linkPreviewAllowed: Bool {
        switch preview {
        case .imagePreviews: return false
        case .voicePreview: return false
        case .filePreview: return false
        default: return useLinkPreviews
        }
    }

    var linkPreview: LinkPreview? {
        switch preview {
        case let .linkPreview(linkPreview): return linkPreview
        default: return nil
        }
    }

    var voiceMessageRecordingFileName: String? {
        switch preview {
        case let .voicePreview(recordingFileName: recordingFileName, _): return recordingFileName
        default: return nil
        }
    }

    var noPreview: Bool {
        switch preview {
        case .noPreview: return true
        default: return false
        }
    }

    var voicePreview: Bool {
        switch preview {
        case .voicePreview: return true
        default: return false
        }
    }

    var attachmentDisabled: Bool {
        if editing || liveMessage != nil { return true }
        switch preview {
        case .noPreview: return false
        case .linkPreview: return false
        default: return true
        }
    }

    var empty: Bool {
        message == "" && noPreview
    }
}

func chatItemPreview(chatItem: ChatItem) -> ComposePreview {
    let chatItemPreview: ComposePreview
    switch chatItem.content.msgContent {
    case .text:
        chatItemPreview = .noPreview
    case let .link(_, preview: preview):
        chatItemPreview = .linkPreview(linkPreview: preview)
    case let .image(_, image):
        chatItemPreview = .imagePreviews(imagePreviews: [(image, nil)])
    case let .voice(_, duration):
        chatItemPreview = .voicePreview(recordingFileName: chatItem.file?.fileName ?? "", duration: duration)
    case .file:
        let fileName = chatItem.file?.fileName ?? ""
        chatItemPreview = .filePreview(fileName: fileName, file: getAppFilePath(fileName))
    default:
        chatItemPreview = .noPreview
    }
    return chatItemPreview
}

enum UploadContent: Equatable {
    case simpleImage(image: UIImage)
    case animatedImage(image: UIImage)

    var uiImage: UIImage {
        switch self {
        case let .simpleImage(image): return image
        case let .animatedImage(image): return image
        }
    }

    static func loadFromURL(url: URL) -> UploadContent? {
        do {
            let data = try Data(contentsOf: url)
            if let image = UIImage(data: data) {
                try image.setGifFromData(data, levelOfIntegrity: 1.0)
                logger.log("UploadContent: added animated image")
                return .animatedImage(image: image)
            } else { return nil }
        } catch {
            do {
                if let image = try UIImage(data: Data(contentsOf: url)) {
                    logger.log("UploadContent: added simple image")
                    return .simpleImage(image: image)
                }
            } catch {}
        }
        return nil
    }
}

struct ComposeView: View {
    @EnvironmentObject var chatModel: ChatModel
    @ObservedObject var chat: Chat
    @Binding var composeState: ComposeState
    @FocusState.Binding var keyboardVisible: Bool

    @State var linkUrl: URL? = nil
    @State var prevLinkUrl: URL? = nil
    @State var pendingLinkUrl: URL? = nil
    @State var cancelledLinks: Set<String> = []

    @State private var showChooseSource = false
    @State private var showImagePicker = false
    @State private var showTakePhoto = false
    @State var chosenImages: [UploadContent] = []
    @State private var showFileImporter = false

    @State private var audioRecorder: AudioRecorder?
    @State private var voiceMessageRecordingTime: TimeInterval?
    @State private var startingRecording: Bool = false
    // for some reason voice message preview playback occasionally
    // fails to stop on ComposeVoiceView.playbackMode().onDisappear,
    // this is a workaround to fire an explicit event in certain cases
    @State private var stopPlayback: Bool = false

    var body: some View {
        VStack(spacing: 0) {
            contextItemView()
            switch (composeState.editing, composeState.preview) {
            case (true, .filePreview): EmptyView()
            case (true, .voicePreview): EmptyView() // ? we may allow playback when editing is allowed
            default: previewView()
            }
            HStack (alignment: .bottom) {
                Button {
                    showChooseSource = true
                } label: {
                    Image(systemName: "paperclip")
                        .resizable()
                }
                .disabled(composeState.attachmentDisabled)
                .frame(width: 25, height: 25)
                .padding(.bottom, 12)
                .padding(.leading, 12)
                SendMessageView(
                    composeState: $composeState,
                    sendMessage: {
                        sendMessage()
                        resetLinkPreview()
                    },
                    sendLiveMessage: sendLiveMessage,
                    updateLiveMessage: updateLiveMessage,
                    cancelLiveMessage: {
                        composeState.liveMessage = nil
                        chatModel.removeLiveDummy()
                    },
                    voiceMessageAllowed: chat.chatInfo.featureEnabled(.voice),
                    showEnableVoiceMessagesAlert: chat.chatInfo.showEnableVoiceMessagesAlert,
                    startVoiceMessageRecording: {
                        Task {
                            await startVoiceMessageRecording()
                        }
                    },
                    finishVoiceMessageRecording: finishVoiceMessageRecording,
                    allowVoiceMessagesToContact: allowVoiceMessagesToContact,
                    onImagesAdded: { images in if !images.isEmpty { chosenImages = images }},
                    keyboardVisible: $keyboardVisible
                )
                .padding(.trailing, 12)
                .background(.background)
            }
        }
        .onChange(of: composeState.message) { _ in
            if composeState.linkPreviewAllowed {
                if composeState.message.count > 0 {
                    showLinkPreview(composeState.message)
                } else {
                    resetLinkPreview()
                }
            }
        }
        .confirmationDialog("Attach", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                showTakePhoto = true
            }
            Button("Choose from library") {
                showImagePicker = true
            }
            if UIPasteboard.general.hasImages {
                Button("Paste image") {
                    UIPasteboard.general.itemProviders.forEach { p in
                        if p.hasItemConformingToTypeIdentifier(UTType.data.identifier) {
                            p.loadFileRepresentation(forTypeIdentifier: UTType.data.identifier) { url, error in
                                if let url = url, let image = UploadContent.loadFromURL(url: url) {
                                    chosenImages.append(image)
                                }
                            }
                        }
                    }
                }
            }
            Button("Choose file") {
                showFileImporter = true
            }
        }
        .fullScreenCover(isPresented: $showTakePhoto) {
            ZStack {
                Color.black.edgesIgnoringSafeArea(.all)
                CameraImageListPicker(images: $chosenImages)
            }
        }
        .sheet(isPresented: $showImagePicker) {
            LibraryImageListPicker(images: $chosenImages, selectionLimit: 10) { itemsSelected in
                showImagePicker = false
                if itemsSelected {
                    DispatchQueue.main.async {
                        composeState = composeState.copy(preview: .imagePreviews(imagePreviews: []))
                    }
                }
            }
        }
        .onChange(of: chosenImages) { images in
            Task {
                var imgs: [(String, UploadContent)] = []
                for image in images {
                    if let img = resizeImageToStrSize(image.uiImage, maxDataSize: 14000) {
                        imgs.append((img, image))
                        await MainActor.run {
                            composeState = composeState.copy(preview: .imagePreviews(imagePreviews: imgs))
                        }
                    }
                }
                if imgs.count == 0 {
                    await MainActor.run {
                        composeState = composeState.copy(preview: .noPreview)
                    }
                }
            }
        }
        .fileImporter(
            isPresented: $showFileImporter,
            allowedContentTypes: [.data],
            allowsMultipleSelection: false
        ) { result in
            if case let .success(files) = result, let fileURL = files.first {
                do {
                    var fileSize: Int? = nil
                    if fileURL.startAccessingSecurityScopedResource() {
                        let resourceValues = try fileURL.resourceValues(forKeys: [.fileSizeKey])
                        fileSize = resourceValues.fileSize
                    }
                    fileURL.stopAccessingSecurityScopedResource()
                    if let fileSize = fileSize,
                        fileSize <= MAX_FILE_SIZE {
                        composeState = composeState.copy(preview: .filePreview(fileName: fileURL.lastPathComponent, file: fileURL))
                    } else {
                        let prettyMaxFileSize = ByteCountFormatter().string(fromByteCount: MAX_FILE_SIZE)
                        AlertManager.shared.showAlertMsg(
                            title: "Large file!",
                            message: "Currently maximum supported file size is \(prettyMaxFileSize)."
                        )
                    }
                } catch {
                    logger.error("ComposeView fileImporter error \(error.localizedDescription)")
                }
            }
        }
        .onDisappear {
            if composeState.liveMessage != nil
                && (!composeState.message.isEmpty || composeState.liveMessage?.sentMsg != nil) {
                cancelCurrentVoiceRecording()
                clearCurrentDraft()
                sendMessage()
                resetLinkPreview()
            } else if !composeState.empty  {
                saveCurrentDraft()
            } else {
                cancelCurrentVoiceRecording()
                clearCurrentDraft()
                clearState()
            }
            chatModel.removeLiveDummy(animated: false)
        }
        .onChange(of: chatModel.stopPreviousRecPlay) { _ in
            if !startingRecording {
                if composeState.voiceMessageRecordingState == .recording {
                    finishVoiceMessageRecording()
                }
            } else {
                startingRecording = false
            }
        }
        .onChange(of: chat.chatInfo.featureEnabled(.voice)) { vmAllowed in
            if !vmAllowed && composeState.voicePreview,
               let fileName = composeState.voiceMessageRecordingFileName {
                cancelVoiceMessageRecording(fileName)
                clearState()
            }
        }
        .onAppear {
            if case let .voicePreview(_, duration) = composeState.preview {
                voiceMessageRecordingTime = TimeInterval(duration)
            }
        }
    }

    private func sendLiveMessage() async {
        let typedMsg = composeState.message
        let lm = composeState.liveMessage
        if (composeState.sendEnabled || composeState.quoting)
            && (lm == nil || lm?.sentMsg == nil),
           let ci = await sendMessageAsync(typedMsg, live: true) {
            await MainActor.run {
                composeState = composeState.copy(liveMessage: LiveMessage(chatItem: ci, typedMsg: typedMsg, sentMsg: typedMsg))
            }
        } else if lm == nil {
            let cItem = chatModel.addLiveDummy(chat.chatInfo)
            await MainActor.run {
                composeState = composeState.copy(liveMessage: LiveMessage(chatItem: cItem, typedMsg: typedMsg, sentMsg: nil))
            }
        }
    }

    private func updateLiveMessage() async {
        let typedMsg = composeState.message
        if let liveMessage = composeState.liveMessage {
            if let sentMsg = liveMessageToSend(liveMessage, typedMsg),
               let ci = await sendMessageAsync(sentMsg, live: true) {
                await MainActor.run {
                    composeState = composeState.copy(liveMessage: LiveMessage(chatItem: ci, typedMsg: typedMsg, sentMsg: sentMsg))
                }
            } else if liveMessage.typedMsg != typedMsg {
                await MainActor.run {
                    var lm = liveMessage
                    lm.typedMsg = typedMsg
                    composeState = composeState.copy(liveMessage: lm)
                }
            }
        }
    }

    private func liveMessageToSend(_ lm: LiveMessage, _ t: String) -> String? {
        let s = t != lm.typedMsg ? truncateToWords(t) : t
        return s != lm.sentMsg && (lm.sentMsg != nil || !s.isEmpty) ? s : nil
    }

    private func truncateToWords(_ s: String) -> String {
        var acc = ""
        var word = ""
        for c in s {
            if c.isLetter || c.isNumber {
                word = word + String(c)
            } else {
                acc = acc + word + String(c)
                word = ""
            }
        }
        return acc
    }

    @ViewBuilder func previewView() -> some View {
        switch composeState.preview {
        case .noPreview:
            EmptyView()
        case let .linkPreview(linkPreview: preview):
            ComposeLinkView(linkPreview: preview, cancelPreview: cancelLinkPreview)
        case let .imagePreviews(imagePreviews: images):
            ComposeImageView(
                images: images.map { (img, _) in img },
                cancelImage: {
                    composeState = composeState.copy(preview: .noPreview)
                    chosenImages = []
                },
                cancelEnabled: !composeState.editing)
        case let .voicePreview(recordingFileName, _):
            ComposeVoiceView(
                recordingFileName: recordingFileName,
                recordingTime: $voiceMessageRecordingTime,
                recordingState: $composeState.voiceMessageRecordingState,
                cancelVoiceMessage: {
                    cancelVoiceMessageRecording($0)
                    clearState()
                },
                cancelEnabled: !composeState.editing,
                stopPlayback: $stopPlayback
            )
        case let .filePreview(fileName, _):
            ComposeFileView(
                fileName: fileName,
                cancelFile: {
                    composeState = composeState.copy(preview: .noPreview)
                },
                cancelEnabled: !composeState.editing)
        }
    }

    @ViewBuilder private func contextItemView() -> some View {
        switch composeState.contextItem {
        case .noContextItem:
            EmptyView()
        case let .quotedItem(chatItem: quotedItem):
            ContextItemView(
                contextItem: quotedItem,
                contextIcon: "arrowshape.turn.up.left",
                cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) }
            )
        case let .editingItem(chatItem: editingItem):
            ContextItemView(
                contextItem: editingItem,
                contextIcon: "pencil",
                cancelContextItem: { clearState() }
            )
        }
    }

    private func sendMessage() {
        logger.debug("ChatView sendMessage")
        Task {
            logger.debug("ChatView sendMessage: in Task")
            _ = await sendMessageAsync(nil, live: false)
        }
    }

    private func sendMessageAsync(_ text: String?, live: Bool) async -> ChatItem? {
        var sent: ChatItem?
        let msgText = text ?? composeState.message
        let liveMessage = composeState.liveMessage
        if !live {
            if liveMessage != nil { composeState = composeState.copy(liveMessage: nil) }
            await sending()
        }
        if case let .editingItem(ci) = composeState.contextItem {
            sent = await updateMessage(ci, live: live)
        } else if let liveMessage = liveMessage, liveMessage.sentMsg != nil {
            sent = await updateMessage(liveMessage.chatItem, live: live)
        } else {
            var quoted: Int64? = nil
            if case let .quotedItem(chatItem: quotedItem) = composeState.contextItem {
                quoted = quotedItem.id
            }

            switch (composeState.preview) {
            case .noPreview:
                sent = await send(.text(msgText), quoted: quoted, live: live)
            case .linkPreview:
                sent = await send(checkLinkPreview(), quoted: quoted, live: live)
            case let .imagePreviews(imagePreviews: images):
                let last = images.count - 1
                if last >= 0 {
                    for i in 0..<last {
                        sent = await sendImage(images[i])
                        _ = try? await Task.sleep(nanoseconds: 100_000000)
                    }
                    sent = await sendImage(images[last], text: msgText, quoted: quoted, live: live)
                }
                if sent == nil {
                    sent = await send(.text(msgText), quoted: quoted, live: live)
                }
            case let .voicePreview(recordingFileName, duration):
                stopPlayback.toggle()
                chatModel.filesToDelete.removeAll { $0 == recordingFileName }
                sent = await send(.voice(text: msgText, duration: duration), quoted: quoted, file: recordingFileName)
            case let .filePreview(_, file):
                if let savedFile = saveFileFromURL(file) {
                    sent = await send(.file(msgText), quoted: quoted, file: savedFile, live: live)
                }
            }
        }
        await MainActor.run { clearState(live: live) }
        return sent

        func sending() async {
            await MainActor.run { composeState.disabled = true }
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                if composeState.disabled { composeState.inProgress = true }
            }
        }

        func updateMessage(_ ei: ChatItem, live: Bool) async -> ChatItem? {
            if let oldMsgContent = ei.content.msgContent {
                do {
                    let mc = updateMsgContent(oldMsgContent)
                    let chatItem = try await apiUpdateChatItem(
                        type: chat.chatInfo.chatType,
                        id: chat.chatInfo.apiId,
                        itemId: ei.id,
                        msg: mc,
                        live: live
                    )
                    await MainActor.run {
                        _ = self.chatModel.upsertChatItem(self.chat.chatInfo, chatItem)
                    }
                    return chatItem
                } catch {
                    logger.error("ChatView.sendMessage error: \(error.localizedDescription)")
                    AlertManager.shared.showAlertMsg(title: "Error updating message", message: "Error: \(responseError(error))")
                }
            }
            return nil
        }

        func updateMsgContent(_ msgContent: MsgContent) -> MsgContent {
            switch msgContent {
            case .text:
                return checkLinkPreview()
            case .link:
                return checkLinkPreview()
            case .image(_, let image):
                return .image(text: msgText, image: image)
            case .voice(_, let duration):
                return .voice(text: msgText, duration: duration)
            case .file:
                return .file(msgText)
            case .unknown(let type, _):
                return .unknown(type: type, text: msgText)
            }
        }

        func sendImage(_ imageData: (String, UploadContent?), text: String = "", quoted: Int64? = nil, live: Bool = false) async -> ChatItem? {
            let (image, data) = imageData
            if let data = data, let savedFile = saveAnyImage(data) {
                return await send(.image(text: text, image: image), quoted: quoted, file: savedFile, live: live)
            }
            return nil
        }

        func send(_ mc: MsgContent, quoted: Int64?, file: String? = nil, live: Bool = false) async -> ChatItem? {
            if let chatItem = await apiSendMessage(
                type: chat.chatInfo.chatType,
                id: chat.chatInfo.apiId,
                file: file,
                quotedItemId: quoted,
                msg: mc,
                live: live
            ) {
                await MainActor.run {
                    chatModel.removeLiveDummy(animated: false)
                    chatModel.addChatItem(chat.chatInfo, chatItem)
                }
                return chatItem
            }
            return nil
        }

        func checkLinkPreview() -> MsgContent {
            switch (composeState.preview) {
            case let .linkPreview(linkPreview: linkPreview):
                if let url = parseMessage(msgText),
                   let linkPreview = linkPreview,
                   url == linkPreview.uri {
                    return .link(text: msgText, preview: linkPreview)
                } else {
                    return .text(msgText)
                }
            default:
                return .text(msgText)
            }
        }

        func saveAnyImage(_ img: UploadContent) -> String? {
            switch img {
            case let .simpleImage(image): return saveImage(image)
            case let .animatedImage(image): return saveAnimImage(image)
            }
        }
    }

    private func startVoiceMessageRecording() async {
        startingRecording = true
        chatModel.stopPreviousRecPlay.toggle()
        let fileName = generateNewFileName("voice", "m4a")
        audioRecorder = AudioRecorder(
            onTimer: { voiceMessageRecordingTime = $0 },
            onFinishRecording: {
                updateComposeVMRFinished()
                if let fileSize = fileSize(getAppFilePath(fileName)) {
                    logger.debug("onFinishRecording recording file size = \(fileSize)")
                }
            }
        )
        if let recStartError = await audioRecorder?.start(fileName: fileName) {
            switch recStartError {
            case .permission:
                AlertManager.shared.showAlert(Alert(
                    title: Text("No permission to record voice message"),
                    message: Text("To record voice message please grant permission to use Microphone."),
                    primaryButton: .default(Text("Open Settings")) {
                        DispatchQueue.main.async {
                            UIApplication.shared.open(URL(string: UIApplication.openSettingsURLString)!, options: [:], completionHandler: nil)
                        }
                    },
                    secondaryButton: .cancel()
                ))
            case let .error(error):
                AlertManager.shared.showAlertMsg(
                    title: "Unable to record voice message",
                    message: "Error: \(error)"
                )
            }
        } else {
            composeState = composeState.copy(
                preview: .voicePreview(recordingFileName: fileName, duration: 0),
                voiceMessageRecordingState: .recording
            )
        }
    }

    private func finishVoiceMessageRecording() {
        audioRecorder?.stop()
        audioRecorder = nil
        updateComposeVMRFinished()
        if let fileName = composeState.voiceMessageRecordingFileName,
           let fileSize = fileSize(getAppFilePath(fileName)) {
            logger.debug("finishVoiceMessageRecording recording file size = \(fileSize)")
        }
    }

    private func allowVoiceMessagesToContact() {
        if case let .direct(contact) = chat.chatInfo {
            allowFeatureToContact(contact, .voice)
        }
    }

    // ? maybe we shouldn't have duration in ComposePreview.voicePreview
    private func updateComposeVMRFinished() {
        var preview = composeState.preview
        if let recordingFileName = composeState.voiceMessageRecordingFileName,
           let recordingTime = voiceMessageRecordingTime {
            preview = .voicePreview(recordingFileName: recordingFileName, duration: Int(recordingTime.rounded()))
        }
        composeState = composeState.copy(
            preview: preview,
            voiceMessageRecordingState: .finished
        )
    }

    private func cancelCurrentVoiceRecording() {
        if let fileName = composeState.voiceMessageRecordingFileName {
            cancelVoiceMessageRecording(fileName)
        }
    }

    private func cancelVoiceMessageRecording(_ fileName: String) {
        stopPlayback.toggle()
        audioRecorder?.stop()
        removeFile(fileName)
    }

    private func clearState(live: Bool = false) {
        if live {
            composeState.disabled = false
            composeState.inProgress = false
        } else {
            composeState = ComposeState()
            resetLinkPreview()
        }
        chosenImages = []
        audioRecorder = nil
        voiceMessageRecordingTime = nil
        startingRecording = false
    }

    private func saveCurrentDraft() {
        if case .recording = composeState.voiceMessageRecordingState {
            finishVoiceMessageRecording()
            if let fileName = composeState.voiceMessageRecordingFileName {
                chatModel.filesToDelete.append(fileName)
            }
        }
        chatModel.draft = composeState
        chatModel.draftChatId = chat.id
    }

    private func clearCurrentDraft() {
        if chatModel.draftChatId == chat.id {
            chatModel.draft = nil
            chatModel.draftChatId = nil
        }
    }

    private func showLinkPreview(_ s: String) {
        prevLinkUrl = linkUrl
        linkUrl = parseMessage(s)
        if let url = linkUrl {
            if url != composeState.linkPreview?.uri && url != pendingLinkUrl {
                pendingLinkUrl = url
                if prevLinkUrl == url {
                    loadLinkPreview(url)
                } else {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) {
                        loadLinkPreview(url)
                    }
                }
            }
        } else {
            composeState = composeState.copy(preview: .noPreview)
        }
    }

    private func parseMessage(_ msg: String) -> URL? {
        let parsedMsg = parseSimpleXMarkdown(msg)
        let uri = parsedMsg?.first(where: { ft in
            ft.format == .uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text)
        })
        if let uri = uri { return URL(string: uri.text) }
        else { return nil }
    }

    private func isSimplexLink(_ link: String) -> Bool {
        link.starts(with: "https://simplex.chat") || link.starts(with: "http://simplex.chat")
    }

    private func cancelLinkPreview() {
        if let uri = composeState.linkPreview?.uri.absoluteString {
            cancelledLinks.insert(uri)
        }
        pendingLinkUrl = nil
        composeState = composeState.copy(preview: .noPreview)
    }

    private func loadLinkPreview(_ url: URL) {
        if pendingLinkUrl == url {
            composeState = composeState.copy(preview: .linkPreview(linkPreview: nil))
            getLinkPreview(url: url) { linkPreview in
                if let linkPreview = linkPreview,
                   pendingLinkUrl == url {
                    composeState = composeState.copy(preview: .linkPreview(linkPreview: linkPreview))
                    pendingLinkUrl = nil
                }
            }
        }
    }

    private func resetLinkPreview() {
        linkUrl = nil
        prevLinkUrl = nil
        pendingLinkUrl = nil
        cancelledLinks = []
    }
}

struct ComposeView_Previews: PreviewProvider {
    static var previews: some View {
        let chat = Chat(chatInfo: ChatInfo.sampleData.direct, chatItems: [])
        @State var composeState = ComposeState(message: "hello")
        @FocusState var keyboardVisible: Bool

        return Group {
            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
            .environmentObject(ChatModel())
            ComposeView(
                chat: chat,
                composeState: $composeState,
                keyboardVisible: $keyboardVisible
            )
            .environmentObject(ChatModel())
        }
    }
}
