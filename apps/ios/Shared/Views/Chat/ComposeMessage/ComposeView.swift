
import SwiftUI
import SimpleXChat
import SwiftyGif
import PhotosUI

let MAX_NUMBER_OF_MENTIONS = 3

enum ComposePreview {
    case noPreview
    case linkPreview(linkPreview: LinkPreview?)
    case mediaPreviews(mediaPreviews: [(String, UploadContent?)])
    case voicePreview(recordingFileName: String, duration: Int)
    case filePreview(fileName: String, file: URL)
}

enum ComposeContextItem: Equatable {
    case noContextItem
    case quotedItem(chatItem: ChatItem)
    case editingItem(chatItem: ChatItem)
    case forwardingItems(chatItems: [ChatItem], fromChatInfo: ChatInfo)
    case reportedItem(chatItem: ChatItem, reason: ReportReason)
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

typealias MentionedMembers = [String: CIMention]

struct ComposeState {
    var message: String
    var parsedMessage: [FormattedText]
    var liveMessage: LiveMessage? = nil
    var preview: ComposePreview
    var contextItem: ComposeContextItem
    var voiceMessageRecordingState: VoiceMessageRecordingState
    var inProgress = false
    var progressByTimeout = false
    var useLinkPreviews: Bool = UserDefaults.standard.bool(forKey: DEFAULT_PRIVACY_LINK_PREVIEWS)
    var mentions: MentionedMembers = [:]

    init(
        message: String = "",
        parsedMessage: [FormattedText] = [],
        liveMessage: LiveMessage? = nil,
        preview: ComposePreview = .noPreview,
        contextItem: ComposeContextItem = .noContextItem,
        voiceMessageRecordingState: VoiceMessageRecordingState = .noRecording,
        mentions: MentionedMembers = [:]
    ) {
        self.message = message
        self.parsedMessage = parsedMessage
        self.liveMessage = liveMessage
        self.preview = preview
        self.contextItem = contextItem
        self.voiceMessageRecordingState = voiceMessageRecordingState
        self.mentions = mentions
    }

    init(editingItem: ChatItem) {
        let text = editingItem.content.text
        self.message = text
        self.parsedMessage = editingItem.formattedText ?? FormattedText.plain(text)
        self.preview = chatItemPreview(chatItem: editingItem)
        self.contextItem = .editingItem(chatItem: editingItem)
        if let emc = editingItem.content.msgContent,
           case .voice = emc {
            self.voiceMessageRecordingState = .finished
        } else {
            self.voiceMessageRecordingState = .noRecording
        }
        self.mentions = editingItem.mentions ?? [:]
    }

    init(forwardingItems: [ChatItem], fromChatInfo: ChatInfo) {
        self.message = ""
        self.parsedMessage = []
        self.preview = .noPreview
        self.contextItem = .forwardingItems(chatItems: forwardingItems, fromChatInfo: fromChatInfo)
        self.voiceMessageRecordingState = .noRecording
    }

    func copy(
        message: String? = nil,
        parsedMessage: [FormattedText]? = nil,
        liveMessage: LiveMessage? = nil,
        preview: ComposePreview? = nil,
        contextItem: ComposeContextItem? = nil,
        voiceMessageRecordingState: VoiceMessageRecordingState? = nil,
        mentions: MentionedMembers? = nil
    ) -> ComposeState {
        ComposeState(
            message: message ?? self.message,
            parsedMessage: parsedMessage ?? self.parsedMessage,
            liveMessage: liveMessage ?? self.liveMessage,
            preview: preview ?? self.preview,
            contextItem: contextItem ?? self.contextItem,
            voiceMessageRecordingState: voiceMessageRecordingState ?? self.voiceMessageRecordingState,
            mentions: mentions ?? self.mentions
        )
    }

    func mentionMemberName(_ name: String) -> String {
        var n = 0
        var tryName = name
        while mentions[tryName] != nil {
            n += 1
            tryName = "\(name)_\(n)"
        }
        return tryName
    }

    var memberMentions: [String: Int64] {
        self.mentions.compactMapValues { $0.memberRef?.groupMemberId }
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

    var forwarding: Bool {
        switch contextItem {
        case .forwardingItems: return true
        default: return false
        }
    }

    var reporting: Bool {
        switch contextItem {
        case .reportedItem: return true
        default: return false
        }
    }

    var submittingValidReport: Bool {
        switch contextItem {
        case let .reportedItem(_, reason):
            switch reason {
            case .other: return !message.isEmpty
            default: return true
            }
        default: return false
        }
    }

    var sendEnabled: Bool {
        switch preview {
        case let .mediaPreviews(media): return !media.isEmpty
        case .voicePreview: return voiceMessageRecordingState == .finished
        case .filePreview: return true
        default: return !whitespaceOnly || forwarding || liveMessage != nil || submittingValidReport
        }
    }

    var endLiveDisabled: Bool {
        liveMessage != nil && message.isEmpty && noPreview && !quoting
    }

    var linkPreviewAllowed: Bool {
        switch preview {
        case .mediaPreviews: return false
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

    var manyMediaPreviews: Bool {
        switch preview {
        case let .mediaPreviews(mediaPreviews): return mediaPreviews.count > 1
        default: return false
        }
    }

    var attachmentDisabled: Bool {
        if editing || forwarding || liveMessage != nil || inProgress || reporting { return true }
        switch preview {
        case .noPreview: return false
        case .linkPreview: return false
        default: return true
        }
    }

    var attachmentPreview: Bool {
        switch preview {
        case .noPreview: false
        case .linkPreview: false
        case let .mediaPreviews(mediaPreviews): !mediaPreviews.isEmpty
        case .voicePreview: false
        case .filePreview: true
        }
    }

    var placeholder: String? {
        switch contextItem {
        case let .reportedItem(_, reason):
            return reason.text
        default:
            return nil
        }
    }

    var empty: Bool {
        whitespaceOnly && noPreview
    }

    var whitespaceOnly: Bool {
        message.allSatisfy { $0.isWhitespace }
    }
}

func chatItemPreview(chatItem: ChatItem) -> ComposePreview {
    switch chatItem.content.msgContent {
    case .text:
        return .noPreview
    case let .link(_, preview: preview):
        return .linkPreview(linkPreview: preview)
    case let .image(_, image):
        return .mediaPreviews(mediaPreviews: [(image, nil)])
    case let .video(_, image, _):
        return .mediaPreviews(mediaPreviews: [(image, nil)])
    case let .voice(_, duration):
        return .voicePreview(recordingFileName: chatItem.file?.fileName ?? "", duration: duration)
    case .file:
        let fileName = chatItem.file?.fileName ?? ""
        return .filePreview(fileName: fileName, file: getAppFilePath(fileName))
    default:
        return .noPreview
    }
}

enum UploadContent: Equatable {
    case simpleImage(image: UIImage)
    case animatedImage(image: UIImage)
    case video(image: UIImage, url: URL, duration: Int)

    var uiImage: UIImage {
        switch self {
        case let .simpleImage(image): return image
        case let .animatedImage(image): return image
        case let .video(image, _, _): return image
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

    static func loadVideoFromURL(url: URL) -> UploadContent? {
        let asset = AVAsset(url: url)
        if let (image, duration) = asset.generatePreview() {
            return .video(image: image, url: url, duration: duration)
        }
        return nil
    }
}

struct ComposeView: View {
    @EnvironmentObject var chatModel: ChatModel
    @EnvironmentObject var theme: AppTheme
    @ObservedObject var chat: Chat
    @ObservedObject var im: ItemsModel
    @Binding var composeState: ComposeState
    @Binding var keyboardVisible: Bool
    @Binding var keyboardHiddenDate: Date
    @Binding var selectedRange: NSRange
    var disabledText: LocalizedStringKey? = nil

    @State var linkUrl: URL? = nil
    @State var hasSimplexLink: Bool = false
    @State var prevLinkUrl: URL? = nil
    @State var pendingLinkUrl: URL? = nil
    @State var cancelledLinks: Set<String> = []

    @Environment(\.colorScheme) private var colorScheme
    @State private var showChooseSource = false
    @State private var showMediaPicker = false
    @State private var showTakePhoto = false
    @State var chosenMedia: [UploadContent] = []
    @State private var showFileImporter = false

    @State private var audioRecorder: AudioRecorder?
    @State private var voiceMessageRecordingTime: TimeInterval?
    @State private var startingRecording: Bool = false
    // for some reason voice message preview playback occasionally
    // fails to stop on ComposeVoiceView.playbackMode().onDisappear,
    // this is a workaround to fire an explicit event in certain cases
    @State private var stopPlayback: Bool = false

    @UserDefault(DEFAULT_PRIVACY_SAVE_LAST_DRAFT) private var saveLastDraft = true
    @UserDefault(DEFAULT_TOOLBAR_MATERIAL) private var toolbarMaterial = ToolbarMaterial.defaultMaterial
    @AppStorage(GROUP_DEFAULT_INCOGNITO, store: groupDefaults) private var incognitoDefault = false

    var body: some View {
        VStack(spacing: 0) {
            Divider()

            if chat.chatInfo.nextConnectPrepared,
               let user = chatModel.currentUser {
                ContextProfilePickerView(
                    chat: chat,
                    selectedUser: user
                )
                Divider()
            }

            if let groupInfo = chat.chatInfo.groupInfo,
               case let .groupChatScopeContext(groupScopeInfo) = im.secondaryIMFilter,
               case let .memberSupport(member) = groupScopeInfo,
               let member = member,
               member.memberPending,
               composeState.contextItem == .noContextItem,
               composeState.noPreview {
                ContextPendingMemberActionsView(
                    groupInfo: groupInfo,
                    member: member
                )
                Divider()
            }

            if case let .reportedItem(_, reason) = composeState.contextItem {
                reportReasonView(reason)
                Divider()
            }
            // preference checks should match checks in forwarding list
            let simplexLinkProhibited = im.secondaryIMFilter == nil && hasSimplexLink && !chat.groupFeatureEnabled(.simplexLinks)
            let fileProhibited = im.secondaryIMFilter == nil && composeState.attachmentPreview && !chat.groupFeatureEnabled(.files)
            let voiceProhibited = composeState.voicePreview && !chat.chatInfo.featureEnabled(.voice)
            let disableSendButton = simplexLinkProhibited || fileProhibited || voiceProhibited
            if simplexLinkProhibited {
                msgNotAllowedView("SimpleX links not allowed", icon: "link")
                Divider()
            } else if fileProhibited {
                msgNotAllowedView("Files and media not allowed", icon: "doc")
                Divider()
            } else if voiceProhibited {
                msgNotAllowedView("Voice messages not allowed", icon: "mic")
                Divider()
            }
            contextItemView()
            switch (composeState.editing, composeState.preview) {
            case (true, .filePreview): EmptyView()
            case (true, .voicePreview): EmptyView() // ? we may allow playback when editing is allowed
            default: previewView()
            }

            let contact = chat.chatInfo.contact

            if chat.chatInfo.groupInfo?.nextConnectPrepared == true {
                if chat.chatInfo.groupInfo?.businessChat == nil {
                    Button(action: connectPreparedGroup) {
                        ZStack(alignment: .trailing) {
                            Label("Join group", systemImage: "person.2.fill")
                                .frame(maxWidth: .infinity)
                            if composeState.progressByTimeout {
                                ProgressView()
                                    .padding()
                            }
                        }
                    }
                    .frame(height: 60)
                    .disabled(composeState.inProgress)
                } else {
                    sendContactRequestView(disableSendButton, icon: "briefcase.fill", sendRequest: connectPreparedGroup)
                }
            } else if contact?.nextSendGrpInv == true {
                contextSendMessageToConnect("Send direct message to connect")
                Divider()
                HStack (alignment: .center) {
                    attachmentButton().disabled(true)
                    sendMessageView(disableSendButton, sendToConnect: sendMemberContactInvitation)
                }
                .padding(.horizontal, 12)
            } else if contact?.nextConnectPrepared == true, let linkType = contact?.preparedContact?.uiConnLinkType {
                switch linkType {
                case .inv:
                    Button(action: sendConnectPreparedContact) {
                        ZStack(alignment: .trailing) {
                            Label("Connect", systemImage: "person.fill.badge.plus")
                                .frame(maxWidth: .infinity)
                            if composeState.progressByTimeout {
                                ProgressView()
                                    .padding()
                            }
                        }
                    }
                    .frame(height: 60)
                    .disabled(composeState.inProgress)
                case .con:
                    sendContactRequestView(disableSendButton, icon: "person.fill.badge.plus", sendRequest: sendConnectPreparedContactRequest)
                }
            } else if contact?.nextAcceptContactRequest == true, let crId = contact?.contactRequestId {
                ContextContactRequestActionsView(contactRequestId: crId)
            } else {
                HStack (alignment: .center) {
                    attachmentButton()
                    sendMessageView(disableSendButton)
                }
                .padding(.horizontal, 12)
            }
        }
        .background {
            Color.clear
                .overlay(ToolbarMaterial.material(toolbarMaterial))
                .ignoresSafeArea(.all, edges: .bottom)
        }
        .onChange(of: composeState.message) { msg in
            let parsedMsg = parseSimpleXMarkdown(msg)
            composeState = composeState.copy(parsedMessage: parsedMsg ?? FormattedText.plain(msg))
            if composeState.linkPreviewAllowed {
                if msg.count > 0 {
                    showLinkPreview(parsedMsg)
                } else {
                    resetLinkPreview()
                    hasSimplexLink = false
                }
            } else if msg.count > 0 && !chat.groupFeatureEnabled(.simplexLinks) {
                (_, hasSimplexLink) = getSimplexLink(parsedMsg)
            } else {
                hasSimplexLink = false
            }
        }
        .onChange(of: chat.chatInfo.sendMsgEnabled) { sendEnabled in
            if !sendEnabled {
                cancelCurrentVoiceRecording()
                clearCurrentDraft()
                clearState()
            }
        }
        .onChange(of: composeState.inProgress) { inProgress in
            if inProgress {
                DispatchQueue.main.asyncAfter(deadline: .now() + 1) {
                    composeState.progressByTimeout = composeState.inProgress
                }
            } else {
                composeState.progressByTimeout = false
            }
        }
        .confirmationDialog("Attach", isPresented: $showChooseSource, titleVisibility: .visible) {
            Button("Take picture") {
                showTakePhoto = true
            }
            Button("Choose from library") {
                showMediaPicker = true
            }
            if UIPasteboard.general.hasImages {
                Button("Paste image") {
                    UIPasteboard.general.itemProviders.forEach { p in
                        if p.hasItemConformingToTypeIdentifier(UTType.data.identifier) {
                            p.loadFileRepresentation(forTypeIdentifier: UTType.data.identifier) { url, error in
                                if let url = url, let image = UploadContent.loadFromURL(url: url) {
                                    chosenMedia.append(image)
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
                CameraImageListPicker(images: $chosenMedia)
            }
        }
        .sheet(isPresented: $showMediaPicker) {
            LibraryMediaListPicker(addMedia: addMediaContent, selectionLimit: 10, finishedPreprocessing: finishedPreprocessingMediaContent) { itemsSelected in
                await MainActor.run {
                    showMediaPicker = false
                    if itemsSelected {
                        composeState = composeState.copy(preview: .mediaPreviews(mediaPreviews: []))
                    }
                }
            }
        }
        .onChange(of: chosenMedia) { selected in
            Task {
                var media: [(String, UploadContent)] = []
                for content in selected {
                    if let img = await resizeImageToStrSize(content.uiImage, maxDataSize: 14000) {
                        media.append((img, content))
                        await MainActor.run {
                            composeState = composeState.copy(preview: .mediaPreviews(mediaPreviews: media))
                        }
                    }
                }
                if media.count == 0 {
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
                        fileSize <= maxFileSize {
                        composeState = composeState.copy(preview: .filePreview(fileName: fileURL.lastPathComponent, file: fileURL))
                    } else {
                        let prettyMaxFileSize = ByteCountFormatter.string(fromByteCount: maxFileSize, countStyle: .binary)
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
                sendMessage(ttl: nil)
                resetLinkPreview()
            } else if (composeState.inProgress) {
                clearCurrentDraft()
            } else if !composeState.empty  {
                if case .recording = composeState.voiceMessageRecordingState {
                    finishVoiceMessageRecording()
                    if let fileName = composeState.voiceMessageRecordingFileName {
                        chatModel.filesToDelete.insert(getAppFilePath(fileName))
                    }
                }
                if saveLastDraft {
                    saveCurrentDraft()
                }
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

    private func sendContactRequestView(_ disableSendButton: Bool, icon: String, sendRequest: @escaping () -> Void) -> some View {
        HStack (alignment: .center) {
            sendMessageView(
                disableSendButton,
                placeholder: NSLocalizedString("Add message", comment: "placeholder for sending contact request"),
                sendToConnect: sendRequest
            )
            if composeState.whitespaceOnly {
                Button(action: sendRequest) {
                    HStack {
                        Text("Connect").fontWeight(.medium)
                        Image(systemName: icon)
                    }
                }
                .padding(.horizontal, 8)
                .disabled(composeState.inProgress)
            }
        }
        .padding(.horizontal, 12)
    }

    private func sendMessageView(_ disableSendButton: Bool, placeholder: String? = nil, sendToConnect: (() -> Void)? = nil) -> some View {
        ZStack(alignment: .leading) {
            SendMessageView(
                placeholder: placeholder,
                composeState: $composeState,
                selectedRange: $selectedRange,
                sendMessage: { ttl in
                    sendMessage(ttl: ttl)
                    resetLinkPreview()
                },
                sendLiveMessage: chat.chatInfo.chatType != .local ? sendLiveMessage : nil,
                updateLiveMessage: updateLiveMessage,
                cancelLiveMessage: {
                    composeState.liveMessage = nil
                    chatModel.removeLiveDummy()
                },
                sendToConnect: sendToConnect,
                hideSendButton: chat.chatInfo.nextConnect && chat.chatInfo.contact?.nextSendGrpInv != true && composeState.whitespaceOnly,
                voiceMessageAllowed: chat.chatInfo.featureEnabled(.voice),
                disableSendButton: disableSendButton,
                showEnableVoiceMessagesAlert: chat.chatInfo.showEnableVoiceMessagesAlert,
                startVoiceMessageRecording: {
                    Task {
                        await startVoiceMessageRecording()
                    }
                },
                finishVoiceMessageRecording: finishVoiceMessageRecording,
                allowVoiceMessagesToContact: allowVoiceMessagesToContact,
                timedMessageAllowed: chat.chatInfo.featureEnabled(.timedMessages),
                onMediaAdded: { media in if !media.isEmpty { chosenMedia = media }},
                keyboardVisible: $keyboardVisible,
                keyboardHiddenDate: $keyboardHiddenDate,
                sendButtonColor: chat.chatInfo.incognito
                ? .indigo.opacity(colorScheme == .dark ? 1 : 0.7)
                : theme.colors.primary
            )
            .disabled(!chat.chatInfo.sendMsgEnabled)

            if let disabledText {
                Text(disabledText)
                    .italic()
                    .foregroundColor(theme.colors.secondary)
                    .padding(.horizontal, 12)
            }
        }
    }

    @ViewBuilder private func attachmentButton() -> some View {
        let b = Button {
            showChooseSource = true
        } label: {
            Image(systemName: "paperclip")
                .resizable()
        }
            .disabled(composeState.attachmentDisabled || !chat.chatInfo.sendMsgEnabled)
            .frame(width: 25, height: 25)
            .tint(theme.colors.primary)
        if im.secondaryIMFilter == nil,
           case let .group(g, _) = chat.chatInfo,
           !g.fullGroupPreferences.files.on(for: g.membership) {
            b.disabled(true).onTapGesture {
                AlertManager.shared.showAlertMsg(
                    title: "Files and media prohibited!",
                    message: "Only group owners can enable files and media."
                )
            }
        } else {
            b
        }
    }

    private func sendMemberContactInvitation() {
        Task {
            do {
                await MainActor.run { hideKeyboard() }
                if let mc = connectCheckLinkPreview() {
                    await sending()
                    let contact = try await apiSendMemberContactInvitation(chat.chatInfo.apiId, mc)
                    await MainActor.run {
                        self.chatModel.updateContact(contact)
                        clearState()
                        NetworkModel.shared.setContactNetworkStatus(contact, .connected)
                    }
                } else {
                    AlertManager.shared.showAlertMsg(title: "Empty message!")
                }
            } catch {
                await MainActor.run { composeState.inProgress = false }
                logger.error("ChatView.sendMemberContactInvitation error: \(error.localizedDescription)")
                AlertManager.shared.showAlertMsg(title: "Error sending member contact invitation", message: "Error: \(responseError(error))")
            }
        }
    }

    // TODO [short links] different messages for business
    private func sendConnectPreparedContactRequest() {
        hideKeyboard()
        let empty = composeState.whitespaceOnly
        AlertManager.shared.showAlert(Alert(
            title: Text("Send contact request?"),
            message: Text("You will be able to send messages **only after your request is accepted**."),
            primaryButton: .default(
                Text(empty ? "Send request without message" : "Send request"),
                action: sendConnectPreparedContact
            ),
            secondaryButton:
                empty
                ? .cancel(Text("Add message"), action: hideKeyboard)
                : .cancel()
        ))
    }

    private func sendConnectPreparedContact() {
        Task {
            await MainActor.run { hideKeyboard() }
            await sending()
            let mc = connectCheckLinkPreview()
            let incognito = chat.chatInfo.profileChangeProhibited ? chat.chatInfo.incognito : incognitoDefault
            if let contact = await apiConnectPreparedContact(contactId: chat.chatInfo.apiId, incognito: incognito, msg: mc) {
                await MainActor.run {
                    self.chatModel.updateContact(contact)
                    NetworkModel.shared.setContactNetworkStatus(contact, .connected)
                    clearState()
                }
            } else {
                await MainActor.run { composeState.inProgress = false }
            }
        }
    }

    private func connectPreparedGroup() {
        Task {
            await MainActor.run { hideKeyboard() }
            await sending()
            let mc = connectCheckLinkPreview()
            let incognito = chat.chatInfo.profileChangeProhibited ? chat.chatInfo.incognito : incognitoDefault
            if let groupInfo = await apiConnectPreparedGroup(groupId: chat.chatInfo.apiId, incognito: incognito, msg: mc) {
                await MainActor.run {
                    self.chatModel.updateGroup(groupInfo)
                    clearState()
                }
            } else {
                await MainActor.run { composeState.inProgress = false }
            }
        }
    }

    @inline(__always)
    private func connectCheckLinkPreview() -> MsgContent? {
        let msgText = composeState.message.trimmingCharacters(in: .whitespacesAndNewlines)
        return msgText.isEmpty ? nil : checkLinkPreview_(msgText)
    }

    @inline(__always)
    private func checkLinkPreview() -> MsgContent {
        checkLinkPreview_(composeState.message.trimmingCharacters(in: .whitespacesAndNewlines))
    }

    private func checkLinkPreview_(_ msgText: String) -> MsgContent {
        switch (composeState.preview) {
        case let .linkPreview(linkPreview: linkPreview):
            if let parsedMsg = parseSimpleXMarkdown(msgText),
               let url = getSimplexLink(parsedMsg).url,
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

    private func addMediaContent(_ content: UploadContent) async {
        if let img = await resizeImageToStrSize(content.uiImage, maxDataSize: 14000) {
            var newMedia: [(String, UploadContent?)] = []
            if case var .mediaPreviews(media) = composeState.preview {
                media.append((img, content))
                newMedia = media
            } else {
                newMedia = [(img, content)]
            }
            await MainActor.run {
                composeState = composeState.copy(preview: .mediaPreviews(mediaPreviews: newMedia))
            }
        }
    }

    // When error occurs while converting video, remove media preview
    private func finishedPreprocessingMediaContent() {
        if case let .mediaPreviews(media) = composeState.preview, media.isEmpty {
            DispatchQueue.main.async {
                composeState = composeState.copy(preview: .noPreview)
            }
        }
    }

    private var maxFileSize: Int64 {
        getMaxFileSize(.xftp)
    }

    private func sendLiveMessage() async {
        let typedMsg = composeState.message
        let lm = composeState.liveMessage
        if (composeState.sendEnabled || composeState.quoting)
            && (lm == nil || lm?.sentMsg == nil),
           let ci = await sendMessageAsync(typedMsg, live: true, ttl: nil) {
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
               let ci = await sendMessageAsync(sentMsg, live: true, ttl: nil) {
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
            ComposeLinkView(
                linkPreview: preview,
                cancelPreview: cancelLinkPreview,
                cancelEnabled: !composeState.inProgress
            )
            Divider()
        case let .mediaPreviews(mediaPreviews: media):
            ComposeImageView(
                images: media.map { (img, _) in img },
                cancelImage: {
                    composeState = composeState.copy(preview: .noPreview)
                    chosenMedia = []
                },
                cancelEnabled: !composeState.editing && !composeState.inProgress)
            Divider()
        case let .voicePreview(recordingFileName, _):
            ComposeVoiceView(
                recordingFileName: recordingFileName,
                recordingTime: $voiceMessageRecordingTime,
                recordingState: $composeState.voiceMessageRecordingState,
                cancelVoiceMessage: {
                    cancelVoiceMessageRecording($0)
                    clearState()
                },
                cancelEnabled: !composeState.editing && !composeState.inProgress,
                stopPlayback: $stopPlayback
            )
            Divider()
        case let .filePreview(fileName, _):
            ComposeFileView(
                fileName: fileName,
                cancelFile: {
                    composeState = composeState.copy(preview: .noPreview)
                },
                cancelEnabled: !composeState.editing && !composeState.inProgress)
            Divider()
        }
    }

    private func msgNotAllowedView(_ reason: LocalizedStringKey, icon: String) -> some View {
        HStack {
            Image(systemName: icon).foregroundColor(theme.colors.secondary)
            Text(reason).italic()
        }
        .padding(12)
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(.thinMaterial)
    }

    private func contextSendMessageToConnect(_ s: LocalizedStringKey) -> some View {
        HStack {
            Image(systemName: "message")
                .foregroundColor(theme.colors.secondary)
            Text(s)
        }
        .padding(12)
        .frame(minHeight: 54)
        .frame(maxWidth: .infinity, alignment: .leading)
        .background(ToolbarMaterial.material(toolbarMaterial))
    }

    private func reportReasonView(_ reason: ReportReason) -> some View {
        let reportText = switch reason {
        case .spam: NSLocalizedString("Report spam: only group moderators will see it.", comment: "report reason")
        case .profile: NSLocalizedString("Report member profile: only group moderators will see it.", comment: "report reason")
        case .community: NSLocalizedString("Report violation: only group moderators will see it.", comment: "report reason")
        case .illegal: NSLocalizedString("Report content: only group moderators will see it.", comment: "report reason")
        case .other: NSLocalizedString("Report other: only group moderators will see it.", comment: "report reason")
        case .unknown: "" // Should never happen
        }

        return Text(reportText)
            .italic()
            .font(.caption)
            .padding(12)
            .frame(minHeight: 44)
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(.thinMaterial)
    }


    @ViewBuilder private func contextItemView() -> some View {
        switch composeState.contextItem {
        case .noContextItem:
            EmptyView()
        case let .quotedItem(chatItem: quotedItem):
            ContextItemView(
                chat: chat,
                contextItems: [quotedItem],
                contextIcon: "arrowshape.turn.up.left",
                cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) }
            )
            Divider()
        case let .editingItem(chatItem: editingItem):
            ContextItemView(
                chat: chat,
                contextItems: [editingItem],
                contextIcon: "pencil",
                cancelContextItem: { clearState() }
            )
            Divider()
        case let .forwardingItems(chatItems, _):
            ContextItemView(
                chat: chat,
                contextItems: chatItems,
                contextIcon: "arrowshape.turn.up.forward",
                cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) }
            )
            Divider()
        case let .reportedItem(chatItem: reportedItem, _):
            ContextItemView(
                chat: chat,
                contextItems: [reportedItem],
                contextIcon: "flag",
                cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) },
                contextIconForeground: Color.red
            )
            Divider()
        }
    }

    private func sendMessage(ttl: Int?) {
        logger.debug("ChatView sendMessage")
        Task {
            logger.debug("ChatView sendMessage: in Task")
            _ = await sendMessageAsync(nil, live: false, ttl: ttl)
        }
    }

    private func sendMessageAsync(_ text: String?, live: Bool, ttl: Int?) async -> ChatItem? {
        var sent: ChatItem?
        let msgText = text ?? composeState.message
        let liveMessage = composeState.liveMessage
        let mentions = composeState.memberMentions
        if !live {
            if liveMessage != nil { composeState = composeState.copy(liveMessage: nil) }
            await sending()
        }
        if case let .forwardingItems(chatItems, fromChatInfo) = composeState.contextItem {
            // Composed text is send as a reply to the last forwarded item
            sent = await forwardItems(chatItems, fromChatInfo, ttl).last
            if !composeState.message.isEmpty {
                _ = await send(checkLinkPreview(), quoted: sent?.id, live: false, ttl: ttl, mentions: mentions)
            }
        } else if case let .editingItem(ci) = composeState.contextItem {
            sent = await updateMessage(ci, live: live)
        } else if let liveMessage = liveMessage, liveMessage.sentMsg != nil {
            sent = await updateMessage(liveMessage.chatItem, live: live)
        } else if case let .reportedItem(chatItem, reason) = composeState.contextItem {
            sent = await send(reason, chatItemId: chatItem.id)
        } else {
            var quoted: Int64? = nil
            if case let .quotedItem(chatItem: quotedItem) = composeState.contextItem {
                quoted = quotedItem.id
            }

            switch (composeState.preview) {
            case .noPreview:
                sent = await send(.text(msgText), quoted: quoted, live: live, ttl: ttl, mentions: mentions)
            case .linkPreview:
                sent = await send(checkLinkPreview(), quoted: quoted, live: live, ttl: ttl, mentions: mentions)
            case let .mediaPreviews(media):
                // TODO: CHECK THIS
                let last = media.count - 1
                var msgs: [ComposedMessage] = []
                if last >= 0 {
                    for i in 0..<last {
                        if i > 0 {
                            // Sleep to allow `progressByTimeout` update be rendered
                            try? await Task.sleep(nanoseconds: 100_000000)
                        }
                        if let (fileSource, msgContent) = mediaContent(media[i], text: "") {
                            msgs.append(ComposedMessage(fileSource: fileSource, msgContent: msgContent))
                        }
                    }
                    if let (fileSource, msgContent) = mediaContent(media[last], text: msgText) {
                        msgs.append(ComposedMessage(fileSource: fileSource, quotedItemId: quoted, msgContent: msgContent))
                    }
                }
                if msgs.isEmpty {
                    msgs = [ComposedMessage(quotedItemId: quoted, msgContent: .text(msgText))]
                }
                sent = await send(msgs, live: live, ttl: ttl).last

            case let .voicePreview(recordingFileName, duration):
                stopPlayback.toggle()
                let file = voiceCryptoFile(recordingFileName)
                sent = await send(.voice(text: msgText, duration: duration), quoted: quoted, file: file, ttl: ttl, mentions: mentions)
            case let .filePreview(_, file):
                if let savedFile = saveFileFromURL(file) {
                    sent = await send(.file(msgText), quoted: quoted, file: savedFile, live: live, ttl: ttl, mentions: mentions)
                }
            }
        }
        await MainActor.run {
            let wasForwarding = composeState.forwarding
            clearState(live: live)
            if wasForwarding,
               chatModel.draftChatId == chat.chatInfo.id,
               let draft = chatModel.draft {
                composeState = draft
            }
        }
        return sent

        func mediaContent(_ media: (String, UploadContent?), text: String) -> (CryptoFile?, MsgContent)? {
            let (previewImage, uploadContent) = media
            return switch uploadContent {
            case let .simpleImage(image):
                (saveImage(image), .image(text: text, image: previewImage))
            case let .animatedImage(image):
                (saveAnimImage(image), .image(text: text, image: previewImage))
            case let .video(_, url, duration):
                (moveTempFileFromURL(url), .video(text: text, image: previewImage, duration: duration))
            case .none:
                nil
            }
        }

        func updateMessage(_ ei: ChatItem, live: Bool) async -> ChatItem? {
            if let oldMsgContent = ei.content.msgContent {
                do {
                    let mc = updateMsgContent(oldMsgContent)
                    if mc != oldMsgContent || (ei.meta.itemLive ?? false) {
                        let chatItem = try await apiUpdateChatItem(
                            type: chat.chatInfo.chatType,
                            id: chat.chatInfo.apiId,
                            scope: chat.chatInfo.groupChatScope(),
                            itemId: ei.id,
                            updatedMessage: UpdatedMessage(msgContent: mc, mentions: composeState.memberMentions),
                            live: live
                        )
                        await MainActor.run {
                            _ = self.chatModel.upsertChatItem(self.chat.chatInfo, chatItem)
                        }
                        return chatItem
                    } else {
                        return nil
                    }
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
            case .video(_, let image, let duration):
                return .video(text: msgText, image: image, duration: duration)
            case .voice(_, let duration):
                return .voice(text: msgText, duration: duration)
            case .file:
                return .file(msgText)
            case .report(_, let reason):
                return .report(text: msgText, reason: reason)
            // TODO [short links] update chat link
            case let .chat(_, chatLink):
                return .chat(text: msgText, chatLink: chatLink)
            case .unknown(let type, _):
                return .unknown(type: type, text: msgText)
            }
        }

        func voiceCryptoFile(_ fileName: String) -> CryptoFile? {
            if !privacyEncryptLocalFilesGroupDefault.get() {
                return CryptoFile.plain(fileName)
            }
            let url = getAppFilePath(fileName)
            let toFile = generateNewFileName("voice", "m4a")
            let toUrl = getAppFilePath(toFile)
            if let cfArgs = try? encryptCryptoFile(fromPath: url.path, toPath: toUrl.path) {
                removeFile(url)
                return CryptoFile(filePath: toFile, cryptoArgs: cfArgs)
            } else {
                return nil
            }
        }

        func send(_ reportReason: ReportReason, chatItemId: Int64) async -> ChatItem? {
            if let chatItems = await apiReportMessage(
                groupId: chat.chatInfo.apiId,
                chatItemId: chatItemId,
                reportReason: reportReason,
                reportText: msgText
            ) {
                if showReportsInSupportChatAlertDefault.get() {
                    await MainActor.run {
                        showReportsInSupportChatAlert()
                    }
                }
                return chatItems.first
            }

            return nil
        }

        func showReportsInSupportChatAlert() {
            showAlert(
                NSLocalizedString("Report sent to moderators", comment: "alert title"),
                message: NSLocalizedString("You can view your reports in Chat with admins.", comment: "alert message"),
                actions: {[
                    UIAlertAction(
                        title: NSLocalizedString("Don't show again", comment: "alert action"),
                        style: .default,
                        handler: { _ in
                            showReportsInSupportChatAlertDefault.set(false)
                        }
                    ),
                    UIAlertAction(
                        title: NSLocalizedString("Ok", comment: "alert action"),
                        style: .default
                    )
                ]}
            )
        }

        func send(_ mc: MsgContent, quoted: Int64?, file: CryptoFile? = nil, live: Bool = false, ttl: Int?, mentions: [String: Int64]) async -> ChatItem? {
            await send(
                [ComposedMessage(fileSource: file, quotedItemId: quoted, msgContent: mc, mentions: mentions)],
                live: live,
                ttl: ttl
            ).first
        }

        func send(_ msgs: [ComposedMessage], live: Bool, ttl: Int?) async -> [ChatItem] {
            if let chatItems = chat.chatInfo.chatType == .local
                ? await apiCreateChatItems(noteFolderId: chat.chatInfo.apiId, composedMessages: msgs)
                : await apiSendMessages(
                    type: chat.chatInfo.chatType,
                    id: chat.chatInfo.apiId,
                    scope: chat.chatInfo.groupChatScope(),
                    live: live,
                    ttl: ttl,
                    composedMessages: msgs
                ) {
                await MainActor.run {
                    chatModel.removeLiveDummy(animated: false)
                    for chatItem in chatItems {
                        chatModel.addChatItem(chat.chatInfo, chatItem)
                    }
                }
                return chatItems
            }
            for msg in msgs {
                if let file = msg.fileSource {
                    removeFile(file.filePath)
                }
            }
            return []
        }

        func forwardItems(_ forwardedItems: [ChatItem], _ fromChatInfo: ChatInfo, _ ttl: Int?) async -> [ChatItem] {
            if let chatItems = await apiForwardChatItems(
                toChatType: chat.chatInfo.chatType,
                toChatId: chat.chatInfo.apiId,
                toScope: chat.chatInfo.groupChatScope(),
                fromChatType: fromChatInfo.chatType,
                fromChatId: fromChatInfo.apiId,
                fromScope: fromChatInfo.groupChatScope(),
                itemIds: forwardedItems.map { $0.id },
                ttl: ttl
            ) {
                await MainActor.run {
                    for chatItem in chatItems {
                        chatModel.addChatItem(chat.chatInfo, chatItem)
                    }
                    if forwardedItems.count != chatItems.count {
                        showAlert(
                            String.localizedStringWithFormat(
                                NSLocalizedString("%d messages not forwarded", comment: "alert title"),
                                forwardedItems.count - chatItems.count
                            ),
                            message: NSLocalizedString("Messages were deleted after you selected them.", comment: "alert message")
                        )
                    }
                }
                return chatItems
            } else {
                return []
            }
        }
    }

    func sending() async {
        await MainActor.run { composeState.inProgress = true }
    }

    private func startVoiceMessageRecording() async {
        startingRecording = true
        let fileName = generateNewFileName("voice", "m4a")
        chatModel.stopPreviousRecPlay = getAppFilePath(fileName)
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
            composeState.inProgress = false
        } else {
            composeState = ComposeState()
            resetLinkPreview()
        }
        chosenMedia = []
        audioRecorder = nil
        voiceMessageRecordingTime = nil
        startingRecording = false
    }

    private func saveCurrentDraft() {
        chatModel.draft = composeState
        chatModel.draftChatId = chat.id
    }

    private func clearCurrentDraft() {
        if chatModel.draftChatId == chat.id {
            chatModel.draft = nil
            chatModel.draftChatId = nil
        }
    }

    private func showLinkPreview(_ parsedMsg: [FormattedText]?) {
        prevLinkUrl = linkUrl
        (linkUrl, hasSimplexLink) = getSimplexLink(parsedMsg)
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

    private func getSimplexLink(_ parsedMsg: [FormattedText]?) -> (url: URL?, hasSimplexLink: Bool) {
        guard let parsedMsg else { return (nil, false) }
        let url: URL? = if let uri = parsedMsg.first(where: { ft in
            ft.format == .uri && !cancelledLinks.contains(ft.text) && !isSimplexLink(ft.text)
        }) {
            URL(string: uri.text)
        } else {
            nil
        }
        let simplexLink = parsedMsgHasSimplexLink(parsedMsg)
        return (url, simplexLink)
    }

    private func isSimplexLink(_ link: String) -> Bool {
        link.starts(with: "https://simplex.chat") || link.starts(with: "http://simplex.chat")
    }

    private func cancelLinkPreview() {
        if let pendingLink = pendingLinkUrl?.absoluteString {
            cancelledLinks.insert(pendingLink)
        }
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
                if let linkPreview, pendingLinkUrl == url {
                    composeState = composeState.copy(preview: .linkPreview(linkPreview: linkPreview))
                } else {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        composeState = composeState.copy(preview: .noPreview)
                    }
                }
                pendingLinkUrl = nil
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
        let im = ItemsModel.shared
        @State var composeState = ComposeState(message: "hello")
        @State var selectedRange = NSRange()

        return Group {
            ComposeView(
                chat: chat,
                im: im,
                composeState: $composeState,
                keyboardVisible: Binding.constant(true),
                keyboardHiddenDate: Binding.constant(Date.now),
                selectedRange: $selectedRange
            )
            .environmentObject(ChatModel())
            ComposeView(
                chat: chat,
                im: im,
                composeState: $composeState,
                keyboardVisible: Binding.constant(true),
                keyboardHiddenDate: Binding.constant(Date.now),
                selectedRange: $selectedRange
            )
            .environmentObject(ChatModel())
        }
    }
}
