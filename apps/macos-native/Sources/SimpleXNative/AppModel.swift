import AppKit
import Foundation

typealias DeleteMessagesOperation = @Sendable ([Int64], NativeChat) async throws -> [NativeMessage]
typealias SendTextOperation = @Sendable (String, Int64?, NativeChat) async throws -> Void
typealias SendAttachmentOperation = @Sendable (PendingAttachment, String, Int64?, NativeChat) async throws -> Void
typealias LoadMessageOperation = @Sendable (NativeChat.ID, Int64) async throws -> NativeMessage?
typealias LoadMessagesOperation = @Sendable (NativeChat.ID, Int64?) async throws -> [NativeMessage]
typealias LoadChatsOperation = @Sendable (Int64) async throws -> [NativeChat]
typealias OpenAttachmentOperation = @Sendable (NativeCryptoFile, String?) async throws -> Void

@MainActor
final class AppModel: ObservableObject {
    enum Phase: Equatable {
        case locked(message: String?)
        case opening
        case ready
        case failed(String)
    }

    private struct ConversationComposerState {
        var draft = ""
        var attachments: [PendingAttachment] = []
        var reply: NativeMessage?

        var isEmpty: Bool {
            draft.isEmpty && attachments.isEmpty && reply == nil
        }
    }

    @Published var phase: Phase = .locked(message: nil)
    @Published var profile: NativeProfile?
    @Published var chats: [NativeChat] = []
    @Published var selectedChatID: NativeChat.ID?
    @Published var messages: [NativeMessage] = []
    @Published var draft = ""
    @Published var searchText = ""
    @Published var sidebarSearchPresented = false
    @Published var conversationSearchText = ""
    @Published var conversationSearchPresented = false
    @Published var isLoadingConversation = false
    @Published var isSending = false
    @Published private(set) var sendingChatID: NativeChat.ID?
    @Published private(set) var isDeletingMessages = false
    @Published var selectedMessageIDs: Set<Int64> = []
    @Published var transcriptFocused = false
    @Published var pendingAttachments: [PendingAttachment] = []
    @Published var attachmentError: String?
    @Published var attachmentOpenError: String?
    @Published var quoteNavigationError: String?
    @Published var replyContextError: String?
    @Published var openingAttachmentIDs: Set<Int64> = []
    @Published var replyingTo: NativeMessage?
    @Published var composerFocusRequest = 0
    @Published var showingDeleteConfirmation = false
    @Published var targetMessageID: Int64?
    @Published var hasStoredPassphrase = false
    @Published var keychainStatusMessage: String?
    @Published private(set) var keychainPassphraseStorageAvailable: Bool
    @Published var density: DesktopChatDensity {
        didSet { UserDefaults.standard.set(density.rawValue, forKey: Self.densityKey) }
    }

    private let core = SimpleXCore()
    private let previewMode: Bool
    private let passphraseStore: any DatabasePassphraseStore
    private let deleteMessagesOperation: DeleteMessagesOperation?
    private let sendTextOperation: SendTextOperation?
    private let sendAttachmentOperation: SendAttachmentOperation?
    private let loadMessageOperation: LoadMessageOperation?
    private let loadMessagesOperation: LoadMessagesOperation?
    private let loadChatsOperation: LoadChatsOperation?
    private let openAttachmentOperation: OpenAttachmentOperation?
    private weak var notificationManager: NativeNotificationManager?
    private var eventTask: Task<Void, Never>?
    private var conversationLoadTask: Task<Void, Never>?
    private var refreshTask: Task<Void, Never>?
    private(set) var sendTask: Task<Void, Never>?
    private var deleteTask: Task<Void, Never>?
    private(set) var quoteNavigationTask: Task<Void, Never>?
    private var notificationRouteQueue = NotificationRouteQueue()
    private var pendingChatOperationErrors: [NativeChat.ID: String] = [:]
    private var pendingReplyContextErrors: [NativeChat.ID: String] = [:]
    private var pendingReplyInvalidationChatIDs: Set<NativeChat.ID> = []
    private var composerStates: [NativeChat.ID: ConversationComposerState] = [:]
    private var deletingChatID: NativeChat.ID?
    private var deletingMessageIDs: Set<Int64> = []
    private var selectionAnchor: Int64?
    private var replyingChatID: NativeChat.ID?
    private var conversationLoadRevision: UInt64 = 0
    @Published private(set) var conversationAnchorMessageID: Int64?
    private var quoteNavigationRevision: UInt64 = 0
    private static let densityKey = "desktopChatDensity"

    init(
        notificationManager: NativeNotificationManager? = nil,
        passphraseStore: any DatabasePassphraseStore = DatabasePassphraseKeychain(),
        previewMode: Bool? = nil,
        deleteMessagesOperation: DeleteMessagesOperation? = nil,
        sendTextOperation: SendTextOperation? = nil,
        sendAttachmentOperation: SendAttachmentOperation? = nil,
        loadMessageOperation: LoadMessageOperation? = nil,
        loadMessagesOperation: LoadMessagesOperation? = nil,
        loadChatsOperation: LoadChatsOperation? = nil,
        openAttachmentOperation: OpenAttachmentOperation? = nil
    ) {
        self.previewMode = previewMode
            ?? (ProcessInfo.processInfo.environment["SIMPLEX_NATIVE_UI_PREVIEW"] == "1")
        self.passphraseStore = passphraseStore
        self.deleteMessagesOperation = deleteMessagesOperation
        self.sendTextOperation = sendTextOperation
        self.sendAttachmentOperation = sendAttachmentOperation
        self.loadMessageOperation = loadMessageOperation
        self.loadMessagesOperation = loadMessagesOperation
        self.loadChatsOperation = loadChatsOperation
        self.openAttachmentOperation = openAttachmentOperation
        keychainPassphraseStorageAvailable = Bundle.main.object(
            forInfoDictionaryKey: "SimpleXKeychainPassphraseStorageEnabled"
        ) as? Bool == true
        density = DesktopChatDensity(
            rawValue: UserDefaults.standard.string(forKey: Self.densityKey) ?? ""
        ) ?? .compact
        self.notificationManager = notificationManager
        notificationManager?.model = self
        if self.previewMode {
            phase = .ready
            profile = NativePreviewData.profile
            chats = NativePreviewData.chats
            selectedChatID = NativePreviewData.chats.first?.id
            messages = selectedChatID.map(NativePreviewData.messages) ?? []
        } else if keychainPassphraseStorageAvailable {
            Task { await attemptAutomaticUnlock() }
        }
    }

    deinit {
        eventTask?.cancel()
        conversationLoadTask?.cancel()
        refreshTask?.cancel()
        sendTask?.cancel()
        deleteTask?.cancel()
        quoteNavigationTask?.cancel()
    }

    var selectedChat: NativeChat? {
        chats.first { $0.id == selectedChatID }
    }

    var filteredChats: [NativeChat] {
        guard !searchText.isEmpty else { return chats }
        return chats.filter {
            $0.displayName.localizedCaseInsensitiveContains(searchText)
                || $0.preview.localizedCaseInsensitiveContains(searchText)
        }
    }

    var canSendDraft: Bool {
        let hasText = !draft.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
        return (hasText || !pendingAttachments.isEmpty)
            && !isSending
            && !isDeletingReplyTarget
            && selectedChat?.kind.canSend == true
    }

    var isSendingSelectedChat: Bool {
        isSending && sendingChatID == selectedChatID
    }

    var isViewingConversationHistory: Bool {
        conversationAnchorMessageID != nil
    }

    var selectedMessagesInTranscriptOrder: [NativeMessage] {
        messages.filter { selectedMessageIDs.contains($0.id) }
    }

    var canDeleteSelectedMessages: Bool {
        let inFlightQuoteID = isSendingSelectedChat ? replyingTo?.id : nil
        let includesInFlightQuote = inFlightQuoteID.map(selectedMessageIDs.contains) ?? false
        return !isDeletingMessages
            && !selectedMessageIDs.isEmpty
            && selectedMessagesInTranscriptOrder.allSatisfy(\.deletable)
            && !includesInFlightQuote
    }

    var canReplyToSelectedMessage: Bool {
        guard selectedMessagesInTranscriptOrder.count == 1,
              let message = selectedMessagesInTranscriptOrder.first else { return false }
        return canReply(to: message)
    }

    var canDismissNearestState: Bool {
        conversationSearchPresented
            || sidebarSearchPresented
            || (replyingTo != nil && !isSendingSelectedChat)
            || (!pendingAttachments.isEmpty && !isSendingSelectedChat)
            || !selectedMessageIDs.isEmpty
    }

    var conversationSearchMatches: [NativeMessage] {
        ConversationSearch.matches(messages, query: conversationSearchText)
    }

    var conversationSearchResultDescription: String {
        let matches = conversationSearchMatches
        let selectedID = selectedMessagesInTranscriptOrder.last?.id
        return ConversationSearch.resultDescription(
            matches: matches,
            selectedID: selectedID,
            queryIsEmpty: conversationSearchText.isEmpty
        )
    }

    func unlock(passphrase: String, rememberPassphrase: Bool = true) {
        guard phase != .opening else { return }
        phase = .opening
        Task {
            await openProfile(
                passphrase: passphrase,
                rememberPassphrase: rememberPassphrase && keychainPassphraseStorageAvailable,
                automatic: false
            )
        }
    }

    func forgetSavedPassphrase() {
        guard keychainPassphraseStorageAvailable else { return }
        Task {
            do {
                try await passphraseStore.delete()
                hasStoredPassphrase = false
                keychainStatusMessage = "The saved passphrase was removed from Mac Keychain."
            } catch {
                keychainStatusMessage = error.localizedDescription
            }
        }
    }

    func selectChat(_ id: NativeChat.ID?) {
        transitionToChat(id)
    }

    func refresh() {
        guard !previewMode else { return }
        guard let userID = profile?.userID else { return }
        refreshTask?.cancel()
        refreshTask = Task {
            do {
                let loadedChats = try await loadChats(userID: userID)
                guard !Task.isCancelled else { return }
                chats = loadedChats
                if let chatID = selectedChatID {
                    conversationLoadTask?.cancel()
                    _ = await loadConversation(chatID: chatID)
                }
            } catch is CancellationError {
                return
            } catch {
                phase = .failed(error.localizedDescription)
            }
        }
    }

    @discardableResult
    func jumpToLatest() -> Task<Void, Never>? {
        guard let chatID = selectedChatID, isViewingConversationHistory else { return nil }
        quoteNavigationTask?.cancel()
        quoteNavigationTask = nil
        quoteNavigationRevision &+= 1
        quoteNavigationError = nil
        return scheduleConversationLoad(chatID: chatID, scrollToLatest: true)
    }

    func sendDraft() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        let attachments = pendingAttachments
        guard (!text.isEmpty || !attachments.isEmpty), let chat = selectedChat,
              !isSending, !isDeletingReplyTarget else { return }
        let quotedMessage = replyingChatID == chat.id ? replyingTo : nil
        if replyingTo != nil, quotedMessage == nil { cancelReply() }
        if previewMode, sendTextOperation == nil, sendAttachmentOperation == nil {
            if !text.isEmpty {
                let nextID = (messages.map(\.id).max() ?? 0) + 1
                messages.append(NativeMessage(
                    id: nextID,
                    text: text,
                    timestamp: Date(),
                    sent: true,
                    author: nil,
                    deletable: true,
                    content: .text,
                    quotedItem: quotedMessage.map {
                        NativeQuote(
                            messageID: $0.id,
                            text: $0.replyPreview,
                            sent: $0.sent,
                            author: $0.author
                        )
                    }
                ))
            }
            draft = ""
            pendingAttachments = []
            cancelReply()
            return
        }
        draft = ""
        sendingChatID = chat.id
        isSending = true
        let core = core
        let sendTextOperation = sendTextOperation
        let sendAttachmentOperation = sendAttachmentOperation
        let loadMessageOperation = loadMessageOperation
        sendTask = Task { [weak self] in
            var draftWasSent = false
            var quoteWasSent = false
            defer {
                self?.finishSending(
                    in: chat.id,
                    quotedMessageID: quotedMessage?.id,
                    quoteWasSent: quoteWasSent
                )
            }
            do {
                if let quotedMessage {
                    let refreshedReply: NativeMessage?
                    if let loadMessageOperation {
                        refreshedReply = try await loadMessageOperation(chat.id, quotedMessage.id)
                    } else if self?.previewMode == true {
                        refreshedReply = quotedMessage
                    } else {
                        refreshedReply = try await core.loadMessage(chatID: chat.id, itemID: quotedMessage.id)
                    }
                    try Task.checkCancellation()
                    guard refreshedReply?.replyable == true else {
                        self?.restoreFailedDraft(text, in: chat.id)
                        self?.invalidateReplyContext(in: chat.id)
                        return
                    }
                }
                if attachments.isEmpty {
                    try Task.checkCancellation()
                    if let sendTextOperation {
                        try await sendTextOperation(text, quotedMessage?.id, chat)
                    } else {
                        try await core.sendText(text, quotedItemID: quotedMessage?.id, to: chat)
                    }
                    draftWasSent = true
                    if let quotedItemID = quotedMessage?.id {
                        quoteWasSent = true
                        self?.clearReply(quotedItemID, in: chat.id)
                    }
                    try Task.checkCancellation()
                } else {
                    let sendSteps = PendingAttachmentBatch.sendSteps(
                        attachments: attachments,
                        caption: text,
                        quotedItemID: quotedMessage?.id
                    )
                    for (index, step) in sendSteps.enumerated() {
                        try Task.checkCancellation()
                        if let sendAttachmentOperation {
                            try await sendAttachmentOperation(
                                step.attachment,
                                step.caption,
                                step.quotedItemID,
                                chat
                            )
                        } else {
                            try await core.sendAttachment(
                                step.attachment,
                                caption: step.caption,
                                quotedItemID: step.quotedItemID,
                                to: chat
                            )
                        }
                        if index == sendSteps.index(before: sendSteps.endIndex) { draftWasSent = true }
                        self?.removeSentAttachment(step.attachment.id, from: chat.id)
                        if let quotedItemID = step.quotedItemID {
                            quoteWasSent = true
                            self?.clearReply(quotedItemID, in: chat.id)
                        }
                        try Task.checkCancellation()
                    }
                }
                let sentMessages = try await core.loadMessages(chatID: chat.id)
                try Task.checkCancellation()
                if self?.selectedChatID == chat.id {
                    self?.conversationAnchorMessageID = nil
                    self?.messages = sentMessages
                }
                if let userID = self?.profile?.userID {
                    let loadedChats = try await core.loadChats(userID: userID)
                    try Task.checkCancellation()
                    self?.chats = loadedChats
                }
            } catch is CancellationError {
                if !draftWasSent { self?.restoreFailedDraft(text, in: chat.id) }
                return
            } catch {
                if !draftWasSent { self?.restoreFailedDraft(text, in: chat.id) }
                self?.finishSendFailure(error.localizedDescription, in: chat.id)
            }
        }
    }

    func chooseAttachments() {
        guard !isSendingSelectedChat else { return }
        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = true
        panel.canChooseDirectories = false
        panel.canChooseFiles = true
        panel.allowedContentTypes = [.data]
        panel.prompt = "Attach"
        panel.message = "Choose images, videos, or documents to send."
        guard panel.runModal() == .OK else { return }
        stageAttachments(panel.urls)
    }

    func stageAttachments(_ urls: [URL]) {
        guard !isSendingSelectedChat else {
            attachmentError = "Wait for the current message to finish sending before changing attachments."
            return
        }
        var failures: [String] = []
        for url in urls {
            do {
                let attachment = try PendingAttachment.stage(url: url)
                if !pendingAttachments.contains(where: { $0.url == attachment.url }) {
                    pendingAttachments.append(attachment)
                }
            } catch {
                failures.append(error.localizedDescription)
            }
        }
        if !failures.isEmpty { attachmentError = failures.joined(separator: "\n") }
    }

    func stageFilesFromPasteboard() {
        guard !isSendingSelectedChat else {
            attachmentError = "Wait for the current message to finish sending before changing attachments."
            return
        }
        let urls = NSPasteboard.general.readObjects(forClasses: [NSURL.self]) as? [URL] ?? []
        if urls.isEmpty {
            attachmentError = "The clipboard does not contain any files."
        } else {
            stageAttachments(urls)
        }
    }

    func removeAttachment(_ id: PendingAttachment.ID) {
        guard !isSendingSelectedChat else { return }
        pendingAttachments.removeAll { $0.id == id }
    }

    func reorderAttachment(_ source: PendingAttachment.ID, before destination: PendingAttachment.ID) {
        guard !isSendingSelectedChat else { return }
        pendingAttachments = PendingAttachment.reordered(pendingAttachments, from: source, before: destination)
    }

    func moveAttachment(_ id: PendingAttachment.ID, by offset: Int) {
        guard !isSendingSelectedChat else { return }
        guard let sourceIndex = pendingAttachments.firstIndex(where: { $0.id == id }) else { return }
        let destinationIndex = min(max(0, sourceIndex + offset), pendingAttachments.count - 1)
        guard sourceIndex != destinationIndex else { return }
        let attachment = pendingAttachments.remove(at: sourceIndex)
        pendingAttachments.insert(attachment, at: destinationIndex)
    }

    func beginReply(to message: NativeMessage) {
        guard let chat = selectedChat, canReply(to: message),
              let currentMessage = messages.first(where: { $0.id == message.id }),
              currentMessage.replyable else { return }
        replyingTo = currentMessage
        replyingChatID = chat.id
        if conversationSearchPresented {
            dismissConversationSearch()
        } else {
            clearMessageSelection()
        }
        composerFocusRequest &+= 1
    }

    func canReply(to message: NativeMessage) -> Bool {
        guard let chat = selectedChat,
              chat.kind.canReply,
              !isSendingSelectedChat,
              let currentMessage = messages.first(where: { $0.id == message.id }) else { return false }
        return currentMessage.replyable
            && !deletionIncludesMessage(currentMessage.id, in: chat.id)
    }

    func replyToSelectedMessage() {
        guard selectedMessagesInTranscriptOrder.count == 1,
              let message = selectedMessagesInTranscriptOrder.first else { return }
        beginReply(to: message)
    }

    func cancelReply() {
        guard !isSendingSelectedChat else { return }
        replyingTo = nil
        replyingChatID = nil
    }

    @discardableResult
    func openReplyTarget() -> Task<Void, Never>? {
        guard let message = replyingTo,
              let chatID = selectedChatID,
              replyingChatID == chatID else { return nil }
        return openQuotedMessage(
            NativeQuote(
                messageID: message.id,
                text: message.replyPreview,
                sent: message.sent,
                author: message.author
            ),
            from: message.id
        )
    }

    @discardableResult
    func openQuotedMessage(_ quote: NativeQuote, from containingMessageID: Int64) -> Task<Void, Never>? {
        guard let chatID = selectedChatID else { return nil }
        quoteNavigationTask?.cancel()
        quoteNavigationTask = nil
        quoteNavigationRevision &+= 1
        quoteNavigationError = nil
        if let messageID = quote.messageID {
            return navigateToMessage(messageID, in: chatID)
        }

        let revision = quoteNavigationRevision
        let operation = loadMessageOperation
        let task = Task {
            defer {
                if quoteNavigationRevision == revision {
                    quoteNavigationTask = nil
                }
            }
            do {
                let refreshedMessage: NativeMessage?
                if let operation {
                    refreshedMessage = try await operation(chatID, containingMessageID)
                } else {
                    refreshedMessage = try await core.loadMessage(
                        chatID: chatID,
                        itemID: containingMessageID
                    )
                }
                guard !Task.isCancelled, selectedChatID == chatID,
                      quoteNavigationRevision == revision else { return }
                guard let refreshedMessage,
                      let quotedMessageID = refreshedMessage.quotedItem?.messageID else {
                    quoteNavigationError = "The original quoted message is no longer available in this conversation."
                    return
                }
                if let index = messages.firstIndex(where: { $0.id == containingMessageID }) {
                    messages[index] = refreshedMessage
                }
                if let navigation = navigateToMessage(quotedMessageID, in: chatID) {
                    await navigation.value
                }
            } catch is CancellationError {
                return
            } catch {
                guard selectedChatID == chatID, quoteNavigationRevision == revision else { return }
                quoteNavigationError = error.localizedDescription
            }
        }
        quoteNavigationTask = task
        return task
    }

    @discardableResult
    func openAttachment(_ message: NativeMessage) -> Task<Void, Never>? {
        guard let initialSource = message.fileSource,
              !openingAttachmentIDs.contains(message.id) else { return nil }
        let chatID = selectedChatID
        let loadMessageOperation = loadMessageOperation
        let openAttachmentOperation = openAttachmentOperation
        openingAttachmentIDs.insert(message.id)
        let task = Task {
            defer { openingAttachmentIDs.remove(message.id) }
            do {
                var source = initialSource
                if source.cryptoArgs == nil, let chatID {
                    let refreshedMessage: NativeMessage?
                    if let loadMessageOperation {
                        refreshedMessage = try await loadMessageOperation(chatID, message.id)
                    } else {
                        refreshedMessage = try await core.loadMessage(chatID: chatID, itemID: message.id)
                    }
                    try Task.checkCancellation()
                    if let refreshedMessage, let refreshedSource = refreshedMessage.fileSource {
                        source = refreshedSource
                        if selectedChatID == chatID,
                           let index = messages.firstIndex(where: { $0.id == message.id }) {
                            messages[index] = refreshedMessage
                        }
                    }
                }

                let fileName = message.content.fileName ?? message.content.attachmentDescription
                if let openAttachmentOperation {
                    try await openAttachmentOperation(source, fileName)
                } else {
                    let url = try await core.openableURL(
                        for: source,
                        fileName: fileName
                    )
                    try Task.checkCancellation()
                    guard NSWorkspace.shared.open(url) else {
                        throw NativeChatError.unavailable("macOS could not open this attachment.")
                    }
                }
            } catch is CancellationError {
                return
            } catch {
                attachmentOpenError = error.localizedDescription
            }
        }
        return task
    }

    @discardableResult
    private func navigateToMessage(_ messageID: Int64, in chatID: NativeChat.ID) -> Task<Void, Never>? {
        guard selectedChatID == chatID else { return nil }
        conversationLoadTask?.cancel()
        if messages.contains(where: { $0.id == messageID }) {
            conversationAnchorMessageID = messageID
            targetMessageID = messageID
            return nil
        }
        return scheduleConversationLoad(
            chatID: chatID,
            around: messageID,
            scrollTo: messageID,
            navigationFailureMessage: "The original quoted message is no longer available in this conversation."
        )
    }

    func selectMessage(_ id: Int64, modifiers: NSEvent.ModifierFlags) {
        let result = MessageSelection.updated(
            current: selectedMessageIDs,
            anchor: selectionAnchor,
            clicked: id,
            orderedIDs: messages.map(\.id),
            command: modifiers.contains(.command),
            shift: modifiers.contains(.shift)
        )
        selectedMessageIDs = result.selection
        selectionAnchor = result.anchor
    }

    func selectAllMessages() {
        guard transcriptFocused else { return }
        selectedMessageIDs = Set(messages.map(\.id))
        selectionAnchor = messages.last?.id
    }

    func moveMessageSelection(by offset: Int) {
        guard transcriptFocused, !messages.isEmpty else { return }
        let currentID = selectedMessagesInTranscriptOrder.last?.id
        let currentIndex = currentID.flatMap { id in messages.firstIndex(where: { $0.id == id }) }
        let proposed = (currentIndex ?? (offset > 0 ? -1 : messages.count)) + offset
        let targetIndex = min(max(0, proposed), messages.count - 1)
        selectedMessageIDs = [messages[targetIndex].id]
        selectionAnchor = messages[targetIndex].id
    }

    func beginConversationSearch() {
        guard selectedChat != nil else { return }
        conversationSearchPresented = true
    }

    func updateConversationSearchSelection() {
        guard let first = conversationSearchMatches.first else {
            clearMessageSelection()
            return
        }
        selectedMessageIDs = [first.id]
        selectionAnchor = first.id
        targetMessageID = first.id
    }

    func moveConversationSearchResult(by offset: Int) {
        let matches = conversationSearchMatches
        let selectedID = selectedMessagesInTranscriptOrder.last?.id
        guard let nextID = ConversationSearch.nextID(in: matches, currentID: selectedID, offset: offset) else { return }
        selectedMessageIDs = [nextID]
        selectionAnchor = nextID
        targetMessageID = nextID
    }

    func dismissConversationSearch() {
        conversationSearchPresented = false
        conversationSearchText = ""
        clearMessageSelection()
    }

    func copySelectedMessages() {
        guard !selectedMessageIDs.isEmpty else { return }
        let text = selectedMessagesInTranscriptOrder.map(\.text).joined(separator: "\n\n")
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(text, forType: .string)
    }

    func requestDeleteSelectedMessages() {
        guard canDeleteSelectedMessages else { return }
        showingDeleteConfirmation = true
    }

    @discardableResult
    func deleteSelectedMessages() -> Task<Void, Never>? {
        guard let chat = selectedChat, canDeleteSelectedMessages else { return nil }
        let identifiers = selectedMessagesInTranscriptOrder.map(\.id)
        showingDeleteConfirmation = false
        if previewMode, deleteMessagesOperation == nil {
            messages.removeAll { identifiers.contains($0.id) }
            identifiers.forEach { clearReply($0, in: chat.id) }
            clearMessageSelection()
            return nil
        }

        deletingChatID = chat.id
        deletingMessageIDs = Set(identifiers)
        isDeletingMessages = true
        clearMessageSelection()
        let operation = deleteMessagesOperation
        let core = core
        let task = Task { [weak self] in
            var deletionCommitted = false
            do {
                let loadedMessages: [NativeMessage]
                if let operation {
                    loadedMessages = try await operation(identifiers, chat)
                    deletionCommitted = true
                } else {
                    try await core.deleteMessages(identifiers, from: chat)
                    deletionCommitted = true
                    try Task.checkCancellation()
                    loadedMessages = try await core.loadMessages(chatID: chat.id)
                }
                try Task.checkCancellation()
                guard let self else { return }
                self.finishMessageDeletion(loadedMessages, deletedIDs: identifiers, in: chat.id)
            } catch is CancellationError {
                if deletionCommitted {
                    self?.finishCommittedMessageDeletion(deletedIDs: identifiers, in: chat.id)
                } else {
                    self?.finishMessageDeletionCancellation()
                }
            } catch {
                if deletionCommitted {
                    self?.finishCommittedMessageDeletion(deletedIDs: identifiers, in: chat.id)
                } else {
                    self?.finishMessageDeletionFailure(error.localizedDescription, in: chat.id)
                }
            }
        }
        deleteTask = task
        return task
    }

    private func finishMessageDeletion(
        _ loadedMessages: [NativeMessage],
        deletedIDs: [Int64],
        in chatID: NativeChat.ID
    ) {
        clearDeletionState()
        deletedIDs.forEach { clearReply($0, in: chatID) }
        guard selectedChatID == chatID else { return }
        conversationAnchorMessageID = nil
        messages = loadedMessages
    }

    private func finishCommittedMessageDeletion(deletedIDs: [Int64], in chatID: NativeChat.ID) {
        clearDeletionState()
        deletedIDs.forEach { clearReply($0, in: chatID) }
        guard selectedChatID == chatID else { return }
        if conversationAnchorMessageID.map(deletedIDs.contains) == true {
            conversationAnchorMessageID = nil
        }
        messages.removeAll { deletedIDs.contains($0.id) }
    }

    private func finishMessageDeletionCancellation() {
        clearDeletionState()
    }

    private func finishMessageDeletionFailure(_ message: String, in chatID: NativeChat.ID) {
        clearDeletionState()
        if selectedChatID == chatID {
            phase = .failed(message)
        } else {
            pendingChatOperationErrors[chatID] = message
        }
    }

    private var isDeletingReplyTarget: Bool {
        guard let replyingTo else { return false }
        return deletionIncludesMessage(replyingTo.id, in: replyingChatID)
    }

    private func deletionIncludesMessage(_ messageID: Int64, in chatID: NativeChat.ID?) -> Bool {
        isDeletingMessages
            && deletingChatID == chatID
            && deletingMessageIDs.contains(messageID)
    }

    private func clearDeletionState() {
        deletingChatID = nil
        deletingMessageIDs = []
        isDeletingMessages = false
        deleteTask = nil
    }

    func clearMessageSelection() {
        selectedMessageIDs = []
        selectionAnchor = nil
    }

    func dismissNearestState() {
        if conversationSearchPresented {
            dismissConversationSearch()
        } else if sidebarSearchPresented {
            sidebarSearchPresented = false
            searchText = ""
        } else if replyingTo != nil, !isSendingSelectedChat {
            cancelReply()
        } else if !pendingAttachments.isEmpty, !isSendingSelectedChat {
            pendingAttachments = []
        } else if !selectedMessageIDs.isEmpty {
            clearMessageSelection()
        }
    }

    func openNotificationRoute(_ route: NotificationRoute) {
        guard case .ready = phase,
              route.userID == nil || route.userID == profile?.userID,
              route.remoteHostID == nil else {
            notificationRouteQueue.enqueue(route)
            return
        }
        guard chats.contains(where: { $0.id == route.chatID }) else {
            notificationRouteQueue.enqueue(route)
            return
        }
        transitionToChat(route.chatID, around: route.messageID, scrollTo: route.messageID)
    }

    private func transitionToChat(
        _ id: NativeChat.ID?,
        around messageID: Int64? = nil,
        scrollTo scrollTarget: Int64? = nil
    ) {
        let changedChat = selectedChatID != id
        guard changedChat || messageID != nil || scrollTarget != nil else { return }

        if changedChat {
            quoteNavigationTask?.cancel()
            quoteNavigationTask = nil
            quoteNavigationRevision &+= 1
            quoteNavigationError = nil
            replyContextError = nil
            if let previousChatID = selectedChatID {
                saveComposerState(for: previousChatID)
            }
            selectedChatID = id
            conversationAnchorMessageID = nil
            messages = []
            clearMessageSelection()
            restoreComposerState(for: id)
            targetMessageID = nil
            conversationSearchText = ""
            conversationSearchPresented = false
            if let id, let message = pendingChatOperationErrors.removeValue(forKey: id) {
                phase = .failed(message)
            }
            if let id {
                replyContextError = pendingReplyContextErrors.removeValue(forKey: id)
            }
        }
        if let id { notificationManager?.removeDeliveredNotifications(chatID: id) }

        conversationLoadTask?.cancel()
        guard let id else {
            messages = []
            isLoadingConversation = false
            return
        }
        if previewMode {
            messages = NativePreviewData.messages(for: id)
            if let scrollTarget, messages.contains(where: { $0.id == scrollTarget }) {
                targetMessageID = scrollTarget
            }
            return
        }
        scheduleConversationLoad(chatID: id, around: messageID, scrollTo: scrollTarget)
    }

    private func saveComposerState(for chatID: NativeChat.ID) {
        storeComposerState(
            ConversationComposerState(draft: draft, attachments: pendingAttachments, reply: replyingTo),
            for: chatID
        )
    }

    private func restoreComposerState(for chatID: NativeChat.ID?) {
        let state = chatID.flatMap { composerStates[$0] } ?? ConversationComposerState()
        draft = state.draft
        pendingAttachments = state.attachments
        replyingTo = state.reply
        replyingChatID = state.reply == nil ? nil : chatID
    }

    private func storeComposerState(_ state: ConversationComposerState, for chatID: NativeChat.ID) {
        if state.isEmpty {
            composerStates.removeValue(forKey: chatID)
        } else {
            composerStates[chatID] = state
        }
    }

    private func updateComposerState(
        for chatID: NativeChat.ID,
        _ update: (inout ConversationComposerState) -> Void
    ) {
        if selectedChatID == chatID {
            var state = ConversationComposerState(
                draft: draft,
                attachments: pendingAttachments,
                reply: replyingTo
            )
            update(&state)
            draft = state.draft
            pendingAttachments = state.attachments
            replyingTo = state.reply
            replyingChatID = state.reply == nil ? nil : chatID
        } else {
            var state = composerStates[chatID] ?? ConversationComposerState()
            update(&state)
            storeComposerState(state, for: chatID)
        }
    }

    private func removeSentAttachment(_ attachmentID: PendingAttachment.ID, from chatID: NativeChat.ID) {
        updateComposerState(for: chatID) { state in
            state.attachments.removeAll { $0.id == attachmentID }
        }
    }

    private func clearReply(_ messageID: Int64, in chatID: NativeChat.ID) {
        updateComposerState(for: chatID) { state in
            if state.reply?.id == messageID { state.reply = nil }
        }
    }

    private func invalidateReplyContext(in chatID: NativeChat.ID) {
        let message = "The message you were replying to is no longer available. Your draft was kept."
        pendingReplyInvalidationChatIDs.remove(chatID)
        updateComposerState(for: chatID) { state in
            state.reply = nil
        }
        if selectedChatID == chatID {
            replyContextError = message
        } else {
            pendingReplyContextErrors[chatID] = message
        }
    }

    private func restoreFailedDraft(_ text: String, in chatID: NativeChat.ID) {
        guard !text.isEmpty else { return }
        updateComposerState(for: chatID) { state in
            if state.draft.isEmpty {
                state.draft = text
            } else if state.draft != text {
                state.draft = "\(text)\n\(state.draft)"
            }
        }
    }

    private func finishSending(
        in chatID: NativeChat.ID,
        quotedMessageID: Int64?,
        quoteWasSent: Bool
    ) {
        let replyWasInvalidated = pendingReplyInvalidationChatIDs.remove(chatID) != nil
        isSending = false
        sendingChatID = nil
        sendTask = nil
        if replyWasInvalidated, !quoteWasSent, quotedMessageID != nil {
            invalidateReplyContext(in: chatID)
        }
    }

    private func finishSendFailure(_ message: String, in chatID: NativeChat.ID) {
        if selectedChatID == chatID {
            phase = .failed(message)
        } else {
            pendingChatOperationErrors[chatID] = message
        }
    }

    @discardableResult
    private func scheduleConversationLoad(
        chatID: NativeChat.ID,
        around messageID: Int64? = nil,
        scrollTo scrollTarget: Int64? = nil,
        navigationFailureMessage: String? = nil,
        scrollToLatest: Bool = false
    ) -> Task<Void, Never> {
        conversationLoadTask?.cancel()
        let task = Task {
            let loaded = await loadConversation(
                chatID: chatID,
                around: messageID,
                navigationFailureMessage: navigationFailureMessage
            )
            guard loaded, !Task.isCancelled, selectedChatID == chatID else { return }
            if scrollToLatest, let latestMessageID = messages.last?.id {
                targetMessageID = latestMessageID
            } else if let scrollTarget {
                if messages.contains(where: { $0.id == scrollTarget }) {
                    targetMessageID = scrollTarget
                } else if let navigationFailureMessage {
                    quoteNavigationError = navigationFailureMessage
                }
            }
        }
        conversationLoadTask = task
        return task
    }

    @discardableResult
    private func loadConversation(
        chatID: NativeChat.ID,
        around messageID: Int64? = nil,
        showProgress: Bool = true,
        navigationFailureMessage: String? = nil,
        reportFailure: Bool = true
    ) async -> Bool {
        guard selectedChatID == chatID else { return false }
        conversationLoadRevision &+= 1
        let revision = conversationLoadRevision
        isLoadingConversation = showProgress
        defer {
            if selectedChatID == chatID, conversationLoadRevision == revision {
                isLoadingConversation = false
            }
        }
        do {
            let loadedMessages: [NativeMessage]
            if let loadMessagesOperation {
                loadedMessages = try await loadMessagesOperation(chatID, messageID)
            } else {
                loadedMessages = try await core.loadMessages(chatID: chatID, around: messageID)
            }
            guard !Task.isCancelled, selectedChatID == chatID,
                  conversationLoadRevision == revision else { return false }
            applyLoadedMessages(loadedMessages, to: chatID)
            conversationAnchorMessageID = messageID
            return true
        } catch is CancellationError {
            return false
        } catch {
            guard !Task.isCancelled, selectedChatID == chatID,
                  conversationLoadRevision == revision else { return false }
            if let navigationFailureMessage {
                quoteNavigationError = navigationFailureMessage
            } else if reportFailure {
                phase = .failed(error.localizedDescription)
            }
            return false
        }
    }

    func applyLoadedMessages(_ loadedMessages: [NativeMessage], to chatID: NativeChat.ID) {
        guard selectedChatID == chatID else { return }
        messages = loadedMessages
        selectedMessageIDs.formIntersection(messages.map(\.id))

        guard replyingChatID == chatID, let currentReply = replyingTo,
              let refreshedReply = loadedMessages.first(where: { $0.id == currentReply.id }) else { return }
        if refreshedReply.replyable {
            pendingReplyInvalidationChatIDs.remove(chatID)
            replyingTo = refreshedReply
        } else if sendingChatID == chatID {
            pendingReplyInvalidationChatIDs.insert(chatID)
        } else {
            invalidateReplyContext(in: chatID)
        }
    }

    private func startEventLoop() {
        eventTask?.cancel()
        eventTask = Task { [weak self] in
            while !Task.isCancelled {
                guard let self else { return }
                guard let event = await core.receiveEvent() else { continue }
                await refreshAfterEvent()
                notificationManager?.handleCoreEvent(event)
            }
        }
    }

    func refreshAfterEvent() async {
        guard let userID = profile?.userID else { return }
        do {
            chats = try await loadChats(userID: userID)
            if let chatID = selectedChatID, !isLoadingConversation {
                let anchor = conversationAnchorMessageID
                let loaded = await loadConversation(
                    chatID: chatID,
                    around: anchor,
                    showProgress: false,
                    reportFailure: anchor == nil
                )
                if !loaded, anchor != nil, !Task.isCancelled,
                   selectedChatID == chatID, !isLoadingConversation {
                    conversationAnchorMessageID = nil
                    _ = await loadConversation(chatID: chatID, showProgress: false)
                }
            }
            consumePendingNotificationRoutes()
        } catch is CancellationError {
            return
        } catch {
            phase = .failed(error.localizedDescription)
        }
    }

    private func loadChats(userID: Int64) async throws -> [NativeChat] {
        if let loadChatsOperation {
            return try await loadChatsOperation(userID)
        }
        return try await core.loadChats(userID: userID)
    }

    private func consumePendingNotificationRoutes() {
        let routes = notificationRouteQueue.consumeIfReady(true)
        for route in routes { openNotificationRoute(route) }
    }

    private func attemptAutomaticUnlock() async {
        phase = .opening
        do {
            guard let passphrase = try await passphraseStore.load() else {
                phase = .locked(message: nil)
                return
            }
            hasStoredPassphrase = true
            await openProfile(passphrase: passphrase, rememberPassphrase: true, automatic: true)
        } catch {
            keychainStatusMessage = error.localizedDescription
            phase = .locked(message: "Couldn’t read the saved passphrase. Enter it to continue.")
        }
    }

    private func openProfile(passphrase: String, rememberPassphrase: Bool, automatic: Bool) async {
        do {
            let (profile, chats) = try await core.open(passphrase: passphrase)

            if rememberPassphrase {
                do {
                    try await passphraseStore.save(passphrase)
                    hasStoredPassphrase = true
                    keychainStatusMessage = nil
                } catch {
                    hasStoredPassphrase = false
                    keychainStatusMessage = "The profile opened, but its passphrase wasn’t saved: \(error.localizedDescription)"
                }
            } else if !automatic {
                do {
                    try await passphraseStore.delete()
                    hasStoredPassphrase = false
                } catch {
                    keychainStatusMessage = error.localizedDescription
                }
            }

            self.profile = profile
            self.chats = chats
            phase = .ready
            if selectedChatID == nil { selectedChatID = chats.first?.id }
            if let chatID = selectedChatID {
                _ = await loadConversation(chatID: chatID)
            }
            startEventLoop()
            notificationManager?.chatSetupReady()
            consumePendingNotificationRoutes()
        } catch {
            if automatic, error.localizedDescription == "That database passphrase is not correct." {
                do {
                    try await passphraseStore.delete()
                    hasStoredPassphrase = false
                } catch {
                    keychainStatusMessage = error.localizedDescription
                }
                phase = .locked(message: "The saved passphrase no longer opens this profile. Enter the current passphrase.")
            } else {
                phase = .locked(message: error.localizedDescription)
            }
        }
    }
}
