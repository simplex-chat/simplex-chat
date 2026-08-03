import AppKit
import Foundation

typealias DeleteMessagesOperation = @Sendable ([Int64], NativeChat) async throws -> [NativeMessage]
typealias SendTextOperation = @Sendable (String, Int64?, NativeChat) async throws -> Void
typealias SendAttachmentOperation = @Sendable (PendingAttachment, String, Int64?, NativeChat) async throws -> Void
typealias LoadMessageOperation = @Sendable (NativeChat.ID, Int64) async throws -> NativeMessage?
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
    private var composerStates: [NativeChat.ID: ConversationComposerState] = [:]
    private var selectionAnchor: Int64?
    private var replyingChatID: NativeChat.ID?
    private var conversationLoadRevision: UInt64 = 0
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
        openAttachmentOperation: OpenAttachmentOperation? = nil
    ) {
        self.previewMode = previewMode
            ?? (ProcessInfo.processInfo.environment["SIMPLEX_NATIVE_UI_PREVIEW"] == "1")
        self.passphraseStore = passphraseStore
        self.deleteMessagesOperation = deleteMessagesOperation
        self.sendTextOperation = sendTextOperation
        self.sendAttachmentOperation = sendAttachmentOperation
        self.loadMessageOperation = loadMessageOperation
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
        return (hasText || !pendingAttachments.isEmpty) && !isSending && selectedChat?.kind.canSend == true
    }

    var isSendingSelectedChat: Bool {
        isSending && sendingChatID == selectedChatID
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
        selectedMessagesInTranscriptOrder.count == 1
            && selectedMessagesInTranscriptOrder.first?.replyable == true
            && selectedChat?.kind.canReply == true
            && !isSendingSelectedChat
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
                let loadedChats = try await core.loadChats(userID: userID)
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

    func sendDraft() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        let attachments = pendingAttachments
        guard (!text.isEmpty || !attachments.isEmpty), let chat = selectedChat, !isSending else { return }
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
        sendTask = Task { [weak self] in
            defer {
                self?.finishSending()
            }
            var draftWasSent = false
            do {
                if attachments.isEmpty {
                    try Task.checkCancellation()
                    if let sendTextOperation {
                        try await sendTextOperation(text, quotedMessage?.id, chat)
                    } else {
                        try await core.sendText(text, quotedItemID: quotedMessage?.id, to: chat)
                    }
                    draftWasSent = true
                    if let quotedItemID = quotedMessage?.id {
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
                            self?.clearReply(quotedItemID, in: chat.id)
                        }
                        try Task.checkCancellation()
                    }
                }
                let sentMessages = try await core.loadMessages(chatID: chat.id)
                try Task.checkCancellation()
                if self?.selectedChatID == chat.id { self?.messages = sentMessages }
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
        guard let chat = selectedChat, chat.kind.canReply, message.replyable,
              messages.contains(where: { $0.id == message.id }),
              !isSendingSelectedChat else { return }
        replyingTo = message
        replyingChatID = chat.id
        clearMessageSelection()
        composerFocusRequest &+= 1
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
    func openQuotedMessage(_ quote: NativeQuote, from containingMessageID: Int64) -> Task<Void, Never>? {
        guard let chatID = selectedChatID else { return nil }
        if let messageID = quote.messageID {
            navigateToMessage(messageID, in: chatID)
            return nil
        }

        quoteNavigationTask?.cancel()
        quoteNavigationRevision &+= 1
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
                navigateToMessage(quotedMessageID, in: chatID)
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

                if let openAttachmentOperation {
                    try await openAttachmentOperation(source, message.content.attachmentDescription)
                } else {
                    let url = try await core.openableURL(
                        for: source,
                        fileName: message.content.attachmentDescription
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

    private func navigateToMessage(_ messageID: Int64, in chatID: NativeChat.ID) {
        guard selectedChatID == chatID else { return }
        if messages.contains(where: { $0.id == messageID }) {
            targetMessageID = messageID
            return
        }
        conversationLoadTask?.cancel()
        scheduleConversationLoad(chatID: chatID, around: messageID, scrollTo: messageID)
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
        if let replyingTo, identifiers.contains(replyingTo.id) { cancelReply() }
        showingDeleteConfirmation = false
        if previewMode, deleteMessagesOperation == nil {
            messages.removeAll { identifiers.contains($0.id) }
            clearMessageSelection()
            return nil
        }

        isDeletingMessages = true
        clearMessageSelection()
        let operation = deleteMessagesOperation
        let core = core
        let task = Task { [weak self] in
            do {
                let loadedMessages: [NativeMessage]
                if let operation {
                    loadedMessages = try await operation(identifiers, chat)
                } else {
                    try await core.deleteMessages(identifiers, from: chat)
                    try Task.checkCancellation()
                    loadedMessages = try await core.loadMessages(chatID: chat.id)
                }
                try Task.checkCancellation()
                guard let self else { return }
                self.finishMessageDeletion(loadedMessages, in: chat.id)
            } catch is CancellationError {
                self?.finishMessageDeletionCancellation()
            } catch {
                self?.finishMessageDeletionFailure(error.localizedDescription, in: chat.id)
            }
        }
        deleteTask = task
        return task
    }

    private func finishMessageDeletion(_ loadedMessages: [NativeMessage], in chatID: NativeChat.ID) {
        isDeletingMessages = false
        deleteTask = nil
        guard selectedChatID == chatID else { return }
        messages = loadedMessages
    }

    private func finishMessageDeletionCancellation() {
        isDeletingMessages = false
        deleteTask = nil
    }

    private func finishMessageDeletionFailure(_ message: String, in chatID: NativeChat.ID) {
        isDeletingMessages = false
        deleteTask = nil
        if selectedChatID == chatID {
            phase = .failed(message)
        } else {
            pendingChatOperationErrors[chatID] = message
        }
    }

    func clearMessageSelection() {
        selectedMessageIDs = []
        selectionAnchor = nil
    }

    func dismissNearestState() {
        if conversationSearchPresented {
            dismissConversationSearch()
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
            clearMessageSelection()
            restoreComposerState(for: id)
            targetMessageID = nil
            conversationSearchText = ""
            conversationSearchPresented = false
            if let id, let message = pendingChatOperationErrors.removeValue(forKey: id) {
                phase = .failed(message)
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

    private func finishSending() {
        isSending = false
        sendingChatID = nil
        sendTask = nil
    }

    private func finishSendFailure(_ message: String, in chatID: NativeChat.ID) {
        if selectedChatID == chatID {
            phase = .failed(message)
        } else {
            pendingChatOperationErrors[chatID] = message
        }
    }

    private func scheduleConversationLoad(
        chatID: NativeChat.ID,
        around messageID: Int64? = nil,
        scrollTo scrollTarget: Int64? = nil
    ) {
        conversationLoadTask?.cancel()
        conversationLoadTask = Task {
            let loaded = await loadConversation(chatID: chatID, around: messageID)
            guard loaded, !Task.isCancelled, selectedChatID == chatID else { return }
            if let scrollTarget, messages.contains(where: { $0.id == scrollTarget }) {
                targetMessageID = scrollTarget
            }
        }
    }

    @discardableResult
    private func loadConversation(
        chatID: NativeChat.ID,
        around messageID: Int64? = nil,
        showProgress: Bool = true
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
            let loadedMessages = try await core.loadMessages(chatID: chatID, around: messageID)
            guard !Task.isCancelled, selectedChatID == chatID,
                  conversationLoadRevision == revision else { return false }
            applyLoadedMessages(loadedMessages, to: chatID)
            return true
        } catch is CancellationError {
            return false
        } catch {
            phase = .failed(error.localizedDescription)
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
            replyingTo = refreshedReply
        } else if sendingChatID != chatID {
            replyingTo = nil
            replyingChatID = nil
            replyContextError = "The message you were replying to is no longer available. Your draft was kept."
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

    private func refreshAfterEvent() async {
        guard let userID = profile?.userID else { return }
        do {
            chats = try await core.loadChats(userID: userID)
            if let chatID = selectedChatID {
                _ = await loadConversation(chatID: chatID, showProgress: false)
            }
            consumePendingNotificationRoutes()
        } catch {
            phase = .failed(error.localizedDescription)
        }
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
