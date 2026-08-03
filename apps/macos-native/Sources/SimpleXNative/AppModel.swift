import AppKit
import Foundation

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
    @Published var selectedMessageIDs: Set<Int64> = []
    @Published var transcriptFocused = false
    @Published var pendingAttachments: [PendingAttachment] = []
    @Published var attachmentError: String?
    @Published var attachmentOpenError: String?
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
    private weak var notificationManager: NativeNotificationManager?
    private var eventTask: Task<Void, Never>?
    private var conversationLoadTask: Task<Void, Never>?
    private var refreshTask: Task<Void, Never>?
    private var sendTask: Task<Void, Never>?
    private var notificationRouteQueue = NotificationRouteQueue()
    private var composerStates: [NativeChat.ID: ConversationComposerState] = [:]
    private var selectionAnchor: Int64?
    private var replyingChatID: NativeChat.ID?
    private var conversationLoadRevision: UInt64 = 0
    private static let densityKey = "desktopChatDensity"

    init(
        notificationManager: NativeNotificationManager? = nil,
        passphraseStore: any DatabasePassphraseStore = DatabasePassphraseKeychain(),
        previewMode: Bool? = nil
    ) {
        self.previewMode = previewMode
            ?? (ProcessInfo.processInfo.environment["SIMPLEX_NATIVE_UI_PREVIEW"] == "1")
        self.passphraseStore = passphraseStore
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

    var selectedMessagesInTranscriptOrder: [NativeMessage] {
        messages.filter { selectedMessageIDs.contains($0.id) }
    }

    var canDeleteSelectedMessages: Bool {
        !selectedMessageIDs.isEmpty && selectedMessagesInTranscriptOrder.allSatisfy(\.deletable)
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
        if previewMode {
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
                            text: $0.text.isEmpty ? ($0.content.attachmentDescription ?? "Message") : $0.text,
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
        isSending = true
        sendTask = Task {
            defer {
                isSending = false
                sendTask = nil
            }
            var contentWasSent = false
            do {
                if attachments.isEmpty {
                    try await core.sendText(text, quotedItemID: quotedMessage?.id, to: chat)
                    try Task.checkCancellation()
                    contentWasSent = true
                    if let quotedItemID = quotedMessage?.id {
                        clearReply(quotedItemID, in: chat.id)
                    }
                } else {
                    let sendSteps = PendingAttachmentBatch.sendSteps(
                        attachments: attachments,
                        caption: text,
                        quotedItemID: quotedMessage?.id
                    )
                    for (index, step) in sendSteps.enumerated() {
                        try Task.checkCancellation()
                        try await core.sendAttachment(
                            step.attachment,
                            caption: step.caption,
                            quotedItemID: step.quotedItemID,
                            to: chat
                        )
                        try Task.checkCancellation()
                        if index == sendSteps.index(before: sendSteps.endIndex) { contentWasSent = true }
                        removeSentAttachment(step.attachment.id, from: chat.id)
                        if let quotedItemID = step.quotedItemID {
                            clearReply(quotedItemID, in: chat.id)
                        }
                    }
                }
                let sentMessages = try await core.loadMessages(chatID: chat.id)
                try Task.checkCancellation()
                if selectedChatID == chat.id { messages = sentMessages }
                if let userID = profile?.userID {
                    let loadedChats = try await core.loadChats(userID: userID)
                    try Task.checkCancellation()
                    chats = loadedChats
                }
            } catch is CancellationError {
                return
            } catch {
                if !contentWasSent { restoreFailedDraft(text, in: chat.id) }
                phase = .failed(error.localizedDescription)
            }
        }
    }

    func chooseAttachments() {
        guard !isSending else { return }
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
        guard !isSending else {
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
        guard !isSending else {
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
        guard !isSending else { return }
        pendingAttachments.removeAll { $0.id == id }
    }

    func reorderAttachment(_ source: PendingAttachment.ID, before destination: PendingAttachment.ID) {
        guard !isSending else { return }
        pendingAttachments = PendingAttachment.reordered(pendingAttachments, from: source, before: destination)
    }

    func moveAttachment(_ id: PendingAttachment.ID, by offset: Int) {
        guard !isSending else { return }
        guard let sourceIndex = pendingAttachments.firstIndex(where: { $0.id == id }) else { return }
        let destinationIndex = min(max(0, sourceIndex + offset), pendingAttachments.count - 1)
        guard sourceIndex != destinationIndex else { return }
        let attachment = pendingAttachments.remove(at: sourceIndex)
        pendingAttachments.insert(attachment, at: destinationIndex)
    }

    func beginReply(to message: NativeMessage) {
        guard let chat = selectedChat, chat.kind.canReply, messages.contains(where: { $0.id == message.id }),
              !isSending else { return }
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
        replyingTo = nil
        replyingChatID = nil
    }

    func openQuotedMessage(_ messageID: Int64) {
        if messages.contains(where: { $0.id == messageID }) {
            targetMessageID = messageID
            return
        }
        conversationLoadTask?.cancel()
        guard let chatID = selectedChatID else { return }
        scheduleConversationLoad(chatID: chatID, around: messageID, scrollTo: messageID)
    }

    func openAttachment(_ message: NativeMessage) {
        guard let source = message.fileSource,
              !openingAttachmentIDs.contains(message.id) else { return }
        openingAttachmentIDs.insert(message.id)
        Task {
            defer { openingAttachmentIDs.remove(message.id) }
            do {
                let url = try await core.openableURL(
                    for: source,
                    fileName: message.content.attachmentDescription
                )
                guard NSWorkspace.shared.open(url) else {
                    throw NativeChatError.unavailable("macOS could not open this attachment.")
                }
            } catch is CancellationError {
                return
            } catch {
                attachmentOpenError = error.localizedDescription
            }
        }
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
        guard transcriptFocused, !selectedMessageIDs.isEmpty else { return }
        let text = selectedMessagesInTranscriptOrder.map(\.text).joined(separator: "\n\n")
        NSPasteboard.general.clearContents()
        NSPasteboard.general.setString(text, forType: .string)
    }

    func requestDeleteSelectedMessages() {
        guard transcriptFocused, canDeleteSelectedMessages else { return }
        showingDeleteConfirmation = true
    }

    func deleteSelectedMessages() {
        guard let chat = selectedChat, canDeleteSelectedMessages else { return }
        let identifiers = selectedMessagesInTranscriptOrder.map(\.id)
        if let replyingTo, identifiers.contains(replyingTo.id) { cancelReply() }
        showingDeleteConfirmation = false
        if previewMode {
            messages.removeAll { identifiers.contains($0.id) }
            clearMessageSelection()
            return
        }
        Task {
            do {
                try await core.deleteMessages(identifiers, from: chat)
                clearMessageSelection()
                messages = try await core.loadMessages(chatID: chat.id)
            } catch {
                phase = .failed(error.localizedDescription)
            }
        }
    }

    func clearMessageSelection() {
        selectedMessageIDs = []
        selectionAnchor = nil
    }

    func dismissNearestState() {
        if conversationSearchPresented {
            dismissConversationSearch()
        } else if replyingTo != nil {
            cancelReply()
        } else if !pendingAttachments.isEmpty {
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
            if let previousChatID = selectedChatID {
                saveComposerState(for: previousChatID)
            }
            selectedChatID = id
            clearMessageSelection()
            restoreComposerState(for: id)
            targetMessageID = nil
            conversationSearchText = ""
            conversationSearchPresented = false
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
            messages = loadedMessages
            selectedMessageIDs.formIntersection(messages.map(\.id))
            return true
        } catch is CancellationError {
            return false
        } catch {
            phase = .failed(error.localizedDescription)
            return false
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
