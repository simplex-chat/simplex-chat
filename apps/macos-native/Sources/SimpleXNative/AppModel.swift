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

    @Published var phase: Phase = .locked(message: nil)
    @Published var profile: NativeProfile?
    @Published var chats: [NativeChat] = []
    @Published var selectedChatID: NativeChat.ID?
    @Published var messages: [NativeMessage] = []
    @Published var draft = ""
    @Published var searchText = ""
    @Published var isLoadingConversation = false
    @Published var isSending = false
    @Published var selectedMessageIDs: Set<Int64> = []
    @Published var transcriptFocused = false
    @Published var pendingAttachments: [PendingAttachment] = []
    @Published var attachmentError: String?
    @Published var showingDeleteConfirmation = false
    @Published var targetMessageID: Int64?
    @Published var density: DesktopChatDensity {
        didSet { UserDefaults.standard.set(density.rawValue, forKey: Self.densityKey) }
    }

    private let core = SimpleXCore()
    private weak var notificationManager: NativeNotificationManager?
    private var eventTask: Task<Void, Never>?
    private var notificationRouteQueue = NotificationRouteQueue()
    private var selectionAnchor: Int64?
    private static let densityKey = "desktopChatDensity"

    init(notificationManager: NativeNotificationManager? = nil) {
        density = DesktopChatDensity(
            rawValue: UserDefaults.standard.string(forKey: Self.densityKey) ?? ""
        ) ?? .compact
        self.notificationManager = notificationManager
        notificationManager?.model = self
    }

    deinit {
        eventTask?.cancel()
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

    func unlock(passphrase: String) {
        guard phase != .opening else { return }
        phase = .opening
        Task {
            do {
                let (profile, chats) = try await core.open(passphrase: passphrase)
                self.profile = profile
                self.chats = chats
                self.phase = .ready
                if selectedChatID == nil { selectedChatID = chats.first?.id }
                await loadSelectedConversation()
                startEventLoop()
                notificationManager?.chatSetupReady()
                consumePendingNotificationRoutes()
            } catch {
                phase = .locked(message: error.localizedDescription)
            }
        }
    }

    func selectChat(_ id: NativeChat.ID?) {
        guard selectedChatID != id else { return }
        selectedChatID = id
        clearMessageSelection()
        pendingAttachments = []
        targetMessageID = nil
        if let id { notificationManager?.removeDeliveredNotifications(chatID: id) }
        Task { await loadSelectedConversation() }
    }

    func refresh() {
        guard let userID = profile?.userID else { return }
        Task {
            do {
                chats = try await core.loadChats(userID: userID)
                await loadSelectedConversation()
            } catch {
                phase = .failed(error.localizedDescription)
            }
        }
    }

    func sendDraft() {
        let text = draft.trimmingCharacters(in: .whitespacesAndNewlines)
        let attachments = pendingAttachments
        guard (!text.isEmpty || !attachments.isEmpty), let chat = selectedChat, !isSending else { return }
        draft = ""
        isSending = true
        Task {
            do {
                if attachments.isEmpty {
                    try await core.sendText(text, to: chat)
                } else {
                    for (index, attachment) in attachments.enumerated() {
                        let caption = index == attachments.index(before: attachments.endIndex) ? text : ""
                        try await core.sendAttachment(attachment, caption: caption, to: chat)
                        pendingAttachments.removeAll { $0.id == attachment.id }
                    }
                }
                messages = try await core.loadMessages(chatID: chat.id)
                if let userID = profile?.userID { chats = try await core.loadChats(userID: userID) }
            } catch {
                draft = text
                phase = .failed(error.localizedDescription)
            }
            isSending = false
        }
    }

    func chooseAttachments() {
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
        let urls = NSPasteboard.general.readObjects(forClasses: [NSURL.self]) as? [URL] ?? []
        if urls.isEmpty {
            attachmentError = "The clipboard does not contain any files."
        } else {
            stageAttachments(urls)
        }
    }

    func removeAttachment(_ id: PendingAttachment.ID) {
        pendingAttachments.removeAll { $0.id == id }
    }

    func reorderAttachment(_ source: PendingAttachment.ID, before destination: PendingAttachment.ID) {
        pendingAttachments = PendingAttachment.reordered(pendingAttachments, from: source, before: destination)
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
        showingDeleteConfirmation = false
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
        if !pendingAttachments.isEmpty {
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
        selectedChatID = route.chatID
        clearMessageSelection()
        targetMessageID = route.messageID
        notificationManager?.removeDeliveredNotifications(chatID: route.chatID)
        Task { await loadSelectedConversation(around: route.messageID) }
    }

    private func loadSelectedConversation(around messageID: Int64? = nil) async {
        guard let chat = selectedChat else {
            messages = []
            return
        }
        isLoadingConversation = true
        do {
            messages = try await core.loadMessages(chatID: chat.id, around: messageID)
            selectedMessageIDs.formIntersection(messages.map(\.id))
        } catch {
            phase = .failed(error.localizedDescription)
        }
        isLoadingConversation = false
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
            if selectedChatID != nil {
                await loadSelectedConversation()
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
}
