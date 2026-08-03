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

    private let core = SimpleXCore()
    private var eventTask: Task<Void, Never>?

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
            } catch {
                phase = .locked(message: error.localizedDescription)
            }
        }
    }

    func selectChat(_ id: NativeChat.ID?) {
        selectedChatID = id
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
        guard !text.isEmpty, let chat = selectedChat, !isSending else { return }
        draft = ""
        isSending = true
        Task {
            do {
                try await core.sendText(text, to: chat)
                messages = try await core.loadMessages(chatID: chat.id)
                if let userID = profile?.userID { chats = try await core.loadChats(userID: userID) }
            } catch {
                draft = text
                phase = .failed(error.localizedDescription)
            }
            isSending = false
        }
    }

    private func loadSelectedConversation() async {
        guard let chat = selectedChat else {
            messages = []
            return
        }
        isLoadingConversation = true
        do {
            messages = try await core.loadMessages(chatID: chat.id)
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
                guard await core.waitForEvent() else { continue }
                await refreshAfterEvent()
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
        } catch {
            phase = .failed(error.localizedDescription)
        }
    }
}
