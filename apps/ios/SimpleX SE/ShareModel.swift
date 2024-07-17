//
//  ShareModel.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UniformTypeIdentifiers
import SwiftUI
import SimpleXChat

class ShareModel: ObservableObject {
    @Published var item: NSExtensionItem?
    @Published var chats = Array<ChatData>()
    @Published var search = String()
    @Published var comment = String()
    @Published var selected: ChatData?
    @Published var isLoaded = false
    @Published var progress: Double?
    @Published var error: ShareError?

    var completion: (() -> Void)?

    var filteredChats: Array<ChatData> {
        search.isEmpty
        ? filterChatsToForwardTo(chats: chats)
        : filterChatsToForwardTo(chats: chats)
            .filter { foundChat($0, search) }
    }

    init() {
        Task {
            switch fetchChats() {
            case let .success(chats):
                await MainActor.run {
                    self.chats = chats
                    withAnimation { self.isLoaded = true }
                }
            case let .failure(error):
                await MainActor.run { self.error = error }
            }
        }
    }

    func send() {
        Task {
            switch await self.sendMessage() {
            case let .success(item):
                await MainActor.run { self.progress = .zero }
                // Listen to the event loop for progress events
                EventLoop.shared.set(itemId: item.chatItem.id, model: self)
            case let .failure(error):
                await MainActor.run { self.error = error }
            }
        }
    }

    private func sendMessage() async -> Result<AChatItem, ShareError> {
        guard let chat = selected else { return .failure(.noChatWasSelected) }
        guard let attachment = item?.attachments?.first else { return .failure(.missingAttachment) }
        do {
            guard let type = attachment.firstMatching(of: [.image, .movie, .data]) else {
                return .failure(.unsupportedFormat)
            }
            let url = try await attachment.inPlaceUrl(type: type)
            guard let cryptoFile = saveFileFromURL(url) else { return .failure(.encryptFile) }
            SEChatState.shared.set(.sendingMessage)
            await waitForOtherProcessesToSuspend()
            let chatItem = try apiSendMessage(
                chatInfo: chat.chatInfo,
                cryptoFile: cryptoFile,
                msgContent: .file(comment)
            )
            SEChatState.shared.set(.inactive)
            if chatItem.chatInfo.chatType == .local { completion?() }
            return .success(chatItem)
        } catch {
            if let chatResponse = error as? ChatResponse {
                return .failure(.apiError(APIError(response: chatResponse)))
            } else {
                return .failure(.loadFileRepresentation(error))
            }
        }
    }

    private func fetchChats() -> Result<Array<ChatData>, ShareError> {
        registerGroupDefaults()
        haskell_init_se()
        let (_, result) = chatMigrateInit()
        guard (result == .ok) else {
            return .failure(.unexpectedMigrationResult(result))
        }
        do {
            guard let user = try apiGetActiveUser() else {
                return .failure(.noActiveUser)
            }
            try apiSetNetworkConfig(getNetCfg())
            try apiSetAppFilePaths(
                filesFolder: getAppFilesDirectory().path,
                tempFolder: getTempFilesDirectory().path,
                assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path
            )
            try apiSetEncryptLocalFiles(privacyEncryptLocalFilesGroupDefault.get())
            let isRunning = try apiStartChat()
            logger.log(level: .debug, "Chat Started. Is running: \(isRunning)")
            return .success(try apiGetChats(userId: user.id))
        } catch {
            return .failure(.apiError(APIError(response: error as! ChatResponse)))
        }
    }
    
    private func waitForOtherProcessesToSuspend() async {
        await withCheckedContinuation { continuation in
            var appSuspended = false
            var nseSuspended = false
            let _ = appMessageSubscriber {
                switch $0 {
                case let .state(appState):
                    appSuspended = appState == .suspended
                }
                if appSuspended && nseSuspended { continuation.resume() }
            }
            let _ = nseMessageSubscriber {
                switch $0 {
                case let .state(nseState):
                    nseSuspended = nseState == .suspended
                }
                if appSuspended && nseSuspended { continuation.resume() }
            }
            Task {
                // If other processes has not suspended in 2 seconds - assume they are not running
                try? await Task.sleep(for: .seconds(2))
                continuation.resume()
            }
        }
    }
}

extension NSItemProvider {
    fileprivate func inPlaceUrl(type: UTType) async throws -> URL {
        try await withCheckedThrowingContinuation { cont in
            let _ = loadFileRepresentation(for: type, openInPlace: true) { url, bool, error in
                if let url = url {
                    cont.resume(returning: url)
                } else if let error = error {
                    cont.resume(throwing: error)
                } else {
                    fatalError("Either `url` or `error` must be present")
                }
            }
        }
    }

    fileprivate func firstMatching(of types: Array<UTType>) -> UTType? {
        for type in types {
            if hasItemConformingToTypeIdentifier(type.identifier) { return type }
        }
        return nil
    }
}

// SEStateGroupDefault must not be used in the share extension directly, only via this singleton
class SEChatState {
    static let shared = SEChatState()
    private var value_ = seStateGroupDefault.get()

    var value: SEState {
        value_
    }

    func set(_ state: SEState) {
        seStateGroupDefault.set(state)
        sendSEState(state)
        value_ = state
    }
}

private final class EventLoop {
    static let shared = EventLoop()

    private var itemId: ChatItem.ID?
    private weak var model: ShareModel?

    func set(itemId: ChatItem.ID, model: ShareModel) {
        self.itemId = itemId
        self.model = model
    }

    init() {
        Task {
            while true {
                switch recvSimpleXMsg() {
                case let .sndFileProgressXFTP(_, aChatItem, _, sentSize, totalSize):
                    if let id = aChatItem?.chatItem.id, id == itemId {
                        await MainActor.run {
                            withAnimation { model?.progress = Double(sentSize) / Double(totalSize) }
                        }
                    }
                case let .sndFileCompleteXFTP(_, aChatItem, _):
                    if aChatItem.chatItem.id == itemId {
                        await MainActor.run {
                            withAnimation { model?.progress = 1 }
                            model?.completion!()
                        }
                    }
                default: break
                }
            }
        }
    }
}
