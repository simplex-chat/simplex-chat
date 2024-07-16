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
            case let .success(aChatItem):
                await MainActor.run { progress = .zero }
                startEventLoop(for: aChatItem)
            case let .failure(error):
                self.error = error
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
            return .success(
                try apiSendMessage(
                    chatInfo: chat.chatInfo,
                    cryptoFile: cryptoFile,
                    msgContent: .file(comment)
                )
            )
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
            let isRunning = try apiStartChat()
            logger.log(level: .debug, "Chat Started. Is running: \(isRunning)")
            return .success(try apiGetChats(userId: user.id))
        } catch {
            return .failure(.apiError(APIError(response: error as! ChatResponse)))
        }
    }

    private func startEventLoop(for sent: AChatItem) {
        Task {
            while true {
                switch recvSimpleXMsg() {
                case let .sndFileProgressXFTP(_, aChatItem, _, sentSize, totalSize):
                    if let id = aChatItem?.chatItem.id, id == sent.chatItem.id {
                        await MainActor.run {
                            withAnimation { self.progress = Double(sentSize) / Double(totalSize) }
                        }
                    }
                case let .sndFileCompleteXFTP(_, aChatItem, _):
                    if aChatItem.chatItem.id == sent.chatItem.id {
                        await MainActor.run {
                            withAnimation { self.progress = 1 }
                            self.completion!()
                        }
                    }
                default: break
                }
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
