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

/// Maximum size of hex encoded media previews
private let MAX_DATA_SIZE: Int64 = 14000

/// Maximum dimension (width or height) of an image, before passed for processing
private let MAX_DOWNSAMPLE_SIZE: Int64 = 2000

class ShareModel: ObservableObject {
    @Published var item: NSExtensionItem?
    @Published var chats = Array<ChatData>()
    @Published var search = String()
    @Published var comment = String()
    @Published var selected: ChatData?
    @Published var isLoaded = false
    @Published var bottomBar: BottomBar = .sendButton
    @Published var error: ShareError?

    enum BottomBar {
        case sendButton
        case loadingSpinner
        case loadingBar(progress: Double)
    }

    var completion: ((Error?) -> Void)!

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
                await MainActor.run { self.bottomBar = .loadingBar(progress: .zero) }
                // Listen to the event loop for progress events
                EventLoop.shared.set(itemId: item.chatItem.id, model: self)
            case let .failure(error):
                await MainActor.run { self.error = error }
            }
        }
    }

    private func sendMessage() async -> Result<AChatItem, ShareError> {
        await MainActor.run { self.bottomBar = .loadingSpinner }
        guard let chat = selected else { return .failure(.noChatWasSelected) }
        guard let attachment = item?.attachments?.first else { return .failure(.missingAttachment) }
        do {
            guard let type = attachment.firstMatching(of: [.image, .movie, .data]) else {
                return .failure(.unsupportedFormat)
            }
            let url = try await attachment.inPlaceUrl(type: type)
            var cryptoFile: CryptoFile
            var msgContent: MsgContent
            switch type {

            // Prepare Image message
            case .image:

                // Animated
                if attachment.hasItemConformingToTypeIdentifier(UTType.gif.identifier) {
                    guard let data = try? Data(contentsOf: url), let image = UIImage(data: data) else {
                        return .failure(.mediaLoading)
                    }
                    guard let file = saveAnimImage(image) else {
                        return .failure(.encryptFile)
                    }
                    guard let imageString = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) else {
                        return .failure(.previewGeneration)
                    }
                    cryptoFile = file
                    msgContent = .image(text: comment, image: imageString)

                // Static
                } else {
                    guard let image = downsampleImage(at: url, to: MAX_DOWNSAMPLE_SIZE) else {
                        return .failure(.imageDownsampling)
                    }
                    guard let file = saveImage(image) else {
                        return .failure(.encryptFile)
                    }
                    guard let imageString = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) else {
                        return .failure(.previewGeneration)
                    }
                    cryptoFile = file
                    msgContent = .image(text: comment, image: imageString)
                }

            // Prepare Data message
            case .data:
                msgContent = .file(comment)
                guard let file = saveFileFromURL(url) else { return .failure(.encryptFile) }
                cryptoFile = file
            default:
                return .failure(.unsupportedFormat)
            }

            // Send
            SEChatState.shared.set(.sendingMessage)
            await waitForOtherProcessesToSuspend()
            let chatItem = try apiSendMessage(
                chatInfo: chat.chatInfo,
                cryptoFile: cryptoFile,
                msgContent: msgContent
            )
            SEChatState.shared.set(.inactive)
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

