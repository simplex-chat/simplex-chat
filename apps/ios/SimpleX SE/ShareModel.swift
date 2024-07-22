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
    @Published var errorAlert: ErrorAlert?

    enum BottomBar {
        case sendButton
        case loadingSpinner
        case loadingBar(progress: Double)

        var isLoading: Bool {
            switch self {
            case .sendButton: false
            case .loadingSpinner: true
            case .loadingBar: true
            }
        }
    }

    var completion: (Error?) -> Void = { _ in
        fatalError("completion has not been set")
    }

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
                    withAnimation { isLoaded = true }
                }
            case let .failure(error):
                await MainActor.run { errorAlert = error }
            }
        }
    }

    func send() {
        Task {
            switch await sendMessage() {
            case let .success(item):
                let isGroupChat = item.chatInfo.chatType == .group
                if !isGroupChat {
                    await MainActor.run {
                        self.bottomBar = .loadingBar(progress: .zero)
                    }
                }
                completion(
                    await handleEvents(isGroupChat: isGroupChat, chatItemId: item.chatItem.id)
                )
            case let .failure(error):
                await MainActor.run { self.errorAlert = error }
            }
        }
    }

    private func sendMessage() async -> Result<AChatItem, ErrorAlert> {
        await MainActor.run { self.bottomBar = .loadingSpinner }
        guard let chat = selected else {
            return .failure(ErrorAlert("Chat Not Selected"))
        }
        guard let attachment = item?.attachments?.first else { 
            return .failure(ErrorAlert("Missing Attachment"))
        }
        do {
            guard let type = attachment.firstMatching(of: [.image, .movie, .data]) else {
                return .failure(ErrorAlert("Unsupported File Format"))
            }
            let url = try await attachment.inPlaceUrl(type: type)
            var cryptoFile: CryptoFile
            var msgContent: MsgContent
            switch type {

            // Prepare Image message
            case .image:

                // Animated
                if attachment.hasItemConformingToTypeIdentifier(UTType.gif.identifier) {
                    if let data = try? Data(contentsOf: url),
                       let image = UIImage(data: data),
                       let file = saveAnimImage(image),
                       let imageString = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) {
                        cryptoFile = file
                        msgContent = .image(text: comment, image: imageString)
                    } else { return .failure(ErrorAlert("Failed Preparing Image")) }

                // Static
                } else {
                    if let image = downsampleImage(at: url, to: MAX_DOWNSAMPLE_SIZE),
                       let file = saveImage(image),
                       let imageString = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) {
                        cryptoFile = file
                        msgContent = .image(text: comment, image: imageString)
                    } else { return .failure(ErrorAlert("Failed Preparing Image")) }
                }

            // Prepare Data message
            case .data:
                if let file = saveFileFromURL(url) {
                    msgContent = .file(comment)
                    cryptoFile = file
                } else { return .failure(ErrorAlert("Failed Preparing File")) }

            default:
                return .failure(ErrorAlert("Unsupported File Format"))
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
            return .failure(ErrorAlert(error))
        }
    }

    private func fetchChats() -> Result<Array<ChatData>, ErrorAlert> {
        registerGroupDefaults()
        haskell_init_se()
        let (_, result) = chatMigrateInit()
        guard (result == .ok) else { return .failure(ErrorAlert("Database Migration Failed")) }
        do {
            guard let user = try apiGetActiveUser() else {
                return .failure(
                    ErrorAlert(
                        title: "No Active User",
                        message: "Please create a user in the SimpleX App"
                    )
                )
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
            return .failure(ErrorAlert(error))
        }
    }
    
    /// Polls and processes chat events
    /// Returns when message sending has completed optionally returning and error.
    private func handleEvents(isGroupChat: Bool, chatItemId: ChatItem.ID) async -> ErrorAlert? {
        await withCheckedContinuation { continuation in
            func isMessage(for item: AChatItem?) -> Bool {
                item.map { $0.chatItem.id == chatItemId } ?? false
            }
            func resume(_ errorAlert: ErrorAlert? = nil) {
                isRunning = false
                groupChatTimeout?.cancel()
                continuation.resume(returning: errorAlert)
            }
            var groupChatTimeout: Task<Void, Never>?
            var isRunning = true
            while isRunning {
                let msg = recvSimpleXMsg()
                switch msg {
                case let .sndFileProgressXFTP(_, aChatItem, _, sentSize, totalSize):
                    guard isMessage(for: aChatItem) && !isGroupChat else { continue }
                    Task {
                        await MainActor.run {
                            withAnimation {
                                let progress = Double(sentSize) / Double(totalSize)
                                bottomBar = .loadingBar(progress: progress)
                            }
                        }
                    }
                case let .sndFileCompleteXFTP(_, aChatItem, _):
                    guard isMessage(for: aChatItem) else { continue }
                    resume()
                case let .chatItemStatusUpdated(_, aChatItem):
                    guard isMessage(for: aChatItem) else { continue }
                    if let statusInfo = aChatItem.chatItem.meta.itemStatus.statusInfo {
                        resume(
                            ErrorAlert(
                                title: LocalizedStringKey(statusInfo.0),
                                message: LocalizedStringKey(statusInfo.1)
                            )
                        )
                    } else if isGroupChat, groupChatTimeout == nil {
                        logger.info("group chat timeout: started")
                        groupChatTimeout = Task {
                            do {
                                await MainActor.run { bottomBar = .loadingBar(progress: .zero) }
                                try await Task.sleep(for: .seconds(0.2))
                                await MainActor.run {
                                    withAnimation(.linear(duration: 5)) { bottomBar = .loadingBar(progress: 1) }
                                }
                                try await Task.sleep(for: .seconds(5))
                                logger.info("group chat timeout: willResume")
                                continuation.resume(returning: nil)
                            } catch {
                                logger.info("group chat timeout: cancelled")
                            }
                        }
                    }
                case let .sndFileError(_, aChatItem, _, errorMessage):
                    guard isMessage(for: aChatItem) else { continue }
                    if let aChatItem { cleanupFile(aChatItem) }
                    resume(ErrorAlert(title: "Send File Error", message: "\(errorMessage)"))
                case let .sndFileWarning(_, aChatItem, _, errorMessage):
                    guard isMessage(for: aChatItem) else { continue }
                    if let aChatItem { cleanupFile(aChatItem) }
                    resume(ErrorAlert(title: "Send File Error", message: "\(errorMessage)"))
                case let .chatError(_, chatError):
                    resume(ErrorAlert(chatError))
                case let .chatCmdError(_, chatError):
                    resume(ErrorAlert(chatError))
                default: logger.debug("UnhandledMessage \(msg?.details ?? "")")
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
