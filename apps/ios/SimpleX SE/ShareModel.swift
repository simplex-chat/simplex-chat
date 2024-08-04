//
//  ShareModel.swift
//  SimpleX SE
//
//  Created by Levitating Pineapple on 09/07/2024.
//  Copyright © 2024 SimpleX Chat. All rights reserved.
//

import UniformTypeIdentifiers
import AVFoundation
import SwiftUI
import SimpleXChat

/// Maximum size of hex encoded media previews
private let MAX_DATA_SIZE: Int64 = 14000

/// Maximum dimension (width or height) of an image, before passed for processing
private let MAX_DOWNSAMPLE_SIZE: Int64 = 2000

class ShareModel: ObservableObject {
    @Published var sharedContent: SharedContent?
    @Published var chats: [ChatData] = []
    @Published var profileImages: [ChatInfo.ID: UIImage] = [:]
    @Published var search = ""
    @Published var comment = ""
    @Published var selected: ChatData?
    @Published var isLoaded = false
    @Published var bottomBar: BottomBar = .loadingSpinner
    @Published var errorAlert: ErrorAlert?
    @Published var hasSimplexLink = false
    @Published var alertRequiresPassword = false
    var networkTimeout = CFAbsoluteTimeGetCurrent()

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

    var completion: () -> Void = {
        fatalError("completion has not been set")
    }
    
    private var itemProvider: NSItemProvider?

    var isSendDisbled: Bool { sharedContent == nil || selected == nil || isProhibited(selected) }
    
    var isLinkPreview: Bool {
        switch sharedContent {
        case .url: true
        default: false
        }
    }

    func isProhibited(_ chat: ChatData?) -> Bool {
        if let chat, let sharedContent {
            sharedContent.prohibited(in: chat, hasSimplexLink: hasSimplexLink)
        } else { false }
    }

    var filteredChats: [ChatData] {
        search.isEmpty
        ? filterChatsToForwardTo(chats: chats)
        : filterChatsToForwardTo(chats: chats)
            .filter { foundChat($0, search.localizedLowercase) }
    }

    func setup(context: NSExtensionContext) {
        if performLAGroupDefault.get() && !allowShareExtensionGroupDefault.get() {
            errorAlert = ErrorAlert(title: "App is locked!", message: "You can allow sharing in Privacy & Security / SimpleX Lock settings.")
            return
        }
        if let item = context.inputItems.first as? NSExtensionItem,
           let itemProvider = item.attachments?.first {
            self.itemProvider = itemProvider
            self.completion = {
                ShareModel.CompletionHandler.isEventLoopEnabled = false
                context.completeRequest(returningItems: [item]) {
                    apiSuspendChat(expired: $0)
                }
            }
            setup()
        }
    }

    func setup(with dbKey: String? = nil) {
        // Init Chat
        Task {
            if let e = initChat(with: dbKey) {
                await MainActor.run { errorAlert = e }
            } else {
                // Load Chats
                Task {
                    switch fetchChats() {
                    case let .success(chats):
                        // Decode base64 images on background thread
                        let profileImages = chats.reduce(into: Dictionary<ChatInfo.ID, UIImage>()) { dict, chatData in
                            if let profileImage = chatData.chatInfo.image,
                               let uiImage = UIImage(base64Encoded: profileImage) {
                                dict[chatData.id] = uiImage
                            }
                        }
                        await MainActor.run {
                            self.chats = chats
                            self.profileImages = profileImages
                            withAnimation { isLoaded = true }
                        }
                    case let .failure(error):
                        await MainActor.run { errorAlert = error }
                    }
                }
                // Process Attachment
                Task {
                    switch await getSharedContent(self.itemProvider!) {
                    case let .success(chatItemContent):
                        await MainActor.run {
                            self.sharedContent = chatItemContent
                            self.bottomBar = .sendButton
                            if case let .text(string) = chatItemContent { comment = string }
                        }
                    case let .failure(errorAlert):
                        await MainActor.run { self.errorAlert = errorAlert }
                    }
                }
            }
        }
    }

    func send() {
        if let sharedContent, let selected {
            Task {
                await MainActor.run { self.bottomBar = .loadingSpinner }
                do {
                    SEChatState.shared.set(.sendingMessage)
                    await waitForOtherProcessesToSuspend()
                    let ci = try apiSendMessage(
                        chatInfo: selected.chatInfo,
                        cryptoFile: sharedContent.cryptoFile,
                        msgContent: sharedContent.msgContent(comment: self.comment)
                    )
                    if selected.chatInfo.chatType == .local {
                        completion()
                    } else {
                        await MainActor.run { self.bottomBar = .loadingBar(progress: 0) }
                        if let e = await handleEvents(
                            isGroupChat: ci.chatInfo.chatType == .group,
                            isWithoutFile: sharedContent.cryptoFile == nil,
                            chatItemId: ci.chatItem.id
                        ) {
                            await MainActor.run { errorAlert = e }
                        } else {
                            completion()
                        }
                    }
                } catch {
                    if let e = error as? ErrorAlert {
                        await MainActor.run { errorAlert = e }
                    }
                }
            }
        }
    }

    private func initChat(with dbKey: String? = nil) -> ErrorAlert? {
        do {
            if hasChatCtrl() && dbKey == nil {
                try apiActivateChat()
            } else {
                resetChatCtrl() // Clears retained migration result
                registerGroupDefaults()
                haskell_init_se()
                let (_, result) = chatMigrateInit(dbKey, confirmMigrations: defaultMigrationConfirmation())
                if let e = migrationError(result) { return e }
                try apiSetAppFilePaths(
                    filesFolder: getAppFilesDirectory().path,
                    tempFolder: getTempFilesDirectory().path,
                    assetsFolder: getWallpaperDirectory().deletingLastPathComponent().path
                )
                let isRunning = try apiStartChat()
                logger.log(level: .debug, "chat started, running: \(isRunning)")
            }
            try apiSetNetworkConfig(getNetCfg())
            try apiSetEncryptLocalFiles(privacyEncryptLocalFilesGroupDefault.get())
        } catch { return ErrorAlert(error) }
        return nil
    }

    private func migrationError(_ r: DBMigrationResult) -> ErrorAlert? {
        let useKeychain = storeDBPassphraseGroupDefault.get()
        let storedDBKey = kcDatabasePassword.get()
        if case .errorNotADatabase = r {
            Task { await MainActor.run { self.alertRequiresPassword = true } }
        }
        return switch r {
        case .errorNotADatabase:
            if useKeychain && storedDBKey != nil && storedDBKey != "" {
                ErrorAlert(
                    title: "Wrong database passphrase",
                    message: "Database passphrase is different from saved in the keychain."
                )
            } else {
                ErrorAlert(
                    title: "Database encrypted!",
                    message: "Database passphrase is required to open chat."
                )
            }
        case let .errorMigration(_, migrationError):
            switch migrationError {
            case .upgrade:
                ErrorAlert(
                    title: "Database upgrade required",
                    message: "Open the app to upgrade the database."
                )
            case .downgrade:
                ErrorAlert(
                    title: "Database downgrade required",
                    message: "Open the app to downgrade the database."
                )
            case let .migrationError(mtrError):
                ErrorAlert(
                    title: "Incompatible database version",
                    message: mtrErrorDescription(mtrError)
                )
            }
        case let .errorSQL(_, migrationSQLError):
            ErrorAlert(
                title: "Database error",
                message: "Error: \(migrationSQLError)"
            )
        case .errorKeychain:
            ErrorAlert(
                title: "Keychain error",
                message: "Cannot access keychain to save database password"
            )
        case .invalidConfirmation:
            ErrorAlert("Invalid migration confirmation")
        case let .unknown(json):
            ErrorAlert(
                title: "Database error",
                message: "Unknown database error: \(json)"
            )
        case .ok: nil
        }
    }
    
    private func fetchChats() -> Result<Array<ChatData>, ErrorAlert> {
        do {
            guard let user = try apiGetActiveUser() else {
                return .failure(
                    ErrorAlert(
                        title: "No active profile",
                        message: "Please create a profile in the SimpleX app"
                    )
                )
            }
            return .success(try apiGetChats(userId: user.id))
        } catch {
            return .failure(ErrorAlert(error))
        }
    }

    actor CompletionHandler {
        static var isEventLoopEnabled = false
        private var fileCompleted = false
        private var messageCompleted = false

        func completeFile() { fileCompleted = true }

        func completeMessage() { messageCompleted = true }

        var isRunning: Bool {
            Self.isEventLoopEnabled && !(fileCompleted && messageCompleted)
        }
    }

    /// Polls and processes chat events
    /// Returns when message sending has completed optionally returning and error.
    private func handleEvents(isGroupChat: Bool, isWithoutFile: Bool, chatItemId: ChatItem.ID) async -> ErrorAlert? {
        func isMessage(for item: AChatItem?) -> Bool {
            item.map { $0.chatItem.id == chatItemId } ?? false
        }

        CompletionHandler.isEventLoopEnabled = true
        let ch = CompletionHandler()
        if isWithoutFile { await ch.completeFile() }
        networkTimeout = CFAbsoluteTimeGetCurrent()
        while await ch.isRunning {
            if CFAbsoluteTimeGetCurrent() - networkTimeout > 30 {
                await MainActor.run {
                    self.errorAlert = ErrorAlert(title: "Slow network?", message: "Sending a message takes longer than expected.") {
                        Button("Wait", role: .cancel) { self.networkTimeout = CFAbsoluteTimeGetCurrent() }
                        Button("Cancel", role: .destructive) { self.completion() }
                    }
                }
            }
            switch recvSimpleXMsg(messageTimeout: 1_000_000) {
            case let .sndFileProgressXFTP(_, ci, _, sentSize, totalSize):
                guard isMessage(for: ci) else { continue }
                networkTimeout = CFAbsoluteTimeGetCurrent()
                await MainActor.run {
                    withAnimation {
                        let progress = Double(sentSize) / Double(totalSize)
                        bottomBar = .loadingBar(progress: progress)
                    }
                }
            case let .sndFileCompleteXFTP(_, ci, _):
                guard isMessage(for: ci) else { continue }
                if isGroupChat {
                    await MainActor.run { bottomBar = .loadingSpinner }
                }
                await ch.completeFile()
                if await !ch.isRunning { break }
            case let .chatItemStatusUpdated(_, ci):
                guard isMessage(for: ci) else { continue }
                if let (title, message) = ci.chatItem.meta.itemStatus.statusInfo {
                    // `title` and `message` already localized and interpolated
                    return ErrorAlert(
                        title: "\(title)",
                        message: "\(message)"
                    )
                } else if case let .sndSent(sndProgress) = ci.chatItem.meta.itemStatus {
                    switch sndProgress {
                    case .complete:
                        await ch.completeMessage()
                    case .partial:
                        if isGroupChat {
                            Task {
                                try? await Task.sleep(nanoseconds: 5 * NSEC_PER_SEC)
                                await ch.completeMessage()
                            }
                        }
                    }
                }
            case let .sndFileError(_, ci, _, errorMessage):
                guard isMessage(for: ci) else { continue }
                if let ci { cleanupFile(ci) }
                return ErrorAlert(title: "File error", message: "\(fileErrorInfo(ci) ?? errorMessage)")
            case let .sndFileWarning(_, ci, _, errorMessage):
                guard isMessage(for: ci) else { continue }
                if let ci { cleanupFile(ci) }
                return ErrorAlert(title: "File error", message: "\(fileErrorInfo(ci) ?? errorMessage)")
            case let .chatError(_, chatError):
                return ErrorAlert(chatError)
            case let .chatCmdError(_, chatError):
                return ErrorAlert(chatError)
            default: continue
            }
        }
        return nil
    }
    
    private func fileErrorInfo(_ ci: AChatItem?) -> String? {
        switch ci?.chatItem.file?.fileStatus {
        case let .sndError(e): e.errorInfo
        case let .sndWarning(e): e.errorInfo
        default: nil
        }
    }
}

/// Chat Item Content extracted from `NSItemProvider` without the comment
enum SharedContent {
    case image(preview: String, cryptoFile: CryptoFile)
    case movie(preview: String, duration: Int, cryptoFile: CryptoFile)
    case url(preview: LinkPreview)
    case text(string: String)
    case data(cryptoFile: CryptoFile)

    var cryptoFile: CryptoFile? {
        switch self {
        case let .image(_, cryptoFile): cryptoFile
        case let .movie(_, _, cryptoFile): cryptoFile
        case .url: nil
        case .text: nil
        case let .data(cryptoFile): cryptoFile
        }
    }

    func msgContent(comment: String) -> MsgContent {
        switch self {
        case let .image(preview, _): .image(text: comment, image: preview)
        case let .movie(preview, duration, _): .video(text: comment, image: preview, duration: duration)
        case let .url(preview): .link(text: preview.uri.absoluteString + (comment == "" ? "" : "\n" + comment), preview: preview)
        case .text: .text(comment)
        case .data: .file(comment)
        }
    }

    func prohibited(in chatData: ChatData, hasSimplexLink: Bool) -> Bool {
        chatData.prohibitedByPref(
            hasSimplexLink: hasSimplexLink,
            isMediaOrFileAttachment: cryptoFile != nil,
            isVoice: false
        )
    }
}

fileprivate func getSharedContent(_ ip: NSItemProvider) async -> Result<SharedContent, ErrorAlert> {
    if let type = firstMatching(of: [.image, .movie, .fileURL, .url, .text]) {
        switch type {
            // Prepare Image message
        case .image:
            // Animated
            return if ip.hasItemConformingToTypeIdentifier(UTType.gif.identifier) {
                if let url = try? await inPlaceUrl(type: type),
                   let data = try? Data(contentsOf: url),
                   let image = UIImage(data: data),
                   let cryptoFile = saveFile(data, generateNewFileName("IMG", "gif"), encrypted: privacyEncryptLocalFilesGroupDefault.get()),
                   let preview = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) {
                    .success(.image(preview: preview, cryptoFile: cryptoFile))
                } else { .failure(ErrorAlert("Error preparing message")) }

            // Static
            } else {
                if let image = await staticImage(),
                   let cryptoFile = saveImage(image),
                   let preview = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE) {
                    .success(.image(preview: preview, cryptoFile: cryptoFile))
                } else { .failure(ErrorAlert("Error preparing message")) }
            }

        // Prepare Movie message
        case .movie:
            if let url = try? await inPlaceUrl(type: type),
               let trancodedUrl = await transcodeVideo(from: url),
               let (image, duration) = AVAsset(url: trancodedUrl).generatePreview(),
               let preview = resizeImageToStrSize(image, maxDataSize: MAX_DATA_SIZE),
               let cryptoFile = moveTempFileFromURL(trancodedUrl) {
                try? FileManager.default.removeItem(at: trancodedUrl)
                return .success(.movie(preview: preview, duration: duration, cryptoFile: cryptoFile))
            } else { return .failure(ErrorAlert("Error preparing message")) }

        // Prepare Data message
        case .fileURL:
            if let url = try? await inPlaceUrl(type: .data) {
                if isFileTooLarge(for: url) {
                    let sizeString = ByteCountFormatter.string(
                        fromByteCount: Int64(getMaxFileSize(.xftp)),
                        countStyle: .binary
                    )
                    return .failure(
                        ErrorAlert(
                            title: "Large file!",
                            message: "Currently maximum supported file size is \(sizeString)."
                        )
                    )
                }
                if let file = saveFileFromURL(url) {
                    return .success(.data(cryptoFile: file))
                }
            }
            return .failure(ErrorAlert("Error preparing file"))

        // Prepare Link message
        case .url:
            if let url = try? await ip.loadItem(forTypeIdentifier: type.identifier) as? URL {
                let content: SharedContent =
                    if privacyLinkPreviewsGroupDefault.get(), let linkPreview = await getLinkPreview(for: url) {
                        .url(preview: linkPreview)
                    } else {
                        .text(string: url.absoluteString)
                    }
                return .success(content)
            } else { return .failure(ErrorAlert("Error preparing message")) }

        // Prepare Text message
        case .text:
            return if let text = try? await ip.loadItem(forTypeIdentifier: type.identifier) as? String {
                .success(.text(string: text))
            } else { .failure(ErrorAlert("Error preparing message")) }
        default: return .failure(ErrorAlert("Unsupported format"))
        }
    } else {
        return .failure(ErrorAlert("Unsupported format"))
    }


    func inPlaceUrl(type: UTType) async throws -> URL {
        try await withCheckedThrowingContinuation { cont in
            let _ = ip.loadInPlaceFileRepresentation(forTypeIdentifier: type.identifier) { url, bool, error in
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

    func firstMatching(of types: Array<UTType>) -> UTType? {
        for type in types {
            if ip.hasItemConformingToTypeIdentifier(type.identifier) { return type }
        }
        return nil
    }

    func staticImage() async -> UIImage? {
        if let url = try? await inPlaceUrl(type: .image),
           let downsampledImage = downsampleImage(at: url, to: MAX_DOWNSAMPLE_SIZE) {
            downsampledImage
        } else {
            /// Fallback to loading image directly from `ItemProvider`
            /// in case loading from disk is not possible. Required for sharing screenshots.
            try? await ip.loadItem(forTypeIdentifier: UTType.image.identifier) as? UIImage
        }
    }
}


fileprivate func transcodeVideo(from input: URL) async -> URL? {
    let outputUrl = URL(
        fileURLWithPath: generateNewFileName(
            getTempFilesDirectory().path + "/" + "video", "mp4",
            fullPath: true
        )
    )
    if await makeVideoQualityLower(input, outputUrl: outputUrl) {
        return outputUrl
    } else {
        try? FileManager.default.removeItem(at: outputUrl)
        return nil
    }
}

fileprivate func isFileTooLarge(for url: URL) -> Bool {
    fileSize(url)
        .map { $0 > getMaxFileSize(.xftp) }
        ?? false
}

