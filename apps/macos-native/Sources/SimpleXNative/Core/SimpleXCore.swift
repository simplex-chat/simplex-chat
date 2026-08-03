import CoreBridge
import Foundation

private final class SimpleXControllerHandle: @unchecked Sendable {
    // Safety: the handle is privately owned by SimpleXCore, never escapes that actor,
    // and closes its immutable native pointer exactly once during teardown.
    // TODO: remove the unchecked conformance when imported C pointers support Sendable ownership.
    let pointer: UnsafeMutableRawPointer

    init(_ pointer: UnsafeMutableRawPointer) {
        self.pointer = pointer
    }

    deinit {
        if let result = sx_core_close_store(pointer) {
            sx_core_free(result)
        }
    }
}

actor SimpleXCore {
    private var controller: SimpleXControllerHandle?
    private var loaded = false
    private let decryptedFilesDirectory: URL

    init() {
        decryptedFilesDirectory = FileManager.default.temporaryDirectory
            .appendingPathComponent("chat.simplex.native", isDirectory: true)
            .appendingPathComponent("OpenedAttachments", isDirectory: true)
            .appendingPathComponent(UUID().uuidString, isDirectory: true)
    }

    deinit {
        try? FileManager.default.removeItem(at: decryptedFilesDirectory)
    }

    func open(passphrase: String) throws -> (NativeProfile, [NativeChat]) {
        try loadIfNeeded()
        let migration = try migrate(passphrase: passphrase)
        guard NativeChatParser.migrationSucceeded(migration) else {
            throw NativeChatError.core(NativeChatParser.migrationError(from: migration))
        }
        guard controller != nil else {
            throw NativeChatError.unavailable("The SimpleX core opened the database without returning a controller.")
        }

        try configureFilePaths()
        try ensureCommandSucceeded(send("/_start main=on snd_files=on"))
        let profile = try NativeChatParser.profile(from: send("/u"))
        let chats = try NativeChatParser.chats(from: send("/_get chats \(profile.userID) pcc=on"))
        return (profile, chats)
    }

    func loadChats(userID: Int64) throws -> [NativeChat] {
        try NativeChatParser.chats(from: send("/_get chats \(userID) pcc=on"))
    }

    func loadMessages(chatID: String, around messageID: Int64? = nil) throws -> [NativeMessage] {
        try NativeChatParser.messages(from: send(Self.chatPageCommand(
            chatID: chatID,
            around: messageID,
            count: 75
        )))
    }

    func loadMessage(chatID: String, itemID: Int64) throws -> NativeMessage? {
        let messages = try NativeChatParser.messages(from: send(Self.chatPageCommand(
            chatID: chatID,
            around: itemID,
            count: 0
        )))
        return messages.first(where: { $0.id == itemID })
    }

    nonisolated static func chatPageCommand(
        chatID: String,
        around messageID: Int64?,
        count: Int
    ) -> String {
        let pagination = messageID.map { "around=\($0) count=\(count)" } ?? "count=\(count)"
        return "/_get chat \(chatID) \(pagination)"
    }

    func sendText(_ text: String, quotedItemID: Int64?, to chat: NativeChat) throws {
        guard chat.kind.canSend else {
            throw NativeChatError.unavailable("This conversation cannot accept messages yet.")
        }
        try sendComposedMessage(
            Self.composedMessage(
                messageContent: ["type": "text", "text": text],
                quotedItemID: quotedItemID
            ),
            to: chat
        )
    }

    func sendAttachment(
        _ attachment: PendingAttachment,
        caption: String,
        quotedItemID: Int64?,
        to chat: NativeChat
    ) throws {
        guard chat.kind.canSend else {
            throw NativeChatError.unavailable("This conversation cannot accept attachments yet.")
        }
        let messageContent: [String: Any]
        switch attachment.kind {
        case .image:
            messageContent = [
                "type": "image",
                "text": caption,
                "image": attachment.previewImage ?? "",
            ]
        case .video:
            messageContent = [
                "type": "video",
                "text": caption,
                "image": "",
                "duration": 0,
            ]
        case .document:
            messageContent = ["type": "file", "text": caption]
        }
        try sendComposedMessage(
            Self.composedMessage(
                messageContent: messageContent,
                fileSource: [
                    "filePath": attachment.url.path,
                    "cryptoArgs": NSNull(),
                ],
                quotedItemID: quotedItemID
            ),
            to: chat
        )
    }

    func openableURL(for source: NativeCryptoFile, fileName: String?) throws -> URL {
        let sourceURL = source.sourceURL
        guard FileManager.default.fileExists(atPath: sourceURL.path) else {
            throw NativeChatError.unavailable("The attachment is no longer stored on this Mac.")
        }
        guard let cryptoArgs = source.cryptoArgs else {
            try Self.validateImageHeader(at: sourceURL, fileName: fileName)
            return sourceURL
        }

        try loadIfNeeded()
        let directory = decryptedFilesDirectory.appendingPathComponent(UUID().uuidString, isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let safeName = Self.openedFileName(preferredName: fileName, sourcePath: source.filePath)
        let destination = directory.appendingPathComponent(safeName.isEmpty ? "Attachment" : safeName)
        let result = sourceURL.path.withCString { fromPath in
            cryptoArgs.fileKey.withCString { key in
                cryptoArgs.fileNonce.withCString { nonce in
                    destination.path.withCString { toPath in
                        sx_core_decrypt_file(fromPath, key, nonce, toPath)
                    }
                }
            }
        }
        guard let result else {
            try? FileManager.default.removeItem(at: directory)
            throw NativeChatError.unavailable("SimpleX could not decrypt the attachment.")
        }
        defer { sx_core_free(result) }
        let error = String(cString: result)
        guard error.isEmpty else {
            try? FileManager.default.removeItem(at: directory)
            throw NativeChatError.core(error)
        }
        guard FileManager.default.fileExists(atPath: destination.path) else {
            try? FileManager.default.removeItem(at: directory)
            throw NativeChatError.unavailable("SimpleX decrypted the attachment without creating a readable file.")
        }
        do {
            try Self.validateImageHeader(at: destination, fileName: fileName)
        } catch {
            try? FileManager.default.removeItem(at: directory)
            throw error
        }
        return destination
    }

    nonisolated static func imageHeaderIsReadable(_ data: Data, fileName: String?) -> Bool {
        let fileExtension = URL(fileURLWithPath: fileName ?? "").pathExtension.lowercased()
        switch fileExtension {
        case "jpg", "jpeg":
            return data.starts(with: [0xff, 0xd8, 0xff])
        case "png":
            return data.starts(with: [0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])
        case "gif":
            return data.starts(with: Data("GIF87a".utf8)) || data.starts(with: Data("GIF89a".utf8))
        case "tif", "tiff":
            return data.starts(with: [0x49, 0x49, 0x2a, 0x00])
                || data.starts(with: [0x4d, 0x4d, 0x00, 0x2a])
        case "bmp":
            return data.starts(with: Data("BM".utf8))
        case "webp":
            return data.count >= 12
                && data.prefix(4) == Data("RIFF".utf8)
                && data.dropFirst(8).prefix(4) == Data("WEBP".utf8)
        case "heic", "heif", "avif":
            return data.count >= 12 && data.dropFirst(4).prefix(4) == Data("ftyp".utf8)
        default:
            return true
        }
    }

    nonisolated static func openedFileName(preferredName: String?, sourcePath: String) -> String {
        let preferred = URL(fileURLWithPath: preferredName ?? "").lastPathComponent
        let source = URL(fileURLWithPath: sourcePath).lastPathComponent
        if preferred.isEmpty { return source }
        if URL(fileURLWithPath: preferred).pathExtension.isEmpty,
           !URL(fileURLWithPath: source).pathExtension.isEmpty {
            return source
        }
        return preferred
    }

    private nonisolated static func validateImageHeader(at url: URL, fileName: String?) throws {
        let header = try Data(contentsOf: url, options: .mappedIfSafe).prefix(16)
        let resolvedName = openedFileName(preferredName: fileName, sourcePath: url.path)
        guard imageHeaderIsReadable(Data(header), fileName: resolvedName) else {
            throw NativeChatError.unavailable(
                "SimpleX could not decode this image. Its stored copy may still be encrypted or incomplete."
            )
        }
    }

    nonisolated static func composedMessage(
        messageContent: [String: Any],
        fileSource: [String: Any]? = nil,
        quotedItemID: Int64?
    ) -> [String: Any] {
        var message: [String: Any] = [
            "msgContent": messageContent,
            "mentions": [String: Int64](),
        ]
        if let fileSource { message["fileSource"] = fileSource }
        if let quotedItemID { message["quotedItemId"] = quotedItemID }
        return message
    }

    func deleteMessages(_ messageIDs: [Int64], from chat: NativeChat) throws {
        guard !messageIDs.isEmpty else { return }
        let identifiers = messageIDs.map(String.init).joined(separator: ",")
        try ensureCommandSucceeded(send("/_delete item \(chat.kind.rawValue)\(chat.apiID) \(identifiers) internal"))
    }

    func receiveEvent(timeoutMicroseconds: Int32 = 500_000) -> Data? {
        guard let controller,
              let pointer = sx_core_recv_msg_wait(controller.pointer, timeoutMicroseconds) else {
            return nil
        }
        defer { sx_core_free(pointer) }
        guard let response = String(validatingUTF8: pointer), !response.isEmpty else { return nil }
        return response.data(using: .utf8)
    }

    private func sendComposedMessage(_ message: [String: Any], to chat: NativeChat) throws {
        try ensureCommandSucceeded(send(Self.sendCommand(message: message, to: chat)))
    }

    nonisolated static func sendCommand(message: [String: Any], to chat: NativeChat) throws -> String {
        if message["quotedItemId"] != nil, !chat.kind.canReply {
            throw NativeChatError.unavailable("Replies are not supported in this conversation.")
        }
        let content = [message]
        let data = try JSONSerialization.data(withJSONObject: content)
        guard let json = String(data: data, encoding: .utf8) else {
            throw NativeChatError.invalidResponse("The message could not be encoded.")
        }
        if chat.kind == .local {
            return "/_create *\(chat.apiID) json \(json)"
        } else {
            let asGroup = chat.sendAsGroup ? "(as_group=on)" : ""
            return "/_send \(chat.kind.rawValue)\(chat.apiID)\(asGroup) live=off ttl=default sign=off json \(json)"
        }
    }

    private func loadIfNeeded() throws {
        guard !loaded else { return }
        let environment = ProcessInfo.processInfo.environment["SIMPLEX_CORE_LIB_DIR"]
        let libraryDirectory = environment ?? Bundle.main.privateFrameworksURL?.path
        guard let libraryDirectory else {
            throw NativeChatError.unavailable("The SimpleX core libraries were not found in the app bundle.")
        }
        var error = [CChar](repeating: 0, count: 4096)
        guard sx_core_load(libraryDirectory, &error, error.count) else {
            throw NativeChatError.unavailable(String(cString: error))
        }
        guard sx_core_initialize(&error, error.count) else {
            throw NativeChatError.unavailable(String(cString: error))
        }
        loaded = true
    }

    private func migrate(passphrase: String) throws -> Data {
        let prefix = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".local/share/simplex/simplex_v1").path
        var rawController: UnsafeMutableRawPointer?
        let result = prefix.withCString { path in
            passphrase.withCString { key in
                "yesUp".withCString { confirmation in
                    sx_core_migrate_init(path, key, confirmation, &rawController)
                }
            }
        }
        if let rawController {
            controller = SimpleXControllerHandle(rawController)
        }
        return try data(from: result)
    }

    private func configureFilePaths() throws {
        let dataDirectory = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".local/share/simplex", isDirectory: true)
        let files = dataDirectory.appendingPathComponent("simplex_v1_files", isDirectory: true)
        let temporary = dataDirectory.appendingPathComponent("tmp", isDirectory: true)
        let assets = dataDirectory.appendingPathComponent("simplex_v1_assets", isDirectory: true)
        try FileManager.default.createDirectory(at: files, withIntermediateDirectories: true)
        try FileManager.default.createDirectory(at: temporary, withIntermediateDirectories: true)
        try FileManager.default.createDirectory(at: assets, withIntermediateDirectories: true)
        let paths = [
            "appFilesFolder": files.path,
            "appTempFolder": temporary.path,
            "appAssetsFolder": assets.path,
        ]
        let encoded = try JSONSerialization.data(withJSONObject: paths)
        guard let json = String(data: encoded, encoding: .utf8) else {
            throw NativeChatError.invalidResponse("The app file paths could not be encoded.")
        }
        try ensureCommandSucceeded(send("/set file paths \(json)"))
    }

    private func send(_ command: String) throws -> Data {
        guard let controller else {
            throw NativeChatError.unavailable("The SimpleX database is not open.")
        }
        let result = command.withCString { sx_core_send_cmd(controller.pointer, $0, 0) }
        return try data(from: result)
    }

    private func ensureCommandSucceeded(_ response: Data) throws {
        if let message = NativeChatParser.commandError(from: response) {
            throw NativeChatError.core(message)
        }
    }

    private func data(from pointer: UnsafePointer<CChar>?) throws -> Data {
        guard let pointer else {
            throw NativeChatError.unavailable("The SimpleX core returned no data.")
        }
        defer { sx_core_free(pointer) }
        guard let string = String(validatingUTF8: pointer), let data = string.data(using: .utf8) else {
            throw NativeChatError.invalidResponse("The SimpleX core returned invalid UTF-8.")
        }
        return data
    }
}
