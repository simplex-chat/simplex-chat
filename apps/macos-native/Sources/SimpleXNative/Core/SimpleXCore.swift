import CoreBridge
import Foundation

actor SimpleXCore {
    private var controller: UnsafeMutableRawPointer?
    private var loaded = false
    private let decryptedFilesDirectory: URL

    init() {
        decryptedFilesDirectory = FileManager.default.temporaryDirectory
            .appendingPathComponent("chat.simplex.native", isDirectory: true)
            .appendingPathComponent("OpenedAttachments", isDirectory: true)
            .appendingPathComponent(UUID().uuidString, isDirectory: true)
    }

    deinit {
        if let controller {
            if let result = sx_core_close_store(controller) {
                sx_core_free(result)
            }
        }
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
        let pagination = messageID.map { "around=\($0) count=75" } ?? "count=75"
        return try NativeChatParser.messages(from: send("/_get chat \(chatID) \(pagination)"))
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
        guard let cryptoArgs = source.cryptoArgs else { return sourceURL }

        try loadIfNeeded()
        let directory = decryptedFilesDirectory.appendingPathComponent(UUID().uuidString, isDirectory: true)
        try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        let safeName = URL(fileURLWithPath: fileName ?? source.filePath).lastPathComponent
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
        return destination
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
              let pointer = sx_core_recv_msg_wait(controller, timeoutMicroseconds) else {
            return nil
        }
        defer { sx_core_free(pointer) }
        guard let response = String(validatingUTF8: pointer), !response.isEmpty else { return nil }
        return response.data(using: .utf8)
    }

    private func sendComposedMessage(_ message: [String: Any], to chat: NativeChat) throws {
        let content = [message]
        let data = try JSONSerialization.data(withJSONObject: content)
        guard let json = String(data: data, encoding: .utf8) else {
            throw NativeChatError.invalidResponse("The message could not be encoded.")
        }
        let command: String
        if chat.kind == .local {
            command = "/_create *\(chat.apiID) json \(json)"
        } else {
            let asGroup = chat.sendAsGroup ? "(as_group=on)" : ""
            command = "/_send \(chat.kind.rawValue)\(chat.apiID)\(asGroup) live=off ttl=default sign=off json \(json)"
        }
        try ensureCommandSucceeded(send(command))
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
        let result = prefix.withCString { path in
            passphrase.withCString { key in
                "yesUp".withCString { confirmation in
                    sx_core_migrate_init(path, key, confirmation, &controller)
                }
            }
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
        let result = command.withCString { sx_core_send_cmd(controller, $0, 0) }
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
