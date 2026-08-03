import CoreBridge
import Foundation

actor SimpleXCore {
    private var controller: UnsafeMutableRawPointer?
    private var loaded = false

    deinit {
        if let controller {
            if let result = sx_core_close_store(controller) {
                sx_core_free(result)
            }
        }
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
        _ = try send("/_start main=on snd_files=on")
        let profile = try NativeChatParser.profile(from: send("/u"))
        let chats = try NativeChatParser.chats(from: send("/_get chats \(profile.userID) pcc=on"))
        return (profile, chats)
    }

    func loadChats(userID: Int64) throws -> [NativeChat] {
        try NativeChatParser.chats(from: send("/_get chats \(userID) pcc=on"))
    }

    func loadMessages(chatID: String) throws -> [NativeMessage] {
        try NativeChatParser.messages(from: send("/_get chat \(chatID) count=75"))
    }

    func sendText(_ text: String, to chat: NativeChat) throws {
        guard chat.kind.canSend else {
            throw NativeChatError.unavailable("This conversation cannot accept messages yet.")
        }
        let content: [[String: Any]] = [[
            "msgContent": ["type": "text", "text": text],
            "mentions": [:],
        ]]
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
        _ = try send(command)
    }

    func waitForEvent(timeoutMicroseconds: Int32 = 500_000) -> Bool {
        guard let controller,
              let pointer = sx_core_recv_msg_wait(controller, timeoutMicroseconds) else {
            return false
        }
        defer { sx_core_free(pointer) }
        guard let response = String(validatingUTF8: pointer) else { return false }
        return !response.isEmpty
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
        _ = try send("/set file paths \(json)")
    }

    private func send(_ command: String) throws -> Data {
        guard let controller else {
            throw NativeChatError.unavailable("The SimpleX database is not open.")
        }
        let result = command.withCString { sx_core_send_cmd(controller, $0, 0) }
        return try data(from: result)
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
