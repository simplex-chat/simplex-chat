import AppKit
import Foundation

enum NativeChatKind: String, Sendable {
    case direct = "@"
    case group = "#"
    case local = "*"
    case contactRequest = "<@"
    case contactConnection = ":"

    var canSend: Bool {
        self == .direct || self == .group || self == .local
    }

    var canReply: Bool {
        self == .direct || self == .group
    }

    var toolbarSubtitle: String {
        switch self {
        case .direct: "SimpleX contact"
        case .group: "Group"
        case .local: "Private notes"
        case .contactRequest: "Contact request"
        case .contactConnection: "Connecting"
        }
    }
}

struct NativeProfile: Sendable {
    let userID: Int64
    let displayName: String
    let image: String?
}

struct NativeChat: Identifiable, Hashable, Sendable {
    let id: String
    let apiID: Int64
    let kind: NativeChatKind
    let displayName: String
    let image: String?
    let preview: String
    let timestamp: Date?
    let unreadCount: Int
    let sendAsGroup: Bool

    var accessibilityDescription: String {
        let unread = unreadCount == 0 ? "" : ", \(unreadCount) unread"
        let message = preview.isEmpty ? "" : ", \(preview)"
        return "\(displayName)\(unread)\(message)"
    }
}

struct NativeMessage: Identifiable, Hashable, Sendable {
    let id: Int64
    let text: String
    let timestamp: Date?
    let sent: Bool
    let author: String?
    let deletable: Bool
    let content: NativeMessageContent
    let replyable: Bool
    let quotedItem: NativeQuote?
    let fileSource: NativeCryptoFile?

    init(
        id: Int64,
        text: String,
        timestamp: Date?,
        sent: Bool,
        author: String?,
        deletable: Bool,
        content: NativeMessageContent,
        replyable: Bool = true,
        quotedItem: NativeQuote? = nil,
        fileSource: NativeCryptoFile? = nil
    ) {
        self.id = id
        self.text = text
        self.timestamp = timestamp
        self.sent = sent
        self.author = author
        self.deletable = deletable
        self.content = content
        self.replyable = replyable
        self.quotedItem = quotedItem
        self.fileSource = fileSource
    }

    var replyPreview: String {
        let normalizedText = text.trimmingCharacters(in: .whitespacesAndNewlines)
        return normalizedText.isEmpty ? (content.attachmentDescription ?? "Message") : normalizedText
    }
}

enum NativeMessageContent: Hashable, Sendable {
    case text
    case image(preview: String?, fileName: String?)
    case video(preview: String?, fileName: String?)
    case file(fileName: String?)

    var attachmentDescription: String? {
        switch self {
        case .text: nil
        case let .image(_, fileName): Self.description(fileName, fallback: "Photo")
        case let .video(_, fileName): Self.description(fileName, fallback: "Video")
        case let .file(fileName): Self.description(fileName, fallback: "File")
        }
    }

    private static func description(_ fileName: String?, fallback: String) -> String {
        let normalizedName = fileName?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        return normalizedName.isEmpty ? fallback : normalizedName
    }
}

struct NativeQuote: Hashable, Sendable {
    let messageID: Int64?
    let text: String
    let sent: Bool
    let author: String?
}

struct NativeCryptoFile: Hashable, Sendable {
    let filePath: String
    let cryptoArgs: NativeCryptoFileArgs?

    var sourceURL: URL {
        if filePath.hasPrefix("/") { return URL(fileURLWithPath: filePath) }
        return FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".local/share/simplex/simplex_v1_files", isDirectory: true)
            .appendingPathComponent(filePath)
    }
}

struct NativeCryptoFileArgs: Hashable, Sendable {
    let fileKey: String
    let fileNonce: String
}

enum NativeChatParser {
    static func profile(from data: Data) throws -> NativeProfile {
        let result = try responseResult(from: data, expectedType: "activeUser")
        guard let user = result["user"] as? [String: Any] else {
            throw NativeChatError.invalidResponse("The active profile was missing from the core response.")
        }
        let profile = user["profile"] as? [String: Any]
        guard let userID = int64(user["userId"]),
              let displayName = string(profile?["displayName"]) ?? string(user["localDisplayName"]) else {
            throw NativeChatError.invalidResponse("The active profile could not be decoded.")
        }
        return NativeProfile(userID: userID, displayName: displayName, image: string(profile?["image"]))
    }

    static func chats(from data: Data) throws -> [NativeChat] {
        let result = try responseResult(from: data, expectedType: "apiChats")
        guard let rawChats = result["chats"] as? [[String: Any]] else {
            throw NativeChatError.invalidResponse("The chat list was missing from the core response.")
        }
        return rawChats.compactMap(parseChat)
    }

    static func messages(from data: Data) throws -> [NativeMessage] {
        let result = try responseResult(from: data, expectedType: "apiChat")
        guard let chat = result["chat"] as? [String: Any],
              let items = chat["chatItems"] as? [[String: Any]] else {
            throw NativeChatError.invalidResponse("The conversation was missing from the core response.")
        }
        return items.compactMap(parseMessage)
    }

    static func migrationSucceeded(_ data: Data) -> Bool {
        guard let value = try? JSONSerialization.jsonObject(with: data, options: .fragmentsAllowed) else { return false }
        if let string = value as? String { return string == "ok" }
        guard let object = value as? [String: Any] else { return false }
        return string(object["type"]) == "ok" || object["ok"] != nil
    }

    static func migrationError(from data: Data) -> String {
        guard let value = try? JSONSerialization.jsonObject(with: data, options: .fragmentsAllowed) else {
            return String(data: data, encoding: .utf8) ?? "Unable to open the database."
        }
        if let object = value as? [String: Any] {
            let type = string(object["type"]) ?? object.keys.first ?? "databaseError"
            switch type {
            case "errorNotADatabase": return "That database passphrase is not correct."
            case "errorMigration": return "The database needs a migration confirmation before it can be opened."
            case "errorSQL": return "The database could not be read."
            default: return "Unable to open the database (\(type))."
            }
        }
        return "Unable to open the database."
    }

    static func commandError(from data: Data) -> String? {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let error = root["error"] else { return nil }
        if let error = error as? [String: Any] {
            let type = (error["errorType"] as? [String: Any]).flatMap { string($0["type"]) }
                ?? string(error["type"])
                ?? error.keys.first
                ?? "coreError"
            return "SimpleX could not complete the action (\(type))."
        }
        return "SimpleX could not complete the action."
    }

    static func image(from encoded: String?) -> NSImage? {
        guard var encoded, !encoded.isEmpty else { return nil }
        if let comma = encoded.firstIndex(of: ","), encoded[..<comma].contains("base64") {
            encoded = String(encoded[encoded.index(after: comma)...])
        }
        guard let data = Data(base64Encoded: encoded) else { return nil }
        return NSImage(data: data)
    }

    private static func responseResult(from data: Data, expectedType: String) throws -> [String: Any] {
        guard let root = try JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            throw NativeChatError.invalidResponse("The SimpleX core returned invalid JSON.")
        }
        if let error = root["error"] {
            throw NativeChatError.core(String(describing: error))
        }
        guard let result = root["result"] as? [String: Any] else {
            throw NativeChatError.invalidResponse("The SimpleX core response had no result.")
        }
        if string(result["type"]) == expectedType { return result }
        if let nested = result[expectedType] as? [String: Any] {
            return nested.merging(["type": expectedType]) { current, _ in current }
        }
        throw NativeChatError.invalidResponse("Expected \(expectedType), received \(string(result["type"]) ?? "an unknown response").")
    }

    private static func parseChat(_ object: [String: Any]) -> NativeChat? {
        guard let info = object["chatInfo"] as? [String: Any],
              let type = string(info["type"]),
              let kind = kind(for: type) else { return nil }

        let payload: [String: Any]
        let idKeys: [String]
        switch kind {
        case .direct:
            payload = info["contact"] as? [String: Any] ?? [:]
            idKeys = ["contactId", "apiId"]
        case .group:
            payload = info["groupInfo"] as? [String: Any] ?? [:]
            idKeys = ["groupId", "apiId"]
        case .local:
            payload = info["noteFolder"] as? [String: Any] ?? [:]
            idKeys = ["noteFolderId", "apiId"]
        case .contactRequest:
            payload = info["contactRequest"] as? [String: Any] ?? [:]
            idKeys = ["contactRequestId", "apiId"]
        case .contactConnection:
            payload = info["contactConnection"] as? [String: Any] ?? [:]
            idKeys = ["contactConnectionId", "apiId"]
        }
        guard let apiID = idKeys.lazy.compactMap({ int64(payload[$0]) }).first else { return nil }

        let profile = payload["profile"] as? [String: Any]
        let groupProfile = payload["groupProfile"] as? [String: Any]
        let displayName = string(payload["localDisplayName"])
            ?? string(payload["displayName"])
            ?? string(profile?["displayName"])
            ?? string(groupProfile?["displayName"])
            ?? "Conversation"
        let image = string(profile?["image"])
            ?? string(groupProfile?["image"])
            ?? string(payload["image"])
        let items = object["chatItems"] as? [[String: Any]] ?? []
        let lastMeta = items.last?["meta"] as? [String: Any]
        let stats = object["chatStats"] as? [String: Any]
        return NativeChat(
            id: "\(kind.rawValue)\(apiID)",
            apiID: apiID,
            kind: kind,
            displayName: displayName,
            image: image,
            preview: string(lastMeta?["itemText"]) ?? "",
            timestamp: date(lastMeta?["itemTs"]),
            unreadCount: int(stats?["unreadCount"]) ?? 0,
            sendAsGroup: bool(payload["sendAsGroup"]) ?? false
        )
    }

    private static func parseMessage(_ object: [String: Any]) -> NativeMessage? {
        guard let meta = object["meta"] as? [String: Any],
              let id = int64(meta["itemId"]) else { return nil }
        let direction = object["chatDir"] as? [String: Any]
        let directionType = string(direction?["type"]) ?? direction?.keys.first ?? ""
        let member = direction?["groupMember"] as? [String: Any]
        let profile = member?["memberProfile"] as? [String: Any]
        let contentContainer = object["content"] as? [String: Any]
        let contentType = string(contentContainer?["type"])
            ?? contentContainer?.keys.first
        let messageContent = (contentContainer?["msgContent"] as? [String: Any])
            ?? ((contentContainer?["sndMsgContent"] as? [String: Any])?["msgContent"] as? [String: Any])
            ?? ((contentContainer?["rcvMsgContent"] as? [String: Any])?["msgContent"] as? [String: Any])
        let file = object["file"] as? [String: Any]
        let fileSource = file?["fileSource"] as? [String: Any]
        let fileName = string(file?["fileName"])
        let filePath = string(fileSource?["filePath"])
        let cryptoArgsObject = fileSource?["cryptoArgs"] as? [String: Any]
        let cryptoArgs = cryptoArgsObject.flatMap { args -> NativeCryptoFileArgs? in
            guard let key = string(args["fileKey"]), let nonce = string(args["fileNonce"]) else { return nil }
            return NativeCryptoFileArgs(fileKey: key, fileNonce: nonce)
        }
        let nativeFileSource = filePath.map { NativeCryptoFile(filePath: $0, cryptoArgs: cryptoArgs) }
        let content: NativeMessageContent
        switch string(messageContent?["type"]) {
        case "image":
            content = .image(preview: string(messageContent?["image"]), fileName: fileName)
        case "video":
            content = .video(preview: string(messageContent?["image"]), fileName: fileName)
        case "file":
            content = .file(fileName: fileName)
        default:
            content = .text
        }
        let isMessageContent = contentType == "sndMsgContent" || contentType == "rcvMsgContent"
        let itemDeleted = meta["itemDeleted"].map { !($0 is NSNull) } ?? false
        let isLive = bool(meta["isLive"]) ?? false
        let isReport = string(messageContent?["type"]) == "report"
        let replyable = isMessageContent && !itemDeleted && !isLive && id >= 0 && !isReport
        return NativeMessage(
            id: id,
            text: string(meta["itemText"]) ?? "",
            timestamp: date(meta["itemTs"]),
            sent: directionType.hasSuffix("Snd"),
            author: string(profile?["displayName"]) ?? string(member?["localDisplayName"]),
            deletable: bool(meta["deletable"]) ?? false,
            content: content,
            replyable: replyable,
            quotedItem: parseQuote(object["quotedItem"]),
            fileSource: nativeFileSource
        )
    }

    private static func parseQuote(_ value: Any?) -> NativeQuote? {
        guard let object = value as? [String: Any],
              let content = object["content"] as? [String: Any] else { return nil }
        let direction = object["chatDir"] as? [String: Any]
        let directionType = string(direction?["type"]) ?? direction?.keys.first ?? ""
        let member = direction?["groupMember"] as? [String: Any]
        let memberProfile = member?["memberProfile"] as? [String: Any]
        let rawText = string(content["text"])?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        let text: String
        if !rawText.isEmpty {
            text = rawText
        } else {
            switch string(content["type"]) {
            case "image": text = "Photo"
            case "video": text = "Video"
            case "voice": text = "Voice message"
            case "file": text = "File"
            default: text = "Message"
            }
        }
        return NativeQuote(
            messageID: int64(object["itemId"]),
            text: text,
            sent: directionType.hasSuffix("Snd"),
            author: string(memberProfile?["displayName"]) ?? string(member?["localDisplayName"])
        )
    }

    private static func kind(for type: String) -> NativeChatKind? {
        switch type {
        case "direct": .direct
        case "group": .group
        case "local": .local
        case "contactRequest": .contactRequest
        case "contactConnection": .contactConnection
        default: nil
        }
    }

    private static func string(_ value: Any?) -> String? { value as? String }
    private static func bool(_ value: Any?) -> Bool? { value as? Bool }
    private static func int(_ value: Any?) -> Int? {
        if let value = value as? Int { return value }
        if let value = value as? NSNumber { return value.intValue }
        return nil
    }
    private static func int64(_ value: Any?) -> Int64? {
        if let value = value as? Int64 { return value }
        if let value = value as? NSNumber { return value.int64Value }
        return nil
    }
    private static func date(_ value: Any?) -> Date? {
        guard let raw = value as? String else { return nil }
        let fractional = ISO8601DateFormatter()
        fractional.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return fractional.date(from: raw) ?? ISO8601DateFormatter().date(from: raw)
    }
}

enum NativeChatError: LocalizedError, Sendable {
    case core(String)
    case invalidResponse(String)
    case unavailable(String)

    var errorDescription: String? {
        switch self {
        case let .core(message), let .invalidResponse(message), let .unavailable(message): message
        }
    }
}
