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

    func markingRead() -> NativeChat {
        NativeChat(
            id: id,
            apiID: apiID,
            kind: kind,
            displayName: displayName,
            image: image,
            preview: preview,
            timestamp: timestamp,
            unreadCount: 0,
            sendAsGroup: sendAsGroup
        )
    }

    func displayedMessageAuthor(sent: Bool, author: String?) -> String {
        if sent { return sendAsGroup ? displayName : "You" }
        let normalizedAuthor = author?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        return normalizedAuthor.isEmpty ? displayName : normalizedAuthor
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
    case link(NativeLinkPreview)
    case image(preview: String?, fileName: String?)
    case video(preview: String?, fileName: String?)
    case voice(fileName: String?, duration: Int?)
    case file(fileName: String?)

    var opensInQuickLook: Bool {
        switch self {
        case .image, .video: true
        case .text, .link, .voice, .file: false
        }
    }

    var inlineAudioFileName: String? {
        switch self {
        case let .voice(fileName, _): fileName ?? "Voice message"
        case let .file(fileName): NativeAudioFile.supports(fileName) ? fileName : nil
        case .text, .link, .image, .video: nil
        }
    }

    var replyContextVisual: NativeReplyContextVisual? {
        switch self {
        case .text, .link: nil
        case let .image(preview, _): .image(preview)
        case let .video(preview, _): .video(preview)
        case .voice: .voice
        case .file: .file
        }
    }

    var attachmentDescription: String? {
        switch self {
        case .text, .link: nil
        case let .image(_, fileName): Self.description(fileName, fallback: "Photo")
        case let .video(_, fileName): Self.description(fileName, fallback: "Video")
        case let .voice(_, duration): Self.voiceDescription(duration: duration)
        case let .file(fileName): Self.description(fileName, fallback: "File")
        }
    }

    var fileName: String? {
        switch self {
        case .text, .link: nil
        case let .image(_, fileName), let .video(_, fileName), let .voice(fileName, _), let .file(fileName):
            fileName
        }
    }

    private static func description(_ fileName: String?, fallback: String) -> String {
        let normalizedName = fileName?.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        return normalizedName.isEmpty ? fallback : normalizedName
    }

    private static func voiceDescription(duration: Int?) -> String {
        guard let duration, duration > 0 else { return "Voice message" }
        return "Voice message, \(Duration.seconds(Double(duration)).formatted(.time(pattern: .minuteSecond)))"
    }
}

enum NativeAudioFile {
    private static let supportedExtensions: Set<String> = [
        "aac", "aif", "aiff", "caf", "flac", "m4a", "mp3", "wav",
    ]

    static func supports(_ fileName: String?) -> Bool {
        guard let fileName else { return false }
        let fileExtension = URL(fileURLWithPath: fileName).pathExtension.lowercased()
        return supportedExtensions.contains(fileExtension)
    }
}

struct NativeLinkPreview: Hashable, Sendable {
    let uri: String
    let title: String
    let description: String
    let image: String?
    let videoDuration: Int?

    var destination: URL? {
        NativeMessageLink.standaloneURL(in: uri)
    }

    var inlineVideoURL: URL? {
        NativeMessageLink.youtubeEmbedURL(for: uri)
    }

    var displayHost: String {
        destination?.host(percentEncoded: false) ?? uri
    }

    var durationLabel: String? {
        guard let videoDuration, videoDuration > 0 else { return nil }
        return Duration.seconds(Double(videoDuration)).formatted(.time(pattern: .minuteSecond))
    }
}

enum NativeMessageLink {
    static func standaloneURL(in text: String) -> URL? {
        let normalized = text.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !normalized.isEmpty,
              let components = URLComponents(string: normalized),
              ["http", "https"].contains(components.scheme?.lowercased() ?? ""),
              components.host?.isEmpty == false else { return nil }
        return components.url
    }

    static func youtubeEmbedURL(for text: String) -> URL? {
        guard let destination = standaloneURL(in: text),
              let components = URLComponents(url: destination, resolvingAgainstBaseURL: false),
              let host = components.host?.lowercased() else { return nil }

        let normalizedHost = host
            .replacingOccurrences(of: "www.", with: "")
            .replacingOccurrences(of: "m.", with: "")
        let pathComponents = components.path.split(separator: "/").map(String.init)
        let videoID: String?
        if normalizedHost == "youtu.be" {
            videoID = pathComponents.first
        } else if ["youtube.com", "youtube-nocookie.com"].contains(normalizedHost) {
            if components.path == "/watch" {
                videoID = components.queryItems?.first(where: { $0.name == "v" })?.value
            } else if let route = pathComponents.first,
                      ["embed", "shorts", "live"].contains(route) {
                videoID = pathComponents.dropFirst().first
            } else {
                videoID = nil
            }
        } else {
            videoID = nil
        }

        guard let videoID, isYouTubeVideoID(videoID) else { return nil }

        var embed = URLComponents()
        embed.scheme = "https"
        embed.host = "www.youtube-nocookie.com"
        embed.path = "/embed/\(videoID)"
        var queryItems = [
            URLQueryItem(name: "autoplay", value: "1"),
            URLQueryItem(name: "playsinline", value: "1"),
            URLQueryItem(name: "rel", value: "0"),
        ]
        if let start = youtubeStartSeconds(in: components), start > 0 {
            queryItems.append(URLQueryItem(name: "start", value: String(start)))
        }
        embed.queryItems = queryItems
        return embed.url
    }

    private static func isYouTubeVideoID(_ value: String) -> Bool {
        value.count == 11 && value.allSatisfy { character in
            character.isASCII && (character.isLetter || character.isNumber || character == "-" || character == "_")
        }
    }

    private static func youtubeStartSeconds(in components: URLComponents) -> Int? {
        guard let value = components.queryItems?
            .first(where: { ["t", "start"].contains($0.name.lowercased()) })?
            .value?.lowercased() else { return nil }
        if let seconds = Int(value) { return seconds }

        var total = 0
        var digits = ""
        for character in value {
            if character.isNumber {
                digits.append(character)
                continue
            }
            guard let amount = Int(digits) else { return nil }
            switch character {
            case "h": total += amount * 3_600
            case "m": total += amount * 60
            case "s": total += amount
            default: return nil
            }
            digits = ""
        }
        if let trailing = Int(digits) { total += trailing }
        return total > 0 ? total : nil
    }
}

enum NativeReplyContextVisual: Hashable, Sendable {
    case image(String?)
    case video(String?)
    case voice
    case file
}

struct NativeQuote: Hashable, Sendable {
    let messageID: Int64?
    let text: String
    let sent: Bool
    let author: String?
    let visual: NativeReplyContextVisual?

    init(
        messageID: Int64?,
        text: String,
        sent: Bool,
        author: String?,
        visual: NativeReplyContextVisual? = nil
    ) {
        self.messageID = messageID
        self.text = text
        self.sent = sent
        self.author = author
        self.visual = visual
    }
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

struct NativeSendReceipt: Equatable, Sendable {
    let committedMessages: [NativeMessage]
    let replyContextConfirmed: Bool

    static let confirmed = NativeSendReceipt(committedMessages: [], replyContextConfirmed: true)
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

    static func commandErrorMakesReplyTargetUnavailable(_ data: Data) -> Bool {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let error = root["error"] else { return false }
        return ["invalidQuote", "chatItemNotFound", "badChatItem"].contains {
            containsType($0, in: error)
        }
    }

    static func validateCommandResponse(
        _ data: Data,
        expectedType: String? = nil,
        requireChatItems: Bool = false
    ) throws {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            throw NativeChatError.invalidResponse("The SimpleX core returned invalid JSON.")
        }
        if let message = commandError(from: data) {
            throw NativeChatError.core(message)
        }
        guard let result = root["result"] as? [String: Any],
              let resultType = string(result["type"]) else {
            throw NativeChatError.invalidResponse("The SimpleX core response had no result.")
        }
        if let expectedType, resultType != expectedType {
            throw NativeChatError.invalidResponse(
                "Expected \(expectedType), received \(resultType)."
            )
        }
        if requireChatItems {
            guard let chatItems = result["chatItems"] as? [[String: Any]], !chatItems.isEmpty else {
                throw NativeChatError.invalidResponse(
                    "SimpleX accepted the send command without returning the sent message."
                )
            }
        }
    }

    static func validateSendResponse(_ data: Data, quotedItemID: Int64?) throws -> NativeSendReceipt {
        try validateCommandResponse(
            data,
            expectedType: "newChatItems",
            requireChatItems: true
        )
        let root = try JSONSerialization.jsonObject(with: data) as? [String: Any]
        let result = root?["result"] as? [String: Any]
        let chatItems = result?["chatItems"] as? [[String: Any]] ?? []
        let committedMessages = chatItems.compactMap { item in
            let chatItem = item["chatItem"] as? [String: Any] ?? item
            return parseMessage(chatItem)
        }
        let replyContextConfirmed = quotedItemID.map { quotedItemID in
            chatItems.contains { item in
                let chatItem = item["chatItem"] as? [String: Any] ?? item
                let quote = chatItem["quotedItem"] as? [String: Any]
                return int64(quote?["itemId"]) == quotedItemID
            }
        } ?? true
        return NativeSendReceipt(
            committedMessages: committedMessages,
            replyContextConfirmed: replyContextConfirmed
        )
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

    private static func containsType(_ expectedType: String, in value: Any) -> Bool {
        if let object = value as? [String: Any] {
            if string(object["type"]) == expectedType { return true }
            return object.values.contains { containsType(expectedType, in: $0) }
        }
        if let values = value as? [Any] {
            return values.contains { containsType(expectedType, in: $0) }
        }
        return false
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
        let sendAsGroup = switch kind {
        case .group: groupSendsAsGroup(info: info, groupInfo: payload)
        default: bool(payload["sendAsGroup"]) ?? false
        }
        return NativeChat(
            id: "\(kind.rawValue)\(apiID)",
            apiID: apiID,
            kind: kind,
            displayName: displayName,
            image: image,
            preview: string(lastMeta?["itemText"]) ?? "",
            timestamp: date(lastMeta?["itemTs"]),
            unreadCount: int(stats?["unreadCount"]) ?? 0,
            sendAsGroup: sendAsGroup
        )
    }

    private static func groupSendsAsGroup(info: [String: Any], groupInfo: [String: Any]) -> Bool {
        if let explicit = bool(groupInfo["sendAsGroup"]) { return explicit }
        if let scope = info["groupChatScope"], !(scope is NSNull) { return false }
        let membership = groupInfo["membership"] as? [String: Any]
        return bool(groupInfo["useRelays"]) == true
            && string(membership?["memberRole"]) == "owner"
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
        case "link":
            if let preview = messageContent?["preview"] as? [String: Any] {
                let linkContent = preview["content"] as? [String: Any]
                content = .link(NativeLinkPreview(
                    uri: string(preview["uri"]) ?? string(messageContent?["text"]) ?? "",
                    title: string(preview["title"]) ?? "",
                    description: string(preview["description"]) ?? "",
                    image: string(preview["image"]),
                    videoDuration: string(linkContent?["type"]) == "video"
                        ? int(linkContent?["duration"])
                        : nil
                ))
            } else {
                content = .text
            }
        case "image":
            content = .image(preview: string(messageContent?["image"]), fileName: fileName)
        case "video":
            content = .video(preview: string(messageContent?["image"]), fileName: fileName)
        case "voice":
            content = .voice(fileName: fileName, duration: int(messageContent?["duration"]))
        case "file":
            content = .file(fileName: fileName)
        default:
            content = .text
        }
        let isMessageContent = contentType == "sndMsgContent" || contentType == "rcvMsgContent"
        let itemDeleted = meta["itemDeleted"].map { !($0 is NSNull) } ?? false
        // The core serializes CIMeta's stored field as `itemLive`; `isLive` is
        // only a derived property in the existing Apple and Kotlin clients.
        // Keep the older alias as a defensive fallback for preview fixtures.
        let isLive = bool(meta["itemLive"]) ?? bool(meta["isLive"]) ?? false
        let isReport = string(messageContent?["type"]) == "report"
        let replyable = isMessageContent && !itemDeleted && !isLive && id >= 0 && !isReport
        return NativeMessage(
            id: id,
            text: string(meta["itemText"]) ?? "",
            timestamp: date(meta["itemTs"]),
            sent: directionType.hasSuffix("Snd"),
            author: string(member?["localDisplayName"]) ?? string(profile?["displayName"]),
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
            author: string(member?["localDisplayName"]) ?? string(memberProfile?["displayName"]),
            visual: quoteVisual(content)
        )
    }

    private static func quoteVisual(_ content: [String: Any]) -> NativeReplyContextVisual? {
        switch string(content["type"]) {
        case "image": .image(string(content["image"]))
        case "video": .video(string(content["image"]))
        case "voice": .voice
        case "file": .file
        default: nil
        }
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
    case replyTargetUnavailable
    case unavailable(String)

    var errorDescription: String? {
        switch self {
        case let .core(message), let .invalidResponse(message), let .unavailable(message): message
        case .replyTargetUnavailable: "The message being replied to is no longer available."
        }
    }
}
