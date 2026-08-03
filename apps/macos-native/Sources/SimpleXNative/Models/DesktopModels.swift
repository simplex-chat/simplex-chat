import AppKit
import Foundation
import UniformTypeIdentifiers

enum DesktopChatDensity: String, CaseIterable, Identifiable, Sendable {
    case compact
    case comfortable
    case spacious

    var id: Self { self }

    var title: String {
        switch self {
        case .compact: "Compact"
        case .comfortable: "Comfortable"
        case .spacious: "Spacious"
        }
    }

    var tokens: DesktopDensityTokens {
        switch self {
        case .compact:
            DesktopDensityTokens(chatRowPadding: 4, avatarSize: 40, messagePadding: 8, transcriptGap: 8, composerPadding: 8)
        case .comfortable:
            DesktopDensityTokens(chatRowPadding: 8, avatarSize: 44, messagePadding: 12, transcriptGap: 12, composerPadding: 12)
        case .spacious:
            DesktopDensityTokens(chatRowPadding: 12, avatarSize: 48, messagePadding: 16, transcriptGap: 16, composerPadding: 16)
        }
    }
}

struct DesktopDensityTokens: Equatable, Sendable {
    let chatRowPadding: CGFloat
    let avatarSize: CGFloat
    let messagePadding: CGFloat
    let transcriptGap: CGFloat
    let composerPadding: CGFloat
}

enum ComposerReturnAction: Equatable, Sendable {
    case send
    case insertNewline
}

enum ComposerKeyboard {
    static func returnAction(shiftPressed: Bool) -> ComposerReturnAction {
        shiftPressed ? .insertNewline : .send
    }
}

enum PendingAttachmentKind: String, Sendable {
    case image
    case video
    case document

    var symbolName: String {
        switch self {
        case .image: "photo"
        case .video: "film"
        case .document: "doc"
        }
    }
}

struct PendingAttachment: Identifiable, Hashable, Sendable {
    static let maximumByteCount: Int64 = 5 * 1_024 * 1_024 * 1_024

    let id: UUID
    let url: URL
    let fileName: String
    let kind: PendingAttachmentKind
    let byteCount: Int64
    let previewImage: String?

    static func stage(url: URL) throws -> PendingAttachment {
        let resolvedURL = url.resolvingSymlinksInPath()
        let values = try resolvedURL.resourceValues(forKeys: [
            .contentTypeKey,
            .fileSizeKey,
            .isRegularFileKey,
            .isReadableKey,
            .nameKey,
        ])
        guard values.isRegularFile == true, values.isReadable != false else {
            throw AttachmentValidationError.notAReadableFile(resolvedURL.lastPathComponent)
        }
        let byteCount = Int64(values.fileSize ?? 0)
        guard byteCount <= maximumByteCount else {
            throw AttachmentValidationError.tooLarge(resolvedURL.lastPathComponent)
        }
        let contentType = values.contentType
        let kind: PendingAttachmentKind
        if contentType?.conforms(to: .image) == true {
            kind = .image
        } else if contentType?.conforms(to: .movie) == true || contentType?.conforms(to: .video) == true {
            kind = .video
        } else {
            kind = .document
        }
        return PendingAttachment(
            id: UUID(),
            url: resolvedURL,
            fileName: values.name ?? resolvedURL.lastPathComponent,
            kind: kind,
            byteCount: byteCount,
            previewImage: kind == .image ? imagePreview(from: resolvedURL) : nil
        )
    }

    static func reordered(_ attachments: [PendingAttachment], from source: UUID, before destination: UUID) -> [PendingAttachment] {
        guard source != destination,
              let sourceIndex = attachments.firstIndex(where: { $0.id == source }),
              let destinationIndex = attachments.firstIndex(where: { $0.id == destination }) else {
            return attachments
        }
        var result = attachments
        let attachment = result.remove(at: sourceIndex)
        let adjustedDestination = sourceIndex < destinationIndex ? destinationIndex - 1 : destinationIndex
        result.insert(attachment, at: adjustedDestination)
        return result
    }

    static func remainingAfterFailure(_ attachments: [PendingAttachment], at failedIndex: Int) -> [PendingAttachment] {
        Array(attachments.dropFirst(min(max(failedIndex, 0), attachments.count)))
    }

    private static func imagePreview(from url: URL) -> String? {
        guard let image = NSImage(contentsOf: url), image.size.width > 0, image.size.height > 0 else { return nil }
        let maximumDimension: CGFloat = 512
        let scale = min(1, maximumDimension / max(image.size.width, image.size.height))
        let size = NSSize(width: image.size.width * scale, height: image.size.height * scale)
        let preview = NSImage(size: size)
        preview.lockFocus()
        image.draw(in: NSRect(origin: .zero, size: size), from: .zero, operation: .copy, fraction: 1)
        preview.unlockFocus()
        guard let tiff = preview.tiffRepresentation,
              let bitmap = NSBitmapImageRep(data: tiff),
              let data = bitmap.representation(using: .jpeg, properties: [.compressionFactor: 0.72]) else {
            return nil
        }
        return "data:image/jpeg;base64,\(data.base64EncodedString())"
    }
}

struct PendingAttachmentSendStep: Equatable, Sendable {
    let attachment: PendingAttachment
    let caption: String
    let quotedItemID: Int64?
}

enum PendingAttachmentBatch {
    static func sendSteps(
        attachments: [PendingAttachment],
        caption: String,
        quotedItemID: Int64?
    ) -> [PendingAttachmentSendStep] {
        attachments.enumerated().map { index, attachment in
            PendingAttachmentSendStep(
                attachment: attachment,
                caption: index == attachments.index(before: attachments.endIndex) ? caption : "",
                quotedItemID: index == attachments.startIndex ? quotedItemID : nil
            )
        }
    }
}

enum AttachmentValidationError: LocalizedError, Equatable {
    case notAReadableFile(String)
    case tooLarge(String)

    var errorDescription: String? {
        switch self {
        case let .notAReadableFile(name): "“\(name)” is not a readable file."
        case let .tooLarge(name): "“\(name)” is larger than SimpleX’s 5 GB file limit."
        }
    }
}

enum MessageSelection {
    static func updated(
        current: Set<Int64>,
        anchor: Int64?,
        clicked: Int64,
        orderedIDs: [Int64],
        command: Bool,
        shift: Bool
    ) -> (selection: Set<Int64>, anchor: Int64?) {
        if shift,
           let anchor,
           let anchorIndex = orderedIDs.firstIndex(of: anchor),
           let clickedIndex = orderedIDs.firstIndex(of: clicked) {
            let range = min(anchorIndex, clickedIndex)...max(anchorIndex, clickedIndex)
            let rangeSelection = Set(range.map { orderedIDs[$0] })
            return (command ? current.union(rangeSelection) : rangeSelection, anchor)
        }
        if command {
            var selection = current
            if !selection.insert(clicked).inserted { selection.remove(clicked) }
            return (selection, clicked)
        }
        return ([clicked], clicked)
    }
}

enum MessageReplyControlVisibility {
    static func isVisible(canReply: Bool, hovering: Bool, selected: Bool) -> Bool {
        canReply && (hovering || selected)
    }
}

enum ConversationSearch {
    static func matches(_ messages: [NativeMessage], query: String) -> [NativeMessage] {
        guard !query.isEmpty else { return [] }
        return messages.filter { $0.text.localizedCaseInsensitiveContains(query) }
    }

    static func nextID(in matches: [NativeMessage], currentID: Int64?, offset: Int) -> Int64? {
        guard !matches.isEmpty else { return nil }
        let currentIndex = currentID.flatMap { id in matches.firstIndex(where: { $0.id == id }) } ?? 0
        let nextIndex = (currentIndex + offset + matches.count) % matches.count
        return matches[nextIndex].id
    }

    static func resultDescription(matches: [NativeMessage], selectedID: Int64?, queryIsEmpty: Bool) -> String {
        guard !matches.isEmpty else { return queryIsEmpty ? "" : "No Results" }
        let index = selectedID.flatMap { id in matches.firstIndex(where: { $0.id == id }) } ?? 0
        return "\(index + 1) of \(matches.count)"
    }
}

enum NotificationPreviewMode: String, CaseIterable, Identifiable, Sendable {
    case message
    case contact
    case hidden

    var id: Self { self }

    var title: String {
        switch self {
        case .message: "Message"
        case .contact: "Contact"
        case .hidden: "Hidden"
        }
    }
}

enum NotificationPermissionState: String, Sendable {
    case notDetermined
    case denied
    case authorized
    case provisional
    case unknown

    var title: String {
        switch self {
        case .notDetermined: "Not Requested"
        case .denied: "Denied"
        case .authorized: "Allowed"
        case .provisional: "Provisional"
        case .unknown: "Unknown"
        }
    }
}

enum DesktopNotificationCategory: String, Sendable {
    case message = "SIMPLEX_MESSAGE"
    case contactRequest = "SIMPLEX_CONTACT_REQUEST"
    case incomingCall = "SIMPLEX_INCOMING_CALL"
}

struct NotificationRoute: Hashable, Sendable {
    let userID: Int64?
    let remoteHostID: Int64?
    let chatID: String
    let messageID: Int64?

    var identifier: String {
        let safeChat = chatID.replacingOccurrences(
            of: "[^A-Za-z0-9._-]",
            with: "_",
            options: .regularExpression
        )
        return "simplex.\(userID ?? -1).\(remoteHostID ?? -1).\(safeChat).\(messageID ?? -1)"
    }
}

struct DesktopNotificationPayload: Sendable {
    let route: NotificationRoute
    let displayName: String
    let preview: String
    let category: DesktopNotificationCategory
}

struct DesktopNotificationPreview: Equatable, Sendable {
    let title: String
    let body: String
}

struct NotificationRouteQueue: Sendable {
    private var routes: [NotificationRoute] = []

    mutating func enqueue(_ route: NotificationRoute) {
        if !routes.contains(route) { routes.append(route) }
    }

    mutating func consumeIfReady(_ ready: Bool) -> [NotificationRoute] {
        guard ready else { return [] }
        defer { routes = [] }
        return routes
    }
}

enum NativeNotificationParser {
    static func payload(from data: Data) -> DesktopNotificationPayload? {
        guard let root = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let result = root["result"] as? [String: Any],
              let type = result["type"] as? String else {
            return nil
        }
        let remoteHostID = int64(root["remoteHostId"])
        switch type {
        case "newChatItems":
            guard let user = result["user"] as? [String: Any],
                  let userID = int64(user["userId"]),
                  let entries = result["chatItems"] as? [[String: Any]] else { return nil }
            for entry in entries {
                guard let info = entry["chatInfo"] as? [String: Any],
                      let item = entry["chatItem"] as? [String: Any],
                      let meta = item["meta"] as? [String: Any],
                      let messageID = int64(meta["itemId"]),
                      isReceived(item) else { continue }
                guard let identity = chatIdentity(info) else { continue }
                return DesktopNotificationPayload(
                    route: NotificationRoute(
                        userID: userID,
                        remoteHostID: remoteHostID,
                        chatID: identity.id,
                        messageID: messageID
                    ),
                    displayName: identity.name,
                    preview: (meta["itemText"] as? String) ?? "New message",
                    category: .message
                )
            }
            return nil
        case "receivedContactRequest":
            guard let user = result["user"] as? [String: Any],
                  let request = result["contactRequest"] as? [String: Any],
                  let requestID = int64(request["contactRequestId"]) else { return nil }
            let profile = request["profile"] as? [String: Any]
            let name = (profile?["displayName"] as? String)
                ?? (request["localDisplayName"] as? String)
                ?? "New contact"
            return DesktopNotificationPayload(
                route: NotificationRoute(
                    userID: int64(user["userId"]),
                    remoteHostID: remoteHostID,
                    chatID: "<@\(requestID)",
                    messageID: nil
                ),
                displayName: name,
                preview: "New contact request",
                category: .contactRequest
            )
        case "callInvitation":
            guard let invitation = result["callInvitation"] as? [String: Any],
                  let user = invitation["user"] as? [String: Any],
                  let contact = invitation["contact"] as? [String: Any],
                  let contactID = int64(contact["contactId"]) else { return nil }
            let profile = contact["profile"] as? [String: Any]
            let callType = invitation["callType"] as? [String: Any]
            let media = callType?["media"] as? String
            return DesktopNotificationPayload(
                route: NotificationRoute(
                    userID: int64(user["userId"]),
                    remoteHostID: int64(invitation["remoteHostId"]) ?? remoteHostID,
                    chatID: "@\(contactID)",
                    messageID: nil
                ),
                displayName: (contact["localDisplayName"] as? String)
                    ?? (profile?["displayName"] as? String)
                    ?? "Incoming call",
                preview: media == "video" ? "Incoming video call" : "Incoming audio call",
                category: .incomingCall
            )
        default:
            return nil
        }
    }

    static func shouldSuppress(
        windowFocused: Bool,
        activeUserID: Int64?,
        activeRemoteHostID: Int64?,
        activeChatID: String?,
        route: NotificationRoute
    ) -> Bool {
        windowFocused
            && activeUserID == route.userID
            && activeRemoteHostID == route.remoteHostID
            && activeChatID == route.chatID
    }

    static func preview(
        for payload: DesktopNotificationPayload,
        mode: NotificationPreviewMode
    ) -> DesktopNotificationPreview {
        let genericBody = switch payload.category {
        case .message: "New message"
        case .contactRequest: "New contact request"
        case .incomingCall: "Incoming call"
        }
        return switch mode {
        case .message:
            DesktopNotificationPreview(title: payload.displayName, body: payload.preview)
        case .contact:
            DesktopNotificationPreview(title: payload.displayName, body: genericBody)
        case .hidden:
            DesktopNotificationPreview(title: "SimpleX Chat", body: genericBody)
        }
    }

    private static func isReceived(_ item: [String: Any]) -> Bool {
        guard let direction = item["chatDir"] as? [String: Any] else { return false }
        let type = (direction["type"] as? String) ?? direction.keys.first ?? ""
        return type.hasSuffix("Rcv")
    }

    private static func chatIdentity(_ info: [String: Any]) -> (id: String, name: String)? {
        guard let type = info["type"] as? String else { return nil }
        let payload: [String: Any]
        let prefix: String
        let identifier: Int64?
        switch type {
        case "direct":
            payload = info["contact"] as? [String: Any] ?? [:]
            prefix = "@"
            identifier = int64(payload["contactId"])
        case "group":
            payload = info["groupInfo"] as? [String: Any] ?? [:]
            prefix = "#"
            identifier = int64(payload["groupId"])
        default:
            return nil
        }
        guard let identifier else { return nil }
        let profile = payload["profile"] as? [String: Any]
        let groupProfile = payload["groupProfile"] as? [String: Any]
        let name = (payload["localDisplayName"] as? String)
            ?? (profile?["displayName"] as? String)
            ?? (groupProfile?["displayName"] as? String)
            ?? "SimpleX"
        return ("\(prefix)\(identifier)", name)
    }

    private static func int64(_ value: Any?) -> Int64? {
        if let value = value as? Int64 { return value }
        if let value = value as? NSNumber { return value.int64Value }
        return nil
    }
}
