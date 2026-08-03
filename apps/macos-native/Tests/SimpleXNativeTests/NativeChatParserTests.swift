import CoreBridge
import Foundation
import Testing
@testable import SimpleXNative

@Test func parsesDesktopChatListResponse() throws {
    let json = #"{"result":{"type":"apiChats","user":{"userId":7},"chats":[{"chatInfo":{"type":"direct","contact":{"contactId":42,"localDisplayName":"Alice","profile":{"displayName":"Alice"}}},"chatItems":[{"meta":{"itemText":"Hello","itemTs":"2026-08-02T20:00:00Z"}}],"chatStats":{"unreadCount":2}}]}}"#
    let chats = try NativeChatParser.chats(from: Data(json.utf8))
    #expect(chats.count == 1)
    #expect(chats[0].id == "@42")
    #expect(chats[0].displayName == "Alice")
    #expect(chats[0].preview == "Hello")
    #expect(chats[0].unreadCount == 2)
}

@Test func parsesConversationDirections() throws {
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":1,"itemText":"Hi","itemTs":"2026-08-02T20:00:00Z"}},{"chatDir":{"type":"directSnd"},"meta":{"itemId":2,"itemText":"Hey","itemTs":"2026-08-02T20:01:00Z"}}]}}}"#
    let messages = try NativeChatParser.messages(from: Data(json.utf8))
    #expect(messages.map(\.sent) == [false, true])
    #expect(messages.map(\.text) == ["Hi", "Hey"])
}

@Test func parsesImageMessagePreviewAndFile() throws {
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":9,"itemText":"A photo","itemTs":"2026-08-02T20:00:00Z","deletable":true},"content":{"type":"rcvMsgContent","msgContent":{"type":"image","text":"A photo","image":"data:image/jpeg;base64,AA=="}},"file":{"fileName":"photo.jpg","fileSource":{"filePath":"photo.jpg","cryptoArgs":null}}}]}}}"#
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)
    #expect(message.deletable)
    #expect(message.content == .image(
        preview: "data:image/jpeg;base64,AA==",
        fileName: "photo.jpg",
        filePath: "photo.jpg"
    ))
    #expect(message.content.fileURL?.lastPathComponent == "photo.jpg")
}

@Test func messageSelectionSupportsRangesAndCommandToggle() {
    let ordered: [Int64] = [10, 20, 30, 40]
    let first = MessageSelection.updated(
        current: [], anchor: nil, clicked: 20, orderedIDs: ordered, command: false, shift: false
    )
    #expect(first.selection == [20])

    let range = MessageSelection.updated(
        current: first.selection, anchor: first.anchor, clicked: 40, orderedIDs: ordered, command: false, shift: true
    )
    #expect(range.selection == [20, 30, 40])

    let toggled = MessageSelection.updated(
        current: range.selection, anchor: range.anchor, clicked: 30, orderedIDs: ordered, command: true, shift: false
    )
    #expect(toggled.selection == [20, 40])
}

@Test func densityTokensStayOnTheMacSpacingGrid() {
    #expect(DesktopChatDensity.compact.tokens.chatRowPadding == 4)
    #expect(DesktopChatDensity.comfortable.tokens.transcriptGap == 12)
    #expect(DesktopChatDensity.spacious.tokens.composerPadding == 16)
    #expect(DesktopChatDensity.compact.tokens.avatarSize < DesktopChatDensity.spacious.tokens.avatarSize)
}

@Test func conversationSearchFindsCaseInsensitiveTextAndWrapsResults() {
    let messages = [
        NativeMessage(id: 1, text: "First PHOTO", timestamp: nil, sent: false, author: nil, deletable: true, content: .text),
        NativeMessage(id: 2, text: "No match", timestamp: nil, sent: true, author: nil, deletable: true, content: .text),
        NativeMessage(id: 3, text: "Another photo", timestamp: nil, sent: false, author: nil, deletable: true, content: .text),
    ]
    let matches = ConversationSearch.matches(messages, query: "photo")
    #expect(matches.map(\.id) == [1, 3])
    #expect(ConversationSearch.nextID(in: matches, currentID: 3, offset: 1) == 1)
    #expect(ConversationSearch.nextID(in: matches, currentID: 1, offset: -1) == 3)
    #expect(ConversationSearch.resultDescription(matches: matches, selectedID: 3, queryIsEmpty: false) == "2 of 2")
}

@Test(.disabled("Requires an Apple-provisioned application identifier; SwiftPM test hosts have none"))
func databasePassphraseKeychainAddsUpdatesLoadsAndDeletes() async throws {
    let service = "chat.simplex.native.tests.\(UUID().uuidString)"
    let store = DatabasePassphraseKeychain(service: service, account: "test-database")
    try await store.delete()

    do {
        #expect(try await store.load() == nil)
        try await store.save("first-passphrase")
        #expect(try await store.load() == "first-passphrase")
        try await store.save("updated-passphrase")
        #expect(try await store.load() == "updated-passphrase")
        try await store.delete()
        #expect(try await store.load() == nil)
    } catch {
        try? await store.delete()
        throw error
    }
}

@Test func attachmentReorderingAndFailureRetentionPreserveOrder() {
    let urls = ["one.jpg", "two.mov", "three.pdf"].map { URL(fileURLWithPath: "/tmp/\($0)") }
    let attachments = [
        PendingAttachment(id: UUID(), url: urls[0], fileName: "one.jpg", kind: .image, byteCount: 1, previewImage: nil),
        PendingAttachment(id: UUID(), url: urls[1], fileName: "two.mov", kind: .video, byteCount: 2, previewImage: nil),
        PendingAttachment(id: UUID(), url: urls[2], fileName: "three.pdf", kind: .document, byteCount: 3, previewImage: nil),
    ]
    let reordered = PendingAttachment.reordered(attachments, from: attachments[2].id, before: attachments[0].id)
    #expect(reordered.map(\.fileName) == ["three.pdf", "one.jpg", "two.mov"])
    #expect(PendingAttachment.remainingAfterFailure(reordered, at: 1).map(\.fileName) == ["one.jpg", "two.mov"])
}

@Test func parsesMessageNotificationRouteAndPrivacyModes() throws {
    let json = #"{"remoteHostId":null,"result":{"type":"newChatItems","user":{"userId":7,"localDisplayName":"Me"},"chatItems":[{"chatInfo":{"type":"direct","contact":{"contactId":42,"localDisplayName":"Alice","profile":{"displayName":"Alice"}}},"chatItem":{"chatDir":{"type":"directRcv"},"meta":{"itemId":99,"itemText":"Secret hello"}}}]}}"#
    let payload = try #require(NativeNotificationParser.payload(from: Data(json.utf8)))
    #expect(payload.route.userID == 7)
    #expect(payload.route.chatID == "@42")
    #expect(payload.route.messageID == 99)
    #expect(payload.route.identifier == "simplex.7.-1._42.99")
    #expect(NativeNotificationParser.preview(for: payload, mode: .message) == .init(
        title: "Alice", body: "Secret hello"
    ))
    #expect(NativeNotificationParser.preview(for: payload, mode: .contact) == .init(
        title: "Alice", body: "New message"
    ))
    #expect(NativeNotificationParser.preview(for: payload, mode: .hidden) == .init(
        title: "SimpleX Chat", body: "New message"
    ))
}

@Test func notificationSuppressionRequiresTheExactFocusedConversation() {
    let route = NotificationRoute(userID: 7, remoteHostID: nil, chatID: "@42", messageID: 99)
    #expect(NativeNotificationParser.shouldSuppress(
        windowFocused: true,
        activeUserID: 7,
        activeRemoteHostID: nil,
        activeChatID: "@42",
        route: route
    ))
    #expect(!NativeNotificationParser.shouldSuppress(
        windowFocused: true,
        activeUserID: 7,
        activeRemoteHostID: nil,
        activeChatID: "@43",
        route: route
    ))
    #expect(!NativeNotificationParser.shouldSuppress(
        windowFocused: false,
        activeUserID: 7,
        activeRemoteHostID: nil,
        activeChatID: "@42",
        route: route
    ))
}

@Test func notificationRoutesQueueUntilTheInterfaceIsReady() {
    var queue = NotificationRouteQueue()
    let first = NotificationRoute(userID: 7, remoteHostID: nil, chatID: "@42", messageID: 1)
    let second = NotificationRoute(userID: 7, remoteHostID: nil, chatID: "#9", messageID: 2)
    queue.enqueue(first)
    queue.enqueue(first)
    queue.enqueue(second)
    #expect(queue.consumeIfReady(false).isEmpty)
    #expect(queue.consumeIfReady(true) == [first, second])
    #expect(queue.consumeIfReady(true).isEmpty)
}

@Test func recognizesMigrationSuccess() {
    #expect(NativeChatParser.migrationSucceeded(Data(#"{"type":"ok"}"#.utf8)))
    #expect(NativeChatParser.migrationSucceeded(Data(#""ok""#.utf8)))
}

@Test func decodesDataURIImagePreview() {
    let onePixelPNG = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="
    #expect(NativeChatParser.image(from: onePixelPNG) != nil)
}

@Test func recognizesCoreCommandFailure() {
    let error = Data(#"{"remoteHostId":null,"error":{"type":"chatError","errorType":{"type":"fileSize","filePath":"huge.mov"}}}"#.utf8)
    #expect(NativeChatParser.commandError(from: error) == "SimpleX could not complete the action (fileSize).")
    let success = Data(#"{"remoteHostId":null,"result":{"type":"cmdOk"}}"#.utf8)
    #expect(NativeChatParser.commandError(from: success) == nil)
}

@Test func opensTemporaryDatabaseWithBundledCore() throws {
    guard let libraryDirectory = ProcessInfo.processInfo.environment["SIMPLEX_CORE_LIB_DIR"] else {
        return
    }

    let temporaryDirectory = FileManager.default.temporaryDirectory
        .appendingPathComponent("simplex-native-core-\(UUID().uuidString)", isDirectory: true)
    try FileManager.default.createDirectory(at: temporaryDirectory, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: temporaryDirectory) }

    var error = [CChar](repeating: 0, count: 4096)
    #expect(sx_core_load(libraryDirectory, &error, error.count))
    #expect(sx_core_initialize(&error, error.count))

    var controller: UnsafeMutableRawPointer?
    let databasePrefix = temporaryDirectory.appendingPathComponent("simplex_v1").path
    let result = databasePrefix.withCString { path in
        "".withCString { key in
            "yesUp".withCString { confirmation in
                sx_core_migrate_init(path, key, confirmation, &controller)
            }
        }
    }

    let response = try #require(result.flatMap(String.init(validatingUTF8:)))
    #expect(NativeChatParser.migrationSucceeded(Data(response.utf8)))
    if let result { sx_core_free(result) }
    if let controller {
        if let closeResult = sx_core_close_store(controller) {
            sx_core_free(closeResult)
        }
    }
}
