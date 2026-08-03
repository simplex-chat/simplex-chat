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

@Test func recognizesMigrationSuccess() {
    #expect(NativeChatParser.migrationSucceeded(Data(#"{"type":"ok"}"#.utf8)))
    #expect(NativeChatParser.migrationSucceeded(Data(#""ok""#.utf8)))
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
