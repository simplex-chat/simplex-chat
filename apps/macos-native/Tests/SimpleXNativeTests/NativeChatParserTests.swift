import AppKit
import CoreBridge
import Foundation
import SwiftUI
import Testing
@testable import SimpleXNative

@Test func processLockAllowsOnlyOneNativeFrontend() throws {
    // Given
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    let lockURL = directory.appendingPathComponent("simplex-native.lock")
    defer { try? FileManager.default.removeItem(at: directory) }

    // When / Then
    var firstGuard = SingleInstanceGuard(lockURL: lockURL)
    #expect(firstGuard != nil)
    #expect(SingleInstanceGuard(lockURL: lockURL) == nil)
    firstGuard = nil
    #expect(SingleInstanceGuard(lockURL: lockURL) != nil)
}

@Test func processLockYieldsToALegacyFrontendThatDoesNotOwnTheLock() throws {
    // Given: an older build is already registered with macOS but predates the lock file.
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    let lockURL = directory.appendingPathComponent("simplex-native.lock")
    defer { try? FileManager.default.removeItem(at: directory) }

    // When / Then: the new process releases the lock and yields to the legacy process.
    #expect(SingleInstanceGuard(
        lockURL: lockURL,
        otherApplicationRunning: { true }
    ) == nil)

    // And: the rejected launch did not strand the lock for the next clean launch.
    #expect(SingleInstanceGuard(
        lockURL: lockURL,
        otherApplicationRunning: { false }
    ) != nil)
}

@Test func parsesDesktopChatListResponse() throws {
    let json = #"{"result":{"type":"apiChats","user":{"userId":7},"chats":[{"chatInfo":{"type":"direct","contact":{"contactId":42,"localDisplayName":"Alice","profile":{"displayName":"Alice"}}},"chatItems":[{"meta":{"itemText":"Hello","itemTs":"2026-08-02T20:00:00Z"}}],"chatStats":{"unreadCount":2}}]}}"#
    let chats = try NativeChatParser.chats(from: Data(json.utf8))
    #expect(chats.count == 1)
    #expect(chats[0].id == "@42")
    #expect(chats[0].displayName == "Alice")
    #expect(chats[0].preview == "Hello")
    #expect(chats[0].unreadCount == 2)
}

@Test func channelOwnersSendGroupRepliesWithTheGroupIdentity() throws {
    // Given
    let json = #"{"result":{"type":"apiChats","user":{"userId":7},"chats":[{"chatInfo":{"type":"group","groupInfo":{"groupId":9,"localDisplayName":"News","useRelays":true,"membership":{"memberRole":"owner"},"groupProfile":{"displayName":"News"}}},"chatItems":[],"chatStats":{}}]}}"#

    // When
    let chat = try #require(NativeChatParser.chats(from: Data(json.utf8)).first)
    let reply = SimpleXCore.composedMessage(
        messageContent: ["type": "text", "text": "Reply"],
        quotedItemID: 42
    )
    let command = try SimpleXCore.sendCommand(message: reply, to: chat)

    // Then
    #expect(chat.sendAsGroup)
    #expect(command.hasPrefix("/_send #9(as_group=on) "))
}

@Test func ordinaryGroupsAndScopedSupportChatsDoNotImpersonateTheGroup() throws {
    // Given
    let json = #"{"result":{"type":"apiChats","user":{"userId":7},"chats":[{"chatInfo":{"type":"group","groupInfo":{"groupId":9,"localDisplayName":"Friends","useRelays":false,"membership":{"memberRole":"owner"},"groupProfile":{"displayName":"Friends"}}},"chatItems":[],"chatStats":{}},{"chatInfo":{"type":"group","groupChatScope":{"type":"memberSupport","groupMember_":null},"groupInfo":{"groupId":10,"localDisplayName":"Support","useRelays":true,"membership":{"memberRole":"owner"},"groupProfile":{"displayName":"Support"}}},"chatItems":[],"chatStats":{}}]}}"#

    // When
    let chats = try NativeChatParser.chats(from: Data(json.utf8))

    // Then
    #expect(chats.count == 2)
    #expect(chats.allSatisfy { !$0.sendAsGroup })
}

@Test func parsesConversationDirections() throws {
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":1,"itemText":"Hi","itemTs":"2026-08-02T20:00:00Z"}},{"chatDir":{"type":"directSnd"},"meta":{"itemId":2,"itemText":"Hey","itemTs":"2026-08-02T20:01:00Z"}}]}}}"#
    let messages = try NativeChatParser.messages(from: Data(json.utf8))
    #expect(messages.map(\.sent) == [false, true])
    #expect(messages.map(\.text) == ["Hi", "Hey"])
}

@Test func markChatReadUsesTheExistingCoreCommand() {
    #expect(SimpleXCore.markChatReadCommand(chatID: "@42") == "/_read chat @42")
    #expect(SimpleXCore.markChatReadCommand(chatID: "#9") == "/_read chat #9")
}

@MainActor
@Test func openingTheLatestMessagesClearsUnreadOnlyAfterCoreConfirmation() async throws {
    let unreadChat = NativeChat(
        id: "@88",
        apiID: 88,
        kind: .direct,
        displayName: "Unread chat",
        image: nil,
        preview: "New message",
        timestamp: nil,
        unreadCount: 3,
        sendAsGroup: false
    )
    let readProbe = MarkReadProbe()
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == unreadChat.id)
            #expect(aroundMessageID == nil)
            return [NativeMessage(
                id: 880,
                text: "New message",
                timestamp: nil,
                sent: false,
                author: nil,
                deletable: true,
                content: .text
            )]
        },
        markChatReadOperation: { chatID in
            await readProbe.mark(chatID)
        },
        windowFocusedOperation: { true }
    )
    model.chats = [unreadChat]

    let load = try #require(model.selectChat(unreadChat.id))
    await load.value

    let recordedChatIDs = await readProbe.recordedChatIDs()
    #expect(recordedChatIDs == [unreadChat.id])
    #expect(model.chats.first(where: { $0.id == unreadChat.id })?.unreadCount == 0)
}

@MainActor
@Test func backgroundConversationRefreshDoesNotClearUnreadMessages() async throws {
    let unreadChat = NativeChat(
        id: "@89",
        apiID: 89,
        kind: .direct,
        displayName: "Background chat",
        image: nil,
        preview: "Keep this unread",
        timestamp: nil,
        unreadCount: 1,
        sendAsGroup: false
    )
    let readProbe = MarkReadProbe()
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { _, _ in [] },
        markChatReadOperation: { chatID in
            await readProbe.mark(chatID)
        },
        windowFocusedOperation: { false }
    )
    model.chats = [unreadChat]

    let load = try #require(model.selectChat(unreadChat.id))
    await load.value

    let recordedChatIDs = await readProbe.recordedChatIDs()
    #expect(recordedChatIDs.isEmpty)
    #expect(model.chats.first?.unreadCount == 1)
}

@MainActor
@Test func longMessageBodyUsesItsFullWrappedHeight() {
    let shortHeight = messageBodyHeight("Short message")
    let longHeight = messageBodyHeight(
        String(repeating: "Unlimited browser profiles keep every profile isolated. ", count: 8)
    )

    #expect(longHeight > 100)
    #expect(longHeight > shortHeight * 2)
}

@MainActor
private func messageBodyHeight(_ text: String) -> CGFloat {
    let view = HStack(alignment: .center, spacing: 4) {
        Color.clear.frame(width: 44, height: 44)
        MessageBodyText(text: text)
            .padding(.horizontal, 12)
            .padding(.vertical, 8)
            .background(Color.accentColor, in: RoundedRectangle(cornerRadius: 16))
    }
    .frame(width: 520, alignment: .trailing)

    return NSHostingView(rootView: view).fittingSize.height
}

@Test func parsesImageMessagePreviewAndFile() throws {
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":9,"itemText":"A photo","itemTs":"2026-08-02T20:00:00Z","deletable":true},"content":{"type":"rcvMsgContent","msgContent":{"type":"image","text":"A photo","image":"data:image/jpeg;base64,AA=="}},"quotedItem":{"chatDir":{"type":"directSnd"},"itemId":7,"sentAt":"2026-08-02T19:59:00Z","content":{"type":"text","text":"Original message"}},"file":{"fileName":"photo.jpg","fileSource":{"filePath":"photo.jpg","cryptoArgs":{"fileKey":"test-key","fileNonce":"test-nonce"}}}}]}}}"#
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)
    #expect(message.deletable)
    #expect(message.content == .image(
        preview: "data:image/jpeg;base64,AA==",
        fileName: "photo.jpg"
    ))
    #expect(message.fileSource?.sourceURL.lastPathComponent == "photo.jpg")
    #expect(message.fileSource?.cryptoArgs == NativeCryptoFileArgs(fileKey: "test-key", fileNonce: "test-nonce"))
    #expect(message.quotedItem == NativeQuote(messageID: 7, text: "Original message", sent: true, author: nil))
}

@Test func parsesLinkPreviewAsOneClickableVideoCard() throws {
    let url = "https://youtu.be/ishgn7-NLIU?t=24"
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":24,"itemText":"https://youtu.be/ishgn7-NLIU?t=24"},"content":{"type":"rcvMsgContent","msgContent":{"type":"link","text":"https://youtu.be/ishgn7-NLIU?t=24","preview":{"uri":"https://youtu.be/ishgn7-NLIU?t=24","title":"Video title","description":"Video description","image":"data:image/jpeg;base64,AA==","content":{"type":"video","duration":24}}}}}]}}}"#
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)
    let preview = try #require({
        if case let .link(preview) = message.content { return preview }
        return nil
    }())

    #expect(message.text == url)
    #expect(preview.uri == url)
    #expect(preview.title == "Video title")
    #expect(preview.description == "Video description")
    #expect(preview.videoDuration == 24)
    #expect(preview.durationLabel == "0:24")
    #expect(preview.destination?.absoluteString == url)
}

@Test func standaloneMessageLinksRequireOneCompleteWebURL() {
    #expect(NativeMessageLink.standaloneURL(in: " https://youtu.be/ishgn7-NLIU?t=24 \n") != nil)
    #expect(NativeMessageLink.standaloneURL(in: "See https://simplex.chat") == nil)
    #expect(NativeMessageLink.standaloneURL(in: "simplex.chat") == nil)
    #expect(NativeMessageLink.standaloneURL(in: "file:///tmp/private") == nil)
}

@Test func youtubeLinksBecomePrivacyEnhancedInlinePlayersAtTheirTimestamp() throws {
    let embed = try #require(NativeMessageLink.youtubeEmbedURL(for: "https://youtu.be/ishgn7-NLIU?t=24"))
    let components = try #require(URLComponents(url: embed, resolvingAgainstBaseURL: false))
    let query = Dictionary(uniqueKeysWithValues: (components.queryItems ?? []).compactMap { item in
        item.value.map { (item.name, $0) }
    })

    #expect(components.host == "www.youtube-nocookie.com")
    #expect(components.path == "/embed/ishgn7-NLIU")
    #expect(query["start"] == "24")
    #expect(query["autoplay"] == "1")
    #expect(query["playsinline"] == "1")

    let longOffset = try #require(NativeMessageLink.youtubeEmbedURL(
        for: "https://www.youtube.com/watch?v=ishgn7-NLIU&t=1m5s"
    ))
    #expect(URLComponents(url: longOffset, resolvingAgainstBaseURL: false)?
        .queryItems?.first(where: { $0.name == "start" })?.value == "65")
    #expect(NativeMessageLink.youtubeEmbedURL(for: "https://example.com/watch?v=ishgn7-NLIU") == nil)
    #expect(NativeMessageLink.youtubeEmbedURL(for: "https://youtu.be/not-safe") == nil)
}

@Test func groupMessagesAndQuotesPreferLocallyDisambiguatedMemberNames() throws {
    // Given
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"groupRcv","groupMember":{"localDisplayName":"Maya (work)","memberProfile":{"displayName":"Maya"}}},"meta":{"itemId":9,"itemText":"Reply","itemTs":"2026-08-02T20:00:00Z"},"content":{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Reply"}},"quotedItem":{"chatDir":{"type":"groupRcv","groupMember":{"localDisplayName":"Jordan (cycling)","memberProfile":{"displayName":"Jordan"}}},"itemId":7,"sentAt":"2026-08-02T19:59:00Z","content":{"type":"text","text":"Original message"}}}]}}}"#

    // When
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)

    // Then
    #expect(message.author == "Maya (work)")
    #expect(message.quotedItem?.author == "Jordan (cycling)")
    #expect(message.quotedItem?.messageID == 7)
    #expect(message.quotedItem?.text == "Original message")
}

private struct ReplyEligibilityCase: Sendable, CustomTestStringConvertible {
    let name: String
    let itemID: Int64
    let metaFields: String
    let content: String
    let expected: Bool

    var testDescription: String { name }
}

private let replyEligibilityCases = [
    ReplyEligibilityCase(
        name: "ordinary message",
        itemID: 1,
        metaFields: "",
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Hello"}}"#,
        expected: true
    ),
    ReplyEligibilityCase(
        name: "system event",
        itemID: 2,
        metaFields: "",
        content: #"{"type":"rcvGroupEvent"}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "deleted message",
        itemID: 3,
        metaFields: #", "itemDeleted":{"type":"deleted"}"#,
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Gone"}}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "live message",
        itemID: 4,
        metaFields: #", "itemLive":true"#,
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Typing"}}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "legacy live message alias",
        itemID: 6,
        metaFields: #", "isLive":true"#,
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Typing"}}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "report message",
        itemID: 5,
        metaFields: "",
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"report","text":"Report"}}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "temporary live placeholder",
        itemID: -2,
        metaFields: "",
        content: #"{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Typing"}}"#,
        expected: false
    ),
    ReplyEligibilityCase(
        name: "other temporary message",
        itemID: -1,
        metaFields: "",
        content: #"{"type":"sndMsgContent","msgContent":{"type":"text","text":"Pending"}}"#,
        expected: false
    ),
]

@Test(arguments: replyEligibilityCases)
private func parserLimitsRepliesToAvailableMessages(testCase: ReplyEligibilityCase) throws {
    // Given
    let json = """
        {"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":\(testCase.itemID),"itemText":"Item","itemTs":"2026-08-02T20:00:00Z"\(testCase.metaFields)},"content":\(testCase.content)}]}}}
        """

    // When
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)

    // Then
    #expect(message.replyable == testCase.expected)
}

private struct QuotedAttachmentCase: Sendable, CustomTestStringConvertible {
    let contentType: String
    let additionalContent: String
    let expectedPreview: String
    let expectedVisual: NativeReplyContextVisual

    var testDescription: String { contentType }
}

private let quotedAttachmentCases = [
    QuotedAttachmentCase(
        contentType: "image",
        additionalContent: ",\"image\":\"quoted-photo-preview\"",
        expectedPreview: "Photo",
        expectedVisual: .image("quoted-photo-preview")
    ),
    QuotedAttachmentCase(
        contentType: "video",
        additionalContent: ",\"image\":\"quoted-video-preview\"",
        expectedPreview: "Video",
        expectedVisual: .video("quoted-video-preview")
    ),
    QuotedAttachmentCase(
        contentType: "voice",
        additionalContent: "",
        expectedPreview: "Voice message",
        expectedVisual: .voice
    ),
    QuotedAttachmentCase(
        contentType: "file",
        additionalContent: "",
        expectedPreview: "File",
        expectedVisual: .file
    ),
]

@Test(arguments: quotedAttachmentCases)
private func whitespaceOnlyQuotedAttachmentsUseMeaningfulPreviews(testCase: QuotedAttachmentCase) throws {
    // Given
    let json = """
        {"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":9,"itemText":"Reply","itemTs":"2026-08-02T20:00:00Z"},"content":{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Reply"}},"quotedItem":{"chatDir":{"type":"directSnd"},"itemId":7,"sentAt":"2026-08-02T19:59:00Z","content":{"type":"\(testCase.contentType)","text":"   "\(testCase.additionalContent)}}}]}}}
        """

    // When
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)

    // Then
    #expect(message.quotedItem?.text == testCase.expectedPreview)
    #expect(message.quotedItem?.visual == testCase.expectedVisual)
}

@Test func replyPreviewNormalizesCaptionsAndFallsBackForAttachments() {
    // Given
    let captioned = NativeMessage(
        id: 1,
        text: "  Keep this caption  ",
        timestamp: nil,
        sent: false,
        author: nil,
        deletable: true,
        content: .image(preview: nil, fileName: "photo.jpg")
    )
    let whitespaceOnly = NativeMessage(
        id: 2,
        text: " \n ",
        timestamp: nil,
        sent: false,
        author: nil,
        deletable: true,
        content: .file(fileName: "document.pdf")
    )
    let unnamedPhoto = NativeMessage(
        id: 3,
        text: "",
        timestamp: nil,
        sent: false,
        author: nil,
        deletable: true,
        content: .image(preview: nil, fileName: "   ")
    )

    // When / Then
    #expect(captioned.replyPreview == "Keep this caption")
    #expect(whitespaceOnly.replyPreview == "document.pdf")
    #expect(unnamedPhoto.replyPreview == "Photo")
}

@Test func replyContextVisualsMatchAttachmentTypes() {
    let imagePreview = "data:image/jpeg;base64,AA=="
    let videoPreview = "data:image/jpeg;base64,AQ=="

    #expect(NativeMessageContent.text.replyContextVisual == nil)
    #expect(NativeMessageContent.image(
        preview: imagePreview,
        fileName: "photo.jpg"
    ).replyContextVisual == .image(imagePreview))
    #expect(NativeMessageContent.video(
        preview: videoPreview,
        fileName: "clip.mp4"
    ).replyContextVisual == .video(videoPreview))
    #expect(NativeMessageContent.voice(
        fileName: "voice.m4a",
        duration: 5
    ).replyContextVisual == .voice)
    #expect(NativeMessageContent.file(fileName: "notes.pdf").replyContextVisual == .file)
}

@Test func fullResolutionMediaUsesNativeQuickLook() {
    #expect(NativeMessageContent.image(preview: nil, fileName: nil).opensInQuickLook)
    #expect(NativeMessageContent.video(preview: nil, fileName: nil).opensInQuickLook)
    #expect(!NativeMessageContent.voice(fileName: nil, duration: nil).opensInQuickLook)
    #expect(!NativeMessageContent.file(fileName: nil).opensInQuickLook)
}

@Test func parsesVoiceMessageForPlaybackAndReplyContext() throws {
    // Given
    let json = #"{"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":12,"itemText":"","itemTs":"2026-08-02T20:00:00Z"},"content":{"type":"rcvMsgContent","msgContent":{"type":"voice","text":"","duration":65}},"file":{"fileName":"voice.m4a","fileSource":{"filePath":"voice.m4a","cryptoArgs":{"fileKey":"key","fileNonce":"nonce"}}}}]}}}"#

    // When
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)

    // Then
    #expect(message.content == .voice(fileName: "voice.m4a", duration: 65))
    #expect(message.content.fileName == "voice.m4a")
    #expect(message.replyPreview == "Voice message, 1:05")
    #expect(message.fileSource == NativeCryptoFile(
        filePath: "voice.m4a",
        cryptoArgs: NativeCryptoFileArgs(fileKey: "key", fileNonce: "nonce")
    ))
    #expect(message.replyable)
}

@Test func composedMessagesIncludeAQuoteOnlyWhenReplying() {
    let reply = SimpleXCore.composedMessage(
        messageContent: ["type": "text", "text": "Reply"],
        quotedItemID: 42
    )
    #expect(reply["quotedItemId"] as? Int64 == 42)

    let ordinary = SimpleXCore.composedMessage(
        messageContent: ["type": "text", "text": "Ordinary"],
        quotedItemID: nil
    )
    #expect(ordinary["quotedItemId"] == nil)
}

@Test func coreSendCommandsPreserveQuoteIdentityForDirectAndGroupReplies() throws {
    // Given
    let message = SimpleXCore.composedMessage(
        messageContent: ["type": "text", "text": "Reply"],
        quotedItemID: 42
    )
    let direct = NativeChat(
        id: "@7",
        apiID: 7,
        kind: .direct,
        displayName: "Maya",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: false
    )
    let group = NativeChat(
        id: "#9",
        apiID: 9,
        kind: .group,
        displayName: "Weekend plans",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: true
    )

    // When
    let directCommand = try SimpleXCore.sendCommand(message: message, to: direct)
    let groupCommand = try SimpleXCore.sendCommand(message: message, to: group)

    // Then
    #expect(directCommand.hasPrefix("/_send @7 live=off ttl=default sign=off json "))
    #expect(groupCommand.hasPrefix("/_send #9(as_group=on) live=off ttl=default sign=off json "))
    for command in [directCommand, groupCommand] {
        let json = try #require(command.components(separatedBy: " json ").last)
        let encoded = try #require(json.data(using: .utf8))
        let messages = try #require(JSONSerialization.jsonObject(with: encoded) as? [[String: Any]])
        #expect(messages.first?["quotedItemId"] as? Int64 == 42)
    }
}

@Test func replyAndQuoteAuthorsRespectGroupSendingIdentity() {
    // Given
    let direct = NativeChat(
        id: "@7",
        apiID: 7,
        kind: .direct,
        displayName: "Maya",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: false
    )
    let ordinaryGroup = NativeChat(
        id: "#8",
        apiID: 8,
        kind: .group,
        displayName: "Friends",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: false
    )
    let channel = NativeChat(
        id: "#9",
        apiID: 9,
        kind: .group,
        displayName: "Announcements",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: true
    )

    // When / Then: outgoing direct and ordinary group messages belong to the user.
    #expect(direct.displayedMessageAuthor(sent: true, author: "Local profile") == "You")
    #expect(ordinaryGroup.displayedMessageAuthor(sent: true, author: "Local profile") == "You")

    // Channel-owner messages use the public group identity in both reply surfaces.
    #expect(channel.displayedMessageAuthor(sent: true, author: "Local profile") == "Announcements")

    // Incoming group authors remain specific, with the conversation name as a safe fallback.
    #expect(ordinaryGroup.displayedMessageAuthor(sent: false, author: "Jordan") == "Jordan")
    #expect(direct.displayedMessageAuthor(sent: false, author: nil) == "Maya")
    #expect(direct.displayedMessageAuthor(sent: false, author: "  ") == "Maya")
}

@Test func coreRejectsQuotedMessagesInNonReplyableConversations() {
    // Given
    let message = SimpleXCore.composedMessage(
        messageContent: ["type": "text", "text": "Reply"],
        quotedItemID: 42
    )
    let notes = NativeChat(
        id: "*1",
        apiID: 1,
        kind: .local,
        displayName: "Private notes",
        image: nil,
        preview: "",
        timestamp: nil,
        unreadCount: 0,
        sendAsGroup: false
    )

    // When / Then
    #expect(throws: NativeChatError.self) {
        _ = try SimpleXCore.sendCommand(message: message, to: notes)
    }
}

@Test func sendResponseValidationRequiresTheCommittedMessage() throws {
    // Given
    let committed = Data(#"{"result":{"type":"newChatItems","chatItems":[{"chatItem":{"meta":{"itemId":9}}}]}}"#.utf8)
    let committedReply = Data(#"{"result":{"type":"newChatItems","chatItems":[{"chatItem":{"meta":{"itemId":9},"quotedItem":{"itemId":42}}}]}}"#.utf8)
    let missingItem = Data(#"{"result":{"type":"newChatItems","chatItems":[]}}"#.utf8)
    let wrongResult = Data(#"{"result":{"type":"cmdOk"}}"#.utf8)

    // When / Then
    try NativeChatParser.validateCommandResponse(
        committed,
        expectedType: "newChatItems",
        requireChatItems: true
    )
    let committedReplyReceipt = try NativeChatParser.validateSendResponse(
        committedReply,
        quotedItemID: 42
    )
    #expect(committedReplyReceipt.replyContextConfirmed)
    #expect(committedReplyReceipt.committedMessages.map(\.id) == [9])
    #expect(try !NativeChatParser.validateSendResponse(
        committed,
        quotedItemID: 42
    ).replyContextConfirmed)
    #expect(try NativeChatParser.validateSendResponse(
        committed,
        quotedItemID: nil
    ).replyContextConfirmed)
    #expect(throws: NativeChatError.self) {
        try NativeChatParser.validateCommandResponse(
            missingItem,
            expectedType: "newChatItems",
            requireChatItems: true
        )
    }
    #expect(throws: NativeChatError.self) {
        try NativeChatParser.validateCommandResponse(
            wrongResult,
            expectedType: "newChatItems"
        )
    }
}

@Test func nestedStoreErrorsIdentifyEveryUnavailableReplyTarget() {
    // Given
    let invalidQuote = Data(#"{"error":{"type":"errorStore","storeError":{"type":"invalidQuote"}}}"#.utf8)
    let missingItem = Data(#"{"error":{"type":"errorStore","storeError":{"type":"chatItemNotFound","itemId":9}}}"#.utf8)
    let badItem = Data(#"{"error":{"type":"errorStore","storeError":{"type":"badChatItem","itemId":9,"itemTs":null}}}"#.utf8)
    let unrelated = Data(#"{"error":{"type":"errorStore","storeError":{"type":"fileNotFound","fileId":3}}}"#.utf8)

    // When / Then
    #expect(NativeChatParser.commandErrorMakesReplyTargetUnavailable(invalidQuote))
    #expect(NativeChatParser.commandErrorMakesReplyTargetUnavailable(missingItem))
    #expect(NativeChatParser.commandErrorMakesReplyTargetUnavailable(badItem))
    #expect(!NativeChatParser.commandErrorMakesReplyTargetUnavailable(unrelated))
}

@Test func singleMessageReloadUsesTheCoreQuoteResolutionPagination() {
    // Given / When
    let command = SimpleXCore.chatPageCommand(chatID: "@42", around: 91, count: 0)

    // Then
    #expect(command == "/_get chat @42 around=91 count=0")
}

@Test func encryptedBytesAreNotMistakenForAPlainJPEG() {
    // Given
    let encryptedHeader = Data([0x6a, 0xbd, 0xf4, 0x48, 0x36, 0x31, 0xf0, 0x54])
    let jpegHeader = Data([0xff, 0xd8, 0xff, 0xe0])

    // When / Then
    #expect(!SimpleXCore.imageHeaderIsReadable(encryptedHeader, fileName: "photo.jpg"))
    #expect(SimpleXCore.imageHeaderIsReadable(jpegHeader, fileName: "photo.jpg"))
}

@Test func attachmentFallbackNamesKeepTheStoredFileExtension() {
    // Given / When / Then
    #expect(SimpleXCore.openedFileName(
        preferredName: "Photo",
        sourcePath: "IMG_20260802_145258.jpg"
    ) == "IMG_20260802_145258.jpg")
    #expect(SimpleXCore.openedFileName(
        preferredName: "holiday.jpg",
        sourcePath: "encrypted.bin"
    ) == "holiday.jpg")
}

@Test func encryptedJPEGWithoutMetadataIsRejectedBeforeOpeningPreview() async throws {
    // Given
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: directory) }
    let sourceURL = directory.appendingPathComponent("photo.jpg")
    try Data([0x6a, 0xbd, 0xf4, 0x48, 0x36, 0x31, 0xf0, 0x54]).write(to: sourceURL)
    let source = NativeCryptoFile(filePath: sourceURL.path, cryptoArgs: nil)

    // When / Then
    do {
        _ = try await SimpleXCore().openableURL(for: source, fileName: "photo.jpg")
        Issue.record("Ciphertext should not be handed to Preview as a JPEG.")
    } catch let error as NativeChatError {
        #expect(error.localizedDescription.contains("still be encrypted or incomplete"))
    }
}

@MainActor
@Test func unresolvedQuoteReloadsItsContainingMessageBeforeNavigating() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let unresolvedQuote = NativeQuote(messageID: nil, text: target.text, sent: target.sent, author: target.author)
    let containingMessage = NativeMessage(
        id: 90,
        text: "Reply",
        timestamp: nil,
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: unresolvedQuote
    )
    let refreshedMessage = NativeMessage(
        id: containingMessage.id,
        text: containingMessage.text,
        timestamp: containingMessage.timestamp,
        sent: containingMessage.sent,
        author: containingMessage.author,
        deletable: containingMessage.deletable,
        content: containingMessage.content,
        quotedItem: NativeQuote(
            messageID: target.id,
            text: target.text,
            sent: target.sent,
            author: target.author
        )
    )
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { chatID, itemID in
            #expect(chatID == "@1")
            #expect(itemID == containingMessage.id)
            return refreshedMessage
        }
    )
    model.messages = [target, containingMessage]

    // When
    let navigation = try #require(model.openQuotedMessage(unresolvedQuote, from: containingMessage.id))
    await navigation.value

    // Then
    #expect(model.messages.last?.quotedItem?.messageID == target.id)
    #expect(model.targetMessageID == target.id)
    #expect(model.quoteNavigationError == nil)
}

@MainActor
@Test func missingQuotedMessageShowsAUsefulError() async throws {
    // Given
    let unresolvedQuote = NativeQuote(messageID: nil, text: "Gone", sent: false, author: "Maya")
    let containingMessage = NativeMessage(
        id: 91,
        text: "Reply",
        timestamp: nil,
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: unresolvedQuote
    )
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in containingMessage }
    )
    model.messages = [containingMessage]
    model.beginConversationSearch()
    model.conversationSearchText = "Reply"
    model.updateConversationSearchSelection()
    model.targetMessageID = nil

    // When
    let navigation = try #require(model.openQuotedMessage(unresolvedQuote, from: containingMessage.id))
    await navigation.value

    // Then
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == "The original quoted message is no longer available in this conversation.")
    #expect(model.conversationSearchPresented)
    #expect(model.conversationSearchText == "Reply")
    #expect(model.selectedMessageIDs == [containingMessage.id])
}

@MainActor
@Test func offscreenQuotedMessageLoadsItsPageBeforeScrolling() async throws {
    // Given
    let target = NativeMessage(
        id: 900,
        text: "Original offscreen message",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .text
    )
    let quote = NativeQuote(messageID: target.id, text: target.text, sent: target.sent, author: target.author)
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == target.id)
            return [target]
        }
    )
    #expect(!model.messages.contains(where: { $0.id == target.id }))
    model.beginConversationSearch()
    model.conversationSearchText = "Hey"
    model.updateConversationSearchSelection()
    #expect(model.selectedMessageIDs == [1])

    // When
    let navigation = try #require(model.openQuotedMessage(quote, from: 901))
    await navigation.value

    // Then
    #expect(model.messages == [target])
    #expect(model.targetMessageID == target.id)
    #expect(model.quoteNavigationError == nil)
    #expect(!model.conversationSearchPresented)
    #expect(model.conversationSearchText.isEmpty)
    #expect(model.selectedMessageIDs.isEmpty)
    #expect(model.phase == .ready)
}

@MainActor
@Test func replyContextNavigatesBackToItsOriginalMessage() throws {
    // Given
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)
    model.beginReply(to: original)
    model.targetMessageID = nil

    // When
    let navigation = model.openReplyTarget()

    // Then
    #expect(navigation == nil)
    #expect(model.targetMessageID == original.id)
    #expect(model.replyingTo?.id == original.id)
}

@MainActor
@Test func quotedMessageNavigationConsumesSearchAndContainingSelection() throws {
    // Given
    let model = AppModel(previewMode: true)
    let containingMessage = try #require(model.messages.first(where: { $0.quotedItem != nil }))
    let quote = try #require(containingMessage.quotedItem)
    let sourceID = try #require(quote.messageID)
    model.beginConversationSearch()
    model.conversationSearchText = "native"
    model.updateConversationSearchSelection()
    #expect(model.selectedMessageIDs == [containingMessage.id])

    // When
    let navigation = model.openQuotedMessage(quote, from: containingMessage.id)

    // Then
    #expect(navigation == nil)
    #expect(!model.conversationSearchPresented)
    #expect(model.conversationSearchText.isEmpty)
    #expect(model.selectedMessageIDs.isEmpty)
    #expect(model.targetMessageID == sourceID)
    #expect(model.conversationAnchorMessageID == sourceID)
}

@MainActor
@Test func offscreenReplyContextLoadsItsOriginalMessageBeforeScrolling() async throws {
    // Given
    let original = NativePreviewData.messages(for: "@1")[0]
    let probe = DelayedValue([original])
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == original.id)
            return await probe.load()
        }
    )
    model.beginReply(to: original)
    model.messages = Array(NativePreviewData.messages(for: "@1").dropFirst())

    // When
    let navigation = try #require(model.openReplyTarget())
    await probe.waitUntilRequested()
    await probe.release()
    await navigation.value

    // Then
    #expect(model.messages == [original])
    #expect(model.targetMessageID == original.id)
    #expect(model.replyingTo?.id == original.id)
}

@MainActor
@Test func missingOffscreenReplyTargetRetiresOnlyTheQuoteAndKeepsTheDraft() async throws {
    // Given: the active reply points to an older message outside the visible page.
    let original = NativePreviewData.messages(for: "@1")[0]
    let visibleMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == original.id)
            return visibleMessages
        }
    )
    model.draft = "Keep this reply draft"
    model.beginReply(to: original)
    model.messages = visibleMessages

    // When: Show Original confirms that the requested target is absent.
    let navigation = try #require(model.openReplyTarget())
    await navigation.value

    // Then: only the invalid quote is retired; user content and transcript survive.
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this reply draft")
    #expect(model.messages == visibleMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == nil)
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")
}

@MainActor
@Test func transientReplyTargetLoadFailureKeepsTheQuoteForRetry() async throws {
    // Given
    let original = NativePreviewData.messages(for: "@1")[0]
    let visibleMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, _ in
            throw NativeChatError.unavailable("Temporary history failure")
        }
    )
    model.draft = "Do not lose this"
    model.beginReply(to: original)
    model.messages = visibleMessages

    // When
    let navigation = try #require(model.openReplyTarget())
    await navigation.value

    // Then: an inconclusive load does not claim the original was deleted.
    #expect(model.replyingTo?.id == original.id)
    #expect(model.draft == "Do not lose this")
    #expect(model.messages == visibleMessages)
    #expect(model.replyContextError == nil)
    #expect(model.quoteNavigationError == "Temporary history failure")
}

@MainActor
@Test func cancellingReplyRetiresItsPendingOriginalMessageNavigation() async throws {
    // Given
    let original = NativePreviewData.messages(for: "@1")[0]
    let visibleMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let probe = DelayedValue([original])
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == original.id)
            return await probe.load()
        }
    )
    model.draft = "Keep this draft"
    model.beginReply(to: original)
    model.messages = visibleMessages

    // When: Show Original is loading and Escape cancels the reply context.
    let navigation = try #require(model.openReplyTarget())
    await probe.waitUntilRequested()
    #expect(model.isLoadingConversation)
    model.dismissNearestState()

    // Then: the composer exits reply mode without losing the draft.
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this draft")

    // When: the cancelled backend request eventually returns.
    await probe.release()
    await navigation.value

    // Then: it cannot replace or jump the visible transcript.
    #expect(model.messages == visibleMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.conversationAnchorMessageID == nil)
    #expect(!model.isLoadingConversation)
    #expect(model.quoteNavigationError == nil)
}

@MainActor
@Test func cancellingReplyDoesNotCancelANewerInlineQuoteNavigation() async throws {
    // Given
    let replyTarget = NativePreviewData.messages(for: "@1")[0]
    let inlineTarget = NativeMessage(
        id: 9_704,
        text: "Newer inline quote destination",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .text
    )
    let replyProbe = DelayedValue([replyTarget])
    let inlineProbe = DelayedValue([inlineTarget])
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, aroundMessageID in
            if aroundMessageID == replyTarget.id {
                return await replyProbe.load()
            }
            #expect(aroundMessageID == inlineTarget.id)
            return await inlineProbe.load()
        }
    )
    model.beginReply(to: replyTarget)
    model.messages = Array(NativePreviewData.messages(for: "@1").dropFirst())

    // When: a newer inline quote navigation supersedes Show Original.
    let replyNavigation = try #require(model.openReplyTarget())
    await replyProbe.waitUntilRequested()
    let quote = NativeQuote(
        messageID: inlineTarget.id,
        text: inlineTarget.text,
        sent: inlineTarget.sent,
        author: inlineTarget.author
    )
    let inlineNavigation = try #require(model.openQuotedMessage(quote, from: 9_705))
    await inlineProbe.waitUntilRequested()
    model.cancelReply()

    // Then: cancelling the old reply context leaves the newer user navigation alive.
    #expect(model.replyingTo == nil)
    await inlineProbe.release()
    await inlineNavigation.value
    #expect(model.messages == [inlineTarget])
    #expect(model.targetMessageID == inlineTarget.id)

    // Cleanup: the cancelled older request cannot overwrite the result when it returns.
    await replyProbe.release()
    await replyNavigation.value
    #expect(model.messages == [inlineTarget])
    #expect(model.targetMessageID == inlineTarget.id)
}

@MainActor
@Test func deletedOffscreenQuoteShowsLocalErrorWithoutBreakingConversation() async throws {
    // Given
    let quote = NativeQuote(messageID: 902, text: "Deleted original", sent: false, author: "Maya")
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, _ in
            throw NativeChatError.unavailable("The core no longer has this item.")
        }
    )
    let originalMessages = model.messages
    model.beginConversationSearch()
    model.conversationSearchText = "Hey"
    model.updateConversationSearchSelection()
    model.targetMessageID = nil
    #expect(model.selectedMessageIDs == [1])

    // When
    let navigation = try #require(model.openQuotedMessage(quote, from: 903))
    await navigation.value

    // Then
    #expect(model.messages == originalMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == "The original quoted message is no longer available in this conversation.")
    #expect(model.conversationSearchPresented)
    #expect(model.conversationSearchText == "Hey")
    #expect(model.selectedMessageIDs == [1])
    #expect(model.phase == .ready)

    // When: a notification changes chats before the quote error is acknowledged.
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "#2",
        messageID: nil
    ))

    // Then: only the originating chat retains the quote-navigation error.
    #expect(model.selectedChatID == "#2")
    #expect(model.quoteNavigationError == nil)
    model.selectChat("@1")
    #expect(model.quoteNavigationError == "The original quoted message is no longer available in this conversation.")
}

@MainActor
@Test func offscreenQuoteMissingFromLoadedPagePreservesTranscriptAndSearch() async throws {
    // Given
    let quote = NativeQuote(messageID: 906, text: "Missing original", sent: false, author: "Maya")
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, aroundMessageID in
            #expect(aroundMessageID == quote.messageID)
            return []
        }
    )
    let originalMessages = model.messages
    model.beginConversationSearch()
    model.conversationSearchText = "Hey"
    model.updateConversationSearchSelection()
    model.targetMessageID = nil

    // When
    let navigation = try #require(model.openQuotedMessage(quote, from: 907))
    await navigation.value

    // Then
    #expect(model.messages == originalMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == "The original quoted message is no longer available in this conversation.")
    #expect(model.conversationSearchPresented)
    #expect(model.conversationSearchText == "Hey")
    #expect(model.selectedMessageIDs == [1])
    #expect(model.phase == .ready)
}

private actor DelayedConversationLoadFailure {
    private var requested = false
    private var released = false

    func load() async throws -> [NativeMessage] {
        requested = true
        while !released { await Task.yield() }
        throw NativeChatError.unavailable("The old conversation load failed late.")
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }
}

private actor DelayedValue<Value: Sendable> {
    private let value: Value
    private var requested = false
    private var released = false

    init(_ value: Value) {
        self.value = value
    }

    func load() async -> Value {
        requested = true
        while !released { await Task.yield() }
        return value
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }
}

private actor MarkReadProbe {
    private var chatIDs: [NativeChat.ID] = []

    func mark(_ chatID: NativeChat.ID) {
        chatIDs.append(chatID)
    }

    func recordedChatIDs() -> [NativeChat.ID] {
        chatIDs
    }
}

private actor ConversationRefreshProbe {
    private let messages: [NativeMessage]
    private let delayFirstRequest: Bool
    private var requests: [Int64?] = []
    private var firstRequestReleased: Bool

    init(messages: [NativeMessage], delayFirstRequest: Bool = false) {
        self.messages = messages
        self.delayFirstRequest = delayFirstRequest
        firstRequestReleased = !delayFirstRequest
    }

    func load(around messageID: Int64?) async -> [NativeMessage] {
        requests.append(messageID)
        if delayFirstRequest, requests.count == 1 {
            while !firstRequestReleased { await Task.yield() }
        }
        return messages
    }

    func waitUntilFirstRequested() async {
        while requests.isEmpty { await Task.yield() }
    }

    func waitUntilRequestCount(_ count: Int) async {
        while requests.count < count { await Task.yield() }
    }

    func releaseFirstRequest() {
        firstRequestReleased = true
    }

    func recordedRequests() -> [Int64?] {
        requests
    }
}

private actor FallbackRefreshProbe {
    private let latestMessages: [NativeMessage]
    private var requests: [Int64?] = []

    init(latestMessages: [NativeMessage]) {
        self.latestMessages = latestMessages
    }

    func load(around messageID: Int64?) throws -> [NativeMessage] {
        requests.append(messageID)
        if messageID != nil {
            throw NativeChatError.unavailable("The anchored message was deleted.")
        }
        return latestMessages
    }

    func recordedRequests() -> [Int64?] {
        requests
    }
}

private actor LatestNavigationProbe {
    private let latestMessages: [NativeMessage]
    private var requests: [Int64?] = []

    init(latestMessages: [NativeMessage]) {
        self.latestMessages = latestMessages
    }

    func load(around messageID: Int64?) -> [NativeMessage] {
        requests.append(messageID)
        return latestMessages
    }

    func recordedRequests() -> [Int64?] {
        requests
    }
}

@MainActor
private func makeLiveRefreshModel(probe: ConversationRefreshProbe) -> AppModel {
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { _, aroundMessageID in
            await probe.load(around: aroundMessageID)
        },
        loadChatsOperation: { userID in
            #expect(userID == NativePreviewData.profile.userID)
            return NativePreviewData.chats
        }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    return model
}

@MainActor
@Test func manualRefreshBlocksConflictingTranscriptActionsFromItsFirstPhase() async throws {
    // Given
    let chatsProbe = DelayedValue(NativePreviewData.chats)
    let originalMessages = NativePreviewData.messages(for: "@1")
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == nil)
            return originalMessages
        },
        loadChatsOperation: { _ in await chatsProbe.load() }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = originalMessages
    let target = try #require(originalMessages.first)
    let quote = NativeQuote(messageID: target.id, text: target.text, sent: target.sent, author: target.author)
    model.draft = "Wait for refresh"
    model.selectMessage(target.id, modifiers: [])

    // When: Refresh is still loading only the chat list.
    model.refresh()
    await chatsProbe.waitUntilRequested()

    // Then: transcript mutations are already disabled before its transcript load begins.
    #expect(model.isRefreshing)
    #expect(!model.isLoadingConversation)
    #expect(!model.canSendDraft)
    #expect(!model.canDeleteSelectedMessages)
    #expect(!model.canNavigateConversationHistory)
    #expect(!model.canRefreshConversation)
    #expect(!model.canReply(to: target))
    model.beginReply(to: target)
    #expect(model.replyingTo == nil)
    model.sendDraft()
    model.requestDeleteSelectedMessages()
    #expect(model.sendTask == nil)
    #expect(!model.showingDeleteConfirmation)
    #expect(model.openQuotedMessage(quote, from: 912) == nil)
    #expect(model.targetMessageID == nil)

    // When: both refresh phases finish.
    await chatsProbe.release()
    for _ in 0..<1_000 {
        if !model.isRefreshing { break }
        await Task.yield()
    }

    // Then: the original controls recover against the refreshed transcript.
    #expect(!model.isRefreshing)
    #expect(!model.isLoadingConversation)
    #expect(model.canSendDraft)
    #expect(model.canDeleteSelectedMessages)
    #expect(model.canNavigateConversationHistory)
    #expect(model.canRefreshConversation)
    #expect(model.canReply(to: target))
    #expect(model.messages == originalMessages)
}

@MainActor
@Test func eventRefreshSkipsItsTranscriptWhenASendStartsDuringChatListLoading() async throws {
    // Given
    let chatsProbe = DelayedValue(NativePreviewData.chats)
    let originalMessages = NativePreviewData.messages(for: "@1")
    let transcriptProbe = ConversationRefreshProbe(messages: originalMessages)
    let sendFailure = "The delayed send failed."
    let sendProbe = DelayedTextSendProbe(failureMessage: sendFailure)
    let model = AppModel(
        previewMode: false,
        sendTextOperation: { _, _, _ in try await sendProbe.send() },
        loadMessagesOperation: { _, aroundMessageID in
            await transcriptProbe.load(around: aroundMessageID)
        },
        loadChatsOperation: { _ in await chatsProbe.load() }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = originalMessages
    model.draft = "Send owns the transcript"

    // When: an event is loading chats and a send starts before its transcript phase.
    let eventRefresh = Task { await model.refreshAfterEvent() }
    await chatsProbe.waitUntilRequested()
    model.sendDraft()
    let send = try #require(model.sendTask)
    await sendProbe.waitUntilRequested()
    await chatsProbe.release()
    await eventRefresh.value

    // Then: the event does not start a competing transcript reload.
    #expect(await transcriptProbe.recordedRequests().isEmpty)
    #expect(model.messages == originalMessages)
    #expect(model.isSendingSelectedChat)

    // Cleanup.
    await sendProbe.release()
    await send.value
    #expect(model.phase == .failed(sendFailure))
}

@MainActor
@Test func cancelledEventAnchorCannotEraseANewerLoadedQuote() async throws {
    // Given
    let originalMessages = NativePreviewData.messages(for: "@1")
    let oldTarget = try #require(originalMessages.first)
    let newerTarget = try #require(originalMessages.dropFirst().first)
    let probe = ConversationRefreshProbe(messages: originalMessages, delayFirstRequest: true)
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { _, aroundMessageID in await probe.load(around: aroundMessageID) },
        loadChatsOperation: { _ in NativePreviewData.chats }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = originalMessages
    let oldQuote = NativeQuote(
        messageID: oldTarget.id,
        text: oldTarget.text,
        sent: oldTarget.sent,
        author: oldTarget.author
    )
    #expect(model.openQuotedMessage(oldQuote, from: 913) == nil)
    #expect(model.conversationAnchorMessageID == oldTarget.id)

    // When: the anchored event load starts, then a newer loaded quote wins.
    let eventRefresh = Task { await model.refreshAfterEvent() }
    await probe.waitUntilFirstRequested()
    let newerQuote = NativeQuote(
        messageID: newerTarget.id,
        text: newerTarget.text,
        sent: newerTarget.sent,
        author: newerTarget.author
    )
    #expect(model.openQuotedMessage(newerQuote, from: 914) == nil)
    #expect(model.conversationAnchorMessageID == newerTarget.id)
    await probe.releaseFirstRequest()
    await eventRefresh.value

    // Then: the failed old load cannot trigger a latest-page fallback over the newer intent.
    #expect(await probe.recordedRequests() == [oldTarget.id])
    #expect(model.messages == originalMessages)
    #expect(model.conversationAnchorMessageID == newerTarget.id)
    #expect(model.targetMessageID == newerTarget.id)
    #expect(model.quoteNavigationError == nil)
}

@MainActor
@Test func liveEventRefreshPreservesTheActiveQuoteNavigationAnchor() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let quote = NativeQuote(
        messageID: target.id,
        text: target.text,
        sent: target.sent,
        author: target.author
    )
    let probe = ConversationRefreshProbe(messages: [target])
    let model = makeLiveRefreshModel(probe: probe)

    // When: the user opens an offscreen quote, then a live event refreshes the transcript.
    let navigation = try #require(model.openQuotedMessage(quote, from: 912))
    await navigation.value
    await model.refreshAfterEvent()

    // Then: both loads stay anchored to the user-selected original message.
    let requests = await probe.recordedRequests()
    #expect(requests == [target.id, target.id])
    #expect(model.conversationAnchorMessageID == target.id)
    #expect(model.messages == [target])
    #expect(model.quoteNavigationError == nil)
    #expect(model.phase == .ready)
}

@MainActor
@Test func liveEventRefreshCannotSupersedeAnInFlightQuoteNavigation() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let quote = NativeQuote(
        messageID: target.id,
        text: target.text,
        sent: target.sent,
        author: target.author
    )
    let probe = ConversationRefreshProbe(messages: [target], delayFirstRequest: true)
    let model = makeLiveRefreshModel(probe: probe)

    // When: a live event arrives while the user-requested page is still loading.
    let navigation = try #require(model.openQuotedMessage(quote, from: 913))
    await probe.waitUntilFirstRequested()
    #expect(model.isLoadingConversation)
    await model.refreshAfterEvent()

    // Then: the event updates the chat list without starting a competing transcript load.
    var requests = await probe.recordedRequests()
    #expect(requests == [target.id])

    // When: the original navigation finishes.
    await probe.releaseFirstRequest()
    await navigation.value

    // Then
    requests = await probe.recordedRequests()
    #expect(requests == [target.id])
    #expect(model.conversationAnchorMessageID == target.id)
    #expect(model.messages == [target])
    #expect(!model.isLoadingConversation)
    #expect(model.quoteNavigationError == nil)
}

@MainActor
@Test func liveEventRefreshFallsBackWhenTheAnchoredMessageDisappears() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let latestMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let probe = FallbackRefreshProbe(latestMessages: latestMessages)
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { _, aroundMessageID in
            try await probe.load(around: aroundMessageID)
        },
        loadChatsOperation: { _ in NativePreviewData.chats }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = [target]
    let quote = NativeQuote(
        messageID: target.id,
        text: target.text,
        sent: target.sent,
        author: target.author
    )
    #expect(model.openQuotedMessage(quote, from: 914) == nil)
    #expect(model.conversationAnchorMessageID == target.id)

    // When
    await model.refreshAfterEvent()

    // Then: the missing anchor is dropped and the newest transcript is loaded.
    let requests = await probe.recordedRequests()
    #expect(requests == [target.id, nil])
    #expect(model.conversationAnchorMessageID == nil)
    #expect(model.messages == latestMessages)
    #expect(model.quoteNavigationError == nil)
    #expect(model.phase == .ready)
}

@MainActor
@Test func jumpToLatestLeavesQuotedHistoryAndScrollsToTheNewestMessage() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let latestMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let probe = LatestNavigationProbe(latestMessages: latestMessages)
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, aroundMessageID in
            await probe.load(around: aroundMessageID)
        }
    )
    model.messages = [target]
    let quote = NativeQuote(
        messageID: target.id,
        text: target.text,
        sent: target.sent,
        author: target.author
    )
    #expect(model.openQuotedMessage(quote, from: 915) == nil)
    #expect(model.isViewingConversationHistory)

    // When
    let navigation = try #require(model.jumpToLatest())
    await navigation.value

    // Then
    let requests = await probe.recordedRequests()
    #expect(requests == [nil])
    #expect(!model.isViewingConversationHistory)
    #expect(model.conversationAnchorMessageID == nil)
    #expect(model.messages == latestMessages)
    #expect(model.targetMessageID == latestMessages.last?.id)
    #expect(model.phase == .ready)
}

@MainActor
@Test func failedJumpToLatestKeepsTheCurrentQuotedHistoryPage() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, _ in
            throw NativeChatError.unavailable("The newest messages could not be loaded.")
        }
    )
    model.messages = [target]
    let quote = NativeQuote(
        messageID: target.id,
        text: target.text,
        sent: target.sent,
        author: target.author
    )
    #expect(model.openQuotedMessage(quote, from: 916) == nil)

    // When
    let navigation = try #require(model.jumpToLatest())
    await navigation.value

    // Then
    #expect(model.isViewingConversationHistory)
    #expect(model.conversationAnchorMessageID == target.id)
    #expect(model.messages == [target])
    #expect(model.phase == .failed("The newest messages could not be loaded."))
}

@MainActor
@Test func switchingChatsClearsStaleMessagesBeforeTheNewTranscriptLoads() async throws {
    // Given
    let oldMessages = NativePreviewData.messages(for: "@1")
    let newMessages = NativePreviewData.messages(for: "#2")
    let probe = DelayedValue(newMessages)
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "#2")
            #expect(aroundMessageID == nil)
            return await probe.load()
        }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = oldMessages
    let staleReplyTarget = try #require(oldMessages.first)

    // When: the new conversation has not loaded yet.
    model.selectChat("#2")

    // Then: no message from the previous conversation remains interactive.
    #expect(model.selectedChatID == "#2")
    #expect(model.messages.isEmpty)
    #expect(!model.canReply(to: staleReplyTarget))
    model.beginReply(to: staleReplyTarget)
    #expect(model.replyingTo == nil)

    // When: the new transcript arrives.
    await probe.waitUntilRequested()
    await probe.release()
    for _ in 0..<1_000 {
        if model.messages == newMessages { break }
        await Task.yield()
    }

    // Then
    #expect(model.messages == newMessages)
    #expect(model.selectedChatID == "#2")
}

@MainActor
@Test func offscreenQuoteFailureCannotLeakIntoANewConversation() async throws {
    // Given
    let probe = DelayedConversationLoadFailure()
    let quote = NativeQuote(messageID: 904, text: "Old conversation", sent: false, author: "Maya")
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, _ in try await probe.load() }
    )

    // When: quote loading begins, then the user switches chats before it fails.
    let navigation = try #require(model.openQuotedMessage(quote, from: 905))
    await probe.waitUntilRequested()
    model.selectChat("#2")
    await probe.release()
    await navigation.value

    // Then: the stale failure is ignored in the newly selected conversation.
    #expect(model.selectedChatID == "#2")
    #expect(model.quoteNavigationError == nil)
    #expect(model.phase == .ready)
}

@MainActor
@Test func newerKnownQuoteCancelsOlderUnresolvedQuoteNavigation() async throws {
    // Given
    let model = AppModel(previewMode: true)
    let newerTarget = try #require(model.messages.first)
    let olderTarget = NativeMessage(
        id: 906,
        text: "Older target",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .text
    )
    let olderContainingMessage = NativeMessage(
        id: 907,
        text: "Reply with unresolved metadata",
        timestamp: nil,
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: NativeQuote(
            messageID: olderTarget.id,
            text: olderTarget.text,
            sent: olderTarget.sent,
            author: olderTarget.author
        )
    )
    let probe = DelayedValue<NativeMessage?>(olderContainingMessage)
    let testModel = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in await probe.load() }
    )
    let unresolvedQuote = NativeQuote(messageID: nil, text: olderTarget.text, sent: false, author: "Maya")

    // When: the older metadata reload starts, then a newer loaded quote is opened.
    let olderNavigation = try #require(testModel.openQuotedMessage(unresolvedQuote, from: olderContainingMessage.id))
    await probe.waitUntilRequested()
    let newerQuote = NativeQuote(
        messageID: newerTarget.id,
        text: newerTarget.text,
        sent: newerTarget.sent,
        author: newerTarget.author
    )
    #expect(testModel.openQuotedMessage(newerQuote, from: 908) == nil)
    await probe.release()
    await olderNavigation.value

    // Then: the late older result cannot replace the newest scroll target.
    #expect(testModel.targetMessageID == newerTarget.id)
    #expect(testModel.quoteNavigationError == nil)
}

@MainActor
@Test func loadedQuoteCancelsOlderOffscreenPageLoad() async throws {
    // Given
    let offscreenTarget = NativeMessage(
        id: 909,
        text: "Offscreen target",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .text
    )
    let probe = DelayedValue([offscreenTarget])
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { _, _ in await probe.load() }
    )
    let originalMessages = model.messages
    let newerTarget = try #require(originalMessages.first)
    let offscreenQuote = NativeQuote(
        messageID: offscreenTarget.id,
        text: offscreenTarget.text,
        sent: offscreenTarget.sent,
        author: offscreenTarget.author
    )

    // When: an offscreen load starts, then a loaded quote is chosen.
    let olderNavigation = try #require(model.openQuotedMessage(offscreenQuote, from: 910))
    await probe.waitUntilRequested()
    let newerQuote = NativeQuote(
        messageID: newerTarget.id,
        text: newerTarget.text,
        sent: newerTarget.sent,
        author: newerTarget.author
    )
    model.draft = "Ready after cancellation"
    #expect(model.openQuotedMessage(newerQuote, from: 911) == nil)

    // Then: the obsolete loader no longer keeps the winning transcript artificially busy.
    #expect(!model.isLoadingConversation)
    #expect(model.canSendDraft)
    #expect(model.canRefreshConversation)

    // When: the cancelled operation eventually cooperates and returns.
    await probe.release()
    await olderNavigation.value

    // Then: the cancelled page cannot replace the transcript or newest target.
    #expect(model.messages == originalMessages)
    #expect(model.targetMessageID == newerTarget.id)
    #expect(model.quoteNavigationError == nil)
}

private actor AttachmentOpenProbe {
    private var openedSource: NativeCryptoFile?

    func open(_ source: NativeCryptoFile) {
        openedSource = source
    }

    func source() -> NativeCryptoFile? {
        openedSource
    }
}

private actor DelayedAttachmentOpenFailure {
    private let message: String
    private var requested = false
    private var released = false

    init(_ message: String) {
        self.message = message
    }

    func open() async throws {
        requested = true
        while !released { await Task.yield() }
        throw NativeChatError.unavailable(message)
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }
}

@MainActor
private func writeTestJPEG(to url: URL) throws {
    let image = NSImage(size: NSSize(width: 32, height: 24))
    image.lockFocus()
    NSColor.systemBlue.setFill()
    NSRect(x: 0, y: 0, width: 32, height: 24).fill()
    image.unlockFocus()
    let tiff = try #require(image.tiffRepresentation)
    let bitmap = try #require(NSBitmapImageRep(data: tiff))
    let jpeg = try #require(bitmap.representation(using: .jpeg, properties: [:]))
    try jpeg.write(to: url)
}

@MainActor
@Test func attachmentOpenRefreshesMissingEncryptionMetadata() async throws {
    // Given
    let plainSource = NativeCryptoFile(filePath: "photo.jpg", cryptoArgs: nil)
    let encryptedSource = NativeCryptoFile(
        filePath: "photo.jpg",
        cryptoArgs: NativeCryptoFileArgs(fileKey: "key", fileNonce: "nonce")
    )
    let original = NativeMessage(
        id: 92,
        text: "Photo",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .image(preview: nil, fileName: "photo.jpg"),
        fileSource: plainSource
    )
    let refreshed = NativeMessage(
        id: original.id,
        text: original.text,
        timestamp: original.timestamp,
        sent: original.sent,
        author: original.author,
        deletable: original.deletable,
        content: original.content,
        fileSource: encryptedSource
    )
    let probe = AttachmentOpenProbe()
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { chatID, itemID in
            #expect(chatID == "@1")
            #expect(itemID == original.id)
            return refreshed
        },
        openAttachmentOperation: { source, _ in
            await probe.open(source)
        }
    )
    model.messages = [original]

    // When
    let opening = try #require(model.openAttachment(original))
    await opening.value

    // Then
    #expect(await probe.source() == encryptedSource)
    #expect(model.messages.first?.fileSource == encryptedSource)
    #expect(model.attachmentOpenError == nil)
    #expect(!model.isOpeningAttachment(original.id))
}

@MainActor
@Test func attachmentOpeningStateAndFailuresStayWithTheirConversation() async throws {
    // Given: an attachment is still opening when the user moves to another chat
    // whose transcript happens to reuse the same message ID.
    let failure = "The stored photo could not be decrypted."
    let probe = DelayedAttachmentOpenFailure(failure)
    let source = NativeCryptoFile(filePath: "/tmp/encrypted-photo.jpg", cryptoArgs: nil)
    let first = NativeMessage(
        id: 92,
        text: "First photo",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .image(preview: nil, fileName: "first.jpg"),
        fileSource: source
    )
    let second = NativeMessage(
        id: first.id,
        text: "Different photo",
        timestamp: nil,
        sent: false,
        author: "Jordan",
        deletable: true,
        content: .image(preview: nil, fileName: "second.jpg"),
        fileSource: source
    )
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { chatID, _ in chatID == "@1" ? first : second },
        openAttachmentOperation: { _, fileName in
            if fileName == "first.jpg" { try await probe.open() }
        }
    )
    model.messages = [first]

    // When: the first chat's open remains in flight and the second chat appears.
    let opening = try #require(model.openAttachment(first))
    await probe.waitUntilRequested()
    #expect(model.isOpeningAttachment(first.id))
    model.selectChat("#2")
    model.messages = [second]

    // Then: the colliding message ID in the new chat is not shown as busy.
    #expect(!model.isOpeningAttachment(second.id))
    #expect(model.attachmentOpenError == nil)
    let secondOpening = try #require(model.openAttachment(second))
    await secondOpening.value
    #expect(!model.isOpeningAttachment(second.id))
    #expect(model.attachmentOpenError == nil)

    // When: the old operation fails after the transition.
    await probe.release()
    await opening.value

    // Then: its error waits for its originating conversation.
    #expect(model.selectedChatID == "#2")
    #expect(model.attachmentOpenError == nil)
    model.selectChat("@1")
    #expect(model.attachmentOpenError == failure)
    #expect(!model.isOpeningAttachment(first.id))
}

@MainActor
@Test func openingAnImagePresentsTheOriginalFileInQuickLook() async throws {
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: directory) }
    let sourceURL = directory.appendingPathComponent("full-size.jpg")
    try writeTestJPEG(to: sourceURL)

    let source = NativeCryptoFile(filePath: sourceURL.path, cryptoArgs: nil)
    let message = NativeMessage(
        id: 93,
        text: "Original photo",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .image(preview: "compressed-thumbnail", fileName: sourceURL.lastPathComponent),
        fileSource: source
    )
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in message }
    )
    model.messages = [message]

    let opening = try #require(model.openAttachment(message))
    await opening.value

    #expect(model.quickLookURL?.standardizedFileURL == sourceURL.standardizedFileURL)
    #expect(model.attachmentOpenError == nil)
    #expect(!model.isOpeningAttachment(message.id))
}

@MainActor
@Test func delayedImageOpenCannotAppearInAnotherConversation() async throws {
    let directory = FileManager.default.temporaryDirectory
        .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    defer { try? FileManager.default.removeItem(at: directory) }
    let sourceURL = directory.appendingPathComponent("delayed-full-size.jpg")
    try writeTestJPEG(to: sourceURL)

    let source = NativeCryptoFile(filePath: sourceURL.path, cryptoArgs: nil)
    let message = NativeMessage(
        id: 94,
        text: "Slow original photo",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .image(preview: "compressed-thumbnail", fileName: sourceURL.lastPathComponent),
        fileSource: source
    )
    let probe = DelayedValue<NativeMessage?>(message)
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in await probe.load() }
    )
    model.messages = [message]

    let opening = try #require(model.openAttachment(message))
    await probe.waitUntilRequested()
    model.selectChat("#2")
    await probe.release()
    await opening.value

    #expect(model.selectedChatID == "#2")
    #expect(model.quickLookURL == nil)
    #expect(model.attachmentOpenError == nil)
}

@MainActor
@Test func replyComposerSendsAndClearsQuotedContext() throws {
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)

    model.beginReply(to: original)
    model.draft = "This is a reply"
    model.sendDraft()

    let sent = try #require(model.messages.last)
    #expect(sent.text == "This is a reply")
    #expect(sent.quotedItem?.messageID == original.id)
    #expect(sent.quotedItem?.text == original.text)
    #expect(model.replyingTo == nil)
    #expect(model.draft.isEmpty)
}

@MainActor
@Test func previewMediaReplyKeepsItsQuoteVisual() throws {
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first { $0.content.replyContextVisual != nil })

    model.beginReply(to: original)
    model.draft = "Replying to the photo"
    model.sendDraft()

    let sent = try #require(model.messages.last)
    #expect(sent.quotedItem?.messageID == original.id)
    #expect(sent.quotedItem?.text == original.replyPreview)
    #expect(sent.quotedItem?.visual == original.content.replyContextVisual)
}

@MainActor
@Test func selectedMessageReplyCanReturnToItsSource() throws {
    // Given: a transcript message is selected through the same model path as the UI.
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)
    model.transcriptFocused = true
    model.selectMessage(original.id, modifiers: [])
    #expect(model.canReplyToSelectedMessage)

    // When: Reply is invoked, composed, and sent.
    #expect(model.replyToSelectedMessage())
    #expect(model.replyingTo?.id == original.id)
    #expect(model.selectedMessageIDs.isEmpty)
    model.draft = "Reply from the selected message"
    model.sendDraft()

    // Then: the new quote preserves the source identity and can navigate back to it.
    let sent = try #require(model.messages.last)
    let quote = try #require(sent.quotedItem)
    #expect(quote.messageID == original.id)
    #expect(quote.text == original.text)
    #expect(model.replyingTo == nil)
    #expect(model.draft.isEmpty)

    let navigation = model.openQuotedMessage(quote, from: sent.id)
    #expect(navigation == nil)
    #expect(model.targetMessageID == original.id)
    #expect(model.conversationAnchorMessageID == original.id)
    #expect(model.isViewingConversationHistory)
}

@MainActor
@Test func replyingFromConversationSearchConsumesTheSelectedResult() throws {
    // Given
    let model = AppModel(previewMode: true)
    let source = try #require(model.messages.first(where: { $0.text.contains("photos") }))
    model.beginConversationSearch()
    model.conversationSearchText = "photos"
    model.updateConversationSearchSelection()
    let previousFocusRequest = model.composerFocusRequest
    #expect(model.selectedMessageIDs == [source.id])

    // When
    #expect(model.replyToSelectedMessage())

    // Then
    #expect(model.replyingTo?.id == source.id)
    #expect(!model.conversationSearchPresented)
    #expect(model.conversationSearchText.isEmpty)
    #expect(model.selectedMessageIDs.isEmpty)
    #expect(model.composerFocusRequest == previousFocusRequest + 1)
}

@MainActor
@Test func replyControlsRejectUnavailableItems() throws {
    // Given
    let source = try #require(NativePreviewData.messages(for: "@1").first)
    let unavailable = NativeMessage(
        id: source.id,
        text: source.text,
        timestamp: source.timestamp,
        sent: source.sent,
        author: source.author,
        deletable: source.deletable,
        content: source.content,
        replyable: false
    )
    let model = AppModel(previewMode: true)
    model.messages = [unavailable]
    model.selectMessage(unavailable.id, modifiers: [])

    // When
    #expect(!model.replyToSelectedMessage())

    // Then
    #expect(!model.canReplyToSelectedMessage)
    #expect(model.replyingTo == nil)
    #expect(model.selectedMessageIDs == [unavailable.id])
}

@MainActor
@Test func replyActionUsesTheCurrentTranscriptItemInsteadOfAStaleRow() throws {
    // Given
    let original = try #require(NativePreviewData.messages(for: "@1").first)
    let unavailable = NativeMessage(
        id: original.id,
        text: "Message deleted",
        timestamp: original.timestamp,
        sent: original.sent,
        author: original.author,
        deletable: false,
        content: original.content,
        replyable: false
    )
    let model = AppModel(previewMode: true)
    model.messages = [unavailable]

    // When: a row action captured before the refresh invokes Reply afterward.
    model.beginReply(to: original)

    // Then
    #expect(model.replyingTo == nil)
    #expect(model.composerFocusRequest == 0)
}

@MainActor
@Test func liveRefreshUpdatesOrInvalidatesReplyContextWithoutLosingDraft() throws {
    // Given
    let original = try #require(NativePreviewData.messages(for: "@1").first)
    let edited = NativeMessage(
        id: original.id,
        text: "Edited source message",
        timestamp: original.timestamp,
        sent: original.sent,
        author: original.author,
        deletable: original.deletable,
        content: original.content
    )
    let unavailable = NativeMessage(
        id: edited.id,
        text: "Message deleted",
        timestamp: edited.timestamp,
        sent: edited.sent,
        author: edited.author,
        deletable: false,
        content: edited.content,
        replyable: false
    )
    let model = AppModel(previewMode: true)
    model.messages = [original]
    model.draft = "Keep this draft"
    model.beginReply(to: original)

    // When: the quoted source is edited by a live refresh.
    model.applyLoadedMessages([edited], to: "@1")

    // Then: the composer shows the current source text.
    #expect(model.replyingTo?.text == "Edited source message")
    #expect(model.replyContextError == nil)

    // When: a later refresh marks the source unavailable.
    model.applyLoadedMessages([unavailable], to: "@1")

    // Then: only the invalid quote is removed; the user's work survives.
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this draft")
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")

    // When: a notification moves to another chat before the warning is acknowledged.
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "#2",
        messageID: nil
    ))

    // Then: the warning follows its draft instead of leaking or disappearing.
    #expect(model.selectedChatID == "#2")
    #expect(model.replyContextError == nil)
    model.selectChat("@1")
    #expect(model.draft == "Keep this draft")
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")
}

@MainActor
@Test func selectionBarActionsWorkAfterTranscriptLosesFocus() throws {
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)

    model.selectMessage(original.id, modifiers: [])
    model.transcriptFocused = false
    model.replyToSelectedMessage()

    #expect(model.replyingTo?.id == original.id)
    #expect(model.selectedMessageIDs.isEmpty)

    model.cancelReply()
    model.selectMessage(original.id, modifiers: [])
    model.transcriptFocused = false
    model.requestDeleteSelectedMessages()

    #expect(model.showingDeleteConfirmation)
}

@MainActor
@Test func cancellingReplyPreservesTheDraftAndAttachments() throws {
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)
    let attachment = PendingAttachment(
        id: UUID(),
        url: URL(fileURLWithPath: "/tmp/reply-photo.jpg"),
        fileName: "reply-photo.jpg",
        kind: .image,
        byteCount: 10,
        previewImage: nil
    )
    model.draft = "Keep this draft"
    model.pendingAttachments = [attachment]
    model.beginReply(to: original)

    model.cancelReply()

    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this draft")
    #expect(model.pendingAttachments == [attachment])
}

@MainActor
@Test func invalidQuoteRaceKeepsTheDraftAndAttachmentsButRetiresTheReply() async throws {
    // Given
    let attachment = PendingAttachment(
        id: UUID(),
        url: URL(fileURLWithPath: "/tmp/reply-race-photo.jpg"),
        fileName: "reply-race-photo.jpg",
        kind: .image,
        byteCount: 10,
        previewImage: nil
    )
    let model = makeSendTestModel(
        sendAttachmentOperation: { _, _, _, _ in
            throw NativeChatError.replyTargetUnavailable
        }
    )
    let original = try #require(model.messages.first)
    model.draft = "Keep this caption"
    model.pendingAttachments = [attachment]
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let task = try #require(model.sendTask)
    await task.value

    // Then
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this caption")
    #expect(model.pendingAttachments == [attachment])
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")
    #expect(model.phase == .ready)
    #expect(!model.isSending)
}

@MainActor
@Test func escapeDismissesSearchBeforeReplyContext() throws {
    // Given
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)
    model.draft = "Keep this draft"
    model.beginReply(to: original)
    model.sidebarSearchPresented = true
    model.searchText = "maya"

    // When: sidebar search is the nearest temporary state.
    #expect(model.canDismissNearestState)
    model.dismissNearestState()

    // Then: only search closes; the reply and draft survive.
    #expect(!model.sidebarSearchPresented)
    #expect(model.searchText.isEmpty)
    #expect(model.replyingTo?.id == original.id)
    #expect(model.draft == "Keep this draft")
    #expect(model.canDismissNearestState)

    // When: Escape is invoked again.
    model.dismissNearestState()

    // Then: the reply context closes without discarding the draft.
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep this draft")
    #expect(!model.canDismissNearestState)
}

private actor AttachmentSendProbe {
    struct Request: Sendable {
        let attachmentID: PendingAttachment.ID
        let caption: String
        let quotedItemID: Int64?
    }

    private let failingRequest: Int?
    private var requests: [Request] = []

    init(failingRequest: Int? = nil) {
        self.failingRequest = failingRequest
    }

    func send(
        _ attachment: PendingAttachment,
        caption: String,
        quotedItemID: Int64?
    ) throws -> NativeSendReceipt {
        requests.append(Request(
            attachmentID: attachment.id,
            caption: caption,
            quotedItemID: quotedItemID
        ))
        if requests.count == failingRequest {
            throw NativeChatError.unavailable("The attachment could not be sent.")
        }
        let committedMessage = NativeMessage(
            id: 20_000 + Int64(requests.count),
            text: caption,
            timestamp: nil,
            sent: true,
            author: nil,
            deletable: true,
            content: .image(preview: nil, fileName: attachment.fileName),
            quotedItem: quotedItemID.map {
                NativeQuote(messageID: $0, text: "Original message", sent: false, author: "Maya")
            }
        )
        return NativeSendReceipt(
            committedMessages: [committedMessage],
            replyContextConfirmed: quotedItemID == nil || committedMessage.quotedItem?.messageID == quotedItemID
        )
    }

    func recordedRequests() -> [Request] {
        requests
    }
}

private actor ReplyValidationProbe {
    private var requested = false
    private var released = false
    private var sendCount = 0

    func validate() async -> NativeMessage? {
        requested = true
        while !released { await Task.yield() }
        return nil
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }

    func recordSend() -> NativeSendReceipt {
        sendCount += 1
        return .confirmed
    }

    func recordedSendCount() -> Int {
        sendCount
    }
}

private actor DelayedTextSendProbe {
    private let failureMessage: String?
    private let cancelAfterSuccess: Bool
    private var requested = false
    private var released = false

    init(failureMessage: String? = nil, cancelAfterSuccess: Bool = false) {
        self.failureMessage = failureMessage
        self.cancelAfterSuccess = cancelAfterSuccess
    }

    func send() async throws -> NativeSendReceipt {
        requested = true
        while !released { await Task.yield() }
        if let failureMessage {
            throw NativeChatError.unavailable(failureMessage)
        }
        if cancelAfterSuccess {
            withUnsafeCurrentTask { $0?.cancel() }
        }
        return .confirmed
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }
}

private actor DelayedPostSendRefreshFailure {
    private let failureMessage: String
    private let recoveredMessages: [NativeMessage]
    private var requestCount = 0
    private var firstRequestReleased = false

    init(failureMessage: String, recoveredMessages: [NativeMessage]) {
        self.failureMessage = failureMessage
        self.recoveredMessages = recoveredMessages
    }

    func load() async throws -> [NativeMessage] {
        requestCount += 1
        guard requestCount == 1 else { return recoveredMessages }
        while !firstRequestReleased { await Task.yield() }
        throw NativeChatError.unavailable(failureMessage)
    }

    func waitUntilRequested() async {
        while requestCount == 0 { await Task.yield() }
    }

    func release() {
        firstRequestReleased = true
    }
}

@MainActor
private func makeSendTestModel(
    sendTextOperation: SendTextOperation? = nil,
    sendAttachmentOperation: SendAttachmentOperation? = nil,
    loadMessageOperation: LoadMessageOperation? = nil,
    loadMessagesOperation: LoadMessagesOperation? = nil,
    loadChatsOperation: LoadChatsOperation? = nil
) -> AppModel {
    let model = AppModel(
        previewMode: true,
        sendTextOperation: sendTextOperation,
        sendAttachmentOperation: sendAttachmentOperation,
        loadMessageOperation: loadMessageOperation,
        loadMessagesOperation: loadMessagesOperation,
        loadChatsOperation: loadChatsOperation
    )
    return model
}

@MainActor
private func unavailableVersion(of message: NativeMessage) -> NativeMessage {
    NativeMessage(
        id: message.id,
        text: "Message deleted",
        timestamp: message.timestamp,
        sent: message.sent,
        author: message.author,
        deletable: false,
        content: message.content,
        replyable: false
    )
}

@MainActor
@Test func failedSendClearsAReplyTargetDeletedWhileTheSendWasInFlight() async throws {
    // Given
    let failure = "The reply could not be sent."
    let original = NativePreviewData.messages(for: "@1")[0]
    let probe = DelayedTextSendProbe(failureMessage: failure)
    let model = makeSendTestModel(
        sendTextOperation: { _, _, _ in try await probe.send() },
        loadMessageOperation: { _, _ in original }
    )
    model.messages = [original]
    model.draft = "Keep this reply text"
    model.beginReply(to: original)

    // When: the source is deleted after validation but before the send fails.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()
    model.applyLoadedMessages([unavailableVersion(of: original)], to: "@1")

    // Then: the in-flight composer remains stable until the operation resolves.
    #expect(model.replyingTo?.id == original.id)
    #expect(model.replyContextError == nil)
    #expect(model.isSendingSelectedChat)

    // When
    await probe.release()
    await send.value

    // Then: the text is restored without retaining an invalid quote.
    #expect(model.draft == "Keep this reply text")
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")
    #expect(model.phase == .failed(failure))
    #expect(!model.isSending)
}

@MainActor
@Test func committedReplyDoesNotReportADeferredTargetInvalidation() async throws {
    // Given
    let original = NativePreviewData.messages(for: "@1")[0]
    let probe = DelayedTextSendProbe(cancelAfterSuccess: true)
    let model = makeSendTestModel(
        sendTextOperation: { _, _, _ in try await probe.send() },
        loadMessageOperation: { _, _ in original }
    )
    model.messages = [original]
    model.draft = "This reply commits"
    model.beginReply(to: original)

    // When: the source is deleted while the core is committing the reply.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()
    model.applyLoadedMessages([unavailableVersion(of: original)], to: "@1")
    await probe.release()
    await send.value

    // Then: the committed quote stays sent and no stale composer warning appears.
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == nil)
    #expect(model.phase == .ready)
    #expect(!model.isSending)
}

@MainActor
@Test func committedMessageWithoutTheRequestedQuoteIsNotRetried() async throws {
    // Given
    let original = NativePreviewData.messages(for: "@1")[0]
    let model = makeSendTestModel(sendTextOperation: { _, quotedItemID, _ in
        #expect(quotedItemID == original.id)
        withUnsafeCurrentTask { $0?.cancel() }
        return NativeSendReceipt(committedMessages: [], replyContextConfirmed: false)
    })
    model.messages = [original]
    model.draft = "Send this once"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await send.value

    // Then: the committed message stays committed, while the UI reports the missing link.
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == "Your message was sent, but SimpleX could not link it to the original message.")
    #expect(model.phase == .ready)
    #expect(!model.isSending)
}

@MainActor
@Test func committedReplyIsNotRestoredWhenOnlyTheRefreshFails() async throws {
    // Given
    let refreshFailure = "The newest messages are temporarily unavailable."
    let original = NativePreviewData.messages(for: "@1")[0]
    let racingEventCopy = NativeMessage(
        id: 9_900,
        text: "Sending reply",
        timestamp: Date(timeIntervalSince1970: 1),
        sent: true,
        author: nil,
        deletable: true,
        content: .text
    )
    let committedReply = NativeMessage(
        id: racingEventCopy.id,
        text: "Committed reply",
        timestamp: Date(timeIntervalSince1970: 2),
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: NativeQuote(
            messageID: original.id,
            text: original.replyPreview,
            sent: original.sent,
            author: original.author
        )
    )
    let model = makeSendTestModel(
        sendTextOperation: { _, quotedItemID, _ in
            #expect(quotedItemID == original.id)
            return NativeSendReceipt(
                committedMessages: [committedReply],
                replyContextConfirmed: true
            )
        },
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == nil)
            throw NativeChatError.unavailable(refreshFailure)
        }
    )
    model.messages = [original, racingEventCopy]
    model.draft = "Send this once"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await send.value

    // Then: the committed message is not restored for a duplicate retry.
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(model.messages == [original, committedReply])
    #expect(model.targetMessageID == committedReply.id)
    #expect(model.phase == .ready)
    #expect(model.sendStatusMessage ==
        "Your message was sent, but the conversation could not refresh. Use Refresh to load it. \(refreshFailure)"
    )
    #expect(!model.isSending)

    // When: the user follows a notification before acknowledging the status.
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "#2",
        messageID: nil
    ))

    // Then: the status is retained only for the conversation that sent the reply.
    #expect(model.selectedChatID == "#2")
    #expect(model.sendStatusMessage == nil)
    model.selectChat("@1")
    #expect(model.sendStatusMessage ==
        "Your message was sent, but the conversation could not refresh. Use Refresh to load it. \(refreshFailure)"
    )
}

@MainActor
@Test func postSendRefreshNoticeWaitsForItsOriginatingConversation() async throws {
    // Given
    let refreshFailure = "The newest messages are temporarily unavailable."
    let original = NativePreviewData.messages(for: "@1")[0]
    let committedReply = NativeMessage(
        id: 9_901,
        text: "Committed before switching chats",
        timestamp: Date(timeIntervalSince1970: 2),
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: NativeQuote(
            messageID: original.id,
            text: original.replyPreview,
            sent: original.sent,
            author: original.author
        )
    )
    let refreshProbe = DelayedPostSendRefreshFailure(
        failureMessage: refreshFailure,
        recoveredMessages: [original, committedReply]
    )
    let model = makeSendTestModel(
        sendTextOperation: { _, quotedItemID, _ in
            #expect(quotedItemID == original.id)
            return NativeSendReceipt(
                committedMessages: [committedReply],
                replyContextConfirmed: true
            )
        },
        loadMessagesOperation: { chatID, _ in
            if chatID == "@1" { return try await refreshProbe.load() }
            return NativePreviewData.messages(for: chatID)
        }
    )
    model.messages = [original]
    model.draft = "Send this once"
    model.beginReply(to: original)

    // When: the reply commits, then the user changes chats before refresh fails.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await refreshProbe.waitUntilRequested()
    model.selectChat("#2")
    await refreshProbe.release()
    await send.value

    // Then: no stale notice interrupts the new conversation.
    #expect(model.selectedChatID == "#2")
    #expect(model.phase == .ready)
    #expect(model.sendStatusMessage == nil)

    // When: the user returns to the conversation that sent the reply.
    model.selectChat("@1")

    // Then: its queued nonfatal status appears there, and only there.
    #expect(model.phase == .ready)
    #expect(model.sendStatusMessage ==
        "Your message was sent, but the conversation could not refresh. Use Refresh to load it. \(refreshFailure)"
    )
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
}

@MainActor
@Test func transientReplyInvalidationCanRecoverBeforeAFailedSendReturns() async throws {
    // Given
    let failure = "The reply could not be sent."
    let original = NativePreviewData.messages(for: "@1")[0]
    let recovered = NativeMessage(
        id: original.id,
        text: "Edited but available again",
        timestamp: original.timestamp,
        sent: original.sent,
        author: original.author,
        deletable: original.deletable,
        content: original.content
    )
    let probe = DelayedTextSendProbe(failureMessage: failure)
    let model = makeSendTestModel(
        sendTextOperation: { _, _, _ in try await probe.send() },
        loadMessageOperation: { _, _ in original }
    )
    model.messages = [original]
    model.draft = "Retry against the recovered target"
    model.beginReply(to: original)

    // When: a transient unavailable state is followed by a valid refresh.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()
    model.applyLoadedMessages([unavailableVersion(of: original)], to: "@1")
    model.applyLoadedMessages([recovered], to: "@1")
    await probe.release()
    await send.value

    // Then: the failed send restores a valid, current reply context.
    #expect(model.draft == "Retry against the recovered target")
    #expect(model.replyingTo == recovered)
    #expect(model.replyContextError == nil)
    #expect(model.phase == .failed(failure))
    #expect(!model.isSending)
}

@MainActor
@Test func deletedReplyTargetIsRecheckedBeforeSendingAndTheDraftSurvivesAChatSwitch() async throws {
    // Given
    let probe = ReplyValidationProbe()
    let model = makeSendTestModel(
        sendTextOperation: { _, _, _ in await probe.recordSend() },
        loadMessageOperation: { _, _ in await probe.validate() }
    )
    let original = try #require(model.messages.first)
    model.draft = "Keep this reply"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()
    model.selectChat("#2")
    await probe.release()
    await send.value

    // Then: nothing is sent or leaked into the currently visible conversation.
    let sendCount = await probe.recordedSendCount()
    #expect(sendCount == 0)
    #expect(model.selectedChatID == "#2")
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == nil)
    #expect(model.phase == .ready)

    // And: returning to the originating chat restores the draft and explains the cancelled quote.
    model.selectChat("@1")
    #expect(model.draft == "Keep this reply")
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == "The message you were replying to is no longer available. Your draft was kept.")
}

@MainActor
@Test func partialAttachmentFailureKeepsOnlyUnsentItemsAndDoesNotRepeatTheQuote() async throws {
    // Given
    let probe = AttachmentSendProbe(failingRequest: 2)
    let model = makeSendTestModel(sendAttachmentOperation: { attachment, caption, quotedItemID, _ in
        try await probe.send(attachment, caption: caption, quotedItemID: quotedItemID)
    })
    let original = try #require(model.messages.first)
    let initialMessageCount = model.messages.count
    let attachments = ["one.jpg", "two.jpg"].map {
        PendingAttachment(
            id: UUID(),
            url: URL(fileURLWithPath: "/tmp/\($0)"),
            fileName: $0,
            kind: .image,
            byteCount: 10,
            previewImage: nil
        )
    }
    model.pendingAttachments = attachments
    model.draft = "Batch caption"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await send.value

    // Then
    let requests = await probe.recordedRequests()
    #expect(requests.map(\.attachmentID) == attachments.map(\.id))
    #expect(requests.map(\.quotedItemID) == [original.id, nil])
    #expect(requests.map(\.caption) == ["", "Batch caption"])
    #expect(model.pendingAttachments == [attachments[1]])
    #expect(model.draft == "Batch caption")
    #expect(model.replyingTo == nil)
    #expect(model.messages.count == initialMessageCount + 1)
    #expect(model.messages.last?.content == .image(preview: nil, fileName: "one.jpg"))
    #expect(model.messages.last?.quotedItem?.messageID == original.id)
    #expect(!model.isSending)
}

@MainActor
@Test func cancellationAfterCoreSuccessCommitsTheSentAttachmentBeforeStopping() async throws {
    // Given
    let model = makeSendTestModel(sendAttachmentOperation: { _, _, _, _ in
        withUnsafeCurrentTask { $0?.cancel() }
        return .confirmed
    })
    let original = try #require(model.messages.first)
    let attachments = ["sent.jpg", "unsent.jpg"].map {
        PendingAttachment(
            id: UUID(),
            url: URL(fileURLWithPath: "/tmp/\($0)"),
            fileName: $0,
            kind: .image,
            byteCount: 10,
            previewImage: nil
        )
    }
    model.pendingAttachments = attachments
    model.draft = "Keep for the unsent item"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await send.value

    // Then
    #expect(model.pendingAttachments == [attachments[1]])
    #expect(model.draft == "Keep for the unsent item")
    #expect(model.replyingTo == nil)
    #expect(model.phase == .ready)
    #expect(!model.isSending)
}

@MainActor
@Test func committedAttachmentWithoutTheRequestedQuoteIsRemovedFromTheTray() async throws {
    // Given
    let attachment = PendingAttachment(
        id: UUID(),
        url: URL(fileURLWithPath: "/tmp/committed-photo.jpg"),
        fileName: "committed-photo.jpg",
        kind: .image,
        byteCount: 10,
        previewImage: nil
    )
    let model = makeSendTestModel(sendAttachmentOperation: { _, _, quotedItemID, _ in
        #expect(quotedItemID != nil)
        withUnsafeCurrentTask { $0?.cancel() }
        return NativeSendReceipt(committedMessages: [], replyContextConfirmed: false)
    })
    let original = try #require(model.messages.first)
    model.pendingAttachments = [attachment]
    model.draft = "Committed caption"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    await send.value

    // Then: neither the file nor its caption can be sent a second time.
    #expect(model.pendingAttachments.isEmpty)
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(model.replyContextError == "Your message was sent, but SimpleX could not link it to the original message.")
    #expect(model.phase == .ready)
    #expect(!model.isSending)
}

@MainActor
@Test func offscreenSendFailureRestoresItsComposerAndWaitsToShowTheError() async throws {
    // Given
    let failure = "The reply could not be sent."
    let model = makeSendTestModel(sendTextOperation: { _, _, _ in
        throw NativeChatError.unavailable(failure)
    })
    let original = try #require(model.messages.first)
    model.draft = "Retry this reply"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    model.selectChat("#2")
    await send.value

    // Then
    #expect(model.phase == .ready)
    #expect(model.draft.isEmpty)
    #expect(model.replyingTo == nil)
    #expect(!model.isSending)

    model.selectChat("@1")
    #expect(model.phase == .failed(failure))
    #expect(model.draft == "Retry this reply")
    #expect(model.replyingTo?.id == original.id)

    // When: a notification opens another chat before the send error is dismissed.
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "#2",
        messageID: nil
    ))

    // Then: the reply failure leaves with its draft and does not leak into the new chat.
    #expect(model.selectedChatID == "#2")
    #expect(model.phase == .ready)
    model.selectChat("@1")
    #expect(model.phase == .failed(failure))
    #expect(model.draft == "Retry this reply")
    #expect(model.replyingTo?.id == original.id)
}

@MainActor
@Test func globalFailureRemainsVisibleAcrossChatTransitions() {
    // Given
    let model = AppModel(previewMode: true)
    model.phase = .failed("The profile connection is unavailable.")

    // When
    model.selectChat("#2")

    // Then: failures without a chat owner stay global.
    #expect(model.phase == .failed("The profile connection is unavailable."))
}

@MainActor
@Test func inFlightSendLocksOnlyItsOriginatingComposer() async throws {
    // Given
    let model = makeSendTestModel(sendAttachmentOperation: { _, _, _, _ in
        try Task.checkCancellation()
        return .confirmed
    })
    let original = try #require(model.messages.first)
    let originAttachments = ["one.jpg", "two.jpg"].map {
        PendingAttachment(
            id: UUID(),
            url: URL(fileURLWithPath: "/tmp/\($0)"),
            fileName: $0,
            kind: .image,
            byteCount: 10,
            previewImage: nil
        )
    }
    model.pendingAttachments = originAttachments
    model.draft = "Origin caption"
    model.beginReply(to: original)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)

    // Then: the originating composer cannot discard in-flight state.
    #expect(model.isSendingSelectedChat)
    #expect(model.sendingChatID == "@1")
    model.cancelReply()
    model.removeAttachment(originAttachments[0].id)
    model.dismissNearestState()
    #expect(model.replyingTo?.id == original.id)
    #expect(model.pendingAttachments == originAttachments)

    // When: another conversation becomes active.
    model.selectChat("#2")
    let groupMessage = try #require(model.messages.first)
    let otherAttachment = PendingAttachment(
        id: UUID(),
        url: URL(fileURLWithPath: "/tmp/other.jpg"),
        fileName: "other.jpg",
        kind: .image,
        byteCount: 10,
        previewImage: nil
    )

    // Then: its reply and attachment controls remain available.
    #expect(!model.isSendingSelectedChat)
    model.beginReply(to: groupMessage)
    #expect(model.replyingTo?.id == groupMessage.id)
    model.pendingAttachments = [otherAttachment]
    model.removeAttachment(otherAttachment.id)
    #expect(model.pendingAttachments.isEmpty)

    // Cleanup and verify the originating composer survives cancellation.
    send.cancel()
    await send.value
    #expect(!model.isSending)
    #expect(model.sendingChatID == nil)

    model.selectChat("@1")
    #expect(model.draft == "Origin caption")
    #expect(model.pendingAttachments == originAttachments)
    #expect(model.replyingTo?.id == original.id)
}

@MainActor
@Test func inFlightSendBlocksTranscriptDeletionUntilItResolves() async throws {
    // Given
    let model = makeSendTestModel(sendTextOperation: { _, _, _ in
        try Task.checkCancellation()
        return .confirmed
    })
    let replyTarget = try #require(model.messages.first)
    let unrelatedMessage = try #require(model.messages.dropFirst().first)
    model.draft = "Reply in progress"
    model.beginReply(to: replyTarget)

    // When
    model.sendDraft()
    let send = try #require(model.sendTask)
    model.selectMessage(replyTarget.id, modifiers: [])
    model.requestDeleteSelectedMessages()

    // Then: deleting the quoted source cannot strand a failed reply.
    #expect(!model.canDeleteSelectedMessages)
    #expect(!model.showingDeleteConfirmation)

    // When: an unrelated message is selected instead.
    model.selectMessage(unrelatedMessage.id, modifiers: [])

    // Then: even unrelated deletion waits so its transcript reload cannot race the send reload.
    #expect(!model.canDeleteSelectedMessages)

    // Cleanup.
    send.cancel()
    await send.value
    #expect(!model.isSending)
    model.selectMessage(replyTarget.id, modifiers: [])
    #expect(model.canDeleteSelectedMessages)
}

@MainActor
@Test func unresolvedQuoteNavigationBlocksSendUntilItsDestinationSettles() async throws {
    // Given
    let target = NativePreviewData.messages(for: "@1")[0]
    let unresolvedQuote = NativeQuote(messageID: nil, text: target.text, sent: target.sent, author: target.author)
    let containingMessage = NativeMessage(
        id: 908,
        text: "Reply with delayed quote metadata",
        timestamp: nil,
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: unresolvedQuote
    )
    let refreshedMessage = NativeMessage(
        id: containingMessage.id,
        text: containingMessage.text,
        timestamp: containingMessage.timestamp,
        sent: containingMessage.sent,
        author: containingMessage.author,
        deletable: containingMessage.deletable,
        content: containingMessage.content,
        quotedItem: NativeQuote(messageID: target.id, text: target.text, sent: target.sent, author: target.author)
    )
    let probe = DelayedValue<NativeMessage?>(refreshedMessage)
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in await probe.load() }
    )
    model.messages = [target, containingMessage]

    // When: resolving the quote metadata is still in flight.
    let navigation = try #require(model.openQuotedMessage(unresolvedQuote, from: containingMessage.id))
    await probe.waitUntilRequested()
    model.draft = "Do not race this send"

    // Then: Send remains disabled even before the transcript page loader begins.
    #expect(!model.isLoadingConversation)
    #expect(!model.canSendDraft)
    model.sendDraft()
    #expect(model.sendTask == nil)
    #expect(model.draft == "Do not race this send")

    // When: the destination resolves.
    await probe.release()
    await navigation.value

    // Then
    #expect(model.targetMessageID == target.id)
    #expect(model.canSendDraft)
}

@MainActor
@Test func newerReplyCancelsAnOlderOffscreenQuotePageLoad() async throws {
    // Given
    let originalMessages = NativePreviewData.messages(for: "@1")
    let replyTarget = try #require(originalMessages.first)
    let offscreenTarget = NativeMessage(
        id: 9_701,
        text: "Older offscreen quote destination",
        timestamp: nil,
        sent: false,
        author: "Maya",
        deletable: true,
        content: .text
    )
    let probe = DelayedValue([offscreenTarget])
    let model = AppModel(
        previewMode: true,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            #expect(aroundMessageID == offscreenTarget.id)
            return await probe.load()
        }
    )
    model.messages = originalMessages
    let quote = NativeQuote(
        messageID: offscreenTarget.id,
        text: offscreenTarget.text,
        sent: offscreenTarget.sent,
        author: offscreenTarget.author
    )

    // When: an offscreen quote page is loading, then Reply becomes the newer intent.
    let navigation = try #require(model.openQuotedMessage(quote, from: 9_702))
    await probe.waitUntilRequested()
    #expect(model.isLoadingConversation)
    model.beginReply(to: replyTarget)

    // Then: Reply immediately keeps the visible transcript and owns the composer.
    #expect(model.replyingTo?.id == replyTarget.id)
    #expect(model.messages == originalMessages)
    #expect(!model.isLoadingConversation)

    // When: the cancelled page loader eventually returns.
    await probe.release()
    await navigation.value

    // Then: it cannot replace the transcript or steal scroll focus.
    #expect(model.replyingTo?.id == replyTarget.id)
    #expect(model.messages == originalMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.conversationAnchorMessageID == nil)
}

@MainActor
@Test func newerReplyCancelsOlderUnresolvedQuoteMetadata() async throws {
    // Given
    let replyTarget = NativePreviewData.messages(for: "@1")[0]
    let unresolvedQuote = NativeQuote(messageID: nil, text: "Older unresolved quote", sent: false, author: "Maya")
    let containingMessage = NativeMessage(
        id: 9_703,
        text: "Reply with delayed metadata",
        timestamp: nil,
        sent: true,
        author: nil,
        deletable: true,
        content: .text,
        quotedItem: unresolvedQuote
    )
    let refreshedMessage = NativeMessage(
        id: containingMessage.id,
        text: containingMessage.text,
        timestamp: containingMessage.timestamp,
        sent: containingMessage.sent,
        author: containingMessage.author,
        deletable: containingMessage.deletable,
        content: containingMessage.content,
        quotedItem: NativeQuote(
            messageID: replyTarget.id,
            text: replyTarget.text,
            sent: replyTarget.sent,
            author: replyTarget.author
        )
    )
    let probe = DelayedValue<NativeMessage?>(refreshedMessage)
    let model = AppModel(
        previewMode: true,
        loadMessageOperation: { _, _ in await probe.load() }
    )
    let originalMessages = [replyTarget, containingMessage]
    model.messages = originalMessages

    // When: metadata resolution is pending, then the user chooses a visible reply target.
    let navigation = try #require(model.openQuotedMessage(unresolvedQuote, from: containingMessage.id))
    await probe.waitUntilRequested()
    #expect(model.quoteNavigationTask != nil)
    model.beginReply(to: replyTarget)

    // Then: the newer reply retires the unresolved navigation immediately.
    #expect(model.replyingTo?.id == replyTarget.id)
    #expect(model.quoteNavigationTask == nil)

    // When: the cancelled metadata request returns.
    await probe.release()
    await navigation.value

    // Then: it cannot update the old row or jump the transcript.
    #expect(model.replyingTo?.id == replyTarget.id)
    #expect(model.messages == originalMessages)
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == nil)
}

@MainActor
@Test func inFlightSendBlocksQuotedHistoryNavigation() async throws {
    // Given
    let failure = "The delayed send failed."
    let probe = DelayedTextSendProbe(failureMessage: failure)
    let model = makeSendTestModel(sendTextOperation: { _, _, _ in try await probe.send() })
    let target = try #require(model.messages.first)
    let quote = NativeQuote(messageID: target.id, text: target.text, sent: target.sent, author: target.author)
    model.draft = "Sending now"

    // When: a send is in flight.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()

    // Then: quote navigation and transcript deletion are both rejected consistently.
    #expect(!model.canNavigateConversationHistory)
    #expect(!model.canRefreshConversation)
    #expect(model.openQuotedMessage(quote, from: 909) == nil)
    #expect(model.targetMessageID == nil)
    #expect(model.conversationAnchorMessageID == nil)
    model.selectMessage(target.id, modifiers: [])
    #expect(!model.canDeleteSelectedMessages)

    // Cleanup.
    await probe.release()
    await send.value
    #expect(model.phase == .failed(failure))
    #expect(model.canNavigateConversationHistory)
}

private actor DelayedDeletionProbe {
    private let messages: [NativeMessage]?
    private let failureMessage: String
    private var requested = false
    private var released = false

    init(messages: [NativeMessage]? = nil, failureMessage: String = "Deletion failed") {
        self.messages = messages
        self.failureMessage = failureMessage
    }

    func delete() async throws -> [NativeMessage] {
        requested = true
        while !released { await Task.yield() }
        if let messages { return messages }
        throw NativeChatError.unavailable(failureMessage)
    }

    func waitUntilRequested() async {
        while !requested { await Task.yield() }
    }

    func release() {
        released = true
    }
}

@MainActor
@Test func failedDeletionPreservesReplyAndBlocksRacingSend() async throws {
    // Given
    let failure = "The reply target could not be deleted."
    let probe = DelayedDeletionProbe(failureMessage: failure)
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in try await probe.delete() }
    )
    let target = try #require(model.messages.first)
    let unrelatedMessage = try #require(model.messages.dropFirst().first)
    model.draft = "Keep this reply"
    model.beginReply(to: target)
    model.selectMessage(target.id, modifiers: [])

    // When: deletion is pending.
    let deletion = try #require(model.deleteSelectedMessages())
    await probe.waitUntilRequested()

    // Then: the reply stays visible, but cannot be sent into the deletion race.
    #expect(model.replyingTo?.id == target.id)
    #expect(!model.canSendDraft)
    #expect(!model.canNavigateConversationHistory)
    #expect(!model.canRefreshConversation)
    #expect(!model.canReply(to: unrelatedMessage))
    model.beginReply(to: unrelatedMessage)
    #expect(model.replyingTo?.id == target.id)
    let quote = NativeQuote(messageID: target.id, text: target.text, sent: target.sent, author: target.author)
    #expect(model.openQuotedMessage(quote, from: 910) == nil)
    #expect(model.targetMessageID == nil)
    model.sendDraft()
    #expect(model.sendTask == nil)

    // When: deletion fails.
    await probe.release()
    await deletion.value

    // Then: the complete composer is restored and can be retried.
    #expect(model.replyingTo?.id == target.id)
    #expect(model.draft == "Keep this reply")
    #expect(model.canSendDraft)
    #expect(model.canReply(to: unrelatedMessage))
    #expect(model.phase == .failed(failure))
}

@MainActor
@Test func successfulDeletionClearsOnlyItsReplyContextAcrossChatSwitch() async throws {
    // Given
    let remainingMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let probe = DelayedDeletionProbe(messages: remainingMessages)
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in try await probe.delete() }
    )
    let target = try #require(model.messages.first)
    model.draft = "Keep as an ordinary draft"
    model.beginReply(to: target)
    model.selectMessage(target.id, modifiers: [])

    // When: deletion starts and the user switches conversations.
    let deletion = try #require(model.deleteSelectedMessages())
    await probe.waitUntilRequested()
    #expect(model.replyingTo?.id == target.id)
    #expect(!model.canSendDraft)
    model.selectChat("#2")
    await probe.release()
    await deletion.value

    // Then: returning restores the draft, but not a quote to the deleted item.
    model.selectChat("@1")
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep as an ordinary draft")
    #expect(model.canSendDraft)
}

@MainActor
@Test func deletionCompletionCannotReplaceAnotherChatsTranscript() async throws {
    // Given
    let deletedChatMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in deletedChatMessages }
    )
    let message = try #require(model.messages.first)
    model.selectMessage(message.id, modifiers: [])

    // When
    let deletion = try #require(model.deleteSelectedMessages())
    model.selectChat("#2")
    let activeChatMessages = model.messages
    await deletion.value

    // Then
    #expect(model.selectedChatID == "#2")
    #expect(model.messages == activeChatMessages)
    #expect(!model.isDeletingMessages)
}

@MainActor
@Test func deletionCompletionRefreshesItsOriginatingTranscript() async throws {
    // Given
    let remainingMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in remainingMessages }
    )
    let message = try #require(model.messages.first)
    model.selectMessage(message.id, modifiers: [])

    // When
    let deletion = try #require(model.deleteSelectedMessages())
    await deletion.value

    // Then
    #expect(model.messages == remainingMessages)
    #expect(model.selectedMessageIDs.isEmpty)
    #expect(!model.isDeletingMessages)
}

@MainActor
@Test func offscreenDeletionFailureWaitsForItsOriginatingChat() async throws {
    // Given
    let message = "The message could not be deleted."
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in throw NativeChatError.unavailable(message) }
    )
    let selected = try #require(model.messages.first)
    model.selectMessage(selected.id, modifiers: [])

    // When
    let deletion = try #require(model.deleteSelectedMessages())
    model.selectChat("#2")
    await deletion.value

    // Then
    #expect(model.phase == .ready)
    #expect(!model.isDeletingMessages)

    model.selectChat("@1")
    #expect(model.phase == .failed(message))
}

@MainActor
@Test func cancelledDeletionDoesNotMutateTheTranscriptOrStayBusy() async throws {
    // Given
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in
            try Task.checkCancellation()
            return []
        }
    )
    let originalMessages = model.messages
    let selected = try #require(model.messages.first)
    model.draft = "Keep this reply"
    model.beginReply(to: selected)
    model.selectMessage(selected.id, modifiers: [])

    // When
    let deletion = try #require(model.deleteSelectedMessages())
    deletion.cancel()
    await deletion.value

    // Then
    #expect(model.messages == originalMessages)
    #expect(model.replyingTo?.id == selected.id)
    #expect(model.draft == "Keep this reply")
    #expect(!model.isDeletingMessages)
}

@MainActor
@Test func cancellationAfterDeletionCommitClearsReplyAndRemovesTarget() async throws {
    // Given
    let remainingMessages = Array(NativePreviewData.messages(for: "@1").dropFirst())
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in
            withUnsafeCurrentTask { $0?.cancel() }
            return remainingMessages
        }
    )
    let target = try #require(model.messages.first)
    model.draft = "Keep as an ordinary draft"
    model.beginReply(to: target)
    model.selectMessage(target.id, modifiers: [])

    // When: the delete operation commits, then observes cancellation.
    let deletion = try #require(model.deleteSelectedMessages())
    await deletion.value

    // Then: committed state wins over cancellation.
    #expect(!model.messages.contains(where: { $0.id == target.id }))
    #expect(model.replyingTo == nil)
    #expect(model.draft == "Keep as an ordinary draft")
    #expect(model.canSendDraft)
    #expect(!model.isDeletingMessages)
    #expect(model.phase == .ready)
}

@MainActor
@Test func notificationChatTransitionCannotLeakAReplyIntoAnotherConversation() throws {
    // Given
    let model = AppModel(previewMode: true)
    let original = try #require(model.messages.first)
    model.beginReply(to: original)
    #expect(model.replyingTo?.id == original.id)

    // When
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "#2",
        messageID: 20
    ))

    // Then
    #expect(model.selectedChatID == "#2")
    #expect(model.replyingTo == nil)
    #expect(model.targetMessageID == 20)

    let groupMessage = try #require(model.messages.first)
    model.selectChat("*3")
    model.beginReply(to: groupMessage)
    #expect(model.replyingTo == nil)

    model.selectChat("@1")
    #expect(model.replyingTo?.id == original.id)
}

@MainActor
@Test func sameChatNotificationWaitsForAFailedSendToReleaseTheTranscript() async throws {
    // Given
    let failure = "The delayed send failed."
    let probe = DelayedTextSendProbe(failureMessage: failure)
    let model = makeSendTestModel(sendTextOperation: { _, _, _ in try await probe.send() })
    let target = try #require(model.messages.last)
    model.draft = "Keep this draft if sending fails"

    // When: a notification for the selected chat arrives during the send.
    model.sendDraft()
    let send = try #require(model.sendTask)
    await probe.waitUntilRequested()
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "@1",
        messageID: target.id
    ))

    // Then: the route cannot replace the transcript owned by the send.
    #expect(model.isSendingSelectedChat)
    #expect(model.targetMessageID == nil)

    // When: the send resolves with an error.
    await probe.release()
    await send.value

    // Then: the draft is restored and the queued click reaches its exact message.
    #expect(model.phase == .failed(failure))
    #expect(model.draft == "Keep this draft if sending fails")
    #expect(!model.isSending)
    #expect(model.targetMessageID == target.id)
}

@MainActor
@Test func sameChatNotificationWaitsForAFailedDeletionToReleaseTheTranscript() async throws {
    // Given
    let failure = "The delayed deletion failed."
    let probe = DelayedDeletionProbe(failureMessage: failure)
    let model = AppModel(
        previewMode: true,
        deleteMessagesOperation: { _, _ in try await probe.delete() }
    )
    let deletionTarget = try #require(model.messages.first)
    let routeTarget = try #require(model.messages.last)
    model.selectMessage(deletionTarget.id, modifiers: [])

    // When: a notification for the selected chat arrives during deletion.
    let deletion = try #require(model.deleteSelectedMessages())
    await probe.waitUntilRequested()
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "@1",
        messageID: routeTarget.id
    ))

    // Then: the in-flight deletion keeps ownership of the transcript.
    #expect(model.isDeletingSelectedChat)
    #expect(model.targetMessageID == nil)

    // When: deletion resolves with an error.
    await probe.release()
    await deletion.value

    // Then: the queued click is consumed without losing the failure state.
    #expect(model.phase == .failed(failure))
    #expect(!model.isDeletingMessages)
    #expect(model.targetMessageID == routeTarget.id)
}

@MainActor
@Test func notificationRouteWaitsForBothPhasesOfManualRefresh() async throws {
    // Given
    let chatsProbe = DelayedValue(NativePreviewData.chats)
    let originalMessages = NativePreviewData.messages(for: "@1")
    let transcriptProbe = ConversationRefreshProbe(messages: originalMessages)
    let model = AppModel(
        previewMode: false,
        loadMessagesOperation: { chatID, aroundMessageID in
            #expect(chatID == "@1")
            return await transcriptProbe.load(around: aroundMessageID)
        },
        loadChatsOperation: { _ in await chatsProbe.load() }
    )
    model.phase = .ready
    model.profile = NativePreviewData.profile
    model.chats = NativePreviewData.chats
    model.selectedChatID = "@1"
    model.messages = originalMessages
    let target = try #require(originalMessages.last)

    // When: the route arrives while refresh is still loading the chat list.
    model.refresh()
    await chatsProbe.waitUntilRequested()
    model.openNotificationRoute(NotificationRoute(
        userID: NativePreviewData.profile.userID,
        remoteHostID: nil,
        chatID: "@1",
        messageID: target.id
    ))

    // Then: it cannot bypass the pending transcript refresh.
    #expect(model.isRefreshing)
    #expect(model.targetMessageID == nil)
    #expect(await transcriptProbe.recordedRequests().isEmpty)

    // When: refresh completes, its queued route loads the exact message afterward.
    await chatsProbe.release()
    await transcriptProbe.waitUntilRequestCount(2)
    for _ in 0..<1_000 {
        if model.targetMessageID == target.id { break }
        await Task.yield()
    }

    // Then
    #expect(!model.isRefreshing)
    #expect(await transcriptProbe.recordedRequests() == [nil, target.id])
    #expect(model.targetMessageID == target.id)
}

@MainActor
@Test func switchingChatsPreservesIndependentComposerStates() throws {
    // Given
    let model = AppModel(previewMode: true)
    let directMessage = try #require(model.messages.first)
    let attachment = PendingAttachment(
        id: UUID(),
        url: URL(fileURLWithPath: "/tmp/photo.jpg"),
        fileName: "photo.jpg",
        kind: .image,
        byteCount: 10,
        previewImage: nil
    )
    model.draft = "Direct draft"
    model.pendingAttachments = [attachment]
    model.beginReply(to: directMessage)

    // When
    model.selectChat("#2")
    let groupMessage = try #require(model.messages.first)
    model.draft = "Group draft"
    model.beginReply(to: groupMessage)
    model.selectChat("@1")

    // Then
    #expect(model.draft == "Direct draft")
    #expect(model.pendingAttachments == [attachment])
    #expect(model.replyingTo?.id == directMessage.id)

    model.selectChat("#2")
    #expect(model.draft == "Group draft")
    #expect(model.pendingAttachments.isEmpty)
    #expect(model.replyingTo?.id == groupMessage.id)
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

@Test func selectedAttachmentsCopyMeaningfulTextInTranscriptOrder() {
    let messages = [
        NativeMessage(
            id: 1,
            text: "First message",
            timestamp: nil,
            sent: false,
            author: nil,
            deletable: true,
            content: .text
        ),
        NativeMessage(
            id: 2,
            text: "",
            timestamp: nil,
            sent: false,
            author: nil,
            deletable: true,
            content: .image(preview: nil, fileName: "photo.jpg")
        ),
        NativeMessage(
            id: 3,
            text: "",
            timestamp: nil,
            sent: false,
            author: nil,
            deletable: true,
            content: .voice(fileName: nil, duration: 75)
        ),
    ]

    #expect(
        MessageSelection.clipboardText(for: messages)
            == "First message\n\nphoto.jpg\n\nVoice message, 1:15"
    )
}

@Test func copyCommandPreservesNativeTextSelectionAndSelectedMessageCopying() {
    #expect(
        DesktopCopyCommandRoute.resolve(transcriptFocused: true, selectedMessageCount: 0)
            == .firstResponder
    )
    #expect(
        DesktopCopyCommandRoute.resolve(transcriptFocused: true, selectedMessageCount: 2)
            == .selectedMessages
    )
    #expect(
        DesktopCopyCommandRoute.resolve(transcriptFocused: false, selectedMessageCount: 2)
            == .firstResponder
    )
}

@Test func replyControlRemainsVisibleAcrossItsHoverRegion() {
    #expect(MessageReplyControlVisibility.isVisible(canReply: true, hovering: true, selected: false))
    #expect(MessageReplyControlVisibility.isVisible(canReply: true, hovering: false, selected: true))
    #expect(!MessageReplyControlVisibility.isVisible(canReply: true, hovering: false, selected: false))
    #expect(!MessageReplyControlVisibility.isVisible(canReply: false, hovering: true, selected: true))
}

@Test func densityTokensStayOnTheMacSpacingGrid() {
    #expect(DesktopChatDensity.compact.tokens.chatRowPadding == 4)
    #expect(DesktopChatDensity.comfortable.tokens.transcriptGap == 12)
    #expect(DesktopChatDensity.spacious.tokens.composerPadding == 16)
    #expect(DesktopChatDensity.compact.tokens.avatarSize < DesktopChatDensity.spacious.tokens.avatarSize)
}

@Test func composerReturnSendsWhileShiftReturnInsertsANewline() {
    #expect(ComposerKeyboard.returnAction(shiftPressed: false) == .send)
    #expect(ComposerKeyboard.returnAction(shiftPressed: true) == .insertNewline)
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
    // Given
    let urls = ["one.jpg", "two.mov", "three.pdf"].map { URL(fileURLWithPath: "/tmp/\($0)") }
    let attachments = [
        PendingAttachment(id: UUID(), url: urls[0], fileName: "one.jpg", kind: .image, byteCount: 1, previewImage: nil),
        PendingAttachment(id: UUID(), url: urls[1], fileName: "two.mov", kind: .video, byteCount: 2, previewImage: nil),
        PendingAttachment(id: UUID(), url: urls[2], fileName: "three.pdf", kind: .document, byteCount: 3, previewImage: nil),
    ]
    // When
    let reordered = PendingAttachment.reordered(attachments, from: attachments[2].id, before: attachments[0].id)
    let sendSteps = PendingAttachmentBatch.sendSteps(
        attachments: reordered,
        caption: "Batch caption",
        quotedItemID: 42
    )

    // Then
    #expect(reordered.map(\.fileName) == ["three.pdf", "one.jpg", "two.mov"])
    #expect(PendingAttachment.remainingAfterFailure(reordered, at: 1).map(\.fileName) == ["one.jpg", "two.mov"])
    #expect(sendSteps.map(\.attachment.fileName) == ["three.pdf", "one.jpg", "two.mov"])
    #expect(sendSteps.map(\.quotedItemID) == [42, nil, nil])
    #expect(sendSteps.map(\.caption) == ["", "", "Batch caption"])
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

@Test func opensTemporaryDatabaseAndDecryptsAttachmentWithBundledCore() async throws {
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
    var encryptedSource: NativeCryptoFile?
    let original = Data([0xff, 0xd8, 0xff, 0xe0, 0x00, 0x10, 0x4a, 0x46, 0x49, 0x46, 0xff, 0xd9])
    if let controller {
        defer {
            if let closeResult = sx_core_close_store(controller) {
                sx_core_free(closeResult)
            }
        }
        func sendCoreCommand(_ command: String) throws -> Data {
            let result = command.withCString { sx_core_send_cmd(controller, $0, 0) }
            guard let result else {
                throw NativeChatError.unavailable("The bundled core returned no command response.")
            }
            defer { sx_core_free(result) }
            guard let response = String(validatingUTF8: result) else {
                throw NativeChatError.invalidResponse("The bundled core returned invalid UTF-8.")
            }
            return Data(response.utf8)
        }

        let createdUser = try sendCoreCommand(
            #"/_create user {"profile":null,"pastTimestamp":false,"userChatRelay":false,"clientService":false}"#
        )
        let profile = try NativeChatParser.profile(from: createdUser)
        let started = try sendCoreCommand("/_start main=on snd_files=on")
        try NativeChatParser.validateCommandResponse(started)
        let localSend = try sendCoreCommand(
            #"/_create *1 json [{"msgContent":{"type":"text","text":"Native response check"},"mentions":{}}]"#
        )
        try NativeChatParser.validateCommandResponse(
            localSend,
            expectedType: "newChatItems",
            requireChatItems: true
        )

        let createdGroup = try sendCoreCommand(
            #"/_group \#(profile.userID) {"displayName":"Native Reply Check","fullName":""}"#
        )
        let groupRoot = try #require(
            JSONSerialization.jsonObject(with: createdGroup) as? [String: Any]
        )
        let groupResult = try #require(groupRoot["result"] as? [String: Any])
        #expect(groupResult["type"] as? String == "groupCreated")
        let groupInfo = try #require(groupResult["groupInfo"] as? [String: Any])
        let groupID = try #require(groupInfo["groupId"] as? NSNumber).int64Value
        let group = NativeChat(
            id: "#\(groupID)",
            apiID: groupID,
            kind: .group,
            displayName: "Native Reply Check",
            image: nil,
            preview: "",
            timestamp: nil,
            unreadCount: 0,
            sendAsGroup: false
        )
        let firstMessage = SimpleXCore.composedMessage(
            messageContent: ["type": "text", "text": "Original bundled-core message"],
            quotedItemID: nil
        )
        let firstSend = try sendCoreCommand(SimpleXCore.sendCommand(message: firstMessage, to: group))
        try NativeChatParser.validateCommandResponse(
            firstSend,
            expectedType: "newChatItems",
            requireChatItems: true
        )
        let firstRoot = try #require(JSONSerialization.jsonObject(with: firstSend) as? [String: Any])
        let firstResult = try #require(firstRoot["result"] as? [String: Any])
        let firstItems = try #require(firstResult["chatItems"] as? [[String: Any]])
        let firstItem = try #require(firstItems.first?["chatItem"] as? [String: Any])
        let firstMeta = try #require(firstItem["meta"] as? [String: Any])
        let firstItemID = try #require(firstMeta["itemId"] as? NSNumber).int64Value

        let replyMessage = SimpleXCore.composedMessage(
            messageContent: ["type": "text", "text": "Bundled-core reply"],
            quotedItemID: firstItemID
        )
        let replySend = try sendCoreCommand(SimpleXCore.sendCommand(message: replyMessage, to: group))
        let replyReceipt = try NativeChatParser.validateSendResponse(
            replySend,
            quotedItemID: firstItemID
        )
        #expect(replyReceipt.replyContextConfirmed)
        let committedReply = try #require(replyReceipt.committedMessages.first)
        #expect(committedReply.text == "Bundled-core reply")
        #expect(committedReply.quotedItem?.messageID == firstItemID)

        let deletedOriginal = try sendCoreCommand(
            "/_delete item #\(groupID) \(firstItemID) internal"
        )
        try NativeChatParser.validateCommandResponse(deletedOriginal)
        let staleReplyMessage = SimpleXCore.composedMessage(
            messageContent: ["type": "text", "text": "Stale bundled-core reply"],
            quotedItemID: firstItemID
        )
        let staleReplySend = try sendCoreCommand(
            SimpleXCore.sendCommand(message: staleReplyMessage, to: group)
        )
        #expect(NativeChatParser.commandErrorMakesReplyTargetUnavailable(staleReplySend))

        let originalURL = temporaryDirectory.appendingPathComponent("original.jpg")
        let encryptedURL = temporaryDirectory.appendingPathComponent("encrypted.jpg")
        try original.write(to: originalURL)

        let encryptResult = originalURL.path.withCString { fromPath in
            encryptedURL.path.withCString { toPath in
                sx_core_encrypt_file(controller, fromPath, toPath)
            }
        }
        let encryptJSON = try #require(encryptResult.flatMap(String.init(validatingUTF8:)))
        if let encryptResult { sx_core_free(encryptResult) }
        let encryptObject = try #require(
            JSONSerialization.jsonObject(with: Data(encryptJSON.utf8)) as? [String: Any]
        )
        let cryptoArgs = try #require(encryptObject["cryptoArgs"] as? [String: Any])
        let key = try #require(cryptoArgs["fileKey"] as? String)
        let nonce = try #require(cryptoArgs["fileNonce"] as? String)
        encryptedSource = NativeCryptoFile(
            filePath: encryptedURL.path,
            cryptoArgs: NativeCryptoFileArgs(fileKey: key, fileNonce: nonce)
        )
    }

    let attachmentCore = SimpleXCore()
    let source = try #require(encryptedSource)
    let decryptedURL = try await attachmentCore.openableURL(
        for: source,
        fileName: "photo.jpg"
    )
    #expect(decryptedURL.lastPathComponent == "photo.jpg")
    #expect(try Data(contentsOf: decryptedURL) == original)
}
