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

private struct QuotedAttachmentCase: Sendable, CustomTestStringConvertible {
    let contentType: String
    let expectedPreview: String

    var testDescription: String { contentType }
}

private let quotedAttachmentCases = [
    QuotedAttachmentCase(contentType: "image", expectedPreview: "Photo"),
    QuotedAttachmentCase(contentType: "video", expectedPreview: "Video"),
    QuotedAttachmentCase(contentType: "voice", expectedPreview: "Voice message"),
    QuotedAttachmentCase(contentType: "file", expectedPreview: "File"),
]

@Test(arguments: quotedAttachmentCases)
private func whitespaceOnlyQuotedAttachmentsUseMeaningfulPreviews(testCase: QuotedAttachmentCase) throws {
    // Given
    let json = """
        {"result":{"type":"apiChat","chat":{"chatItems":[{"chatDir":{"type":"directRcv"},"meta":{"itemId":9,"itemText":"Reply","itemTs":"2026-08-02T20:00:00Z"},"content":{"type":"rcvMsgContent","msgContent":{"type":"text","text":"Reply"}},"quotedItem":{"chatDir":{"type":"directSnd"},"itemId":7,"sentAt":"2026-08-02T19:59:00Z","content":{"type":"\(testCase.contentType)","text":"   "}}}]}}}
        """

    // When
    let message = try #require(NativeChatParser.messages(from: Data(json.utf8)).first)

    // Then
    #expect(message.quotedItem?.text == testCase.expectedPreview)
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

    // When
    let navigation = try #require(model.openQuotedMessage(unresolvedQuote, from: containingMessage.id))
    await navigation.value

    // Then
    #expect(model.targetMessageID == nil)
    #expect(model.quoteNavigationError == "The original quoted message is no longer available in this conversation.")
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
    #expect(model.openingAttachmentIDs.isEmpty)
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

    func send(_ attachment: PendingAttachment, caption: String, quotedItemID: Int64?) throws {
        requests.append(Request(
            attachmentID: attachment.id,
            caption: caption,
            quotedItemID: quotedItemID
        ))
        if requests.count == failingRequest {
            throw NativeChatError.unavailable("The attachment could not be sent.")
        }
    }

    func recordedRequests() -> [Request] {
        requests
    }
}

@MainActor
private func makeSendTestModel(
    sendTextOperation: SendTextOperation? = nil,
    sendAttachmentOperation: SendAttachmentOperation? = nil
) -> AppModel {
    let model = AppModel(
        previewMode: true,
        sendTextOperation: sendTextOperation,
        sendAttachmentOperation: sendAttachmentOperation
    )
    return model
}

@MainActor
@Test func partialAttachmentFailureKeepsOnlyUnsentItemsAndDoesNotRepeatTheQuote() async throws {
    // Given
    let probe = AttachmentSendProbe(failingRequest: 2)
    let model = makeSendTestModel(sendAttachmentOperation: { attachment, caption, quotedItemID, _ in
        try await probe.send(attachment, caption: caption, quotedItemID: quotedItemID)
    })
    let original = try #require(model.messages.first)
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
    #expect(!model.isSending)
}

@MainActor
@Test func cancellationAfterCoreSuccessCommitsTheSentAttachmentBeforeStopping() async throws {
    // Given
    let model = makeSendTestModel(sendAttachmentOperation: { _, _, _, _ in
        withUnsafeCurrentTask { $0?.cancel() }
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
}

@MainActor
@Test func inFlightSendLocksOnlyItsOriginatingComposer() async throws {
    // Given
    let model = makeSendTestModel(sendAttachmentOperation: { _, _, _, _ in
        try Task.checkCancellation()
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
@Test func inFlightReplyTargetCannotBeDeletedUntilTheSendResolves() async throws {
    // Given
    let model = makeSendTestModel(sendTextOperation: { _, _, _ in
        try Task.checkCancellation()
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

    // Then: unrelated deletion remains available.
    #expect(model.canDeleteSelectedMessages)

    // Cleanup.
    send.cancel()
    await send.value
    #expect(!model.isSending)
    model.selectMessage(replyTarget.id, modifiers: [])
    #expect(model.canDeleteSelectedMessages)
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
    model.selectMessage(selected.id, modifiers: [])

    // When
    let deletion = try #require(model.deleteSelectedMessages())
    deletion.cancel()
    await deletion.value

    // Then
    #expect(model.messages == originalMessages)
    #expect(!model.isDeletingMessages)
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
