import XCTest
@testable import SimpleX
@testable import SimpleXChat

final class FileUploadTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    @MainActor
    override func tearDown() async throws {
        ChatModel.shared.fileUploads.values.forEach { $0() }
        ChatModel.shared.messageDelivery.values.forEach { $0() }
        sendSimpleXCmdStr = sendCmd
    }

    @MainActor
    func testSentFileBeginsTask() async throws {
        try replay("files/send-file")
        _ = await apiSendMessages(type: .direct, id: 2, scope: nil, composedMessages: [ComposedMessage(fileSource: .plain("test.pdf"), msgContent: .file(""))])
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [1])
    }

    @MainActor
    func testSentGroupFileBeginsTask() async throws {
        try replay("files/send-group-file")
        _ = await apiSendMessages(type: .group, id: 1, scope: nil, composedMessages: [ComposedMessage(fileSource: .plain("test.pdf"), msgContent: .file(""))])
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [1])
    }

    @MainActor
    func testSentTextBeginsNoTask() async throws {
        try replay("files/send-text")
        _ = await apiSendMessages(type: .direct, id: 2, scope: nil, composedMessages: [ComposedMessage(msgContent: .text("hi"))])
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    func testCompletedUploadEndsTask() async throws {
        beginUploadTask(fileId: 1)
        await processReceivedMsg(try event(XCTUnwrap((try fixtureJSON("files/upload-complete") as? [Any])?.first)))
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    func testUploadErrorEndsTask() async throws {
        beginUploadTask(fileId: 1)
        await processReceivedMsg(try event(XCTUnwrap((try fixtureJSON("files/upload-error") as? [Any])?.first)))
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    func testCancelledUploadEndsTask() async throws {
        beginUploadTask(fileId: 1)
        try replay("files/cancel")
        _ = await apiCancelFile(fileId: 1)
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    func testDeletedItemEndsTask() async throws {
        beginUploadTask(fileId: 1)
        try replay("files/delete")
        _ = try await apiDeleteChatItems(type: .direct, id: 2, scope: nil, itemIds: [1], mode: .cidmInternal)
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    func testDeletedItemEventEndsTask() async throws {
        beginUploadTask(fileId: 1)
        await processReceivedMsg(try event(try response("files/delete")))
        XCTAssertEqual(Array(ChatModel.shared.fileUploads.keys), [])
    }

    @MainActor
    private func beginUploadTask(fileId: Int64) {
        beginFileUploadTasks([ChatItem.getSample(1, .directSnd, .now, "", file: CIFile.getSample(fileId: fileId, fileStatus: .sndStored))])
    }

    private func response(_ fixture: String) throws -> Any {
        try XCTUnwrap((try fixtureJSON(fixture) as? [String: [String: Any]])?["commands"]?.values.first)
    }

    private func replay(_ fixture: String) throws {
        let recorded = try response(fixture)
        sendSimpleXCmdStr = { _, _, _ in try? JSONSerialization.data(withJSONObject: singleFieldJSON(recorded)) }
    }

    private func event(_ json: Any) throws -> ChatEvent {
        let decoded: APIResult<ChatEvent> = decodeAPIResult(try JSONSerialization.data(withJSONObject: singleFieldJSON(json)))
        let event: ChatEvent? = if case let .result(r) = decoded { r } else { nil }
        return try XCTUnwrap(event, "event not decoded: \(decoded.responseType)")
    }
}
