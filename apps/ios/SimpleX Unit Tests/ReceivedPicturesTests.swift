import XCTest
import UIKit
import ImageIO
@testable import SimpleX
@testable import SimpleXChat

final class ReceivedPicturesTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr
    private let lock = NSLock()
    private var unrecorded: [String] = []
    private var receivedFile: URL?

    override func tearDown() {
        if let receivedFile { try? FileManager.default.removeItem(at: receivedFile) }
        sendSimpleXCmdStr = sendCmd
        XCTAssertEqual(requests(), [], "requests without recorded responses")
    }

    func testReceivedPictureIsLoaded() async throws {
        let file = try await receivedPicture()
        try receive(Data(contentsOf: fixtureURL("test.jpg")))
        XCTAssertEqual(getLoadedImage(file)?.size, CGSize(width: 1280, height: 720))
    }

    func testReceivedPicturesAreLoadedWithinLimits() async throws {
        let file = try await receivedPicture()
        try receive(grayPngData(4320, 10))
        XCTAssertEqual(getLoadedImage(file)?.size, CGSize(width: 4320, height: 10))
        try receive(grayPngData(16384, 4096))
        XCTAssertEqual(getLoadedImage(file)?.size, CGSize(width: 4320, height: 1080))
        try receive(grayPngData(1, 16385))
        XCTAssertNil(getLoadedImage(file))
    }

    func testBase64PicturesAreDecodedWithinLimits() throws {
        XCTAssertEqual(try imageFromBase64(base64Png(1024, 512))?.size, CGSize(width: 1024, height: 512))
        XCTAssertEqual(try imageFromBase64(base64Png(4320, 540))?.size, CGSize(width: 1024, height: 128))
        XCTAssertNil(try imageFromBase64(base64Png(1, 4321)))
    }

    func testSharedPictureFileIsDownsampled() throws {
        let url = FileManager.default.temporaryDirectory.appendingPathComponent("shared-picture-test.png")
        try grayPngData(400, 300).write(to: url)
        defer { try? FileManager.default.removeItem(at: url) }
        XCTAssertEqual(downsampleImage(at: url, to: 100)?.size, CGSize(width: 100, height: 75))
    }

    private func receivedPicture() async throws -> CIFile {
        let commands = try XCTUnwrap(try JSONSerialization.jsonObject(with: Data(contentsOf: fixtureURL("api/files/received-image.json"))) as? [String: [String: Any]])["commands"] ?? [:]
        sendSimpleXCmdStr = { [unowned self] cmd, _, _ in
            guard let response = commands[cmd] else { self.unrecordedRequest(cmd); return nil }
            return try? JSONSerialization.data(withJSONObject: singleFieldJSON(response))
        }
        let (chat, _) = try await apiGetChat(chatId: "@2", scope: nil, pagination: .last(count: 1))
        let file = try XCTUnwrap(chat.chatItems.last?.file)
        let received = try getAppFilePath(XCTUnwrap(file.fileSource).filePath)
        XCTAssertFalse(FileManager.default.fileExists(atPath: received.path), "\(received.path) exists")
        receivedFile = received
        return file
    }

    private func receive(_ data: Data) throws {
        try FileManager.default.createDirectory(at: getAppFilesDirectory(), withIntermediateDirectories: true)
        try data.write(to: XCTUnwrap(receivedFile))
    }

    private func fixtureURL(_ path: String) -> URL {
        URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("../../../tests/fixtures/\(path)")
    }

    private func unrecordedRequest(_ request: String) {
        lock.lock()
        defer { lock.unlock() }
        unrecorded.append(request)
    }

    private func requests() -> [String] {
        lock.lock()
        defer { lock.unlock() }
        return unrecorded
    }
}

private func base64Png(_ width: Int, _ height: Int) throws -> String {
    try "data:image/png;base64," + grayPngData(width, height).base64EncodedString()
}

private func grayPngData(_ width: Int, _ height: Int) throws -> Data {
    let bytesPerRow = (width + 7) / 8
    let provider = try XCTUnwrap(CGDataProvider(data: Data(count: bytesPerRow * height) as CFData))
    let image = try XCTUnwrap(CGImage(width: width, height: height, bitsPerComponent: 1, bitsPerPixel: 1, bytesPerRow: bytesPerRow, space: CGColorSpaceCreateDeviceGray(), bitmapInfo: [], provider: provider, decode: nil, shouldInterpolate: false, intent: .defaultIntent))
    let data = NSMutableData()
    let destination = try XCTUnwrap(CGImageDestinationCreateWithData(data as CFMutableData, "public.png" as CFString, 1, nil))
    CGImageDestinationAddImage(destination, image, nil)
    XCTAssertTrue(CGImageDestinationFinalize(destination))
    return data as Data
}
