import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class MarkdownFileTests: XCTestCase {
    @MainActor
    func testCopyAsksAbove128KB() async throws {
        let text = "alpha\n\ngamma"
        UIPasteboard.general.string = "unchanged"
        copyMarkdownFile(CIFile.getSample(fileSize: 131_072), text)
        XCTAssertEqual(UIPasteboard.general.string, text)
        UIPasteboard.general.string = "unchanged"
        copyMarkdownFile(CIFile.getSample(fileSize: 131_073), text)
        try await waitFor { (getTopViewController() as? UIAlertController)?.isBeingPresented == false }
        let alert = try XCTUnwrap(getTopViewController() as? UIAlertController)
        XCTAssertEqual(alert.title, "File too large")
        XCTAssertEqual(alert.message, "Your file is 128 KB. Do you want to copy it?")
        XCTAssertEqual(UIPasteboard.general.string, "unchanged")
        let copy = try XCTUnwrap(alert.actions.first { $0.title == "Copy" })
        let handler = unsafeBitCast(copy.value(forKey: "handler") as AnyObject, to: (@convention(block) (UIAlertAction) -> Void).self)
        await withCheckedContinuation { done in alert.dismiss(animated: false) { done.resume() } }
        handler(copy)
        XCTAssertEqual(UIPasteboard.general.string, text)
    }
}
