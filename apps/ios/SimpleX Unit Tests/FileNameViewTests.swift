import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class FileNameViewTests: XCTestCase {
    @MainActor
    func testLongFileNameExtensionShown() throws {
        for c in ["a", "\u{05E9}"] {
            let name = String(repeating: c, count: 200)
            for end in ["", ".", ". ."] {
                XCTAssertNotEqual(try fileItemImage("\(name).txt\(end)"), try fileItemImage("\(name).pdf\(end)"), "\(c)\(end)")
            }
        }
    }

    @MainActor
    func testFileNameControlAndTextDirectionCharactersShown() throws {
        for c in ["\n", "\u{2028}", "\u{202E}", "\u{2066}"] {
            XCTAssertNotEqual(try fileItemImage("a.txt\(c)"), try fileItemImage("a.txt"), c.unicodeScalars.map { String(format: "U+%04X", $0.value) }.joined())
        }
    }
}

@MainActor
private func fileItemImage(_ fileName: String) throws -> Data {
    let windowScene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
    let item = CIFileView(chat: Chat.sampleData, file: CIFile.getSample(fileName: fileName), meta: CIMeta.getSample(1, previewDate, ""))
        .environmentObject(ChatModel.shared)
        .environmentObject(CurrentColors.toAppTheme())
    let window = UIWindow(windowScene: windowScene)
    window.overrideUserInterfaceStyle = .light
    window.rootViewController = UIHostingController(rootView: item)
    window.frame = CGRect(x: 0, y: 0, width: 300, height: 200)
    window.isHidden = false
    RunLoop.main.run(until: Date().addingTimeInterval(0.6))
    let image = UIGraphicsImageRenderer(bounds: window.bounds).image { _ in
        _ = window.drawHierarchy(in: window.bounds, afterScreenUpdates: true)
    }
    window.isHidden = true
    window.rootViewController = nil
    return try XCTUnwrap(image.pngData())
}
