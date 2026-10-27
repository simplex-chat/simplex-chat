import XCTest
import SwiftUI
import UIKit
@testable import SimpleX
@testable import SimpleXChat

final class SnapshotTests: XCTestCase {
    @MainActor
    func testPreviews() throws {
        NSTimeZone.default = try XCTUnwrap(TimeZone(identifier: "UTC"))
        let env = ProcessInfo.processInfo.environment
        let dir = URL(fileURLWithPath: env["SNAPSHOT_DIR"] ?? NSTemporaryDirectory() + "snapshots", isDirectory: true)
        let size = env["SNAPSHOT_SIZE"] ?? "large"
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        let windowScene = try XCTUnwrap(UIApplication.shared.connectedScenes.compactMap { $0 as? UIWindowScene }.first)
        var index: [[String: Any]] = []
        for dark in [false, true] {
            let theme = snapshotTheme(dark)
            CurrentColors = theme
            for scene in previewScenes() {
                let themeName = dark ? "dark" : "light"
                let file = "\(scene.name)-\(themeName)-\(size).png"
                let image = try renderScene(scene, theme.toAppTheme(), dark: dark, in: windowScene)
                try XCTUnwrap(image.pngData()).write(to: dir.appendingPathComponent(file))
                index.append([
                    "scene": scene.name,
                    "theme": themeName,
                    "size": size,
                    "file": file,
                    "width": Int((image.size.width * image.scale).rounded()),
                    "height": Int((image.size.height * image.scale).rounded()),
                    "scale": image.scale,
                    "contentSizeCategory": UIApplication.shared.preferredContentSizeCategory.rawValue
                ])
            }
        }
        let data = try JSONSerialization.data(withJSONObject: index, options: [.prettyPrinted, .sortedKeys])
        try data.write(to: dir.appendingPathComponent("index-\(size).json"))
    }
}

private struct SnapshotScene {
    var name: String
    var height: CGFloat? = nil
    var view: AnyView
}

private func previewScenes() -> [SnapshotScene] {
    [
        SnapshotScene(name: "ChatInfoImage", view: AnyView(ChatInfoImage_Previews.previews))
    ]
}

private func snapshotTheme(_ dark: Bool) -> ThemeManager.ActiveTheme {
    let base: DefaultTheme = dark ? .DARK : .LIGHT
    return ThemeManager.ActiveTheme(
        name: base.themeName,
        base: base,
        colors: (dark ? DarkColorPalette : LightColorPalette).clone(),
        appColors: (dark ? DarkColorPaletteApp : LightColorPaletteApp).clone(),
        wallpaper: AppWallpaper(background: nil, tint: nil, type: .empty)
    )
}

@MainActor
private func renderScene(_ scene: SnapshotScene, _ theme: AppTheme, dark: Bool, in windowScene: UIWindowScene) throws -> UIImage {
    let screen = windowScene.screen
    let width = screen.bounds.width
    let root = scene.view
        .environmentObject(ChatModel.shared)
        .environmentObject(theme)
        .frame(width: width, height: scene.height)
    let host = UIHostingController(rootView: AnyView(root))
    if #available(iOS 16.4, *) {
        host.safeAreaRegions = []
    }
    let window = UIWindow(windowScene: windowScene)
    window.overrideUserInterfaceStyle = dark ? .dark : .light
    window.rootViewController = host
    window.frame = CGRect(x: 0, y: 0, width: width, height: scene.height ?? screen.bounds.height)
    window.isHidden = false
    settle()
    if scene.height == nil {
        for _ in 0..<2 {
            let fit = host.sizeThatFits(in: CGSize(width: width, height: .greatestFiniteMagnitude))
            window.frame = CGRect(x: 0, y: 0, width: width, height: ceil(fit.height))
            settle()
        }
    }
    let format = UIGraphicsImageRendererFormat.default()
    format.scale = 1
    format.preferredRange = .standard
    format.opaque = true
    let image = UIGraphicsImageRenderer(bounds: window.bounds, format: format).image { _ in
        _ = window.drawHierarchy(in: window.bounds, afterScreenUpdates: true)
    }
    window.isHidden = true
    window.rootViewController = nil
    return image
}

private func settle() {
    RunLoop.main.run(until: Date().addingTimeInterval(0.6))
}
