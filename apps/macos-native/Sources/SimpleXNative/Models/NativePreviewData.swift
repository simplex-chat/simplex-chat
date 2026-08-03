import AppKit
import Foundation

enum NativePreviewData {
    static let profile = NativeProfile(userID: 1, displayName: "Alex", image: nil)

    static let chats: [NativeChat] = [
        NativeChat(
            id: "@1",
            apiID: 1,
            kind: .direct,
            displayName: "Maya",
            image: nil,
            preview: "The photos came through perfectly.",
            timestamp: Date().addingTimeInterval(-240),
            unreadCount: 2,
            sendAsGroup: false
        ),
        NativeChat(
            id: "#2",
            apiID: 2,
            kind: .group,
            displayName: "Weekend plans",
            image: nil,
            preview: "Jordan: Saturday works for me",
            timestamp: Date().addingTimeInterval(-3_600),
            unreadCount: 0,
            sendAsGroup: false
        ),
        NativeChat(
            id: "*3",
            apiID: 3,
            kind: .local,
            displayName: "Private notes",
            image: nil,
            preview: "Packing list",
            timestamp: Calendar.current.date(byAdding: .day, value: -1, to: Date()),
            unreadCount: 0,
            sendAsGroup: false
        ),
    ]

    static func messages(for chatID: String) -> [NativeMessage] {
        guard chatID == "@1" else {
            return [
                message(20, "This is a quiet preview conversation.", minutesAgo: 70, sent: false, author: "Jordan"),
                message(21, "It already feels much more like a Mac app.", minutesAgo: 65, sent: true),
            ]
        }
        return [
            message(1, "Hey! Are you still free this evening?", minutesAgo: 38, sent: false, author: "Maya"),
            message(2, "Yep — around seven should work.", minutesAgo: 36, sent: true),
            message(3, "Perfect. I’ll send the address in a minute.", minutesAgo: 35, sent: false, author: "Maya"),
            message(
                4,
                "The photos came through perfectly.",
                minutesAgo: 4,
                sent: false,
                author: "Maya",
                content: .image(preview: sampleImage, fileName: "evening.jpg")
            ),
            NativeMessage(
                id: 5,
                text: "Nice. The native image view was worth fixing.",
                timestamp: Date().addingTimeInterval(-2 * 60),
                sent: true,
                author: nil,
                deletable: true,
                content: .text,
                quotedItem: NativeQuote(
                    messageID: 4,
                    text: "The photos came through perfectly.",
                    sent: false,
                    author: "Maya",
                    visual: .image(sampleImage)
                )
            ),
        ]
    }

    private static func message(
        _ id: Int64,
        _ text: String,
        minutesAgo: TimeInterval,
        sent: Bool,
        author: String? = nil,
        content: NativeMessageContent = .text
    ) -> NativeMessage {
        NativeMessage(
            id: id,
            text: text,
            timestamp: Date().addingTimeInterval(-minutesAgo * 60),
            sent: sent,
            author: author,
            deletable: true,
            content: content
        )
    }

    private static let sampleImage: String? = {
        let size = NSSize(width: 640, height: 360)
        let image = NSImage(size: size)
        image.lockFocus()
        NSGradient(colors: [.systemIndigo, .systemTeal])?.draw(in: NSRect(origin: .zero, size: size), angle: -20)
        let symbol = NSImage(systemSymbolName: "photo.on.rectangle.angled", accessibilityDescription: nil)
        symbol?.draw(
            in: NSRect(x: 272, y: 132, width: 96, height: 96),
            from: .zero,
            operation: .sourceOver,
            fraction: 0.8
        )
        image.unlockFocus()
        guard let tiff = image.tiffRepresentation,
              let bitmap = NSBitmapImageRep(data: tiff),
              let data = bitmap.representation(using: .jpeg, properties: [.compressionFactor: 0.82]) else {
            return nil
        }
        return "data:image/jpeg;base64,\(data.base64EncodedString())"
    }()
}
