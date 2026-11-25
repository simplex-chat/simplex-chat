import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

private let btc = "bc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66505d4u"
private let xmr = "8A3ZWAXrrQddvnT1fPrtbK86ZAoM4nai3Gjg1LEow3JWcryJtovMnHYZnxTJpCLmAbfWbnPMeTzPmMBjAhyd4xoM89hYq1c"
private let eth = "0xD7047Fe3Eecb2f2FF78d839dD927Be27Bc12c86a"
private let invoice = "lnbc2500u1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpu9qrsgquk0rl77nj30yxdy8j9vdx85fkpmdla2087ne0xh8nhedh8w27kyke0lp53ut353s06fv3qfegext0eh0ymjpf39tuven09sam30g4vgpfna3rh"

final class PaymentLinksTests: XCTestCase {
    private let sendCmd = sendSimpleXCmdStr

    override func tearDown() {
        sendSimpleXCmdStr = sendCmd
    }

    func testLinksShowNetworkSignAndShortenedAddress() throws {
        let markdown = try XCTUnwrap(try recorded()["markdown"] as? [String: Any])
        let shown = try markdown.mapValues { parsed in
            try draw(jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed))).formattedText).string.string
        }
        let expected: [String: String] = [
            "<bitcoin:\(btc)?amount=0.001&label=SimpleX>": "₿\u{00A0}0.001 · SimpleX",
            "Cheaper on Arbitrum: ethereum:\(eth)@42161?value=1e16": "Cheaper on Arbitrum: Ξ\u{00A0}0.01 · Arbitrum One",
            "Donations: \(btc). Cheers": "Donations: ₿\u{00A0}bc1q2g…5d4u. Cheers",
            "For the coffee: \(invoice) thanks!": "For the coffee: ϟ\u{00A0}0.0025 BTC thanks!",
            "If you want to support the project: [Donate](monero:\(xmr)). Cheers": "If you want to support the project: ɱ\u{00A0}Donate. Cheers",
            "On Ethereum: \(eth)": "On Ethereum: Ξ\u{00A0}0xD704…c86a",
            "Or this exact amount: monero:\(xmr)?tx_amount=0.25&recipient_name=SimpleX": "Or this exact amount: ɱ\u{00A0}0.25 · SimpleX",
            "Other networks: t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg, ltc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66snwfdv, D99pV4n9TrPxBPCkQGx4w4SMSa6QjRBxPf, qq6c8vfvxqrk6rhdysgvkhqc24sggkfsx5nqvdlqcg":
                "Other networks: ⓩ\u{00A0}t1fwjQ…S5Jg, Ł\u{00A0}ltc1q2…wfdv, Ð\u{00A0}D99pV4…BxPf, ₿\u{00A0}qq6c8v…lqcg",
            "Suggested donation: bitcoin:\(btc)?amount=0.001&label=SimpleX Thank you!": "Suggested donation: ₿\u{00A0}0.001 · SimpleX Thank you!",
            "[Donate](monero:\(xmr))": "ɱ\u{00A0}Donate",
            "[\(btc)](bitcoin:\(btc))": "₿\u{00A0}bc1q2g…5d4u",
            btc: "₿\u{00A0}bc1q2g…5d4u",
            "bitcoin:\(btc)?amount=0.001&label=SimpleX&message=Thank%20you": "₿\u{00A0}0.001 · SimpleX",
            "ethereum:0xd7047fe3eecb2f2ff78d839dd927be27bc12c86a": "Ξ\u{00A0}0xD704…c86a",
            "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@1/transfer?address=\(eth)&uint256=5000000": "Ξ\u{00A0}0xD704…c86a",
            "lightning:LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS": "ϟ\u{00A0}lnurl1…5fns",
            "lightning:lno1pgx9getnwss8vetrw3hhyuckyypwa3eyt44h6txtxquqh7lz5djge4afgfjn7k4rgrkuag0jsd5xvxg": "ϟ\u{00A0}lno1pg…xvxg",
            invoice: "ϟ\u{00A0}0.0025 BTC",
            "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=0.5": "◎\u{00A0}0.5"
        ]
        XCTAssertEqual(shown, expected)
    }

    @MainActor
    func testReceivedLinksHaveNoTapTarget() async throws {
        let commands = try XCTUnwrap(try recorded()["commands"] as? [String: Any])
        sendSimpleXCmdStr = { cmd, _, _ in commands[cmd].flatMap { try? JSONSerialization.data(withJSONObject: singleFieldJSON($0)) } }
        let (chat, _) = try await apiGetChat(chatId: "@2", scope: nil, pagination: .last(count: 1))
        let r = draw(try XCTUnwrap(chat.chatItems.first).formattedText)
        let sign = (r.string.string as NSString).range(of: "₿").location
        XCTAssertEqual(r.string.string, "donate ₿\u{00A0}bc1q2g…5d4u, ɱ\u{00A0}Donate or ϟ\u{00A0}0.0025 BTC")
        XCTAssertNil(r.string.attribute(.underlineStyle, at: sign, effectiveRange: nil))
        XCTAssertNotNil(r.string.attribute(.underlineStyle, at: sign + 2, effectiveRange: nil))
        XCTAssertFalse(r.handleTaps)
    }

    private func recorded() throws -> [String: Any] {
        try XCTUnwrap(try fixtureJSON("payments/links") as? [String: Any])
    }

    private func draw(_ formattedText: [FormattedText]?) -> MsgTextResult {
        messageText("", formattedText, sender: nil, mentions: nil, userMemberId: nil, showSecrets: nil, backgroundColor: .white)
    }
}
