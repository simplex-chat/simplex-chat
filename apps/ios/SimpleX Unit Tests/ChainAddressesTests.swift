import XCTest
import UIKit
@testable import SimpleX
@testable import SimpleXChat

private let btc = "bc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66505d4u"
private let xmr = "8A3ZWAXrrQddvnT1fPrtbK86ZAoM4nai3Gjg1LEow3JWcryJtovMnHYZnxTJpCLmAbfWbnPMeTzPmMBjAhyd4xoM89hYq1c"
private let eth = "0xD7047Fe3Eecb2f2FF78d839dD927Be27Bc12c86a"
private let invoice = "lnbc2500u1pvjluezsp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpu9qrsgquk0rl77nj30yxdy8j9vdx85fkpmdla2087ne0xh8nhedh8w27kyke0lp53ut353s06fv3qfegext0eh0ymjpf39tuven09sam30g4vgpfna3rh"
private let labeledToken = "[Dinner](ethereum:0xc2132D05D31c914a87C6611C10748AEb04B58e8F@137/transfer?address=\(eth)&uint256=10000000)"
private let labeledUnknownChain = "[Donate](ethereum:\(eth)@99999)"

final class ChainAddressesTests: RecordedAPITestCase {
    func testLinkTextOfEachCurrency() throws {
        let shown = try recordedMarkdown().mapValues { draw($0).string.string }
        let expected: [String: String] = [
            "<bitcoin:\(btc)?amount=0.001&label=SimpleX>": "SimpleX ₿\u{00A0}0.001",
            "Cheaper on Arbitrum: ethereum:\(eth)@42161?value=1e16": "Cheaper on Arbitrum: ETH\u{00A0}0.01 (Arbitrum One)",
            "Donations: \(btc). Cheers": "Donations: ₿\u{00A0}bc1q2g…\u{2060}5d4u. Cheers",
            "For the coffee: \(invoice) thanks!": "For the coffee: ϟ\u{00A0}0.0025 thanks!",
            "If you want to support the project: [Donate](monero:\(xmr)). Cheers": "If you want to support the project: Donate XMR\u{00A0}8A3ZWA…\u{2060}Yq1c. Cheers",
            "On Ethereum: \(eth)": "On Ethereum: ⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain)",
            "Or this exact amount: monero:\(xmr)?tx_amount=0.25&recipient_name=SimpleX": "Or this exact amount: SimpleX XMR\u{00A0}0.25",
            "Other networks: t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg, ltc1q2gy6f02nn6vvcxs0pnu29tpnpyz0qf66snwfdv, D99pV4n9TrPxBPCkQGx4w4SMSa6QjRBxPf, qq6c8vfvxqrk6rhdysgvkhqc24sggkfsx5nqvdlqcg, solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu":
                "Other networks: ZEC\u{00A0}t1fwjQ…\u{2060}S5Jg, LTC\u{00A0}ltc1q2…\u{2060}wfdv, DOGE\u{00A0}D99pV4…\u{2060}BxPf, BCH\u{00A0}qq6c8v…\u{2060}lqcg, SOL\u{00A0}7JCf5m…\u{2060}ARnu",
            "Suggested donation: bitcoin:\(btc)?amount=0.001&label=SimpleX Thank you!": "Suggested donation: SimpleX ₿\u{00A0}0.001 Thank you!",
            "[Donate](monero:\(xmr))": "Donate XMR\u{00A0}8A3ZWA…\u{2060}Yq1c",
            labeledToken: "Dinner USDT\u{00A0}10 (Polygon)",
            labeledUnknownChain: "Donate ⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain)",
            "[\(btc)](bitcoin:\(btc))": "₿\u{00A0}bc1q2g…\u{2060}5d4u",
            btc: "₿\u{00A0}bc1q2g…\u{2060}5d4u",
            "bitcoin:\(btc)?amount=0.001&label=SimpleX&message=Thank%20you": "SimpleX ₿\u{00A0}0.001",
            "ethereum:0x1111111111111111111111111111111111111111@137/transfer?address=\(eth)&uint256=10000000": "⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown token)",
            "ethereum:\(eth)@100?value=1e16": "⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain)",
            "ethereum:0xc2132D05D31c914a87C6611C10748AEb04B58e8F@137/transfer?address=\(eth)&uint256=10000000": "USDT\u{00A0}10 (Polygon)",
            "ethereum:0xd7047fe3eecb2f2ff78d839dd927be27bc12c86a": "⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain)",
            "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@1/transfer?address=\(eth)&uint256=5000000": "USDT\u{00A0}5",
            "ethereum:0xdAC17F958D2ee523a2206206994597C13D831ec7@99999/transfer?address=\(eth)&uint256=10000000": "⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain and token)",
            "flux:t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg?amount=5&message=Thanks": "FLUX\u{00A0}5",
            "lightning:LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS": "ϟ\u{00A0}lnurl1…\u{2060}5fns",
            "lightning:lno1pgx9getnwss8vetrw3hhyuckyypwa3eyt44h6txtxquqh7lz5djge4afgfjn7k4rgrkuag0jsd5xvxg": "ϟ\u{00A0}lno1pg…\u{2060}xvxg",
            invoice: "ϟ\u{00A0}0.0025",
            "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=0.5": "SOL\u{00A0}0.5",
            "solana:7JCf5m3TiHmYKZVr6jCu1KeZVtb9Y1jRMQDU69p5ARnu?amount=10&spl-token=EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v": "USDC\u{00A0}10 (Solana)",
            "za19w5tpzrsd8x250l6h8qdxv70kg4an9ywljf4cqs5qk2hkw5ej5jw0zc0htg7qxws8vgm6xs3pj3": "FLUX\u{00A0}za19w5…\u{2060}3pj3",
            "zel:t1fwjQW5gpFhDqXNhxqDWyF9j9WeKvVS5Jg?amount=5": "FLUX\u{00A0}5"
        ]
        XCTAssertEqual(shown, expected)
    }

    func testUnknownCurrencyIsOrange() throws {
        let markdown = try recordedMarkdown()
        let bitcoin = draw(try XCTUnwrap(markdown["Donations: \(btc). Cheers"])).string
        let unknownChain = draw(try XCTUnwrap(markdown["On Ethereum: \(eth)"])).string
        XCTAssertEqual(linkColors(bitcoin), [uiLinkColor])
        XCTAssertEqual(linkColors(unknownChain), [.systemOrange])
        let sign = (bitcoin.string as NSString).range(of: "₿").location
        XCTAssertNil(bitcoin.attribute(.underlineStyle, at: sign, effectiveRange: nil))
        XCTAssertNotNil(bitcoin.attribute(.underlineStyle, at: sign + 2, effectiveRange: nil))
    }

    func testSignAfterLabelIsUnderlinedAndChainNameIsItalic() throws {
        let markdown = try recordedMarkdown()
        let labeled = draw(try XCTUnwrap(markdown["<bitcoin:\(btc)?amount=0.001&label=SimpleX>"])).string
        let chain = draw(try XCTUnwrap(markdown[labeledToken])).string
        XCTAssertNotNil(labeled.attribute(.underlineStyle, at: (labeled.string as NSString).range(of: "₿").location, effectiveRange: nil))
        let chainFont = try XCTUnwrap(chain.attribute(.font, at: (chain.string as NSString).range(of: "(Polygon)").location, effectiveRange: nil) as? UIFont)
        XCTAssertTrue(chainFont.fontDescriptor.symbolicTraits.contains(.traitItalic))
    }

    func testNotSignedKeepsLabelAndUnknownReason() throws {
        let markdown = try recordedMarkdown()
        let unsigned = ChainAddressSender(name: "alice", forwarded: false, verified: false)
        let drawn = try ["<bitcoin:\(btc)?amount=0.001&label=SimpleX>", "ethereum:\(eth)@100?value=1e16"].map { draw(try XCTUnwrap(markdown[$0]), unsigned).string.string }
        XCTAssertEqual(drawn, ["SimpleX ⚠\u{FE0E}\u{00A0}bc1q2g…\u{2060}5d4u (not signed)", "⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain, not signed)"])
    }

    @MainActor
    func testGroupAddressWithoutVerifiedSignatureIsNotSigned() async throws {
        try recorded("chain-addresses/group")
        let (chat, _) = try await apiGetChat(chatId: "#1", scope: nil, pagination: .last(count: 2))
        let drawn = chat.chatItems.map { draw($0.formattedText, chainAddressSender($0, chat.chatInfo)).string }
        XCTAssertEqual(drawn.map(\.string), ["donate ₿\u{00A0}bc1q2g…\u{2060}5d4u", "thanks, or ⚠\u{FE0E}\u{00A0}ltc1q2…\u{2060}wfdv (not signed)"])
        XCTAssertEqual(drawn.map(linkColors), [[uiLinkColor], [.systemOrange]])
        XCTAssertEqual(drawn.flatMap(taps).map(alertMessage), ["alice: ₿\u{00A0}bc1q2g…\u{2060}5d4u", "alice: LTC\u{00A0}ltc1q2…\u{2060}wfdv"])
    }

    @MainActor
    func testAlertNamesSenderLinkAndAddress() async throws {
        try recorded("chain-addresses/links")
        let (chat, _) = try await apiGetChat(chatId: "@2", scope: nil, pagination: .last(count: 1))
        let item = try XCTUnwrap(chat.chatItems.first)
        let r = draw(item.formattedText, chainAddressSender(item, chat.chatInfo))
        XCTAssertTrue(r.handleTaps)
        XCTAssertEqual(taps(r.string).map(alertMessage), ["alice: ₿\u{00A0}bc1q2g…\u{2060}5d4u", "alice: Donate XMR\u{00A0}8A3ZWA…\u{2060}Yq1c", "alice: ϟ\u{00A0}0.0025"])
        let markdown = try recordedMarkdown()
        let alice = ChainAddressSender(name: "alice", forwarded: false, verified: true)
        let forwarded = ChainAddressSender(name: "alice", forwarded: true, verified: true)
        let messages = try [
            ("<bitcoin:\(btc)?amount=0.001&label=SimpleX>", alice),
            ("<bitcoin:\(btc)?amount=0.001&label=SimpleX>", forwarded),
            ("ethereum:0xc2132D05D31c914a87C6611C10748AEb04B58e8F@137/transfer?address=\(eth)&uint256=10000000", alice),
            ("ethereum:\(eth)@100?value=1e16", alice),
            ("lightning:LNURL1DP68GURN8GHJ7UM9WFMXJCM99E3K7MF0V9CXJ0M385EKVCENXC6R2C35XVUKXEFCV5MKVV34X5EKZD3EV56NYD3HXQURZEPEXEJXXEPNXSCRVWFNV9NXZCN9XQ6XYEFHVGCXXCMYXYMNSERXFQ5FNS", alice)
        ].flatMap { key, sender in taps(draw(try XCTUnwrap(markdown[key]), sender).string).map(alertMessage) }
        XCTAssertEqual(messages, [
            "alice: SimpleX ₿\u{00A0}0.001 to bc1q2g…\u{2060}5d4u",
            "alice forwarded: SimpleX ₿\u{00A0}0.001 to bc1q2g…\u{2060}5d4u",
            "alice: USDT\u{00A0}10 (Polygon) to 0xD704…\u{2060}c86a",
            "alice: ⚠\u{FE0E}\u{00A0}0xD704…\u{2060}c86a (unknown chain)",
            "alice: ϟ\u{00A0}lnurl1…\u{2060}5fns"
        ])
    }

    @MainActor
    func testOpenWithoutWalletOffersCopyAndQRCode() async throws {
        try recorded("chain-addresses/links")
        let (chat, _) = try await apiGetChat(chatId: "@2", scope: nil, pagination: .last(count: 1))
        let item = try XCTUnwrap(chat.chatItems.first)
        let tap = try XCTUnwrap(taps(draw(item.formattedText, chainAddressSender(item, chat.chatInfo)).string).first)
        getTopViewController()?.present(openWalletAlert(tap), animated: false)
        try await assertTexts(["Open in wallet?", "alice: ₿\u{00A0}bc1q2g…\u{2060}5d4u", "Cancel", "Open"])
        try await press("Open")
        try await assertTexts(["No wallet app", "Copy the address or scan the QR code with a wallet.", "Copy", "QR code", "Cancel"])
        try await press("Copy")
        XCTAssertEqual(UIPasteboard.general.string, btc)
    }

    private func recordedMarkdown() throws -> [String: [FormattedText]?] {
        let markdown = try XCTUnwrap(try fixtureJSON("chain-addresses/links") as? [String: Any])["markdown"] as? [String: Any]
        return try XCTUnwrap(markdown).mapValues { parsed in
            try jsonDecoder.decode(ParsedMarkdown.self, from: JSONSerialization.data(withJSONObject: singleFieldJSON(parsed))).formattedText
        }
    }

    private func draw(_ formattedText: [FormattedText]?, _ sender: ChainAddressSender? = nil) -> MsgTextResult {
        messageText("", formattedText, sender: nil, mentions: nil, userMemberId: nil, chainAddressSender: sender, showSecrets: nil, backgroundColor: .white)
    }

    private func linkColors(_ s: NSAttributedString) -> [UIColor] {
        var colors: [UIColor] = []
        s.enumerateAttribute(.foregroundColor, in: NSRange(location: 0, length: s.length)) { color, _, _ in
            if let color = color as? UIColor, color != UIColor.label { colors.append(color) }
        }
        return colors
    }

    private func taps(_ s: NSAttributedString) -> [ChainAddressTap] {
        var taps: [ChainAddressTap] = []
        var linkEnd = -1
        s.enumerateAttribute(chainAddressAttrKey, in: NSRange(location: 0, length: s.length)) { tap, range, _ in
            if let tap = tap as? ChainAddressTap {
                if range.location != linkEnd { taps.append(tap) }
                linkEnd = range.location + range.length
            }
        }
        return taps
    }

    private func alertMessage(_ tap: ChainAddressTap) -> String {
        openWalletAlert(tap).message ?? ""
    }
}
