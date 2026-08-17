import Foundation

// Paying for a SimpleX name. This is the seam where StoreKit goes; until then
// `purchaseFor` returns a development token the mock service accepts. Replacing
// this one type with a StoreKit call is the whole client-side billing change.
enum NamePayment {
    static let isLive = false

    // Receipts paid for but not yet spent, keyed including the remote host so
    // one profile's receipt cannot be handed to another. A registration can
    // fail after the charge; holding the receipt means a retry re-submits it
    // instead of paying twice.
    private static var unspent: [String: String] = [:]

    static func productId(_ years: Int) -> String { "chat.simplex.name.\(years)y" }

    // DEVELOPMENT ONLY: no charge is made and nothing is verified.
    static func purchaseFor(_ key: String, years: Int) -> String? {
        if let t = unspent[key] { return t }
        let t = "dev-mock-payment-\(productId(years))"
        unspent[key] = t
        return t
    }

    static func spent(_ key: String) { unspent[key] = nil }
}
