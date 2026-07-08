//
//  WrongQRCode.swift
//  SimpleX
//
//  iOS mirror of the Kotlin WrongQRCode.kt (apps/multiplatform/.../views/newchat/WrongQRCode.kt).
//
//  NOTE: written without an Xcode/macOS toolchain and NOT compiled here — build/verify on CI or a
//  Mac. The "where to scan" wording should be reviewed against the real iOS navigation labels.
//

import Foundation
import SwiftUI
import SimpleXChat

// The kinds of QR code the app knows how to scan, apart from a contact's security code (which has
// no scheme and cannot be recognised by shape). Each scanner expects one of these; when a scanner's
// own parser rejects a scan, identifyQRCode reports which of the others it turned out to be.
private enum ScannedQRCode {
    case connectionLink(SimplexLinkType)
    case serverAddress
    case migrationLink
    case desktopAddress
    case securityCode

    // "This is a <kind>." — names what was actually scanned.
    var typeDescription: String {
        switch self {
        case let .connectionLink(linkType):
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@.", comment: "wrong QR code alert"), linkType.description)
        case .serverAddress: return NSLocalizedString("This is a SimpleX server address.", comment: "wrong QR code alert")
        case .migrationLink: return NSLocalizedString("This is a link to migrate to another device.", comment: "wrong QR code alert")
        case .desktopAddress: return NSLocalizedString("This is an address to connect to a desktop app.", comment: "wrong QR code alert")
        case .securityCode: return NSLocalizedString("This is a contact's security code.", comment: "wrong QR code alert")
        }
    }

    // "<Where to scan it>." — the screen that accepts this kind, or nil when there is no scanner for
    // it (a relay address is a connection link but cannot be used to connect from New chat).
    var whereToScan: String? {
        switch self {
        case let .connectionLink(linkType):
            return linkType == .relay ? nil : NSLocalizedString("To use it, tap New chat, then scan or paste it there.", comment: "wrong QR code alert")
        case .serverAddress: return NSLocalizedString("To use it, open Network & servers, Your servers, then Scan server QR code.", comment: "wrong QR code alert")
        case .migrationLink: return NSLocalizedString("To use it, when setting up a new device, choose to migrate from another device.", comment: "wrong QR code alert")
        case .desktopAddress: return NSLocalizedString("To use it, open Use from desktop, then Scan QR code from desktop.", comment: "wrong QR code alert")
        case .securityCode: return NSLocalizedString("To use it, open the chat, tap the contact's name, then Verify security code.", comment: "wrong QR code alert")
        }
    }
}

// The QR-encoded form of a desktop session address always starts with this scheme
// (single slash; see simplexmq RCSignedInvitation encoding).
let desktopAddressScheme = "xrcp:/"

func strHasSimplexFileLink(_ text: String) -> Bool {
    text.starts(with: "simplex:/file") || text.starts(with: "https://simplex.chat/file")
}

// The "megaparser": try every known parser and return the first match, or nil when the scan is not
// a SimpleX QR code at all. No new parsing logic — the same parsers each scanner already uses.
private func identifyQRCode(_ text: String, detectSecurityCode: Bool) -> ScannedQRCode? {
    let t = text.trimmingCharacters(in: .whitespacesAndNewlines)
    if let md = parseSimpleXMarkdown(t), md.count == 1,
       case let .simplexLink(_, linkType, _, _) = md[0].format {
        return .connectionLink(linkType)
    }
    if parseServerAddress(t) != nil { return .serverAddress }
    if strHasSimplexFileLink(t) { return .migrationLink }
    if t.hasPrefix(desktopAddressScheme) { return .desktopAddress }
    if detectSecurityCode && isSecurityCode(t) { return .securityCode }
    return nil
}

// A contact's security/verification code has no scheme; the QR encodes it verbatim. The core builds
// it as `verificationCode = unwords . chunks 5 . show . os2ip` — the decimal digits of a hash in
// space-separated groups of 5 (e.g. "61889 38426 ... 25"), ~77 digits. Recognise that exact shape:
// 2+ whitespace-separated groups of ASCII digits, 32+ digits total. Requiring the grouping keeps a
// bare long number from matching. Checked last, after every scheme-based parser.
private func isSecurityCode(_ t: String) -> Bool {
    // ASCII whitespace only, to match the Kotlin `\s+` split (Java \s does not include
    // Unicode spaces like NBSP) — both platforms must classify identically.
    let groups = t.split(whereSeparator: { $0.isWhitespace && $0.isASCII })
    return groups.count >= 2
        && groups.reduce(0) { $0 + $1.count } >= 32
        && groups.allSatisfy { $0.allSatisfy { $0.isNumber && $0.isASCII } }
}

// For a scan the current screen rejected: "This is X.\n\nTo use it, scan it there." when it is
// recognised as another known kind, or nil when it is not a SimpleX QR code at all (the caller
// then shows its own "unknown" message). Each caller presents this through its OWN alert state so
// the alert appears within the scanner's presentation (a global/root alert may not show over it).
func wrongQRCodeMessage(_ text: String, detectSecurityCode: Bool = true) -> String? {
    guard let qr = identifyQRCode(text, detectSecurityCode: detectSecurityCode) else { return nil }
    return [qr.typeDescription, qr.whereToScan].compactMap { $0 }.joined(separator: "\n\n")
}

// The shared "not a SimpleX QR code" message for the unrecognised case, for scanners that have no
// more specific wording of their own.
func notSimplexQRCodeMessage() -> String {
    NSLocalizedString("The code you scanned is not a SimpleX link QR code.", comment: "wrong QR code alert")
}

// The shared "Wrong QR code" alert. Callers present it through their own alert state so it appears
// within the scanner's presentation (a global/root alert may not show over a modal scanner).
func wrongQRCodeAlert(_ message: String) -> Alert {
    Alert(title: Text("Wrong QR code"), message: Text(verbatim: message))
}
