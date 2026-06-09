//
//  QRCodeType.swift
//  SimpleX
//
//  iOS mirror of the Kotlin unified QR classifier
//  (apps/multiplatform/.../views/newchat/QRCodeType.kt).
//
//  NOTE: this Swift was written without an Xcode/macOS toolchain available and has
//  NOT been compiled. It needs to be built/verified on CI or a Mac. The iOS
//  "where to scan" wording in QRCodeScan.swift should also be reviewed against the
//  real iOS navigation.
//

import Foundation
import SimpleXChat

// What a scanner accepts. No KClass on Swift; the verify-code screen accepts `.securityCode`.
enum QRCodeKind {
    case connectionLink, serverAddress, migrationLink, desktopAddress, securityCode, unknown
}

// One unified type for every kind of QR code (or pasted string) the app can scan.
enum QRCodeType {
    case connectionLink(text: String, linkType: SimplexLinkType)
    case serverAddress(text: String)
    case migrationLink(text: String)
    case desktopAddress(text: String)
    case securityCode(text: String)
    case unknown(text: String)

    var text: String {
        switch self {
        case let .connectionLink(text, _): return text
        case let .serverAddress(text): return text
        case let .migrationLink(text): return text
        case let .desktopAddress(text): return text
        case let .securityCode(text): return text
        case let .unknown(text): return text
        }
    }

    var kind: QRCodeKind {
        switch self {
        case .connectionLink: return .connectionLink
        case .serverAddress: return .serverAddress
        case .migrationLink: return .migrationLink
        case .desktopAddress: return .desktopAddress
        case .securityCode: return .securityCode
        case .unknown: return .unknown
        }
    }
}

let desktopAddressScheme = "xrcp:/"

// Migration file-link check (moved here from a private method in MigrateToDevice so the
// classifier can reuse it).
func strHasSimplexFileLink(_ text: String) -> Bool {
    text.starts(with: "simplex:/file") || text.starts(with: "https://simplex.chat/file")
}

// Classify a scanned/pasted string by composing the existing local parsers in priority
// order, returning the first match, else `.unknown`.
func parseQRCode(_ raw: String) -> QRCodeType {
    let t = raw.trimmingCharacters(in: .whitespacesAndNewlines)
    if strHasSimplexFileLink(t) { return .migrationLink(text: t) }
    if t.hasPrefix(desktopAddressScheme) { return .desktopAddress(text: t) }
    // Match only when the WHOLE string is exactly one SimpleX link — same as `strIsSimplexLink`
    // and the master connect scanner — not a link embedded among other text. A SimpleX *name*
    // (Format.simplexName) is therefore not a connectionLink here; it falls through to .unknown,
    // exactly as the master scanner treats it (names are handled only by the paste path).
    if let md = parseSimpleXMarkdown(t), md.count == 1,
       case let .simplexLink(_, linkType, _, _) = md[0].format {
        return .connectionLink(text: t, linkType: linkType)
    }
    if parseServerAddress(t) != nil { return .serverAddress(text: t) }
    if isSecurityCode(t) { return .securityCode(text: t) }
    return .unknown(text: t)
}

// A contact's security / verification code QR encodes the raw code with no scheme. The core builds
// it as `verificationCode = T.pack . unwords . chunks 5 . show . os2ip` — decimal digits of a SHA
// integer, in groups (of 5) separated by spaces (e.g. "61889 38426 ... 25"). Recognise it by that
// exact shape: 2+ whitespace-separated groups of ASCII decimal digits, 32+ digits total. Requiring
// the grouping keeps a bare long number from being mistaken for a code. Checked last; the scanned
// text keeps its spaces, so verifyCode still matches.
func isSecurityCode(_ t: String) -> Bool {
    let groups = t.split(whereSeparator: { $0.isWhitespace })
    return groups.count >= 2
        && groups.reduce(0) { $0 + $1.count } >= 32
        && groups.allSatisfy { $0.allSatisfy { $0.isNumber && $0.isASCII } }
}
