//
//  QRCodeScan.swift
//  SimpleX
//
//  iOS mirror of the Kotlin handleScan + wrong-type alert (QRCodeScan.kt).
//
//  NOTE: NOT compiled in this environment — needs Xcode/CI verification. The "where to
//  scan" breadcrumbs use best-effort iOS navigation wording and should be reviewed.
//

import SwiftUI
import SimpleXChat

// The shared gate every scanner routes through. If the scan is the accepted kind, run the
// screen's onMatch; otherwise show the one wrong-type alert. iOS has no Bool return / de-dup
// (the scanner controls re-scanning via scanMode / scannerPaused).
func handleScan(_ raw: String, expected: QRCodeKind, theme: AppTheme, onMatch: (QRCodeType) -> Void) {
    let qr = parseQRCode(raw)
    if qr.kind == expected {
        onMatch(qr)
    } else {
        showWrongQRCodeAlert(qr, expected: expected, theme: theme)
    }
}

private func showWrongQRCodeAlert(_ scanned: QRCodeType, expected: QRCodeKind, theme: AppTheme) {
    // Relay link: it cannot be used to connect — reuse the existing relay alert (title + message).
    if case .connectionLink(_, .relay) = scanned {
        AlertManager.shared.showAlert(mkAlert(
            title: "Relay address",
            message: "This is a chat relay address, it cannot be used to connect."
        ))
        return
    }
    // Unrecognised: we cannot name the kind. Keep the existing "invalid" wording, expected-aware
    // (the server screen keeps its own, more specific "Invalid server address!" title).
    if case .unknown = scanned {
        let forServer = expected == .serverAddress
        AlertManager.shared.showAlert(mkAlert(
            title: forServer ? "Invalid server address!" : "Invalid QR code",
            message: forServer
                ? "Check server address and try again."
                : "The code you scanned is not a SimpleX link QR code."
        ))
        return
    }
    // Recognised wrong kind: "<what it is>\n\n<where to scan it>".
    let hasUser = ChatModel.shared.currentUser != nil
    let whereTo = hasUser
        ? scanned.whereToScanText
        : NSLocalizedString("Finish setting up SimpleX first, then scan this in the app.", comment: "qr where to scan")
    let message = [scanned.kindName, whereTo].compactMap { $0 }.filter { !$0.isEmpty }.joined(separator: "\n\n")
    // The only safe action is "Connect" for a connection link, and only when a user exists.
    if hasUser, case let .connectionLink(text, _) = scanned {
        AlertManager.shared.showAlert(Alert(
            title: Text("Wrong QR code"),
            message: Text(verbatim: message),
            primaryButton: .default(Text("Connect")) {
                planAndConnect(text, theme: theme, dismiss: true)
            },
            secondaryButton: .cancel()
        ))
    } else {
        AlertManager.shared.showAlert(Alert(
            title: Text("Wrong QR code"),
            message: Text(verbatim: message)
        ))
    }
}

private extension QRCodeType {
    // A short, complete sentence naming the kind (ends with a period so it reads as its own line,
    // not a fragment running into the instruction). Reuses SimplexLinkType.description for links.
    var kindName: String {
        switch self {
        case let .connectionLink(_, linkType):
            if let lt = linkType { return String(format: NSLocalizedString("This is a %@.", comment: "qr type"), lt.description) }
            return NSLocalizedString("This is a SimpleX address.", comment: "qr type")
        case .serverAddress: return NSLocalizedString("This is a SimpleX server address.", comment: "qr type")
        case .migrationLink: return NSLocalizedString("This is a link to migrate to another device.", comment: "qr type")
        case .desktopAddress: return NSLocalizedString("This is an address for linking a mobile to a SimpleX desktop app.", comment: "qr type")
        case .securityCode: return NSLocalizedString("This is a contact's security code.", comment: "qr type")
        case .unknown: return ""
        }
    }

    // Where to scan this kind. Best-effort iOS navigation wording — review against the app.
    var whereToScanText: String? {
        switch self {
        case let .connectionLink(_, linkType):
            return linkType == .relay ? nil : NSLocalizedString("Open New chat, then scan or paste the link.", comment: "qr where to scan")
        case .serverAddress:
            return NSLocalizedString("Open Settings, Network & servers, your servers, then Scan server QR code.", comment: "qr where to scan")
        case .migrationLink:
            return NSLocalizedString("On the new device, when first setting up the app, choose Migrate from another device.", comment: "qr where to scan")
        case .desktopAddress:
            return NSLocalizedString("Open Settings, Use from desktop, then Scan QR code from desktop.", comment: "qr where to scan")
        case .securityCode:
            return NSLocalizedString("Open the chat, tap the contact's name, then Verify security code.", comment: "qr where to scan")
        case .unknown:
            return nil
        }
    }
}
