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
func handleScan(_ raw: String, expected: QRCodeKind, theme: AppTheme, scannerPaused: Binding<Bool>? = nil, onMatch: (QRCodeType) -> Void) {
    let qr = parseQRCode(raw)
    if qr.kind == expected {
        onMatch(qr)
    } else {
        // Pause a continuous scanner while the wrong-type alert is up so it doesn't re-fire the
        // alert every scanInterval; resume on dismissal. oncePerCode scanners pass nil (no-op).
        scannerPaused?.wrappedValue = true
        showWrongQRCodeAlert(qr, expected: expected, theme: theme) { scannerPaused?.wrappedValue = false }
    }
}

private func showWrongQRCodeAlert(_ scanned: QRCodeType, expected: QRCodeKind, theme: AppTheme, onDismiss: @escaping () -> Void) {
    // Relay link: it cannot be used to connect — reuse the existing relay alert (title + message).
    if case .connectionLink(_, .relay) = scanned {
        AlertManager.shared.showAlert(Alert(
            title: Text("Relay address"),
            message: Text("This is a chat relay address, it cannot be used to connect."),
            dismissButton: .default(Text("Ok"), action: onDismiss)
        ))
        return
    }
    // Unrecognised: we cannot name the kind. Keep each screen's own existing "invalid" wording —
    // the server screen says "Invalid server address!", the verify screen says "Incorrect security
    // code!" (it scans a code, not a link), everything else the generic "not a SimpleX link".
    if case .unknown = scanned {
        let title: LocalizedStringKey
        let message: LocalizedStringKey?
        switch expected {
        case .serverAddress: title = "Invalid server address!"; message = "Check server address and try again."
        case .securityCode:  title = "Incorrect security code!"; message = nil
        default:             title = "Invalid QR code";          message = "The code you scanned is not a SimpleX link QR code."
        }
        AlertManager.shared.showAlert(Alert(
            title: Text(title),
            message: message.map { Text($0) },
            dismissButton: .default(Text("Ok"), action: onDismiss)
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
                onDismiss()
                planAndConnect(text, theme: theme, dismiss: true)
            },
            secondaryButton: .cancel(onDismiss)
        ))
    } else {
        AlertManager.shared.showAlert(Alert(
            title: Text("Wrong QR code"),
            message: Text(verbatim: message),
            dismissButton: .default(Text("Ok"), action: onDismiss)
        ))
    }
}

private extension QRCodeType {
    // A short, complete sentence naming the kind (ends with a period so it reads as its own line,
    // not a fragment running into the instruction). Reuses SimplexLinkType.description for links.
    var kindName: String {
        switch self {
        case let .connectionLink(_, linkType):
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@.", comment: "qr type"), linkType.description)
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
            return NSLocalizedString("Tap your profile image, then Settings, Network & servers, Your servers, then Scan server QR code.", comment: "qr where to scan")
        case .migrationLink:
            return NSLocalizedString("On the new device, when first setting up the app, choose Migrate from another device.", comment: "qr where to scan")
        case .desktopAddress:
            return NSLocalizedString("Tap your profile image, then Use from desktop, then Scan QR code from desktop.", comment: "qr where to scan")
        case .securityCode:
            return NSLocalizedString("Open the chat, tap the contact's name, then Verify security code.", comment: "qr where to scan")
        case .unknown:
            return nil
        }
    }
}
