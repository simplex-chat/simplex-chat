//
//  WrongQRCode.swift
//  SimpleX
//
//  iOS mirror of the Kotlin WrongQRCode.kt (apps/multiplatform/.../views/newchat/WrongQRCode.kt).
//
//  NOTE: written without an Xcode/macOS toolchain and NOT compiled here — build/verify on CI or a Mac.
//

import Foundation
import SwiftUI
import SimpleXChat

// Message shown when a scanner (or the Migrate paste field) is handed a valid
// SimpleX code of a kind it does not accept: name what it actually is and where
// to use it. The type comes from the core classifier (checkLink), so this is
// purely presentation — no parsing here. iOS uses %@ (not %s) for the substitution.
func wrongQRCodeMessage(_ type: ScannedLinkType) -> String {
    switch type {
    case let .connection(linkType):
        if linkType == .relay {
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@. To use it, open Network & servers, Your servers, Add server, then Chat relay, and paste the address there.", comment: "wrong QR code alert"), linkType.description)
        } else {
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@. To use it, open New chat, then scan or paste it there.", comment: "wrong QR code alert"), linkType.description)
        }
    case .server:
        return NSLocalizedString("This is a SimpleX server address. To use it, open Network & servers, Your servers, Add server, then Scan server QR code.", comment: "wrong QR code alert")
    case .fileDescription:
        return NSLocalizedString("This is a link to migrate to another device. To use it, when setting up a new device, choose to migrate from another device.", comment: "wrong QR code alert")
    case .desktopCtrl:
        return NSLocalizedString("This is an address to connect to a desktop app. To use it, open Use from desktop and scan the QR code shown in the desktop app.", comment: "wrong QR code alert")
    case .verificationCode:
        return NSLocalizedString("This is a security code. To use it, open the chat, then the contact's or member's name, then Verify security code.", comment: "wrong QR code alert")
    }
}

// The shared "Wrong QR code" alert. Callers present it through their own alert
// state so it appears within the scanner's presentation (a global/root alert
// may not show over a modal scanner).
func wrongQRCodeAlert(_ message: String) -> Alert {
    Alert(title: Text("Wrong QR code"), message: Text(verbatim: message))
}
