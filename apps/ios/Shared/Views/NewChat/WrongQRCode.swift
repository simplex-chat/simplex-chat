//
//  WrongQRCode.swift
//  SimpleX
//

import Foundation
import SwiftUI
import SimpleXChat

func wrongQRCodeMessage(_ type: ScannedLinkType) -> String {
    switch type {
    case let .connection(linkType):
        if linkType == .relay {
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@. To use it, open Network & servers, Your servers, Add server, then Chat relay, paste the address, then Test relay.", comment: "wrong QR code alert"), linkType.description)
        } else {
            return String.localizedStringWithFormat(NSLocalizedString("This is a %@. To use it, open New chat, then scan or paste it there.", comment: "wrong QR code alert"), linkType.description)
        }
    case .server:
        return NSLocalizedString("This is a SimpleX server address. To use it, open Network & servers, Your servers, Add server, then Scan server QR code.", comment: "wrong QR code alert")
    case .fileDescription:
        return NSLocalizedString("This is a link to migrate to another device. To use it, choose Migrate when setting up a new device.", comment: "wrong QR code alert")
    case .desktopCtrl:
        return NSLocalizedString("This is an address to connect to a desktop app. To use it, open Use from desktop and scan the QR code shown in the desktop app.", comment: "wrong QR code alert")
    case .verificationCode:
        return NSLocalizedString("This is a security code. To use it, open the chat, then the contact's or member's name, then Verify security code.", comment: "wrong QR code alert")
    }
}

// Returned rather than shown, because a global/root alert may not appear over a modal scanner.
func wrongQRCodeAlert(_ message: String) -> Alert {
    Alert(title: Text("Wrong QR code"), message: Text(verbatim: message))
}
