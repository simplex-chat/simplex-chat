//
//  CIRcvDecryptionError.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

let decryptErrorReason: LocalizedStringKey = "It can happen when you or your connection used the old database backup."

struct CIRcvDecryptionError: View {
    var msgDecryptError: MsgDecryptError
    var msgCount: UInt32
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        CIMsgError(chatItem: chatItem, showMember: showMember) {
            var message: Text
            let why = Text(decryptErrorReason)
            let permanent = Text("This error is permanent for this connection, please re-connect.")
            switch msgDecryptError {
            case .ratchetHeader:
                message = Text("\(msgCount) messages failed to decrypt.") + Text("\n") + why + Text("\n") + permanent
            case .earlier:
                message = Text("\(msgCount) messages failed to decrypt and won't be shown.") + Text("\n") + why
            case .tooManySkipped:
                message = Text("\(msgCount) messages skipped.") + Text("\n") + why + Text("\n") + permanent
            }
            AlertManager.shared.showAlert(Alert(title: Text("Decryption error"), message: message))
        }
    }
}

//struct CIRcvDecryptionError_Previews: PreviewProvider {
//    static var previews: some View {
//        CIRcvDecryptionError(msgDecryptError: .ratchetHeader, msgCount: 1, chatItem: ChatItem.getIntegrityErrorSample())
//    }
//}
