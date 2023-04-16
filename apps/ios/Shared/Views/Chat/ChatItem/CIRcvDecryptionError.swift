//
//  CIRcvDecryptionError.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 15/04/2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIRcvDecryptionError: View {
    var msgDecryptError: MsgDecryptError
    var msgCount: UInt32
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        ErrorItemView(chatItem: chatItem, showMember: showMember) {
            var message: Text
            let why = chatItem.chatDir.isGroup
                    ? Text("It can happen when you or group member use the old database backup.")
                    : Text("It can happen when you or your contact use the old database backup.")
            let permanent = Text("This error is permanent for this connection, please re-connect.")
            switch msgDecryptError {
            case .ratchetHeader:
                message = Text("\(msgCount) messages failed to decrypt.\n") + why + Text("\n") + permanent
            case .earlier:
                message = Text("\(msgCount) messages failed to decrypt and won't be shown.\n") + why
            case .tooManySkipped:
                message = Text("\(msgCount) messages skipped.\n") + why + Text("\n") + permanent
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
