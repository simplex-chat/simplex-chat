//
//  IntegrityErrorItemView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 28/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct IntegrityErrorItemView: View {
    var msgError: MsgErrorType
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        ErrorItemView(chatItem: chatItem, showMember: showMember) {
            switch msgError {
            case .msgSkipped: skippedMessagesAlert()
            case .msgBadHash: msgBadHashAlert()
            case .msgBadId: msgBadIdAlert()
            case .msgDuplicate: msgBadIdAlert()
            }
        }
    }

    private func skippedMessagesAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Skipped messages",
            message: "It can happen when the messages expire on the server after 30 days (or in the sending client after 2 days), or when decryotion was out of sync, or, less likely, when the connection was compromised."
        )
    }

    private func msgBadHashAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Bad message hash",
            message: "The hash of the previous message is different. It can happen when decryption was out of sync or when the connection was compromised. Please report it to the developers."
        )
    }

    private func msgBadIdAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Wrong message ID",
            message: "The ID of the next message is incorrect (less or equal to the previous. It can happen because of some malfunction or when the connection is compromised. Please report it to the developers."
        )
    }
}

struct ErrorItemView: View {
    var chatItem: ChatItem
    var showMember = false
    var onTap: () -> Void

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            if showMember, let member = chatItem.memberDisplayName {
                Text(member).fontWeight(.medium) + Text(": ")
            }
            Text(chatItem.content.text)
                .foregroundColor(.red)
                .italic()
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
        .onTapGesture(perform: onTap)
    }
}

struct IntegrityErrorItemView_Previews: PreviewProvider {
    static var previews: some View {
        IntegrityErrorItemView(msgError: .msgBadHash, chatItem: ChatItem.getIntegrityErrorSample())
    }
}
