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
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    var msgError: MsgErrorType
    var chatItem: ChatItem

    var body: some View {
        CIMsgError(chat: chat, chatItem: chatItem) {
            switch msgError {
            case .msgSkipped:
                AlertManager.shared.showAlertMsg(
                    title: "Skipped messages",
                    message: """
                        It can happen when:
                        1. The messages expired in the sending client after 2 days or on the server after 30 days.
                        2. Message decryption failed, because you or your contact used old database backup.
                        3. The connection was compromised.
                        """
                )
            case .msgBadHash:
                AlertManager.shared.showAlert(Alert(
                    title: Text("Bad message hash"),
                    message: Text("The hash of the previous message is different.") + Text("\n") +
                        Text(decryptErrorReason) + Text("\n") +
                        Text("Please report it to the developers.")
                ))
            case .msgBadId: msgBadIdAlert()
            case .msgDuplicate: msgBadIdAlert()
            }
        }
    }

    private func msgBadIdAlert() {
        AlertManager.shared.showAlert(Alert(
            title: Text("Bad message ID"),
            message: Text("""
                The ID of the next message is incorrect (less or equal to the previous).
                It can happen because of some bug or when the connection is compromised.
                """) + Text("\n") +
                Text("Please report it to the developers.")
        ))
    }
}

struct CIMsgError: View {
    @ObservedObject var chat: Chat
    @EnvironmentObject var theme: AppTheme
    var chatItem: ChatItem
    var onTap: () -> Void

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            Text(chatItem.content.text)
                .foregroundColor(.red)
                .italic()
            CIMetaView(chat: chat, chatItem: chatItem, metaColor: theme.colors.secondary)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .textSelection(.disabled)
        .onTapGesture(perform: onTap)
    }
}

struct IntegrityErrorItemView_Previews: PreviewProvider {
    static var previews: some View {
        IntegrityErrorItemView(chat: Chat.sampleData, msgError: .msgBadHash, chatItem: ChatItem.getIntegrityErrorSample())
    }
}
