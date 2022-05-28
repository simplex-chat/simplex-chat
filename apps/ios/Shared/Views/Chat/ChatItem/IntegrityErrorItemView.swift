//
//  IntegrityErrorItemView.swift
//  SimpleX (iOS)
//
//  Created by Evgeny on 28/05/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct IntegrityErrorItemView: View {
    var chatItem: ChatItem
    var showMember = false

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
        .onTapGesture { skippedMessagesAlert() }
    }

    private func skippedMessagesAlert() {
        AlertManager.shared.showAlertMsg(
            title: "Skipped messages",
            message: """
                It can happen when:
                1. The messages expire on the server if they were not received for 30 days,
                2. The server you use to receive the messages from this contact was updated and restarted.
                3. The connection is compromised.
                Please connect to the developers via Settings to receive the updates.
                We will be adding server redundancy to prevent lost messages.
                """
        )
    }
}

struct IntegrityErrorItemView_Previews: PreviewProvider {
    static var previews: some View {
        IntegrityErrorItemView(chatItem: ChatItem.getIntegrityErrorSample())
    }
}
