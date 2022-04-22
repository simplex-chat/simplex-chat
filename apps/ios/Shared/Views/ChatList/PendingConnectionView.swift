//
//  PendingConnectionView.swift
//  SimpleX (iOS)
//
//  Created by Ian Davies on 22/04/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI

struct PendingConnectionView: View {
    var chat: Chat

    var body: some View {
        return HStack(spacing: 8) {
            ChatInfoImage(chat: chat)
                .frame(width: 63, height: 63)
            VStack(alignment: .leading, spacing: 4) {
                HStack(alignment: .top) {
                    Text(chat.chatInfo.localDisplayName)
                        .font(.title3)
                        .fontWeight(.bold)
                        .foregroundColor(.blue)
                        .padding(.leading, 8)
                        .padding(.top, 4)
                        .frame(maxHeight: .infinity, alignment: .topLeading)
                    Spacer()
                    timestampText(chat.chatInfo.createdAt)
                        .font(.subheadline)
                        .padding(.trailing, 8)
                        .padding(.top, 4)
                        .frame(minWidth: 60, alignment: .trailing)
                        .foregroundColor(.secondary)
                }
                Text("Contact connection pending…")
                    .frame(minHeight: 44, maxHeight: 44, alignment: .topLeading)
                    .padding([.leading, .trailing], 8)
                    .padding(.bottom, 4)
                    .padding(.top, 1)
            }
        }
    }
}

struct PendingConnectionView_Previews: PreviewProvider {
    static var previews: some View {
        let contact = Contact.init(
            contactId: 0,
            localDisplayName: "Alice",
            profile: Profile.sampleData,
            activeConn: Connection(connStatus: "connecting"),
            createdAt: .now
        )
        let cInfo = ChatInfo.direct(contact: contact)
        PendingConnectionView(chat: Chat(
            chatInfo: cInfo,
            chatItems: []
        ))
            .previewLayout(.fixed(width: 360, height: 80))
    }
}
