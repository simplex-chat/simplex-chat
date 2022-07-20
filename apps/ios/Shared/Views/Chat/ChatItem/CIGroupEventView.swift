//
//  CIGroupEventView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 20.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIGroupEventView: View {
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            if showMember, let member = chatItem.memberDisplayName {
                Text(member)
                    .foregroundColor(.secondary)
                    .italic()
                + Text(" ")
            }
            Text(chatItem.content.text)
                .foregroundColor(.secondary)
                .italic()
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .textSelection(.disabled)
    }
}

struct CIGroupEventView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            CIGroupEventView(chatItem: ChatItem.getGroupEventSample(), showMember: true)
            CIGroupEventView(chatItem: ChatItem.getGroupEventSample())
        }
    }
}
