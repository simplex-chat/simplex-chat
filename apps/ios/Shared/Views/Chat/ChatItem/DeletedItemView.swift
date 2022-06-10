//
//  FramedItemView.swift
//  SimpleX
//
//  Created by JRoberts on 04/02/2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChatSDK

struct DeletedItemView: View {
    var chatItem: ChatItem
    var showMember = false

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            if showMember, let member = chatItem.memberDisplayName {
                Text(member).fontWeight(.medium) + Text(": ")
            }
            Text(chatItem.content.text)
                .foregroundColor(.secondary)
                .italic()
            CIMetaView(chatItem: chatItem)
                .padding(.horizontal, 12)
        }
        .padding(.leading, 12)
        .padding(.vertical, 6)
        .background(Color(uiColor: .tertiarySystemGroupedBackground))
        .cornerRadius(18)
        .textSelection(.disabled)
    }
}

struct DeletedItemView_Previews: PreviewProvider {
    static var previews: some View {
        Group {
            DeletedItemView(chatItem: ChatItem.getDeletedContentSample())
            DeletedItemView(
                chatItem: ChatItem.getDeletedContentSample(dir: .groupRcv(groupMember: GroupMember.sampleData)),
                showMember: true
            )
        }
        .previewLayout(.fixed(width: 360, height: 200))
    }
}
