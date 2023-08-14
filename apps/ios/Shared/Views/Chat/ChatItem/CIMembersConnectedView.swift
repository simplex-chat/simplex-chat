//
//  CIMembersConnectedView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.08.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct CIMembersConnectedView: View {
    @EnvironmentObject var chat: Chat
    var chatItem: ChatItem
    var members: [GroupMember]
    @State private var selectedMember: GroupMember? = nil

    var body: some View {
        HStack(alignment: .bottom, spacing: 0) {
            chatEventText(chatItem)
        }
        .padding(.leading, 6)
        .padding(.bottom, 6)
        .textSelection(.disabled)
    }
}

struct CIMembersConnectedView_Previews: PreviewProvider {
    static var previews: some View {
        CIMembersConnectedView(
            chatItem: ChatItem.getMembersConnectedSample(),
            members: [GroupMember.sampleData, GroupMember.sampleData, GroupMember.sampleData]
        )
    }
}
