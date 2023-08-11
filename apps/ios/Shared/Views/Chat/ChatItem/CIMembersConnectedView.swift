//
//  CIMembersConnectedView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.08.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat
import WrappingHStack

struct CIMembersConnectedView: View {
    @EnvironmentObject var chat: Chat
    var chatItem: ChatItem
    var members: [GroupMember]
    @State private var selectedMember: GroupMember? = nil

    var body: some View {
        VStack(alignment: .leading) {
            WrappingHStack(0..<members.count, id: \.self, spacing: .constant(8), lineSpacing: 4) { index in
                memberPicture(members[index])
            }
            chatEventText(chatItem)
        }
    }

    @ViewBuilder func memberPicture(_ member: GroupMember) -> some View {
        let v = ProfileImage(imageStr: member.memberProfile.image)
            .frame(width: memberImageSize, height: memberImageSize)
        if case let .group(groupInfo) = chat.chatInfo {
            v
                .onTapGesture { selectedMember = member }
                .appSheet(item: $selectedMember) { member in
                    GroupMemberInfoView(groupInfo: groupInfo, member: member, navigation: true)
                }
        } else {
            v
        }
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
