//
//  GroupMemberInfoView.swift
//  SimpleX (iOS)
//
//  Created by JRoberts on 25.07.2022.
//  Copyright © 2022 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct GroupMemberInfoView: View {
    @EnvironmentObject var chatModel: ChatModel
    var member: GroupMember

    var body: some View {
        VStack{
            ProfileImage(imageStr: member.image)
                .frame(width: 192, height: 192)
                .padding(.top, 48)
                .padding()
            Text(member.localDisplayName).font(.largeTitle)
                .padding(.bottom, 2)
            Text(member.fullName).font(.title)
                .padding(.bottom)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .top)
    }
}

struct GroupMemberInfoView_Previews: PreviewProvider {
    static var previews: some View {
        GroupMemberInfoView(member: GroupMember.sampleData)
    }
}
