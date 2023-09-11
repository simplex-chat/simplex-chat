//
//  ContextDirectMemberView.swift
//  SimpleX (iOS)
//
//  Created by spaced4ndy on 11.09.2023.
//  Copyright © 2023 SimpleX Chat. All rights reserved.
//

import SwiftUI
import SimpleXChat

struct ContextDirectMemberView: View {
    @Environment(\.colorScheme) var colorScheme
    let directMember: GroupMember?
    let cancelDirectMemberContext: () -> Void

    var body: some View {
        HStack {
            if let directMember = directMember {
                ProfileImage(imageStr: directMember.image)
                    .frame(width: 30, height: 30)
                    .padding(.trailing, 2)
                Text("Directly to \(directMember.chatViewName)")
                    .lineLimit(1)
            } else {
                Text("Message will be sent to all members")
            }
            Spacer()
            Button {
                withAnimation {
                    cancelDirectMemberContext()
                }
            } label: {
                Image(systemName: "multiply")
            }
        }
        .padding(12)
        .frame(minHeight: 50)
        .frame(maxWidth: .infinity)
        .background(colorScheme == .light ? sentColorLight : sentColorDark)
        .padding(.top, 8)
    }
}

struct ContextDirectMemberView_Previews: PreviewProvider {
    static var previews: some View {
        ContextDirectMemberView(directMember: GroupMember.sampleData, cancelDirectMemberContext: {})
    }
}
